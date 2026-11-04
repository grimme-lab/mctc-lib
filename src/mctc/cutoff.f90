! This file is part of mctc-lib.
!
! Licensed under the Apache License, Version 2.0 (the "License");
! you may not use this file except in compliance with the License.
! You may obtain a copy of the License at
!
!     http://www.apache.org/licenses/LICENSE-2.0
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

!> @file mctc/cutoff.f90
!> Provides realspace cutoff and lattice generation

!> Realspace cutoff and lattice point generator utilities
!>
!> Periodic directions are selected by a mask over the three lattice vectors,
!> lattice vectors of nonperiodic directions are ignored. A one-element mask,
!> as created for molecular structures, applies to all three directions.
module mctc_cutoff
   use, intrinsic :: iso_fortran_env, only : error_unit
   use mctc_env, only : wp, error_type, fatal_error
   use mctc_io_math, only : matdet_3x3, matinv_3x3
   implicit none
   private

   public :: get_lattice_points, get_periodic_axes, get_inverse_metric, &
      & wrap_to_central_cell


   interface get_lattice_points
      module procedure :: get_lattice_points_cutoff
      module procedure :: get_lattice_points_cutoff_legacy
      module procedure :: get_lattice_points_rep_3d
      module procedure :: get_lattice_points_rep_3d_legacy
   end interface get_lattice_points

   !> Tolerance for detecting linearly dependent periodic lattice vectors
   real(wp), parameter :: tol_deg = 1.0e-12_wp


contains


!> Extract the number and indices of the periodic lattice directions
pure subroutine get_periodic_axes(periodic, ndim, p_axes)

   !> Periodic dimensions, a one-element mask applies to all directions
   logical, intent(in) :: periodic(:)

   !> Number of periodic dimensions (0, 1, 2, or 3)
   integer, intent(out) :: ndim

   !> Indices of the periodic directions in 1..ndim
   integer, intent(out) :: p_axes(3)

   integer :: i
   logical :: active

   ndim = 0
   p_axes(:) = 0
   do i = 1, 3
      if (size(periodic) == 1) then
         active = periodic(1)
      else
         active = periodic(i)
      end if
      if (active) then
         ndim = ndim + 1
         p_axes(ndim) = i
      end if
   end do

end subroutine get_periodic_axes


!> Lattice vectors of the periodic directions and the inverse metric of the
!> periodic subspace.
!>
!> For the periodic lattice vectors A the metric is G = A^T A. A vector r has
!> the fractional coordinates s = G^-1 A^T r within the periodic subspace, and
!> 1 / sqrt(G^-1_kk) is the height of the cell along lattice vector k.
!> Nonperiodic directions have zero entries in both results, such that the
!> same expressions hold for 1D, 2D and 3D periodicity.
pure subroutine get_inverse_metric(periodic, lattice, lat, ginv, error)

   !> Periodic dimensions
   logical, intent(in) :: periodic(:)

   !> Lattice vectors
   real(wp), intent(in) :: lattice(:, :)

   !> Lattice vectors of the periodic directions, zero for nonperiodic ones
   real(wp), intent(out) :: lat(3, 3)

   !> Inverse metric of the periodic subspace, zero for nonperiodic directions
   real(wp), intent(out) :: ginv(3, 3)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   integer :: k, ndim, p_axes(3)
   logical :: active(3)
   real(wp) :: metric(3, 3)

   call get_periodic_axes(periodic, ndim, p_axes)
   active(:) = .false.
   active(p_axes(:ndim)) = .true.

   lat(:, :) = 0.0_wp
   metric(:, :) = 0.0_wp
   do k = 1, 3
      if (active(k)) then
         lat(:, k) = lattice(:, k)
      else
         ! Decoupled unit entry keeps the metric invertible
         metric(k, k) = 1.0_wp
      end if
   end do
   metric(:, :) = metric + matmul(transpose(lat), lat)

   ! Squared volume of the periodic subspace relative to the squared lengths
   if (.not. (matdet_3x3(metric) > tol_deg**2 &
      & * metric(1, 1) * metric(2, 2) * metric(3, 3))) then
      call fatal_error(error, "Periodic lattice vectors are linearly dependent")
      return
   end if

   ginv(:, :) = matinv_3x3(metric)
   do k = 1, 3
      if (.not. active(k)) then
         ginv(k, :) = 0.0_wp
         ginv(:, k) = 0.0_wp
      end if
   end do

end subroutine get_inverse_metric


!> Generate lattice points from repetitions
pure subroutine get_lattice_points_rep_3d(lat, rep, origin, trans, error)

   !> Lattice vectors
   real(wp), intent(in) :: lat(:, :)

   !> Repeatitions of lattice points to generate
   integer, intent(in) :: rep(:)

   !> Include the origin in the generated lattice points
   logical, intent(in) :: origin

   !> Generated lattice points
   real(wp), allocatable, intent(out) :: trans(:, :)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   integer :: itr, ix, iy, iz, jx, jy, jz

   if (any(rep(:3) < 0)) then
      call fatal_error(error, "Negative repetition count for lattice points")
      return
   end if

   ! Translations are indexed by default integers
   if (product(2.0_wp * real(rep(:3), wp) + 1.0_wp) > real(huge(rep), wp)) then
      call fatal_error(error, "Number of lattice points exceeds the integer range")
      return
   end if

   if (origin) then
      allocate(trans(3, product(2*rep(:3)+1)))
   else
      allocate(trans(3, product(2*rep(:3)+1)-1))
   end if

   itr = 0
   do ix = 0, rep(1)
      do iy = 0, rep(2)
         do iz = 0, rep(3)
            if (.not. origin .and. ix == 0 .and. iy == 0 .and. iz == 0) cycle
            do jx = 1, merge(-1, 1, ix > 0), -2
               do jy = 1, merge(-1, 1, iy > 0), -2
                  do jz = 1, merge(-1, 1, iz > 0), -2
                     itr = itr + 1
                     trans(:, itr) = lat(:, 1)*ix*jx &
                        & + lat(:, 2)*iy*jy + lat(:, 3)*iz*jz
                  end do
               end do
            end do
         end do
      end do
   end do

end subroutine get_lattice_points_rep_3d


!> Create lattice points within a given cutoff
pure subroutine get_lattice_points_cutoff(periodic, lat, rthr, trans, error)

   !> Periodic dimensions
   logical, intent(in) :: periodic(:)

   !> Real space cutoff
   real(wp), intent(in) :: rthr

   !> Lattice parameters
   real(wp), intent(in) :: lat(:, :)

   !> Generated lattice points
   real(wp), allocatable, intent(out) :: trans(:, :)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   integer :: k
   real(wp) :: lattice(3, 3), ginv(3, 3), extent(3)

   if (.not. any(periodic)) then
      allocate(trans(3, 1), source=0.0_wp)
      return
   end if

   if (.not. (rthr >= 0.0_wp)) then
      call fatal_error(error, "Negative cutoff for lattice points")
      return
   end if

   call get_inverse_metric(periodic, lat, lattice, ginv, error)
   if (allocated(error)) return

   ! The cutoff sphere spans rthr / height cells along each lattice vector
   do k = 1, 3
      extent(k) = rthr * sqrt(ginv(k, k))
   end do

   if (.not. all(extent < real(huge(k), wp))) then
      call fatal_error(error, "Number of lattice points exceeds the integer range")
      return
   end if

   call get_lattice_points_rep_3d(lattice, ceiling(extent), .true., trans, error)

end subroutine get_lattice_points_cutoff


!> Generate lattice points from repetitions.
!>
!> @note Deprecated: pass an explicit `error` argument instead.
subroutine get_lattice_points_rep_3d_legacy(lat, rep, origin, trans)

   !> Lattice vectors
   real(wp), intent(in) :: lat(:, :)

   !> Repeatitions of lattice points to generate
   integer, intent(in) :: rep(:)

   !> Include the origin in the generated lattice points
   logical, intent(in) :: origin

   !> Generated lattice points
   real(wp), allocatable, intent(out) :: trans(:, :)

   type(error_type), allocatable :: error

   call get_lattice_points_rep_3d(lat, rep, origin, trans, error)
   if (allocated(error)) then
      write(error_unit, "(a)") error%message
      error stop
   end if

end subroutine get_lattice_points_rep_3d_legacy


!> Create lattice points within a given cutoff.
!>
!> @note Deprecated: pass an explicit `error` argument instead.
subroutine get_lattice_points_cutoff_legacy(periodic, lat, rthr, trans)

   !> Periodic dimensions
   logical, intent(in) :: periodic(:)

   !> Real space cutoff
   real(wp), intent(in) :: rthr

   !> Lattice parameters
   real(wp), intent(in) :: lat(:, :)

   !> Generated lattice points
   real(wp), allocatable, intent(out) :: trans(:, :)

   type(error_type), allocatable :: error

   call get_lattice_points_cutoff(periodic, lat, rthr, trans, error)
   if (allocated(error)) then
      write(error_unit, "(a)") error%message
      error stop
   end if

end subroutine get_lattice_points_cutoff_legacy


!> Wrap Cartesian coordinates into the central cell along the periodic
!> directions, the components perpendicular to the periodic subspace are kept.
pure subroutine wrap_to_central_cell(xyz, lattice, periodic)

   !> Cartesian atomic coordinates, updated in place
   real(wp), intent(inout) :: xyz(:, :)

   !> Lattice vectors
   real(wp), intent(in) :: lattice(:, :)

   !> Periodic dimensions
   logical, intent(in) :: periodic(:)

   integer :: iat
   real(wp) :: lat(3, 3), ginv(3, 3), frac(3)
   type(error_type), allocatable :: error

   if (.not. any(periodic)) return

   ! A cell without volume in the periodic subspace cannot be wrapped into
   call get_inverse_metric(periodic, lattice, lat, ginv, error)
   if (allocated(error)) return

   do iat = 1, size(xyz, 2)
      frac(:) = matmul(ginv, matmul(transpose(lat), xyz(:, iat)))
      xyz(:, iat) = xyz(:, iat) + matmul(lat, shift_back_abc(frac) - frac)
   end do

end subroutine wrap_to_central_cell


!> Shift a fractional coordinate into the half-open unit interval [0, 1)
elemental function shift_back_abc(in) result(out)
   !> fractional coordinate in (-∞,+∞)
   real(wp),intent(in) :: in
   !> fractional coordinate in [0,1)
   real(wp) :: out
   real(wp),parameter :: p_pbc_eps = 1.0e-14_wp
   out = modulo(in, 1.0_wp)
   if (abs(out - 1.0_wp) < p_pbc_eps) then
     out = out - 1.0_wp
   end if
end function shift_back_abc


end module mctc_cutoff
