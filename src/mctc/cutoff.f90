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
module mctc_cutoff
   use, intrinsic :: ieee_arithmetic, only : ieee_is_finite
   use, intrinsic :: iso_fortran_env, only : error_unit
   use mctc_env, only : wp, i8, error_type, fatal_error
   use mctc_io_math, only : crossprod, matinv_3x3
   implicit none
   private

   public :: get_lattice_points, wrap_to_central_cell


   interface get_lattice_points
      module procedure :: get_lattice_points_cutoff
      module procedure :: get_lattice_points_cutoff_legacy
      module procedure :: get_lattice_points_rep_3d
      module procedure :: get_lattice_points_rep_3d_legacy
   end interface get_lattice_points

   !> Tolerance for detecting degenerate lattice vectors, areas, and volumes
   real(wp), parameter :: tol_deg = 1.0e-12_wp

   !> Maximum number of generated lattice translation vectors
   integer(i8), parameter :: max_cutoff_trans = 1000000_i8


contains


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

   integer :: i, itr, ix, iy, iz, jx, jy, jz
   integer(i8) :: ntrans
   real(wp) :: vec(3)

   if (size(lat, 1) < 3 .or. size(lat, 2) < 3 .or. size(rep) < 3) then
      call fatal_error(error, &
         & "Invalid or degenerate active periodic lattice in cutoff enumeration")
      return
   end if

   do i = 1, 3
      if (rep(i) < 0) then
         call fatal_error(error, &
            & "Negative or nonfinite cutoff or repetition bound in cutoff enumeration")
         return
      end if
      if (rep(i) > 0) then
         if (.not. all(ieee_is_finite(lat(1:3, i)))) then
            call fatal_error(error, &
               & "Nonfinite lattice vectors in cutoff enumeration")
            return
         end if
      end if
   end do

   ! Use 64-bit arithmetic for the translation-count product.
   ntrans = 1_i8
   do i = 1, 3
      ntrans = ntrans * (2_i8 * int(rep(i), i8) + 1_i8)
      if (merge(ntrans, ntrans - 1_i8, origin) > max_cutoff_trans) then
         call fatal_error(error, &
            & "Lattice translation count exceeds maximum resource limit")
         return
      end if
   end do
   if (.not. origin) ntrans = ntrans - 1_i8

   allocate(trans(3, ntrans))

   itr = 0
   if (origin) then
      do ix = 0, rep(1)
         do iy = 0, rep(2)
            do iz = 0, rep(3)
               do jx = 1, merge(-1, 1, ix > 0), -2
                  do jy = 1, merge(-1, 1, iy > 0), -2
                     do jz = 1, merge(-1, 1, iz > 0), -2
                        itr = itr + 1
                        vec = 0.0_wp
                        if (ix > 0) vec = vec + lat(:, 1)*ix*jx
                        if (iy > 0) vec = vec + lat(:, 2)*iy*jy
                        if (iz > 0) vec = vec + lat(:, 3)*iz*jz
                        trans(:, itr) = vec
                     end do
                  end do
               end do
            end do
         end do
      end do
   else
      do ix = 0, rep(1)
         do iy = 0, rep(2)
            do iz = 0, rep(3)
               if (ix == 0 .and. iy == 0 .and. iz == 0) cycle
               do jx = 1, merge(-1, 1, ix > 0), -2
                  do jy = 1, merge(-1, 1, iy > 0), -2
                     do jz = 1, merge(-1, 1, iz > 0), -2
                        itr = itr + 1
                        vec = 0.0_wp
                        if (ix > 0) vec = vec + lat(:, 1)*ix*jx
                        if (iy > 0) vec = vec + lat(:, 2)*iy*jy
                        if (iz > 0) vec = vec + lat(:, 3)*iz*jz
                        trans(:, itr) = vec
                     end do
                  end do
               end do
            end do
         end do
      end do
   end if

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

   integer :: rep(3)

   if (rthr < 0.0_wp .or. .not. ieee_is_finite(rthr)) then
      call fatal_error(error, &
         & "Negative or nonfinite cutoff or repetition bound in cutoff enumeration")
      return
   end if

   if (.not.any(periodic)) then
      allocate(trans(3, 1))
      trans(:, :) = 0.0_wp
   else
      call get_translations(periodic, lat, rthr, rep, error)
      if (allocated(error)) return

      call get_lattice_points_rep_3d(lat, rep, .true., trans, error)
   end if

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


!> Generate supercell repetition counts for active periodic directions based on a
!> realspace cutoff using the metric of the active periodic subspace.
pure subroutine get_translations(periodic, lat, rthr, rep, error)
   !> Periodic dimensions
   logical, intent(in) :: periodic(:)
   !> Lattice parameters
   real(wp), intent(in) :: lat(:, :)
   !> Real space cutoff
   real(wp), intent(in) :: rthr
   !> Supercell repetition bounds along each lattice vector
   integer, intent(out) :: rep(3)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   integer :: i, ndim, p_axes(3)
   integer(i8) :: ntrans
   real(wp) :: a1(3), a2(3), a3(3)
   real(wp) :: normx(3), normy(3), normz(3)
   real(wp) :: len1, len2, len3, area, vol
   real(wp) :: cos10, cos21, cos32, ratio1, ratio2, ratio3

   rep(:) = 0

   if (rthr < 0.0_wp .or. .not. ieee_is_finite(rthr)) then
      call fatal_error(error, &
         & "Negative or nonfinite cutoff or repetition bound in cutoff enumeration")
      return
   end if
   if (.not. any(periodic)) return
   if (size(lat, 1) < 3 .or. size(lat, 2) < 3 .or. size(periodic) < 3) then
      call fatal_error(error, &
         & "Invalid or degenerate active periodic lattice in cutoff enumeration")
      return
   end if

   ndim = 0
   p_axes(:) = 0
   do i = 1, 3
      if (periodic(i)) then
         if (.not. all(ieee_is_finite(lat(1:3, i)))) then
            call fatal_error(error, &
               & "Nonfinite lattice vectors in cutoff enumeration")
            return
         end if
         ndim = ndim + 1
         p_axes(ndim) = i
      end if
   end do

   select case (ndim)
   case (1)

      ! 1D periodic axis: repetition bound is ceiling(rthr / |a1|)
      a1 = lat(:, p_axes(1))
      len1 = norm2(a1)
      if (len1 <= tol_deg) then
         call fatal_error(error, &
            & "Invalid or degenerate active periodic lattice in cutoff enumeration")
         return
      end if
      ratio1 = rthr / len1
      if (.not. (ratio1 <= real(max_cutoff_trans, wp))) then
         call fatal_error(error, &
            & "Lattice translation count exceeds maximum resource limit")
         return
      end if
      rep(p_axes(1)) = ceiling(ratio1)

   case (2)

      ! 2D periodic plane: perpendicular heights are h1 = area / |a2|, h2 = area / |a1|
      a1 = lat(:, p_axes(1))
      a2 = lat(:, p_axes(2))
      len1 = norm2(a1)
      len2 = norm2(a2)
      if (len1 <= tol_deg .or. len2 <= tol_deg) then
         call fatal_error(error, &
            & "Invalid or degenerate active periodic lattice in cutoff enumeration")
         return
      end if
      area = norm2(crossprod(a1, a2))
      if (area <= tol_deg * len1 * len2) then
         call fatal_error(error, &
            & "Invalid or degenerate active periodic lattice in cutoff enumeration")
         return
      end if
      ratio1 = rthr * len2 / area
      ratio2 = rthr * len1 / area
      if (.not. (ratio1 <= real(max_cutoff_trans, wp) &
         & .and. ratio2 <= real(max_cutoff_trans, wp))) then
         call fatal_error(error, &
            & "Lattice translation count exceeds maximum resource limit")
         return
      end if
      rep(p_axes(1)) = ceiling(ratio1)
      rep(p_axes(2)) = ceiling(ratio2)

   case (3)

      ! 3D periodic cell: perpendicular heights are vol / |a_j x a_k|
      a1 = lat(:, 1)
      a2 = lat(:, 2)
      a3 = lat(:, 3)
      len1 = norm2(a1)
      len2 = norm2(a2)
      len3 = norm2(a3)
      if (len1 <= tol_deg .or. len2 <= tol_deg .or. len3 <= tol_deg) then
         call fatal_error(error, &
            & "Invalid or degenerate active periodic lattice in cutoff enumeration")
         return
      end if
      normx = crossprod(a2, a3)
      normy = crossprod(a3, a1)
      normz = crossprod(a1, a2)
      vol = abs(dot_product(a1, normx))
      if (vol <= tol_deg * len1 * len2 * len3) then
         call fatal_error(error, &
            & "Invalid or degenerate active periodic lattice in cutoff enumeration")
         return
      end if
      cos10 = vol / norm2(normx)
      cos21 = vol / norm2(normy)
      cos32 = vol / norm2(normz)
      ratio1 = rthr / cos10
      ratio2 = rthr / cos21
      ratio3 = rthr / cos32
      if (.not. (ratio1 <= real(max_cutoff_trans, wp) &
         & .and. ratio2 <= real(max_cutoff_trans, wp) &
         & .and. ratio3 <= real(max_cutoff_trans, wp))) then
         call fatal_error(error, &
            & "Lattice translation count exceeds maximum resource limit")
         return
      end if
      rep(1) = ceiling(ratio1)
      rep(2) = ceiling(ratio2)
      rep(3) = ceiling(ratio3)

   case default
      call fatal_error(error, &
         & "Invalid or degenerate active periodic lattice in cutoff enumeration")
      return
   end select

   ! Use 64-bit arithmetic for the translation-count product.
   ntrans = 1_i8
   do i = 1, 3
      ntrans = ntrans * (2_i8 * int(rep(i), i8) + 1_i8)
      if (ntrans > max_cutoff_trans) then
         call fatal_error(error, &
            & "Lattice translation count exceeds maximum resource limit")
         return
      end if
   end do

end subroutine get_translations


!> Wrap Cartesian coordinates into the central periodic cell [0, 1)^d along active axes,
!> preserving orthogonal non-periodic coordinates in 1D and 2D.
pure subroutine wrap_to_central_cell(xyz, lattice, periodic)
   !> Cartesian atomic coordinates (3, nat), updated in place
   real(wp), intent(inout) :: xyz(:, :)
   !> Lattice vectors (3, 3)
   real(wp), intent(in) :: lattice(:, :)
   !> Periodic dimensions (3)
   logical, intent(in) :: periodic(:)

   real(wp) :: invlat(3, 3), vec(3), a1(3), a2(3), a3(3)
   real(wp) :: g11, g12, g22, det2, b1, b2, s1, s2, s1_wrap, s2_wrap
   real(wp) :: len1, len2, len3, vol
   integer :: i, iat, ndim, p_axes(3)

   if (.not.any(periodic)) return
   if (size(lattice, 1) < 3 .or. size(lattice, 2) < 3) return

   ndim = 0
   p_axes(:) = 0
   do i = 1, min(3, size(periodic))
      if (periodic(i)) then
         if (.not. all(ieee_is_finite(lattice(:, i)))) return
         ndim = ndim + 1
         p_axes(ndim) = i
      end if
   end do

   select case (ndim)
   case (1)

      ! Project onto the active 1D axis and shift only the parallel component
      a1 = lattice(:, p_axes(1))
      g11 = dot_product(a1, a1)
      if (sqrt(g11) <= tol_deg) return
      do iat = 1, size(xyz, 2)
         if (.not. all(ieee_is_finite(xyz(:, iat)))) cycle
         s1 = dot_product(xyz(:, iat), a1) / g11
         s1_wrap = shift_back_abc(s1)
         xyz(:, iat) = xyz(:, iat) + (s1_wrap - s1) * a1
      end do

   case (2)

      ! Project onto the active 2D plane via the 2x2 Gram matrix and preserve the normal component
      a1 = lattice(:, p_axes(1))
      a2 = lattice(:, p_axes(2))
      g11 = dot_product(a1, a1)
      g12 = dot_product(a1, a2)
      g22 = dot_product(a2, a2)
      if (sqrt(g11) <= tol_deg .or. sqrt(g22) <= tol_deg) return
      det2 = g11 * g22 - g12 * g12
      if (det2 <= (tol_deg**2) * g11 * g22) return
      do iat = 1, size(xyz, 2)
         if (.not. all(ieee_is_finite(xyz(:, iat)))) cycle
         b1 = dot_product(xyz(:, iat), a1)
         b2 = dot_product(xyz(:, iat), a2)
         s1 = (g22 * b1 - g12 * b2) / det2
         s2 = (-g12 * b1 + g11 * b2) / det2
         s1_wrap = shift_back_abc(s1)
         s2_wrap = shift_back_abc(s2)
         xyz(:, iat) = xyz(:, iat) + (s1_wrap - s1) * a1 + (s2_wrap - s2) * a2
      end do

   case (3)

      ! Wrap all three fractional coordinates in 3D via the 3x3 inverse lattice
      a1 = lattice(:, 1)
      a2 = lattice(:, 2)
      a3 = lattice(:, 3)
      len1 = norm2(a1)
      len2 = norm2(a2)
      len3 = norm2(a3)
      if (len1 <= tol_deg .or. len2 <= tol_deg .or. len3 <= tol_deg) return
      vol = abs(dot_product(a1, crossprod(a2, a3)))
      if (vol <= tol_deg * len1 * len2 * len3) return
      invlat = matinv_3x3(lattice)
      do iat = 1, size(xyz, 2)
         if (.not. all(ieee_is_finite(xyz(:, iat)))) cycle
         vec(:) = matmul(invlat, xyz(:, iat))
         vec(:) = shift_back_abc(vec)
         xyz(:, iat) = matmul(lattice, vec)
      end do
   case default
      continue
   end select

end subroutine wrap_to_central_cell


!> Shift a fractional coordinate into the half-open unit interval [0, 1)
elemental function shift_back_abc(in) result(out)
   !> fractional coordinate in (-∞,+∞)
   real(wp),intent(in) :: in
   !> fractional coordinate in [0,1)
   real(wp) :: out
   real(wp),parameter :: p_pbc_eps = 1.0e-14_wp
   out = in
   if (.not. ieee_is_finite(in)) return
   if (abs(in) > real((huge(1_i8) - 1_i8) / 2_i8, wp)) return
   if(in < (0.0_wp - p_pbc_eps)) then
     out = in + real(ceiling(-in, i8),wp)
   end if
   if(in > (1.0_wp + p_pbc_eps)) then
     out = in - real(floor  ( in, i8),wp)
   end if
   if (abs(out - 1.0_wp) < p_pbc_eps) then
     out = out - 1.0_wp
   end if
end function shift_back_abc


end module mctc_cutoff
