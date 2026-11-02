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
   use mctc_env, only : wp
   use mctc_io_math, only : crossprod, matinv_3x3
   implicit none
   private

   public :: get_lattice_points, wrap_to_central_cell


   interface get_lattice_points
      module procedure :: get_lattice_points_cutoff
      module procedure :: get_lattice_points_rep_3d
   end interface get_lattice_points

   !> Tolerance for detecting degenerate lattice vectors, areas, and volumes
   real(wp), parameter :: tol_deg = 1.0e-12_wp


contains


!> Generate lattice points from repetitions
pure subroutine get_lattice_points_rep_3d(lat, rep, origin, trans)

   !> Lattice vectors
   real(wp), intent(in) :: lat(:, :)

   !> Repeatitions of lattice points to generate
   integer, intent(in) :: rep(:)

   !> Include the origin in the generated lattice points
   logical, intent(in) :: origin

   !> Generated lattice points
   real(wp), allocatable, intent(out) :: trans(:, :)

   integer :: itr, ix, iy, iz, jx, jy, jz
   real(wp) :: vec(3)

   itr = 0
   if (origin) then
      allocate(trans(3, product(2*rep+1)))
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
      allocate(trans(3, product(2*rep+1)-1))
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
pure subroutine get_lattice_points_cutoff(periodic, lat, rthr, trans)

   !> Periodic dimensions
   logical, intent(in) :: periodic(:)

   !> Real space cutoff
   real(wp), intent(in) :: rthr

   !> Lattice parameters
   real(wp), intent(in) :: lat(:, :)

   !> Generated lattice points
   real(wp), allocatable, intent(out) :: trans(:, :)

   integer :: rep(3)

   if (.not.any(periodic)) then
      allocate(trans(3, 1))
      trans(:, :) = 0.0_wp
   else
      call get_translations(periodic, lat, rthr, rep)
      call get_lattice_points(lat, rep, .true., trans)
   end if

end subroutine get_lattice_points_cutoff


!> Generate supercell repetition counts for active periodic directions based on a
!> realspace cutoff using the metric of the active periodic subspace.
pure subroutine get_translations(periodic, lat, rthr, rep)
   logical, intent(in) :: periodic(:)
   real(wp), intent(in) :: lat(:, :)
   real(wp), intent(in) :: rthr
   integer, intent(out) :: rep(3)

   integer :: i, ndim, p_axes(3)
   real(wp) :: a1(3), a2(3), a3(3)
   real(wp) :: normx(3), normy(3), normz(3)
   real(wp) :: len1, len2, len3, area, vol
   real(wp) :: cos10, cos21, cos32

   rep(:) = 0
   if (rthr < 0.0_wp .or. .not. is_finite_scalar(rthr)) return
   if (size(lat, 1) < 3 .or. size(lat, 2) < 3) return

   ndim = 0
   p_axes(:) = 0
   do i = 1, min(3, size(periodic))
      if (periodic(i)) then
         if (.not. is_finite_vec(lat(:, i))) return
         ndim = ndim + 1
         p_axes(ndim) = i
      end if
   end do

   select case (ndim)
   case (1)
      a1 = lat(:, p_axes(1))
      len1 = norm2(a1)
      if (len1 <= tol_deg) return
      rep(p_axes(1)) = ceiling(rthr / len1)

   case (2)
      a1 = lat(:, p_axes(1))
      a2 = lat(:, p_axes(2))
      len1 = norm2(a1)
      len2 = norm2(a2)
      if (len1 <= tol_deg .or. len2 <= tol_deg) return
      area = norm2(crossprod(a1, a2))
      if (area <= tol_deg * len1 * len2) return
      rep(p_axes(1)) = ceiling(rthr * len2 / area)
      rep(p_axes(2)) = ceiling(rthr * len1 / area)

   case (3)
      a1 = lat(:, 1)
      a2 = lat(:, 2)
      a3 = lat(:, 3)
      len1 = norm2(a1)
      len2 = norm2(a2)
      len3 = norm2(a3)
      if (len1 <= tol_deg .or. len2 <= tol_deg .or. len3 <= tol_deg) return
      normx = crossprod(a2, a3)
      normy = crossprod(a3, a1)
      normz = crossprod(a1, a2)
      vol = abs(dot_product(a1, normx))
      if (vol <= tol_deg * len1 * len2 * len3) return
      cos10 = vol / norm2(normx)
      cos21 = vol / norm2(normy)
      cos32 = vol / norm2(normz)
      rep(1) = ceiling(rthr / cos10)
      rep(2) = ceiling(rthr / cos21)
      rep(3) = ceiling(rthr / cos32)
   case default
      continue
   end select

end subroutine get_translations


pure elemental function is_finite_scalar(val) result(finite)
   real(wp), intent(in) :: val
   logical :: finite

   finite = (val == val) .and. (abs(val) <= huge(1.0_wp))

end function is_finite_scalar


pure function is_finite_vec(vec) result(finite)
   real(wp), intent(in) :: vec(:)
   logical :: finite

   finite = all(is_finite_scalar(vec))

end function is_finite_vec


pure subroutine wrap_to_central_cell(xyz, lattice, periodic)
   real(wp), intent(inout) :: xyz(:, :)
   real(wp), intent(in) :: lattice(:, :)
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
         if (.not. is_finite_vec(lattice(:, i))) return
         ndim = ndim + 1
         p_axes(ndim) = i
      end if
   end do

   select case (ndim)
   case (1)
      a1 = lattice(:, p_axes(1))
      g11 = dot_product(a1, a1)
      if (sqrt(g11) <= tol_deg) return
      do iat = 1, size(xyz, 2)
         s1 = dot_product(xyz(:, iat), a1) / g11
         s1_wrap = shift_back_abc(s1)
         xyz(:, iat) = xyz(:, iat) + (s1_wrap - s1) * a1
      end do

   case (2)
      a1 = lattice(:, p_axes(1))
      a2 = lattice(:, p_axes(2))
      g11 = dot_product(a1, a1)
      g12 = dot_product(a1, a2)
      g22 = dot_product(a2, a2)
      if (sqrt(g11) <= tol_deg .or. sqrt(g22) <= tol_deg) return
      det2 = g11 * g22 - g12 * g12
      if (det2 <= (tol_deg**2) * g11 * g22) return
      do iat = 1, size(xyz, 2)
         b1 = dot_product(xyz(:, iat), a1)
         b2 = dot_product(xyz(:, iat), a2)
         s1 = (g22 * b1 - g12 * b2) / det2
         s2 = (-g12 * b1 + g11 * b2) / det2
         s1_wrap = shift_back_abc(s1)
         s2_wrap = shift_back_abc(s2)
         xyz(:, iat) = xyz(:, iat) + (s1_wrap - s1) * a1 + (s2_wrap - s2) * a2
      end do

   case (3)
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
         vec(:) = matmul(invlat, xyz(:, iat))
         vec(:) = shift_back_abc(vec)
         xyz(:, iat) = matmul(lattice, vec)
      end do
   case default
      continue
   end select

end subroutine wrap_to_central_cell


elemental function shift_back_abc(in) result(out)
   !> fractional coordinate in (-∞,+∞)
   real(wp),intent(in) :: in
   !> fractional coordinate in [0,1)
   real(wp) :: out
   real(wp),parameter :: p_pbc_eps = 1.0e-14_wp
   out = in
   if(in < (0.0_wp - p_pbc_eps)) then
     out = in + real(ceiling(-in),wp)
   end if
   if(in > (1.0_wp + p_pbc_eps)) then
     out = in - real(floor  ( in),wp)
   end if
   if (abs(out - 1.0_wp) < p_pbc_eps) then
     out = out - 1.0_wp
   end if
end function shift_back_abc


end module mctc_cutoff
