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
   use mctc_env, only : wp, i8, error_type, fatal_error
   use mctc_io_math, only : crossprod, matinv_3x3
   implicit none
   private

   public :: get_lattice_points, get_translations, wrap_to_central_cell
   public :: cutoff_stat, max_cutoff_trans


   !> Status codes for real-space lattice point enumeration
   type :: enum_cutoff_stat
      !> Valid zero-dimensional (nonperiodic) enumeration
      integer :: valid_0d = 0
      !> Valid periodic lattice point enumeration
      integer :: valid_pbc = 1
      !> Invalid or degenerate active periodic lattice
      integer :: invalid_lattice = 2
      !> Negative or nonfinite cutoff or negative repetition bound
      integer :: invalid_cutoff = 3
      !> Nonfinite lattice vectors
      integer :: nonfinite_input = 4
      !> Integer overflow during repetition bound or volume calculation
      integer :: integer_overflow = 5
      !> Total lattice point count exceeds resource limit
      integer :: resource_exhausted = 6
   end type enum_cutoff_stat

   !> Instance of lattice cutoff status enumerator
   type(enum_cutoff_stat), parameter :: cutoff_stat = enum_cutoff_stat()


   interface get_lattice_points
      module procedure :: get_lattice_points_cutoff
      module procedure :: get_lattice_points_rep_3d
   end interface get_lattice_points

   !> Tolerance for detecting degenerate lattice vectors, areas, and volumes
   real(wp), parameter :: tol_deg = 1.0e-12_wp

   !> Default maximum number of generated lattice translation vectors
   integer(i8), parameter :: max_cutoff_trans = 10000000_i8

   !> Maximum real value convertible to a signed 32-bit repetition bound without overflow
   real(wp), parameter :: max_rep_real = real(huge(0) / 2 - 4, wp)


contains


!> Convert a cutoff enumeration status code into a descriptive fatal_error
pure subroutine report_cutoff_error(stat, error)
   !> Status code from lattice enumeration validation
   integer, intent(in) :: stat
   !> Allocated error instance
   type(error_type), allocatable, intent(out) :: error

   select case (stat)
   case (cutoff_stat%invalid_lattice)
      call fatal_error(error, &
         & "Invalid or degenerate active periodic lattice in cutoff enumeration", &
         & stat=stat)
   case (cutoff_stat%invalid_cutoff)
      call fatal_error(error, &
         & "Negative or nonfinite cutoff or repetition bound in cutoff enumeration", &
         & stat=stat)
   case (cutoff_stat%nonfinite_input)
      call fatal_error(error, &
         & "Nonfinite lattice vectors in cutoff enumeration", &
         & stat=stat)
   case (cutoff_stat%integer_overflow)
      call fatal_error(error, &
         & "Integer overflow in lattice point repetition bound or volume calculation", &
         & stat=stat)
   case (cutoff_stat%resource_exhausted)
      call fatal_error(error, &
         & "Lattice translation count exceeds maximum resource limit", &
         & stat=stat)
   case default
      call fatal_error(error, &
         & "Failed to enumerate periodic lattice translations", &
         & stat=stat)
   end select
end subroutine report_cutoff_error


!> Generate lattice points from repetitions
pure subroutine get_lattice_points_rep_3d(lat, rep, origin, trans, error, stat, max_trans)

   !> Lattice vectors
   real(wp), intent(in) :: lat(:, :)

   !> Repeatitions of lattice points to generate
   integer, intent(in) :: rep(:)

   !> Include the origin in the generated lattice points
   logical, intent(in) :: origin

   !> Generated lattice points
   real(wp), allocatable, intent(out) :: trans(:, :)

   !> Optional error handling
   type(error_type), allocatable, intent(out), optional :: error

   !> Optional status code
   integer, intent(out), optional :: stat

   !> Optional override for maximum generated translation count
   integer(i8), intent(in), optional :: max_trans

   integer :: i, itr, ix, iy, iz, jx, jy, jz, ntr, local_stat
   integer(i8) :: ntot_i8, factor_i8, limit_i8
   real(wp) :: vec(3)

   limit_i8 = max_cutoff_trans
   if (present(max_trans)) limit_i8 = max_trans

   if (size(lat, 1) < 3 .or. size(lat, 2) < 3 .or. size(rep) < 3) then
      local_stat = cutoff_stat%invalid_lattice
      if (present(stat)) stat = local_stat
      if (present(error)) call report_cutoff_error(local_stat, error)
      return
   end if

   do i = 1, 3
      if (rep(i) < 0) then
         local_stat = cutoff_stat%invalid_cutoff
         if (present(stat)) stat = local_stat
         if (present(error)) call report_cutoff_error(local_stat, error)
         return
      end if
      if (rep(i) > 0) then
         if (.not. is_finite_vec(lat(1:3, i))) then
            local_stat = cutoff_stat%nonfinite_input
            if (present(stat)) stat = local_stat
            if (present(error)) call report_cutoff_error(local_stat, error)
            return
         end if
      end if
   end do

   if (limit_i8 < 0_i8) then
      local_stat = cutoff_stat%resource_exhausted
      if (present(stat)) stat = local_stat
      if (present(error)) call report_cutoff_error(local_stat, error)
      return
   end if

   ! Compute 3D lattice point count in 64-bit integer arithmetic with overflow guards
   ntot_i8 = 1_i8
   do i = 1, 3
      if (int(rep(i), i8) > int(huge(0) / 2 - 1, i8)) then
         local_stat = cutoff_stat%integer_overflow
         if (present(stat)) stat = local_stat
         if (present(error)) call report_cutoff_error(local_stat, error)
         return
      end if
      factor_i8 = 2_i8 * int(rep(i), i8) + 1_i8
      if (ntot_i8 > huge(1_i8) / factor_i8) then
         local_stat = cutoff_stat%integer_overflow
         if (present(stat)) stat = local_stat
         if (present(error)) call report_cutoff_error(local_stat, error)
         return
      end if
      ntot_i8 = ntot_i8 * factor_i8
   end do

   if (.not. origin) ntot_i8 = ntot_i8 - 1_i8

   ! Guard 32-bit default-integer extent (trans has shape (3, ntr)) before resource check
   if (ntot_i8 > int(huge(0), i8) / 3_i8) then
      local_stat = cutoff_stat%integer_overflow
      if (present(stat)) stat = local_stat
      if (present(error)) call report_cutoff_error(local_stat, error)
      return
   end if

   if (ntot_i8 > limit_i8) then
      local_stat = cutoff_stat%resource_exhausted
      if (present(stat)) stat = local_stat
      if (present(error)) call report_cutoff_error(local_stat, error)
      return
   end if

   ntr = int(ntot_i8)
   allocate(trans(3, ntr))

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

   if (present(stat)) then
      if (any(rep(1:3) > 0)) then
         stat = cutoff_stat%valid_pbc
      else
         stat = cutoff_stat%valid_0d
      end if
   end if

end subroutine get_lattice_points_rep_3d


!> Create lattice points within a given cutoff
pure subroutine get_lattice_points_cutoff(periodic, lat, rthr, trans, error, stat, max_trans)

   !> Periodic dimensions
   logical, intent(in) :: periodic(:)

   !> Real space cutoff
   real(wp), intent(in) :: rthr

   !> Lattice parameters
   real(wp), intent(in) :: lat(:, :)

   !> Generated lattice points
   real(wp), allocatable, intent(out) :: trans(:, :)

   !> Optional error handling
   type(error_type), allocatable, intent(out), optional :: error

   !> Optional status code
   integer, intent(out), optional :: stat

   !> Optional override for maximum generated translation count
   integer(i8), intent(in), optional :: max_trans

   integer :: rep(3), local_stat

   if (rthr < 0.0_wp .or. .not. is_finite_scalar(rthr)) then
      local_stat = cutoff_stat%invalid_cutoff
      if (present(stat)) stat = local_stat
      if (present(error)) call report_cutoff_error(local_stat, error)
      return
   end if

   if (.not.any(periodic)) then
      allocate(trans(3, 1))
      trans(:, :) = 0.0_wp
      if (present(stat)) stat = cutoff_stat%valid_0d
   else
      call get_translations(periodic, lat, rthr, rep, local_stat, max_trans)
      if (local_stat /= cutoff_stat%valid_pbc) then
         if (present(stat)) stat = local_stat
         if (present(error)) call report_cutoff_error(local_stat, error)
         return
      end if
      call get_lattice_points_rep_3d(lat, rep, .true., trans, error, stat, max_trans)
      if (present(stat) .and. allocated(trans)) stat = cutoff_stat%valid_pbc
   end if

end subroutine get_lattice_points_cutoff


!> Generate supercell repetition counts for active periodic directions based on a
!> realspace cutoff using the metric of the active periodic subspace.
pure subroutine get_translations(periodic, lat, rthr, rep, stat, max_trans)
   logical, intent(in) :: periodic(:)
   real(wp), intent(in) :: lat(:, :)
   real(wp), intent(in) :: rthr
   integer, intent(out) :: rep(3)
   integer, intent(out), optional :: stat
   integer(i8), intent(in), optional :: max_trans

   integer :: i, ndim, p_axes(3)
   integer(i8) :: rep_i8(3), ntot_i8, factor_i8, limit_i8
   real(wp) :: a1(3), a2(3), a3(3)
   real(wp) :: normx(3), normy(3), normz(3)
   real(wp) :: len1, len2, len3, area, vol
   real(wp) :: cos10, cos21, cos32, ratio1, ratio2, ratio3

   rep(:) = 0
   limit_i8 = max_cutoff_trans
   if (present(max_trans)) limit_i8 = max_trans

   if (rthr < 0.0_wp .or. .not. is_finite_scalar(rthr)) then
      if (present(stat)) stat = cutoff_stat%invalid_cutoff
      return
   end if
   if (.not. any(periodic)) then
      if (present(stat)) stat = cutoff_stat%valid_0d
      return
   end if
   if (size(lat, 1) < 3 .or. size(lat, 2) < 3 .or. size(periodic) < 3) then
      if (present(stat)) stat = cutoff_stat%invalid_lattice
      return
   end if
   if (limit_i8 <= 0_i8) then
      if (present(stat)) stat = cutoff_stat%resource_exhausted
      return
   end if

   ndim = 0
   p_axes(:) = 0
   do i = 1, 3
      if (periodic(i)) then
         if (.not. is_finite_vec(lat(1:3, i))) then
            if (present(stat)) stat = cutoff_stat%nonfinite_input
            return
         end if
         ndim = ndim + 1
         p_axes(ndim) = i
      end if
   end do

   rep_i8(:) = 0_i8
   select case (ndim)
   case (1)
      a1 = lat(:, p_axes(1))
      len1 = norm2(a1)
      if (len1 <= tol_deg) then
         if (present(stat)) stat = cutoff_stat%invalid_lattice
         return
      end if
      ratio1 = rthr / len1
      if (.not. (ratio1 <= max_rep_real)) then
         if (present(stat)) stat = cutoff_stat%integer_overflow
         return
      end if
      rep_i8(p_axes(1)) = ceiling(ratio1, i8)

   case (2)
      a1 = lat(:, p_axes(1))
      a2 = lat(:, p_axes(2))
      len1 = norm2(a1)
      len2 = norm2(a2)
      if (len1 <= tol_deg .or. len2 <= tol_deg) then
         if (present(stat)) stat = cutoff_stat%invalid_lattice
         return
      end if
      area = norm2(crossprod(a1, a2))
      if (area <= tol_deg * len1 * len2) then
         if (present(stat)) stat = cutoff_stat%invalid_lattice
         return
      end if
      ratio1 = rthr * len2 / area
      ratio2 = rthr * len1 / area
      if (.not. (ratio1 <= max_rep_real .and. ratio2 <= max_rep_real)) then
         if (present(stat)) stat = cutoff_stat%integer_overflow
         return
      end if
      rep_i8(p_axes(1)) = ceiling(ratio1, i8)
      rep_i8(p_axes(2)) = ceiling(ratio2, i8)

   case (3)
      a1 = lat(:, 1)
      a2 = lat(:, 2)
      a3 = lat(:, 3)
      len1 = norm2(a1)
      len2 = norm2(a2)
      len3 = norm2(a3)
      if (len1 <= tol_deg .or. len2 <= tol_deg .or. len3 <= tol_deg) then
         if (present(stat)) stat = cutoff_stat%invalid_lattice
         return
      end if
      normx = crossprod(a2, a3)
      normy = crossprod(a3, a1)
      normz = crossprod(a1, a2)
      vol = abs(dot_product(a1, normx))
      if (vol <= tol_deg * len1 * len2 * len3) then
         if (present(stat)) stat = cutoff_stat%invalid_lattice
         return
      end if
      cos10 = vol / norm2(normx)
      cos21 = vol / norm2(normy)
      cos32 = vol / norm2(normz)
      ratio1 = rthr / cos10
      ratio2 = rthr / cos21
      ratio3 = rthr / cos32
      if (.not. (ratio1 <= max_rep_real .and. ratio2 <= max_rep_real &
         & .and. ratio3 <= max_rep_real)) then
         if (present(stat)) stat = cutoff_stat%integer_overflow
         return
      end if
      rep_i8(1) = ceiling(ratio1, i8)
      rep_i8(2) = ceiling(ratio2, i8)
      rep_i8(3) = ceiling(ratio3, i8)

   case default
      if (present(stat)) stat = cutoff_stat%invalid_lattice
      return
   end select

   ! Validate total translation count in 64-bit arithmetic before setting rep
   ntot_i8 = 1_i8
   do i = 1, 3
      if (rep_i8(i) < 0_i8 .or. rep_i8(i) > int(huge(0) / 2 - 1, i8)) then
         if (present(stat)) stat = cutoff_stat%integer_overflow
         return
      end if
      factor_i8 = 2_i8 * rep_i8(i) + 1_i8
      if (ntot_i8 > huge(1_i8) / factor_i8) then
         if (present(stat)) stat = cutoff_stat%integer_overflow
         return
      end if
      ntot_i8 = ntot_i8 * factor_i8
   end do

   if (ntot_i8 > int(huge(0), i8) / 3_i8) then
      if (present(stat)) stat = cutoff_stat%integer_overflow
      return
   end if

   if (ntot_i8 > limit_i8) then
      if (present(stat)) stat = cutoff_stat%resource_exhausted
      return
   end if

   rep(:) = int(rep_i8)
   if (present(stat)) stat = cutoff_stat%valid_pbc

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
         if (.not. is_finite_vec(xyz(:, iat))) cycle
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
         if (.not. is_finite_vec(xyz(:, iat))) cycle
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
         if (.not. is_finite_vec(xyz(:, iat))) cycle
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
   if (.not. is_finite_scalar(in)) return
   if (abs(in) > real(huge(1_i8) / 2_i8, wp)) return
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
