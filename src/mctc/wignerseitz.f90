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

!> @file mctc/wignerseitz.f90
!> Declaration of the cyclic cluster Wigner-Seitz type, generation, and weight evaluation
module mctc_wignerseitz
   use mctc_cutoff, only : get_lattice_points
   use mctc_env, only : wp, i8, error_type, fatal_error
   use mctc_io, only : structure_type
   use mctc_io_math, only : crossprod, outerprod
   implicit none
   private

   public :: new_wignerseitz_cell, get_wignerseitz_weights, wignerseitz_cell, &
      & get_pairs, get_wsc_rep, wsc_stat, max_wsc_trans

   !> Status codes for Wigner-Seitz cell construction and bound estimation
   type :: enum_wsc_stat
      !> Valid zero-dimensional (non-periodic) geometry
      integer :: valid_0d = 0
      !> Valid periodic geometry
      integer :: valid_pbc = 1
      !> Invalid or degenerate active periodic lattice
      integer :: degenerate_lattice = 2
      !> Nonfinite coordinates or lattice vectors
      integer :: nonfinite_input = 3
      !> Integer overflow during bound or image-count calculation
      integer :: integer_overflow = 4
      !> Candidate translation count or allocation exceeds resource limit
      integer :: resource_exhausted = 5
      !> Invalid molecular structure (e.g., nat <= 0 or unallocated xyz)
      integer :: invalid_structure = 6
   end type enum_wsc_stat

   !> Instance of Wigner-Seitz status enumerator
   type(enum_wsc_stat), parameter :: wsc_stat = enum_wsc_stat()

   !> Wigner-Seitz cell and its nearest image translations
   type :: wignerseitz_cell
      !> Maximum number of images per atom pair
      integer :: nimg_max = 0
      !> Number of images for each atom pair
      integer, allocatable :: nimg(:, :)
      !> Translation indices for each atom pair
      integer, allocatable :: tridx(:, :, :)
      !> Flattened atom-pair image offsets
      integer(i8), allocatable :: itr_list(:)
      !> Number of images for each atom pair in flattened storage
      integer, allocatable :: nimg_list(:)
      !> Flattened translation indices
      integer, allocatable :: tridx_list(:)
      !> Lattice translation vectors
      real(wp), allocatable :: trans(:, :)
   end type wignerseitz_cell

   interface get_pairs
      module procedure get_pairs
      module procedure get_pairs_csr
   end interface get_pairs


   !> Small cutoff threshold to create only closest cells
   real(wp), parameter :: thr = sqrt(epsilon(0.0_wp))

   !> Squared-distance interval for smoothly averaging competing nearest images
   real(wp), parameter :: tol = 0.3_wp

   !> Tolerance for detecting degenerate lattice vectors, areas, and volumes
   real(wp), parameter :: tol_deg = 1.0e-12_wp

   !> Maximum candidate translation vectors before refusing allocation
   integer(i8), parameter :: max_wsc_trans = 100000_i8

   !> Maximum real value convertible to a signed 32-bit repetition bound without overflow
   real(wp), parameter :: max_rep_real = real(huge(0) / 2 - 4, wp)


contains


!> Construct a Wigner-Seitz cell for a molecular structure
subroutine new_wignerseitz_cell(self, mol, error, stat, max_trans)

   !> Wigner-Seitz cell instance
   type(wignerseitz_cell), intent(out) :: self

   !> Molecular structure data
   type(structure_type), intent(in) :: mol

   !> Error handling
   type(error_type), allocatable, intent(out), optional :: error

   !> Status code from Wigner-Seitz bound and cell construction
   integer, intent(out), optional :: stat

   !> Optional override for maximum candidate translation count
   integer(i8), intent(in), optional :: max_trans

   integer :: iat, jat, ntr, nimg, rep(3), local_stat
   integer(i8) :: nat_i8, npair_i8, ntr_i8
   integer, allocatable :: tridx(:)
   real(wp) :: vec(3)
   real(wp), allocatable :: trans(:, :)

   ! Determine required lattice repetition bounds and validate periodic geometry
   call get_wsc_rep(mol, rep, local_stat, max_trans)
   if (present(stat)) stat = local_stat

   if (local_stat /= wsc_stat%valid_0d .and. local_stat /= wsc_stat%valid_pbc) then
      if (present(error)) then
         call report_wsc_error(local_stat, error)
      end if
      return
   end if

   if (local_stat == wsc_stat%valid_0d) then
      allocate(trans(3, 1))
      trans(:, :) = 0.0_wp
   else
      call get_lattice_points(mol%lattice, rep, .true., trans)
      if (.not. allocated(trans)) then
         local_stat = wsc_stat%resource_exhausted
         if (present(stat)) stat = local_stat
         if (present(error)) then
            call report_wsc_error(local_stat, error)
         end if
         return
      end if
   end if

   ntr = size(trans, 2)
   nat_i8 = int(mol%nat, i8)
   ntr_i8 = int(ntr, i8)
   if (nat_i8 > huge(1_i8) / max(1_i8, nat_i8)) then
      local_stat = wsc_stat%integer_overflow
      if (present(stat)) stat = local_stat
      if (present(error)) then
         call report_wsc_error(local_stat, error)
      end if
      return
   end if
   npair_i8 = nat_i8 * nat_i8

   ! Guard 32-bit default-integer allocation extents before allocating nimg/tridx
   if (npair_i8 > int(huge(0), i8) .or. ntr_i8 > int(huge(0), i8) / max(1_i8, npair_i8)) then
      local_stat = wsc_stat%integer_overflow
      if (present(stat)) stat = local_stat
      if (present(error)) then
         call report_wsc_error(local_stat, error)
      end if
      return
   end if

   allocate(self%nimg(mol%nat, mol%nat), self%tridx(ntr, mol%nat, mol%nat), &
   & tridx(ntr))

   !$omp parallel do default(none) schedule(runtime) collapse(2) &
   !$omp& shared(mol, trans, self) private(iat, jat, vec, nimg, tridx)
   do iat = 1, mol%nat
      do jat = 1, mol%nat
         vec(:) = mol%xyz(:, iat) - mol%xyz(:, jat)
         call get_pairs(nimg, trans, vec, tridx)
         self%nimg(jat, iat) = nimg
         self%tridx(:, jat, iat) = tridx
      end do
   end do

   if (mol%nat > 0) self%nimg_max = maxval(self%nimg)
   call move_alloc(trans, self%trans)

end subroutine new_wignerseitz_cell


!> Convert a Wigner-Seitz status code into a descriptive fatal_error
pure subroutine report_wsc_error(stat, error)
   !> Status code from Wigner-Seitz validation
   integer, intent(in) :: stat
   !> Allocated error instance
   type(error_type), allocatable, intent(out) :: error

   select case (stat)
   case (wsc_stat%degenerate_lattice)
      call fatal_error(error, &
         & "Invalid or degenerate active periodic lattice in Wigner-Seitz construction", &
         & stat=stat)
   case (wsc_stat%nonfinite_input)
      call fatal_error(error, &
         & "Nonfinite atomic coordinates or lattice vectors in Wigner-Seitz construction", &
         & stat=stat)
   case (wsc_stat%integer_overflow)
      call fatal_error(error, &
         & "Integer overflow in Wigner-Seitz lattice repetition bound calculation", &
         & stat=stat)
   case (wsc_stat%resource_exhausted)
      call fatal_error(error, &
         & "Wigner-Seitz candidate translation count exceeds maximum resource limit", &
         & stat=stat)
   case default
      call fatal_error(error, &
         & "Invalid molecular structure in Wigner-Seitz construction", &
         & stat=stat)
   end select
end subroutine report_wsc_error


!> Check whether a 1D array contains only finite IEEE values
pure function is_finite_vec(vec) result(finite)
   !> Vector to inspect
   real(wp), intent(in) :: vec(:)
   !> True if all entries are finite
   logical :: finite

   integer :: i

   finite = .true.
   do i = 1, size(vec)
      if (.not. (abs(vec(i)) <= huge(1.0_wp))) then
         finite = .false.
         return
      end if
   end do
end function is_finite_vec


!> Determine lattice repetition bounds needed to cover all Wigner-Seitz nearest
!> images, accounting for unwrapped coordinates and skewed periodic subspaces.
pure subroutine get_wsc_rep(mol, rep, stat, max_trans)

   !> Molecular structure data
   type(structure_type), intent(in) :: mol

   !> Repetition bounds along each lattice vector
   integer, intent(out) :: rep(3)

   !> Optional status code distinguishing valid 0D/PBC from error conditions
   integer, intent(out), optional :: stat

   !> Optional override for maximum candidate translation count
   integer(i8), intent(in), optional :: max_trans

   integer :: i, iat, ndim, p_axes(3), local_stat
   integer(i8) :: rep_i8(3), ntr_i8, factor_i8, limit_i8
   real(wp) :: a1(3), a2(3), a3(3)

   rep(:) = 0
   limit_i8 = max_wsc_trans
   if (present(max_trans)) limit_i8 = max_trans

   ! Validate basic molecular structure allocation and extents
   if (mol%nat <= 0) then
      if (present(stat)) stat = wsc_stat%invalid_structure
      return
   end if
   if (.not. allocated(mol%xyz)) then
      if (present(stat)) stat = wsc_stat%invalid_structure
      return
   end if
   if (size(mol%xyz, 1) < 3 .or. size(mol%xyz, 2) < mol%nat) then
      if (present(stat)) stat = wsc_stat%invalid_structure
      return
   end if

   ! Validate finiteness of atomic coordinates
   do iat = 1, mol%nat
      if (.not. is_finite_vec(mol%xyz(1:3, iat))) then
         if (present(stat)) stat = wsc_stat%nonfinite_input
         return
      end if
   end do

   ! Handle non-periodic (0D) structures explicitly
   if (.not. allocated(mol%periodic)) then
      if (present(stat)) stat = wsc_stat%valid_0d
      return
   end if
   if (.not. any(mol%periodic)) then
      if (present(stat)) stat = wsc_stat%valid_0d
      return
   end if
   if (size(mol%periodic) < 3) then
      if (present(stat)) stat = wsc_stat%invalid_structure
      return
   end if

   ! Periodic structure requires an allocated 3x3 lattice
   if (.not. allocated(mol%lattice)) then
      if (present(stat)) stat = wsc_stat%invalid_structure
      return
   end if
   if (size(mol%lattice, 1) < 3 .or. size(mol%lattice, 2) < 3) then
      if (present(stat)) stat = wsc_stat%invalid_structure
      return
   end if
   if (limit_i8 <= 0_i8) then
      if (present(stat)) stat = wsc_stat%resource_exhausted
      return
   end if

   ndim = 0
   p_axes(:) = 0
   do i = 1, 3
      if (mol%periodic(i)) then
         ndim = ndim + 1
         p_axes(ndim) = i
      end if
   end do

   rep_i8(:) = 0_i8
   local_stat = wsc_stat%valid_pbc

   select case (ndim)
   case (1)
      a1 = mol%lattice(:, p_axes(1))
      if (.not. is_finite_vec(a1)) then
         if (present(stat)) stat = wsc_stat%nonfinite_input
         return
      end if
      call get_wsc_rep_1d(mol%xyz(:, :mol%nat), a1, rep_i8(p_axes(1)), &
         & local_stat, limit_i8)

   case (2)
      a1 = mol%lattice(:, p_axes(1))
      a2 = mol%lattice(:, p_axes(2))
      if (.not. is_finite_vec(a1) .or. .not. is_finite_vec(a2)) then
         if (present(stat)) stat = wsc_stat%nonfinite_input
         return
      end if
      call get_wsc_rep_2d(mol%xyz(:, :mol%nat), a1, a2, &
         & rep_i8(p_axes(1)), rep_i8(p_axes(2)), local_stat, limit_i8)

   case (3)
      a1 = mol%lattice(:, 1)
      a2 = mol%lattice(:, 2)
      a3 = mol%lattice(:, 3)
      if (.not. is_finite_vec(a1) .or. .not. is_finite_vec(a2) &
         & .or. .not. is_finite_vec(a3)) then
         if (present(stat)) stat = wsc_stat%nonfinite_input
         return
      end if
      call get_wsc_rep_3d(a1, a2, a3, rep_i8, local_stat)

   case default
      local_stat = wsc_stat%invalid_structure
   end select

   if (local_stat /= wsc_stat%valid_pbc) then
      rep(:) = 0
      if (present(stat)) stat = local_stat
      return
   end if

   ! Verify 32-bit integer representability and 64-bit product bounds
   ntr_i8 = 1_i8
   do i = 1, 3
      if (rep_i8(i) < 0_i8 .or. rep_i8(i) > int(huge(0) / 2 - 1, i8)) then
         rep(:) = 0
         if (present(stat)) stat = wsc_stat%integer_overflow
         return
      end if
      factor_i8 = 2_i8 * rep_i8(i) + 1_i8
      if (ntr_i8 > huge(1_i8) / factor_i8) then
         rep(:) = 0
         if (present(stat)) stat = wsc_stat%integer_overflow
         return
      end if
      ntr_i8 = ntr_i8 * factor_i8
   end do

   if (ntr_i8 > limit_i8) then
      rep(:) = 0
      if (present(stat)) stat = wsc_stat%resource_exhausted
      return
   end if

   rep(:) = int(rep_i8)
   if (present(stat)) stat = wsc_stat%valid_pbc

end subroutine get_wsc_rep


!> Exact 1D Wigner-Seitz repetition bound for arbitrary coordinates
pure subroutine get_wsc_rep_1d(xyz, a1, rep1, stat, max_trans)
   !> Cartesian atomic coordinates (3, nat)
   real(wp), intent(in) :: xyz(:, :)
   !> Active 1D lattice vector
   real(wp), intent(in) :: a1(3)
   !> Repetition count along a1
   integer(i8), intent(out) :: rep1
   !> Status code
   integer, intent(out) :: stat
   !> Maximum candidate translation count
   integer(i8), intent(in) :: max_trans

   integer :: nat, iat, jat
   integer(i8) :: n0, n_lo, n_hi, k_min
   real(wp) :: len1, g11, rij(3), ds, d_perp2, xi, d0_par2, r_comp2, delta_s, min_step

   rep1 = 0_i8
   len1 = norm2(a1)
   if (len1 <= tol_deg) then
      stat = wsc_stat%degenerate_lattice
      return
   end if
   g11 = len1 * len1
   nat = size(xyz, 2)

   rep1 = 1_i8
   do iat = 1, nat
      do jat = 1, iat
         rij = xyz(:, iat) - xyz(:, jat)
         ds = dot_product(rij, a1) / g11
         if (.not. (abs(ds) <= max_rep_real)) then
            rep1 = 0_i8
            stat = wsc_stat%integer_overflow
            return
         end if
         d_perp2 = sum((rij - ds * a1)**2)
         n0 = nint(ds, i8)
         xi = ds - real(n0, wp)
         d0_par2 = xi * xi * g11
         if (d_perp2 + d0_par2 < thr) then
            min_step = sqrt(max(0.0_wp, thr - d_perp2) / g11)
            if (.not. (min_step <= max_rep_real)) then
               rep1 = 0_i8
               stat = wsc_stat%integer_overflow
               return
            end if
            k_min = max(1_i8, ceiling(min_step - abs(xi), i8))
            d0_par2 = (real(k_min, wp) - abs(xi))**2 * g11
         end if
         r_comp2 = (d0_par2 + tol) * (1.0_wp + 1.0e-12_wp)
         delta_s = sqrt(r_comp2 / g11)
         if (.not. (abs(ds) + delta_s <= max_rep_real)) then
            rep1 = 0_i8
            stat = wsc_stat%integer_overflow
            return
         end if
         n_lo = ceiling(ds - delta_s, i8)
         n_hi = floor(ds + delta_s, i8)
         rep1 = max(rep1, abs(n_lo), abs(n_hi))
         if (2_i8 * rep1 + 1_i8 > max_trans) then
            rep1 = 0_i8
            stat = wsc_stat%resource_exhausted
            return
         end if
      end do
   end do

   stat = wsc_stat%valid_pbc

end subroutine get_wsc_rep_1d


!> Exact 2D Wigner-Seitz repetition bounds via Lagrange-Gauss reduction and Cholesky search
pure subroutine get_wsc_rep_2d(xyz, a1, a2, rep1, rep2, stat, max_trans)
   !> Cartesian atomic coordinates (3, nat)
   real(wp), intent(in) :: xyz(:, :)
   !> First active 2D lattice vector
   real(wp), intent(in) :: a1(3)
   !> Second active 2D lattice vector
   real(wp), intent(in) :: a2(3)
   !> Repetition count along a1
   integer(i8), intent(out) :: rep1
   !> Repetition count along a2
   integer(i8), intent(out) :: rep2
   !> Status code
   integer, intent(out) :: stat
   !> Maximum candidate translation count
   integer(i8), intent(in) :: max_trans

   integer :: nat, iat, jat, iter
   integer(i8) :: u_mat(2, 2), tmp_u(2), mu
   integer(i8) :: n1_0, n2_0, n1, n2, n1_lo, n1_hi, n2_lo, n2_hi, k_min
   integer(i8) :: m1_orig, m2_orig, u_max
   real(wp) :: len1, len2, area, b1(3), b2(3), tmp_v(3)
   real(wp) :: g11, g12, g22, det2, h2_sq
   real(wp) :: rij(3), proj1, proj2, ds1, ds2, d_perp2
   real(wp) :: c1_0, c1, d0_par2, dmin_par2, r_max2, r_comp2, d2_sq, d_par2
   real(wp) :: delta1, delta2, mu_real

   rep1 = 0_i8
   rep2 = 0_i8
   len1 = norm2(a1)
   len2 = norm2(a2)
   if (len1 <= tol_deg .or. len2 <= tol_deg) then
      stat = wsc_stat%degenerate_lattice
      return
   end if
   area = norm2(crossprod(a1, a2))
   if (area <= tol_deg * len1 * len2) then
      stat = wsc_stat%degenerate_lattice
      return
   end if

   ! 2D Lagrange-Gauss basis reduction: [b1, b2] = [a1, a2] * U
   b1 = a1
   b2 = a2
   u_mat = reshape([1_i8, 0_i8, 0_i8, 1_i8], [2, 2])
   do iter = 1, 100
      if (dot_product(b1, b1) > dot_product(b2, b2)) then
         tmp_v = b1; b1 = b2; b2 = tmp_v
         tmp_u = u_mat(:, 1); u_mat(:, 1) = u_mat(:, 2); u_mat(:, 2) = tmp_u
      end if
      g11 = dot_product(b1, b1)
      if (g11 <= tol_deg**2) then
         stat = wsc_stat%degenerate_lattice
         return
      end if
      mu_real = dot_product(b1, b2) / g11
      if (.not. (abs(mu_real) <= max_rep_real)) then
         stat = wsc_stat%integer_overflow
         return
      end if
      mu = nint(mu_real, i8)
      if (mu == 0_i8) exit
      if (maxval(abs(u_mat)) > int(huge(0) / 4, i8) / max(1_i8, abs(mu))) then
         stat = wsc_stat%integer_overflow
         return
      end if
      b2 = b2 - real(mu, wp) * b1
      u_mat(:, 2) = u_mat(:, 2) - mu * u_mat(:, 1)
   end do

   g11 = dot_product(b1, b1)
   g12 = dot_product(b1, b2)
   g22 = dot_product(b2, b2)
   det2 = g11 * g22 - g12 * g12
   if (det2 <= (tol_deg**2) * g11 * g22) then
      stat = wsc_stat%degenerate_lattice
      return
   end if
   h2_sq = det2 / g11
   if (sqrt(tol / g11) > max_rep_real .or. sqrt(tol / h2_sq) > max_rep_real) then
      stat = wsc_stat%integer_overflow
      return
   end if
   if (sqrt(tol / g11) > real(max_trans, wp) .or. sqrt(tol / h2_sq) > real(max_trans, wp)) then
      stat = wsc_stat%resource_exhausted
      return
   end if

   u_max = max(1_i8, maxval(abs(u_mat)))
   nat = size(xyz, 2)
   rep1 = 1_i8
   rep2 = 1_i8

   do iat = 1, nat
      do jat = 1, iat
         rij = xyz(:, iat) - xyz(:, jat)
         proj1 = dot_product(rij, b1)
         proj2 = dot_product(rij, b2)
         ds1 = (g22 * proj1 - g12 * proj2) / det2
         ds2 = (-g12 * proj1 + g11 * proj2) / det2
         if (.not. (abs(ds1) <= max_rep_real .and. abs(ds2) <= max_rep_real)) then
            rep1 = 0_i8
            rep2 = 0_i8
            stat = wsc_stat%integer_overflow
            return
         end if
         d_perp2 = sum((rij - ds1 * b1 - ds2 * b2)**2)

         ! Babai nearest-plane point for initial upper bound d0_par2
         n2_0 = nint(ds2, i8)
         c1_0 = ds1 - (g12 / g11) * (real(n2_0, wp) - ds2)
         if (.not. (abs(c1_0) <= max_rep_real)) then
            rep1 = 0_i8
            rep2 = 0_i8
            stat = wsc_stat%integer_overflow
            return
         end if
         n1_0 = nint(c1_0, i8)
         d0_par2 = g11 * (real(n1_0, wp) - c1_0)**2 + h2_sq * (real(n2_0, wp) - ds2)**2
         if (d_perp2 + d0_par2 < thr) then
            if (.not. (sqrt(max(0.0_wp, thr - d_perp2) / g11) <= max_rep_real)) then
               rep1 = 0_i8
               rep2 = 0_i8
               stat = wsc_stat%integer_overflow
               return
            end if
            k_min = max(1_i8, ceiling(sqrt(max(0.0_wp, thr - d_perp2) / g11) &
               & - abs(real(n1_0, wp) - c1_0), i8))
            d0_par2 = g11 * (abs(real(n1_0, wp) - c1_0) + real(k_min, wp))**2 &
               & + h2_sq * (real(n2_0, wp) - ds2)**2
         end if

         ! Pass 1: exact minimum periodic distance squared dmin_par2
         r_max2 = (d0_par2 + tol) * (1.0_wp + 1.0e-12_wp)
         delta2 = sqrt(r_max2 / h2_sq)
         if (.not. (abs(ds2) + delta2 <= max_rep_real &
            & .and. sqrt(r_max2 / g11) <= max_rep_real)) then
            rep1 = 0_i8
            rep2 = 0_i8
            stat = wsc_stat%integer_overflow
            return
         end if
         if (2.0_wp * delta2 + 1.0_wp > real(max_trans, wp) &
            & .or. 2.0_wp * sqrt(r_max2 / g11) + 1.0_wp > real(max_trans, wp)) then
            rep1 = 0_i8
            rep2 = 0_i8
            stat = wsc_stat%resource_exhausted
            return
         end if
         n2_lo = ceiling(ds2 - delta2, i8)
         n2_hi = floor(ds2 + delta2, i8)
         dmin_par2 = d0_par2

         do n2 = n2_lo, n2_hi
            d2_sq = h2_sq * (real(n2, wp) - ds2)**2
            if (d2_sq > r_max2) cycle
            c1 = ds1 - (g12 / g11) * (real(n2, wp) - ds2)
            delta1 = sqrt((r_max2 - d2_sq) / g11)
            if (.not. (abs(c1) + delta1 <= max_rep_real)) then
               rep1 = 0_i8
               rep2 = 0_i8
               stat = wsc_stat%integer_overflow
               return
            end if
            n1_lo = ceiling(c1 - delta1, i8)
            n1_hi = floor(c1 + delta1, i8)
            do n1 = n1_lo, n1_hi
               d_par2 = g11 * (real(n1, wp) - c1)**2 + d2_sq
               if (d_perp2 + d_par2 < thr) cycle
               dmin_par2 = min(dmin_par2, d_par2)
            end do
         end do

         ! Pass 2: collect all competing images within tol of dmin_par2
         r_comp2 = (dmin_par2 + tol) * (1.0_wp + 1.0e-12_wp)
         delta2 = sqrt(r_comp2 / h2_sq)
         n2_lo = ceiling(ds2 - delta2, i8)
         n2_hi = floor(ds2 + delta2, i8)

         do n2 = n2_lo, n2_hi
            d2_sq = h2_sq * (real(n2, wp) - ds2)**2
            if (d2_sq > r_comp2) cycle
            c1 = ds1 - (g12 / g11) * (real(n2, wp) - ds2)
            delta1 = sqrt((r_comp2 - d2_sq) / g11)
            n1_lo = ceiling(c1 - delta1, i8)
            n1_hi = floor(c1 + delta1, i8)
            do n1 = n1_lo, n1_hi
               d_par2 = g11 * (real(n1, wp) - c1)**2 + d2_sq
               if (d_perp2 + d_par2 < thr) cycle
               if (d_par2 > r_comp2) cycle
               if (max(abs(n1), abs(n2)) > (huge(1_i8) / 4_i8) / u_max) then
                  rep1 = 0_i8
                  rep2 = 0_i8
                  stat = wsc_stat%integer_overflow
                  return
               end if
               m1_orig = u_mat(1, 1) * n1 + u_mat(1, 2) * n2
               m2_orig = u_mat(2, 1) * n1 + u_mat(2, 2) * n2
               rep1 = max(rep1, abs(m1_orig))
               rep2 = max(rep2, abs(m2_orig))
            end do
         end do
      end do
   end do

   stat = wsc_stat%valid_pbc

end subroutine get_wsc_rep_2d


!> Check whether a 3D lattice is finite and non-degenerate
pure subroutine get_wsc_rep_3d(a1, a2, a3, rep, stat)
   !> First 3D lattice vector
   real(wp), intent(in) :: a1(3)
   !> Second 3D lattice vector
   real(wp), intent(in) :: a2(3)
   !> Third 3D lattice vector
   real(wp), intent(in) :: a3(3)
   !> Repetition counts along (a1, a2, a3)
   integer(i8), intent(out) :: rep(3)
   !> Status code
   integer, intent(out) :: stat

   real(wp) :: len1, len2, len3, vol

   rep(:) = 0_i8
   len1 = norm2(a1)
   len2 = norm2(a2)
   len3 = norm2(a3)
   if (len1 <= tol_deg .or. len2 <= tol_deg .or. len3 <= tol_deg) then
      stat = wsc_stat%degenerate_lattice
      return
   end if
   vol = abs(dot_product(a1, crossprod(a2, a3)))
   if (vol <= tol_deg * len1 * len2 * len3) then
      stat = wsc_stat%degenerate_lattice
      return
   end if

   rep(:) = 1_i8
   stat = wsc_stat%valid_pbc

end subroutine get_wsc_rep_3d

!> Find the nearest translation images for an interatomic vector
subroutine get_pairs(iws, trans, rij, list)
   !> Number of nearest images
   integer, intent(out) :: iws
   !> Translation vectors
   real(wp), intent(in) :: trans(:, :)
   !> Interatomic vector
   real(wp), intent(in) :: rij(3)
   !> Indices of the nearest translation images
   integer, intent(out) :: list(:)

   logical :: mask(size(list))
   real(wp) :: dist(size(list)), vec(3), r2
   integer :: itr, img, pos
   integer :: index(size(list))

   iws = 0
   img = 0
   list(:) = 0
   index(:) = 0
   mask(:) = .true.

   do itr = 1, size(trans, 2)
      vec(:) = rij - trans(:, itr)
      r2 = vec(1)**2 + vec(2)**2 + vec(3)**2
      if (r2 < thr) cycle
      img = img + 1
      dist(img) = r2
      index(img) = itr
   end do

   if (img == 0) return

   pos = minloc(dist(:img), dim=1)

   r2 = dist(pos)
   mask(pos) = .false.

   iws = 1
   list(iws) = index(pos)
   if (img <= iws) return

   do while (iws < img)
      pos = minloc(dist(:img), dim=1, mask=mask(:img))
      if (pos <= 0) exit
      if (dist(pos) - r2 > tol) exit
      mask(pos) = .false.
      iws = iws + 1
      list(iws) = index(pos)
   end do

end subroutine get_pairs

!> Find nearest translation images and their minimum squared distance
subroutine get_pairs_csr(trans, rij, iws, list, min_r2)
   !> Translation vectors
   real(wp), intent(in) :: trans(:, :)
   !> Interatomic vector
   real(wp), intent(in) :: rij(3)
   !> Number of images for a pair
   integer, intent(out) :: iws
   !> Indices of the nearest translation images
   integer, intent(out) :: list(:)
   !> Minimum squared distance found
   real(wp), intent(out) :: min_r2

   real(wp) :: dx, dy, dz, dist(size(trans, 2))
   integer :: itr, pos

   iws = 0
   min_r2 = huge(1.0_wp)

   ! First pass, distances of all images and the actual minimum
   do itr = 1, size(trans, 2)
      dx = rij(1) - trans(1, itr)
      dy = rij(2) - trans(2, itr)
      dz = rij(3) - trans(3, itr)
      dist(itr) = dx*dx + dy*dy + dz*dz
      if (dist(itr) < thr) cycle
      min_r2 = min(min_r2, dist(itr))
   end do

   ! Second pass, collect all images within the tolerance of the minimum,
   ! sorted by distance via insertion
   do itr = 1, size(trans, 2)
      if (dist(itr) < thr) cycle
      if (dist(itr) - min_r2 > tol) cycle
      iws = iws + 1
      pos = iws
      do while (pos > 1)
         if (dist(list(pos - 1)) <= dist(itr)) exit
         list(pos) = list(pos - 1)
         pos = pos - 1
      end do
      list(pos) = itr
   end do

end subroutine get_pairs_csr

!> Compact C2 switching function for competing nearest images
pure elemental function smooth_image_weight(delta) result(weight)
   !> Squared-distance difference from the nearest image
   real(wp), intent(in) :: delta
   real(wp) :: weight, x

   x = min(1.0_wp, max(0.0_wp, delta)/tol)
   weight = max(0.0_wp, 1.0_wp - 10.0_wp*x**3 + 15.0_wp*x**4 - 6.0_wp*x**5)
end function smooth_image_weight


!> Derivative of the compact switching function with respect to squared distance
pure elemental function smooth_image_weight_derivative(delta) result(derivative)
   !> Squared-distance difference from the nearest image
   real(wp), intent(in) :: delta
   real(wp) :: derivative, x

   x = min(1.0_wp, max(0.0_wp, delta)/tol)
   derivative = -30.0_wp*x*x*(1.0_wp - x)**2/tol
end function smooth_image_weight_derivative


!> Evaluate smooth weights for competing nearest Wigner-Seitz images
subroutine get_wignerseitz_weights(self, jat, iat, rij, weight, dwdr, dwdL)
   !> Wigner-Seitz cell instance
   type(wignerseitz_cell), intent(in) :: self
   !> Index of reference atom in the Wigner-Seitz image arrays
   integer, intent(in) :: iat
   !> Index of examined atom in the Wigner-Seitz image arrays
   integer, intent(in) :: jat
   !> Cartesian pair vector
   real(wp), intent(in) :: rij(3)
   !> Normalized image weights
   real(wp), intent(out) :: weight(:)
   !> Weight derivatives with respect to the pair vector
   real(wp), intent(out), optional :: dwdr(:, :)
   !> Weight derivatives with respect to strain
   real(wp), intent(out), optional :: dwdL(:, :, :)

   integer :: img, idx, nimg, refidx
   real(wp) :: delta, dshape, sum_shape
   real(wp) :: vec(3), refvec(3), ddelta(3), sum_dr(3)
   real(wp) :: ddeltaL(3, 3), sum_dL(3, 3)

   weight(:) = 0.0_wp
   if (present(dwdr)) dwdr(:, :) = 0.0_wp
   if (present(dwdL)) dwdL(:, :, :) = 0.0_wp

   nimg = self%nimg(jat, iat)
   if (nimg == 0) return

   refidx = self%tridx(1, jat, iat)
   refvec = rij - self%trans(:, refidx)
   sum_shape = 0.0_wp

   do img = 1, nimg
      idx = self%tridx(img, jat, iat)
      vec = rij - self%trans(:, idx)
      delta = max(0.0_wp, dot_product(vec, vec) - dot_product(refvec, refvec))
      weight(img) = smooth_image_weight(delta)
      sum_shape = sum_shape + weight(img)
   end do

   weight(:nimg) = weight(:nimg) / sum_shape
   if (.not.present(dwdr) .and. .not.present(dwdL)) return

   sum_dr(:) = 0.0_wp
   sum_dL(:, :) = 0.0_wp
   do img = 1, nimg
      idx = self%tridx(img, jat, iat)
      vec = rij - self%trans(:, idx)
      delta = max(0.0_wp, dot_product(vec, vec) - dot_product(refvec, refvec))
      dshape = smooth_image_weight_derivative(delta)
      ddelta(:) = 2.0_wp*(vec - refvec)
      ddeltaL(:, :) = 2.0_wp*(outerprod(vec, vec) - outerprod(refvec, refvec))
      sum_dr(:) = sum_dr + dshape*ddelta
      sum_dL(:, :) = sum_dL + dshape*ddeltaL
   end do

   do img = 1, nimg
      idx = self%tridx(img, jat, iat)
      vec = rij - self%trans(:, idx)
      delta = max(0.0_wp, dot_product(vec, vec) - dot_product(refvec, refvec))
      dshape = smooth_image_weight_derivative(delta)
      ddelta(:) = 2.0_wp*(vec - refvec)
      ddeltaL(:, :) = 2.0_wp*(outerprod(vec, vec) - outerprod(refvec, refvec))
      if (present(dwdr)) then
         dwdr(:, img) = (dshape*ddelta - weight(img)*sum_dr) / sum_shape
      end if
      if (present(dwdL)) then
         dwdL(:, :, img) = (dshape*ddeltaL - weight(img)*sum_dL) / sum_shape
      end if
   end do

end subroutine get_wignerseitz_weights

end module mctc_wignerseitz
