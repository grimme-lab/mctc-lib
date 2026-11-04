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
!>
!> Periodic Wigner-Seitz cell construction for DFT-D4 and related models following:
!> E. Caldeweyher, J.-M. Mewes, S. Ehlert and S. Grimme, *Phys. Chem. Chem. Phys.*,
!> **2020**, 22, 8499-8512. DOI: [10.1039/D0CP00502A](https://doi.org/10.1039/D0CP00502A)
module mctc_wignerseitz
   use, intrinsic :: ieee_arithmetic, only : ieee_is_finite
   use, intrinsic :: iso_fortran_env, only : error_unit
   use mctc_cutoff, only : get_lattice_points
   use mctc_env, only : wp, i8, error_type, fatal_error
   use mctc_io, only : structure_type
   use mctc_io_math, only : crossprod, outerprod
   implicit none
   private

   public :: new_wignerseitz_cell, get_wignerseitz_weights, wignerseitz_cell, &
      & get_pairs

   !> Wigner-Seitz cell and its nearest image translations.
   !>
   !> Dense Wigner-Seitz construction (new_wignerseitz_cell) populates nimg, tridx,
   !> and trans. CSR neighbor-list construction (mctc_csrlist_type) populates the
   !> compressed list arrays itr_list, nimg_list, and tridx_list.
   type :: wignerseitz_cell
      !> Maximum number of images per atom pair
      integer :: nimg_max = 0
      !> Number of images for each atom pair (nat, nat)
      integer, allocatable :: nimg(:, :)
      !> Translation indices for each atom pair (nimg_alloc, nat, nat)
      integer, allocatable :: tridx(:, :, :)
      !> Flattened atom-pair image offsets used by CSR neighbor lists
      integer(i8), allocatable :: itr_list(:)
      !> Number of images per CSR neighbor-list pair
      integer, allocatable :: nimg_list(:)
      !> Flattened translation indices used by CSR neighbor lists
      integer, allocatable :: tridx_list(:)
      !> Lattice translation vectors (3, ntrans)
      real(wp), allocatable :: trans(:, :)
   end type wignerseitz_cell

   interface new_wignerseitz_cell
      module procedure :: new_wignerseitz_cell
      module procedure :: new_wignerseitz_cell_legacy
   end interface new_wignerseitz_cell

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

   !> Safe 64-bit upper bound for unimodular matrix entries and reduced coordinates
   integer(i8), parameter :: max_u_step_i8 = int((huge(0) - 3) / 4, i8)

   !> Maximum real coordinate projection before rounding in reduced basis
   real(wp), parameter :: max_rep_real = real(max_u_step_i8, wp)


contains


!> Construct a Wigner-Seitz cell for a molecular structure
subroutine new_wignerseitz_cell(self, mol, error)

   !> Wigner-Seitz cell instance
   type(wignerseitz_cell), intent(out) :: self

   !> Molecular structure data
   type(structure_type), intent(in) :: mol

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   integer, parameter :: init_tridx_cap = 8
   integer :: iat, jat, nimg, nkeep, nimg_alloc, rep(3)
   integer :: tridx_buf(init_tridx_cap)
   integer, allocatable :: tridx_init(:, :, :)
   real(wp) :: vec(3), min_r2
   real(wp), allocatable :: trans(:, :)

   ! Determine required lattice repetition bounds and validate periodic geometry
   call get_wsc_rep(mol, rep, error)
   if (allocated(error)) return

   ! Generate candidate lattice translations covering all nearest and competing images
   if (.not. allocated(mol%periodic)) then
      allocate(trans(3, 1))
      trans(:, :) = 0.0_wp
   else if (.not. any(mol%periodic)) then
      allocate(trans(3, 1))
      trans(:, :) = 0.0_wp
   else
      call get_lattice_points(mol%lattice, rep, .true., trans, error)
      if (allocated(error)) return
   end if

   ! First pass over atom pairs: record exact competing-image counts in self%nimg
   ! and store up to init_tridx_cap = 8 translation indices per pair.
   allocate(self%nimg(mol%nat, mol%nat), tridx_init(init_tridx_cap, mol%nat, mol%nat))
   tridx_init(:, :, :) = 0

   !$omp parallel do default(none) schedule(runtime) collapse(2) &
   !$omp& shared(mol, trans, self, tridx_init) &
   !$omp& private(iat, jat, vec, nimg, nkeep, tridx_buf, min_r2)
   do iat = 1, mol%nat
      do jat = 1, mol%nat
         vec(:) = mol%xyz(:, iat) - mol%xyz(:, jat)
         call find_wsc_pairs(trans, vec, nimg, tridx_buf, min_r2)
         self%nimg(jat, iat) = nimg
         nkeep = min(nimg, init_tridx_cap)
         if (nkeep > 0) tridx_init(1:nkeep, jat, iat) = tridx_buf(1:nkeep)
      end do
   end do

   ! Compact self%tridx to the actual maximum image count across all pairs,
   ! re-evaluating pairs only if a high-symmetry pair exceeded init_tridx_cap.
   self%nimg_max = maxval(self%nimg)
   nimg_alloc = max(1, self%nimg_max)

   if (nimg_alloc == init_tridx_cap) then
      call move_alloc(tridx_init, self%tridx)
   else if (nimg_alloc < init_tridx_cap) then
      allocate(self%tridx(nimg_alloc, mol%nat, mol%nat))
      self%tridx(1:nimg_alloc, :, :) = tridx_init(1:nimg_alloc, :, :)
      deallocate(tridx_init)
   else
      deallocate(tridx_init)
      allocate(self%tridx(nimg_alloc, mol%nat, mol%nat))
      self%tridx(:, :, :) = 0
      !$omp parallel do default(none) schedule(runtime) collapse(2) &
      !$omp& shared(mol, trans, self) private(iat, jat, vec, nimg, min_r2)
      do iat = 1, mol%nat
         do jat = 1, mol%nat
            if (self%nimg(jat, iat) <= 0) cycle
            vec(:) = mol%xyz(:, iat) - mol%xyz(:, jat)
            call find_wsc_pairs(trans, vec, nimg, self%tridx(:, jat, iat), min_r2)
         end do
      end do
   end if

   call move_alloc(trans, self%trans)

end subroutine new_wignerseitz_cell


!> Construct a Wigner-Seitz cell for a molecular structure.
!>
!> @note Deprecated: pass an explicit `error` argument instead.
subroutine new_wignerseitz_cell_legacy(self, mol)

   !> Wigner-Seitz cell instance
   type(wignerseitz_cell), intent(out) :: self

   !> Molecular structure data
   type(structure_type), intent(in) :: mol

   type(error_type), allocatable :: error

   call new_wignerseitz_cell(self, mol, error)
   if (allocated(error)) then
      write(error_unit, "(a)") error%message
      error stop
   end if

end subroutine new_wignerseitz_cell_legacy


!> Determine lattice repetition bounds needed to cover all Wigner-Seitz nearest
!> images, accounting for unwrapped coordinates and skewed periodic subspaces.
pure subroutine get_wsc_rep(mol, rep, error)

   !> Molecular structure data
   type(structure_type), intent(in) :: mol

   !> Repetition bounds along each lattice vector
   integer, intent(out) :: rep(3)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   integer :: i, ndim, p_axes(3)
   integer(i8) :: rep_i8(3), ntr_i8

   rep(:) = 0

   ! Validate basic molecular structure allocation and extents
   if (mol%nat <= 0) then
      call fatal_error(error, &
         & "Invalid molecular structure in Wigner-Seitz construction")
      return
   end if
   if (.not. allocated(mol%xyz)) then
      call fatal_error(error, &
         & "Invalid molecular structure in Wigner-Seitz construction")
      return
   end if
   if (size(mol%xyz, 1) < 3 .or. size(mol%xyz, 2) < mol%nat) then
      call fatal_error(error, &
         & "Invalid molecular structure in Wigner-Seitz construction")
      return
   end if

   ! Validate finiteness of atomic coordinates
   if (.not. all(ieee_is_finite(mol%xyz(1:3, 1:mol%nat)))) then
      call fatal_error(error, &
         & "Nonfinite atomic coordinates or lattice vectors in Wigner-Seitz construction")
      return
   end if

   ! Handle non-periodic (0D) structures
   if (.not. allocated(mol%periodic)) return
   if (.not. any(mol%periodic)) return
   if (size(mol%periodic) < 3) then
      call fatal_error(error, &
         & "Invalid molecular structure in Wigner-Seitz construction")
      return
   end if

   ! Periodic structure requires an allocated 3x3 lattice
   if (.not. allocated(mol%lattice)) then
      call fatal_error(error, &
         & "Invalid molecular structure in Wigner-Seitz construction")
      return
   end if
   if (size(mol%lattice, 1) < 3 .or. size(mol%lattice, 2) < 3) then
      call fatal_error(error, &
         & "Invalid molecular structure in Wigner-Seitz construction")
      return
   end if

   ! Collect active periodic axes and validate finiteness of active lattice vectors
   ndim = 0
   p_axes(:) = 0
   do i = 1, 3
      if (mol%periodic(i)) then
         if (.not. all(ieee_is_finite(mol%lattice(:, i)))) then
            call fatal_error(error, &
               & "Nonfinite atomic coordinates or lattice vectors in Wigner-Seitz construction")
            return
         end if
         ndim = ndim + 1
         p_axes(ndim) = i
      end if
   end do

   rep_i8(:) = 0_i8

   select case (ndim)
   case (1)
      call get_wsc_rep_1d(mol%xyz(:, :mol%nat), mol%lattice(:, p_axes(1)), &
         & rep_i8(p_axes(1)), error)

   case (2)
      call get_wsc_rep_2d(mol%xyz(:, :mol%nat), mol%lattice(:, p_axes(1)), &
         & mol%lattice(:, p_axes(2)), rep_i8(p_axes(1)), rep_i8(p_axes(2)), error)

   case (3)
      call get_wsc_rep_3d(mol%xyz(:, :mol%nat), mol%lattice(:, 1), &
         & mol%lattice(:, 2), mol%lattice(:, 3), rep_i8, error)

   case default
      call fatal_error(error, &
         & "Invalid molecular structure in Wigner-Seitz construction")
   end select

   if (allocated(error)) return

   ! Bound each repetition count and accumulated product by max_wsc_trans in 64-bit
   ! integer arithmetic so intermediate products and 32-bit conversion cannot overflow.
   ntr_i8 = 1_i8
   do i = 1, 3
      if (rep_i8(i) > max_wsc_trans) then
         call fatal_error(error, &
            & "Wigner-Seitz candidate translation count exceeds maximum resource limit")
         return
      end if
      ntr_i8 = ntr_i8 * (2_i8 * rep_i8(i) + 1_i8)
      if (ntr_i8 > max_wsc_trans) then
         call fatal_error(error, &
            & "Wigner-Seitz candidate translation count exceeds maximum resource limit")
         return
      end if
   end do

   rep(:) = int(rep_i8)

end subroutine get_wsc_rep


!> Exact 1D Wigner-Seitz repetition bound for arbitrary coordinates
pure subroutine get_wsc_rep_1d(xyz, a1, rep1, error)
   !> Cartesian atomic coordinates (3, nat)
   real(wp), intent(in) :: xyz(:, :)
   !> Active 1D lattice vector
   real(wp), intent(in) :: a1(3)
   !> Repetition count along a1
   integer(i8), intent(out) :: rep1
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   integer :: nat, iat, jat
   integer(i8) :: n0, n_lo, n_hi, k_min, rep_max
   real(wp) :: len1, g11, rij(3), ds, d_perp2, xi, d0_par2, r_comp2, delta_s, min_step

   rep1 = 0_i8
   len1 = norm2(a1)
   if (len1 <= tol_deg) then
      call fatal_error(error, &
         & "Invalid or degenerate active periodic lattice in Wigner-Seitz construction")
      return
   end if
   g11 = len1 * len1
   if (sqrt(tol / g11) > real(max_wsc_trans, wp)) then
      call fatal_error(error, &
         & "Wigner-Seitz candidate translation count exceeds maximum resource limit")
      return
   end if

   nat = size(xyz, 2)
   rep_max = 1_i8

   do iat = 1, nat
      do jat = 1, iat

         ! Coordinate transformation: decompose pair vector rij into parallel fractional
         ! coordinate ds = (rij . a1) / |a1|^2 and perpendicular squared distance d_perp2
         rij = xyz(:, iat) - xyz(:, jat)
         ds = dot_product(rij, a1) / g11
         if (.not. (abs(ds) <= max_rep_real)) then
            call fatal_error(error, &
               & "Integer overflow in Wigner-Seitz lattice repetition bound calculation")
            return
         end if
         d_perp2 = sum((rij - ds * a1)**2)

         ! Initial search bound: nearest integer shift n0 = nint(ds); if (n0) is a
         ! self-image (total squared distance < thr), step to the closest non-self integer
         n0 = nint(ds, i8)
         xi = ds - real(n0, wp)
         d0_par2 = xi * xi * g11
         if (d_perp2 + d0_par2 < thr) then
            min_step = sqrt(max(0.0_wp, thr - d_perp2) / g11)
            k_min = max(1_i8, ceiling(min_step - abs(xi), i8))
            d0_par2 = (real(k_min, wp) - abs(xi))**2 * g11
         end if

         ! Competing images: find all integer shifts n with (n - ds)^2 |a1|^2 <= d0_par2 + tol
         r_comp2 = (d0_par2 + tol) * (1.0_wp + 1.0e-12_wp)
         delta_s = sqrt(r_comp2 / g11)
         n_lo = ceiling(ds - delta_s, i8)
         n_hi = floor(ds + delta_s, i8)
         rep_max = max(rep_max, abs(n_lo), abs(n_hi))
         if (2_i8 * rep_max + 1_i8 > max_wsc_trans) then
            call fatal_error(error, &
               & "Wigner-Seitz candidate translation count exceeds maximum resource limit")
            return
         end if
      end do
   end do

   rep1 = rep_max

end subroutine get_wsc_rep_1d


!> Exact 2D Wigner-Seitz repetition bounds via Lagrange-Gauss reduction and Cholesky search
pure subroutine get_wsc_rep_2d(xyz, a1, a2, rep1, rep2, error)
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
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   real(wp), parameter :: tol_mu = 0.5_wp * (1.0_wp + 1.0e-12_wp)
   integer :: nat, iat, jat, iter
   integer(i8) :: u_mat(2, 2), tmp_u(2), mu
   integer(i8) :: n1_0, n2_0, n1, n2, n1_lo, n1_hi, n2_lo, n2_hi, k_min
   integer(i8) :: m1_orig, m2_orig, rep1_max, rep2_max
   real(wp) :: len1, len2, area, b1(3), b2(3), tmp_v(3)
   real(wp) :: g11, g12, g22, det2, h2_sq
   real(wp) :: rij(3), proj1, proj2, ds1, ds2, d_perp2
   real(wp) :: c1_0, c1, d0_par2, dmin_par2, r_max2, r_comp2, d2_sq, d_par2
   real(wp) :: delta1, delta2, mu_real
   logical :: reduced

   rep1 = 0_i8
   rep2 = 0_i8
   len1 = norm2(a1)
   len2 = norm2(a2)
   if (len1 <= tol_deg .or. len2 <= tol_deg) then
      call fatal_error(error, &
         & "Invalid or degenerate active periodic lattice in Wigner-Seitz construction")
      return
   end if
   area = norm2(crossprod(a1, a2))
   if (area <= tol_deg * len1 * len2) then
      call fatal_error(error, &
         & "Invalid or degenerate active periodic lattice in Wigner-Seitz construction")
      return
   end if

   ! Basis reduction: Lagrange-Gauss reduction [b1, b2] = [a1, a2] * U with
   ! unimodular U in GL(2, Z) such that |b1| <= |b2| and |b1 . b2| <= |b1|^2 / 2.
   ! Unlike Ewald summation (which sums directly in the reduced basis), Wigner-Seitz
   ! cell construction must track U to map competing images back to the input basis.
   b1 = a1
   b2 = a2
   u_mat = reshape([1_i8, 0_i8, 0_i8, 1_i8], [2, 2])
   reduced = .false.
   do iter = 1, 100
      if (dot_product(b1, b1) > dot_product(b2, b2)) then
         tmp_v = b1; b1 = b2; b2 = tmp_v
         tmp_u = u_mat(:, 1); u_mat(:, 1) = u_mat(:, 2); u_mat(:, 2) = tmp_u
      end if
      g11 = dot_product(b1, b1)
      if (g11 <= tol_deg**2) then
         call fatal_error(error, &
            & "Invalid or degenerate active periodic lattice in Wigner-Seitz construction")
         return
      end if
      mu_real = dot_product(b1, b2) / g11
      if (.not. (abs(mu_real) <= max_rep_real)) then
         call fatal_error(error, &
            & "Integer overflow in Wigner-Seitz lattice repetition bound calculation")
         return
      end if
      if (abs(mu_real) <= tol_mu) then
         reduced = .true.
         exit
      end if
      mu = nint(mu_real, i8)
      if (maxval(abs(u_mat)) > max_u_step_i8 / max(1_i8, abs(mu))) then
         call fatal_error(error, &
            & "Integer overflow in Wigner-Seitz lattice repetition bound calculation")
         return
      end if
      b2 = b2 - real(mu, wp) * b1
      u_mat(:, 2) = u_mat(:, 2) - mu * u_mat(:, 1)
   end do
   if (.not. reduced) then
      call fatal_error(error, &
         & "Invalid or degenerate active periodic lattice in Wigner-Seitz construction")
      return
   end if

   ! Metric tensor G = B^T B and Cholesky height h2^2 = det(G) / g11 of b2 orthogonal to b1
   g11 = dot_product(b1, b1)
   g12 = dot_product(b1, b2)
   g22 = dot_product(b2, b2)
   det2 = g11 * g22 - g12 * g12
   if (det2 <= (tol_deg**2) * g11 * g22) then
      call fatal_error(error, &
         & "Invalid or degenerate active periodic lattice in Wigner-Seitz construction")
      return
   end if
   h2_sq = det2 / g11
   if (sqrt(tol / g11) > real(max_wsc_trans, wp) .or. sqrt(tol / h2_sq) > real(max_wsc_trans, wp)) then
      call fatal_error(error, &
         & "Wigner-Seitz candidate translation count exceeds maximum resource limit")
      return
   end if

   nat = size(xyz, 2)
   rep1_max = 1_i8
   rep2_max = 1_i8

   do iat = 1, nat
      do jat = 1, iat

         ! Coordinate transformation: express pair vector rij in the reduced basis
         ! [b1, b2] via G^{-1} to obtain fractional coordinates (ds1, ds2) and the
         ! out-of-plane perpendicular squared distance d_perp2
         rij = xyz(:, iat) - xyz(:, jat)
         proj1 = dot_product(rij, b1)
         proj2 = dot_product(rij, b2)
         ds1 = (g22 * proj1 - g12 * proj2) / det2
         ds2 = (-g12 * proj1 + g11 * proj2) / det2
         if (.not. (abs(ds1) <= max_rep_real .and. abs(ds2) <= max_rep_real)) then
            call fatal_error(error, &
               & "Integer overflow in Wigner-Seitz lattice repetition bound calculation")
            return
         end if
         d_perp2 = sum((rij - ds1 * b1 - ds2 * b2)**2)

         ! Initial search bound: Babai nearest-plane integer estimate (n1_0, n2_0) using
         ! the Cholesky decomposition d_par^2 = g11 * (n1 - c1)^2 + h2_sq * (n2 - ds2)^2;
         ! if this point is a self-image (< thr), step along b1 to a valid upper bound d0_par2
         n2_0 = nint(ds2, i8)
         c1_0 = ds1 - (g12 / g11) * (real(n2_0, wp) - ds2)
         n1_0 = nint(c1_0, i8)
         d0_par2 = g11 * (real(n1_0, wp) - c1_0)**2 + h2_sq * (real(n2_0, wp) - ds2)**2
         if (d_perp2 + d0_par2 < thr) then
            k_min = max(1_i8, ceiling(sqrt(max(0.0_wp, thr - d_perp2) / g11) &
               & - abs(real(n1_0, wp) - c1_0), i8))
            d0_par2 = g11 * (abs(real(n1_0, wp) - c1_0) + real(k_min, wp))**2 &
               & + h2_sq * (real(n2_0, wp) - ds2)**2
         end if

         ! Nearest-image search: enumerate all integer points (n1, n2) inside the
         ! Cholesky ellipse d_par^2 <= d0_par2 + tol to find the exact minimum dmin_par2
         r_max2 = (d0_par2 + tol) * (1.0_wp + 1.0e-12_wp)
         delta2 = sqrt(r_max2 / h2_sq)
         if (2.0_wp * delta2 + 1.0_wp > real(max_wsc_trans, wp) &
            & .or. 2.0_wp * sqrt(r_max2 / g11) + 1.0_wp > real(max_wsc_trans, wp)) then
            call fatal_error(error, &
               & "Wigner-Seitz candidate translation count exceeds maximum resource limit")
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
            n1_lo = ceiling(c1 - delta1, i8)
            n1_hi = floor(c1 + delta1, i8)
            do n1 = n1_lo, n1_hi
               d_par2 = g11 * (real(n1, wp) - c1)**2 + d2_sq
               if (d_perp2 + d_par2 < thr) cycle
               dmin_par2 = min(dmin_par2, d_par2)
            end do
         end do

         ! Competing images: enumerate all integer points within tol of dmin_par2
         ! that receive non-zero weight in the smooth Wigner-Seitz switching window
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

               ! Return to the original basis: map reduced integer coordinates
               ! (n1, n2) back to unreduced lattice indices m = U * n
               m1_orig = u_mat(1, 1) * n1 + u_mat(1, 2) * n2
               m2_orig = u_mat(2, 1) * n1 + u_mat(2, 2) * n2
               rep1_max = max(rep1_max, abs(m1_orig))
               rep2_max = max(rep2_max, abs(m2_orig))
            end do
         end do
      end do
   end do

   rep1 = rep1_max
   rep2 = rep2_max

end subroutine get_wsc_rep_2d


!> Compute Gram-Schmidt orthogonalization of a 3D lattice basis B = [b1, b2, b3]
pure subroutine gs_ortho_3d(bmat, bstar, bnorm2, mu21, mu31, mu32, valid)
   !> Reduced lattice basis vectors (3, 3)
   real(wp), intent(in) :: bmat(3, 3)
   !> Mutually orthogonal Gram-Schmidt vectors (3, 3)
   real(wp), intent(out) :: bstar(3, 3)
   !> Squared norms of Gram-Schmidt vectors
   real(wp), intent(out) :: bnorm2(3)
   !> Gram-Schmidt projection coefficients
   real(wp), intent(out) :: mu21, mu31, mu32
   !> True if all Gram-Schmidt vectors are non-degenerate and finite
   logical, intent(out) :: valid

   valid = .false.
   mu21 = 0.0_wp
   mu31 = 0.0_wp
   mu32 = 0.0_wp
   bnorm2(:) = 0.0_wp

   bstar(:, 1) = bmat(:, 1)
   bnorm2(1) = dot_product(bstar(:, 1), bstar(:, 1))
   if (.not. (bnorm2(1) > tol_deg**2 .and. bnorm2(1) <= huge(1.0_wp))) return

   mu21 = dot_product(bmat(:, 2), bstar(:, 1)) / bnorm2(1)
   bstar(:, 2) = bmat(:, 2) - mu21 * bstar(:, 1)
   bnorm2(2) = dot_product(bstar(:, 2), bstar(:, 2))
   if (.not. (bnorm2(2) > tol_deg**2 .and. bnorm2(2) <= huge(1.0_wp))) return

   mu31 = dot_product(bmat(:, 3), bstar(:, 1)) / bnorm2(1)
   mu32 = dot_product(bmat(:, 3), bstar(:, 2)) / bnorm2(2)
   bstar(:, 3) = bmat(:, 3) - mu32 * bstar(:, 2) - mu31 * bstar(:, 1)
   bnorm2(3) = dot_product(bstar(:, 3), bstar(:, 3))
   if (.not. (bnorm2(3) > tol_deg**2 .and. bnorm2(3) <= huge(1.0_wp))) return

   valid = .true.
end subroutine gs_ortho_3d


!> Size-reduce column i against column j in 3D LLL reduction and refresh Gram-Schmidt data
pure subroutine size_reduce_pair_3d(i, j, mu_ij, bmat, u_mat, bstar, bnorm2, &
      & mu21, mu31, mu32, error)
   !> Target column index (2 or 3)
   integer, intent(in) :: i
   !> Reference column index (< i)
   integer, intent(in) :: j
   !> Current Gram-Schmidt coefficient mu_{i,j}
   real(wp), intent(in) :: mu_ij
   !> Working lattice basis (3, 3)
   real(wp), intent(inout) :: bmat(3, 3)
   !> Unimodular transformation matrix (3, 3)
   integer(i8), intent(inout) :: u_mat(3, 3)
   !> Gram-Schmidt orthogonal vectors (3, 3)
   real(wp), intent(inout) :: bstar(3, 3)
   !> Squared Gram-Schmidt norms (3)
   real(wp), intent(inout) :: bnorm2(3)
   !> Gram-Schmidt projection coefficients
   real(wp), intent(inout) :: mu21, mu31, mu32
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   real(wp), parameter :: tol_mu = 0.5_wp * (1.0_wp + 1.0e-12_wp)
   integer(i8) :: q_red
   logical :: valid

   if (.not. (abs(mu_ij) <= max_rep_real)) then
      call fatal_error(error, &
         & "Integer overflow in Wigner-Seitz lattice repetition bound calculation")
      return
   end if
   if (abs(mu_ij) <= tol_mu) return

   q_red = nint(mu_ij, i8)
   if (maxval(abs(u_mat)) > max_u_step_i8 / max(1_i8, abs(q_red))) then
      call fatal_error(error, &
         & "Integer overflow in Wigner-Seitz lattice repetition bound calculation")
      return
   end if
   bmat(:, i) = bmat(:, i) - real(q_red, wp) * bmat(:, j)
   u_mat(:, i) = u_mat(:, i) - q_red * u_mat(:, j)
   call gs_ortho_3d(bmat, bstar, bnorm2, mu21, mu31, mu32, valid)
   if (.not. valid) then
      call fatal_error(error, &
         & "Invalid or degenerate active periodic lattice in Wigner-Seitz construction")
   end if
end subroutine size_reduce_pair_3d


!> Exact 3D Wigner-Seitz repetition bounds via LLL basis reduction and Cholesky sphere search
pure subroutine get_wsc_rep_3d(xyz, a1, a2, a3, rep, error)
   !> Cartesian atomic coordinates (3, nat)
   real(wp), intent(in) :: xyz(:, :)
   !> First 3D lattice vector
   real(wp), intent(in) :: a1(3)
   !> Second 3D lattice vector
   real(wp), intent(in) :: a2(3)
   !> Third 3D lattice vector
   real(wp), intent(in) :: a3(3)
   !> Repetition counts along (a1, a2, a3)
   integer(i8), intent(out) :: rep(3)
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   real(wp), parameter :: delta_lll = 0.99_wp
   integer :: nat, iat, jat, iter, k
   integer(i8) :: u_mat(3, 3), tmp_u(3)
   integer(i8) :: n1_0, n2_0, n3_0, n1, n2, n3, k_min
   integer(i8) :: n1_lo, n1_hi, n2_lo, n2_hi, n3_lo, n3_hi, m_orig(3), rep_max(3)
   real(wp) :: len1, len2, len3, vol
   real(wp) :: bmat(3, 3), bstar(3, 3), bnorm2(3), tmp_v(3)
   real(wp) :: mu21, mu31, mu32
   real(wp) :: rij(3), alpha1, alpha2, alpha3, c2_0, c1_0, c2, c1
   real(wp) :: d0_sq, dmin_sq, r_max2, r_comp2, d3_sq, d23_sq, d_sq
   real(wp) :: delta3, delta2, delta1
   logical :: valid, reduced

   rep(:) = 0_i8
   len1 = norm2(a1)
   len2 = norm2(a2)
   len3 = norm2(a3)
   if (len1 <= tol_deg .or. len2 <= tol_deg .or. len3 <= tol_deg) then
      call fatal_error(error, &
         & "Invalid or degenerate active periodic lattice in Wigner-Seitz construction")
      return
   end if
   vol = abs(dot_product(a1, crossprod(a2, a3)))
   if (vol <= tol_deg * len1 * len2 * len3) then
      call fatal_error(error, &
         & "Invalid or degenerate active periodic lattice in Wigner-Seitz construction")
      return
   end if

   ! Basis reduction: 3D LLL reduction B = A * U with unimodular U in GL(3, Z).
   ! Size reduction enforces |mu_{i,j}| <= 1/2 for j < i, and the Lovasz condition
   ! enforces |b_k^*|^2 >= (delta_lll - mu_{k,k-1}^2) |b_{k-1}^*|^2 so the Gram-Schmidt
   ! heights |b_k^*|^2 cannot collapse on skewed cells. Unlike Ewald summation (which
   ! sums directly in the reduced basis), WSC construction tracks U to map competing
   ! integer shifts back to the original lattice vectors.
   bmat(:, 1) = a1
   bmat(:, 2) = a2
   bmat(:, 3) = a3
   u_mat = reshape([ &
      & 1_i8, 0_i8, 0_i8, &
      & 0_i8, 1_i8, 0_i8, &
      & 0_i8, 0_i8, 1_i8], [3, 3])

   reduced = .false.
   do iter = 1, 100
      call gs_ortho_3d(bmat, bstar, bnorm2, mu21, mu31, mu32, valid)
      if (.not. valid) then
         call fatal_error(error, &
            & "Invalid or degenerate active periodic lattice in Wigner-Seitz construction")
         return
      end if

      call size_reduce_pair_3d(2, 1, mu21, bmat, u_mat, bstar, bnorm2, &
         & mu21, mu31, mu32, error)
      if (allocated(error)) return

      call size_reduce_pair_3d(3, 2, mu32, bmat, u_mat, bstar, bnorm2, &
         & mu21, mu31, mu32, error)
      if (allocated(error)) return

      call size_reduce_pair_3d(3, 1, mu31, bmat, u_mat, bstar, bnorm2, &
         & mu21, mu31, mu32, error)
      if (allocated(error)) return

      ! Lovasz condition between columns 1 and 2
      if (bnorm2(2) < (delta_lll - mu21 * mu21) * bnorm2(1)) then
         tmp_v = bmat(:, 1); bmat(:, 1) = bmat(:, 2); bmat(:, 2) = tmp_v
         tmp_u = u_mat(:, 1); u_mat(:, 1) = u_mat(:, 2); u_mat(:, 2) = tmp_u
         cycle
      end if

      ! Lovasz condition between columns 2 and 3
      if (bnorm2(3) < (delta_lll - mu32 * mu32) * bnorm2(2)) then
         tmp_v = bmat(:, 2); bmat(:, 2) = bmat(:, 3); bmat(:, 3) = tmp_v
         tmp_u = u_mat(:, 2); u_mat(:, 2) = u_mat(:, 3); u_mat(:, 3) = tmp_u
         cycle
      end if

      reduced = .true.
      exit
   end do
   if (.not. reduced) then
      call fatal_error(error, &
         & "Invalid or degenerate active periodic lattice in Wigner-Seitz construction")
      return
   end if

   if (any(sqrt(tol / bnorm2) > real(max_wsc_trans, wp))) then
      call fatal_error(error, &
         & "Wigner-Seitz candidate translation count exceeds maximum resource limit")
      return
   end if

   nat = size(xyz, 2)
   rep_max(:) = 1_i8

   do iat = 1, nat
      do jat = 1, iat

         ! Coordinate transformation: project pair vector rij onto the orthogonal
         ! Gram-Schmidt basis {b1^*, b2^*, b3^*}, giving upper-triangular Cholesky
         ! coordinates ||B*n - rij||^2 = sum_k |b_k^*|^2 * (n_k - c_k)^2
         rij = xyz(:, iat) - xyz(:, jat)
         alpha1 = dot_product(rij, bstar(:, 1)) / bnorm2(1)
         alpha2 = dot_product(rij, bstar(:, 2)) / bnorm2(2)
         alpha3 = dot_product(rij, bstar(:, 3)) / bnorm2(3)
         if (.not. (abs(alpha1) <= max_rep_real .and. abs(alpha2) <= max_rep_real &
            & .and. abs(alpha3) <= max_rep_real)) then
            call fatal_error(error, &
               & "Integer overflow in Wigner-Seitz lattice repetition bound calculation")
            return
         end if

         ! Initial search bound: Babai nearest-plane integer estimate (n1_0, n2_0, n3_0)
         ! by rounding from n3 down to n1; if this gives a self-image (< thr), step
         ! along b1 to obtain a guaranteed positive distance bound d0_sq
         n3_0 = nint(alpha3, i8)
         c2_0 = alpha2 - mu32 * real(n3_0, wp)
         n2_0 = nint(c2_0, i8)
         c1_0 = alpha1 - mu21 * real(n2_0, wp) - mu31 * real(n3_0, wp)
         n1_0 = nint(c1_0, i8)

         d0_sq = bnorm2(1) * (real(n1_0, wp) - c1_0)**2 &
            & + bnorm2(2) * (real(n2_0, wp) - c2_0)**2 &
            & + bnorm2(3) * (real(n3_0, wp) - alpha3)**2

         if (d0_sq < thr) then
            k_min = max(1_i8, ceiling(sqrt(thr / bnorm2(1)) - abs(real(n1_0, wp) - c1_0), i8))
            d0_sq = bnorm2(1) * (abs(real(n1_0, wp) - c1_0) + real(k_min, wp))**2 &
               & + bnorm2(2) * (real(n2_0, wp) - c2_0)**2 &
               & + bnorm2(3) * (real(n3_0, wp) - alpha3)**2
         end if

         ! Nearest-image search: enumerate all integer points (n1, n2, n3) inside the
         ! Cholesky ellipsoid ||B*n - rij||^2 <= d0_sq + tol to find the exact minimum dmin_sq
         r_max2 = (d0_sq + tol) * (1.0_wp + 1.0e-12_wp)
         if (any(2.0_wp * sqrt(r_max2 / bnorm2) + 1.0_wp > real(max_wsc_trans, wp))) then
            call fatal_error(error, &
               & "Wigner-Seitz candidate translation count exceeds maximum resource limit")
            return
         end if

         delta3 = sqrt(r_max2 / bnorm2(3))
         n3_lo = ceiling(alpha3 - delta3, i8)
         n3_hi = floor(alpha3 + delta3, i8)
         dmin_sq = d0_sq

         do n3 = n3_lo, n3_hi
            d3_sq = bnorm2(3) * (real(n3, wp) - alpha3)**2
            if (d3_sq > r_max2) cycle
            c2 = alpha2 - mu32 * real(n3, wp)
            delta2 = sqrt((r_max2 - d3_sq) / bnorm2(2))
            n2_lo = ceiling(c2 - delta2, i8)
            n2_hi = floor(c2 + delta2, i8)
            do n2 = n2_lo, n2_hi
               d23_sq = d3_sq + bnorm2(2) * (real(n2, wp) - c2)**2
               if (d23_sq > r_max2) cycle
               c1 = alpha1 - mu21 * real(n2, wp) - mu31 * real(n3, wp)
               delta1 = sqrt((r_max2 - d23_sq) / bnorm2(1))
               n1_lo = ceiling(c1 - delta1, i8)
               n1_hi = floor(c1 + delta1, i8)
               do n1 = n1_lo, n1_hi
                  d_sq = d23_sq + bnorm2(1) * (real(n1, wp) - c1)**2
                  if (d_sq < thr) cycle
                  dmin_sq = min(dmin_sq, d_sq)
               end do
            end do
         end do

         ! Competing images: enumerate all integer points within tol of dmin_sq
         ! that receive non-zero weight in the smooth Wigner-Seitz switching window
         r_comp2 = (dmin_sq + tol) * (1.0_wp + 1.0e-12_wp)
         delta3 = sqrt(r_comp2 / bnorm2(3))
         n3_lo = ceiling(alpha3 - delta3, i8)
         n3_hi = floor(alpha3 + delta3, i8)

         do n3 = n3_lo, n3_hi
            d3_sq = bnorm2(3) * (real(n3, wp) - alpha3)**2
            if (d3_sq > r_comp2) cycle
            c2 = alpha2 - mu32 * real(n3, wp)
            delta2 = sqrt((r_comp2 - d3_sq) / bnorm2(2))
            n2_lo = ceiling(c2 - delta2, i8)
            n2_hi = floor(c2 + delta2, i8)
            do n2 = n2_lo, n2_hi
               d23_sq = d3_sq + bnorm2(2) * (real(n2, wp) - c2)**2
               if (d23_sq > r_comp2) cycle
               c1 = alpha1 - mu21 * real(n2, wp) - mu31 * real(n3, wp)
               delta1 = sqrt((r_comp2 - d23_sq) / bnorm2(1))
               n1_lo = ceiling(c1 - delta1, i8)
               n1_hi = floor(c1 + delta1, i8)
               do n1 = n1_lo, n1_hi
                  d_sq = d23_sq + bnorm2(1) * (real(n1, wp) - c1)**2
                  if (d_sq < thr) cycle
                  if (d_sq > r_comp2) cycle

                  ! Return to the original basis: map reduced integer coordinates
                  ! (n1, n2, n3) back to unreduced lattice indices m = U * n
                  do k = 1, 3
                     m_orig(k) = u_mat(k, 1) * n1 + u_mat(k, 2) * n2 + u_mat(k, 3) * n3
                     rep_max(k) = max(rep_max(k), abs(m_orig(k)))
                  end do
               end do
            end do
         end do
      end do
   end do

   rep(:) = rep_max(:)

end subroutine get_wsc_rep_3d


!> Find the nearest translation images for an interatomic vector.
!>
!> Up to `size(list)` sorted image indices are returned (`iws = min(nimg_total, size(list))`).
!> Callers must provide `list` with sufficient capacity for the expected maximum number
!> of equidistant nearest images (e.g. `size(list) >= 27` for a `[-1, 0, 1]^3` shell).
pure subroutine get_pairs(iws, trans, rij, list)
   !> Number of nearest images stored in `list` (capped at `size(list)`)
   integer, intent(out) :: iws
   !> Translation vectors
   real(wp), intent(in) :: trans(:, :)
   !> Interatomic vector
   real(wp), intent(in) :: rij(3)
   !> Output buffer receiving sorted indices of the nearest translation images
   integer, intent(out) :: list(:)

   real(wp) :: min_r2

   call get_pairs_csr(trans, rij, iws, list, min_r2)

end subroutine get_pairs


!> Find nearest translation images and their minimum squared distance.
!>
!> Up to `size(list)` sorted image indices are written to `list` (`iws = min(nimg_total, size(list))`).
pure subroutine get_pairs_csr(trans, rij, iws, list, min_r2)
   !> Translation vectors
   real(wp), intent(in) :: trans(:, :)
   !> Interatomic vector
   real(wp), intent(in) :: rij(3)
   !> Number of images written to `list` (capped at `size(list)`)
   integer, intent(out) :: iws
   !> Output buffer receiving sorted indices of the nearest translation images
   integer, intent(out) :: list(:)
   !> Minimum squared distance found
   real(wp), intent(out) :: min_r2

   integer :: nimg_total

   call find_wsc_pairs(trans, rij, nimg_total, list, min_r2)
   iws = min(nimg_total, size(list))

end subroutine get_pairs_csr


!> Two-pass O(1)-stack nearest-image finder returning the exact count of
!> competing images within tol and filling up to size(list) sorted indices.
pure subroutine find_wsc_pairs(trans, rij, nimg_total, list, min_r2)
   !> Translation vectors
   real(wp), intent(in) :: trans(:, :)
   !> Interatomic vector
   real(wp), intent(in) :: rij(3)
   !> Total number of competing images within tol of min_r2
   integer, intent(out) :: nimg_total
   !> Sorted indices of the closest translation images (up to size(list))
   integer, intent(out) :: list(:)
   !> Minimum squared distance found
   real(wp), intent(out) :: min_r2

   real(wp) :: dx, dy, dz, r2, r2_prev
   integer :: itr, pos, prev, cap, nkept

   nimg_total = 0
   list(:) = 0
   min_r2 = huge(1.0_wp)

   ! First pass: find minimum non-self squared distance using O(1) stack scalars
   do itr = 1, size(trans, 2)
      dx = rij(1) - trans(1, itr)
      dy = rij(2) - trans(2, itr)
      dz = rij(3) - trans(3, itr)
      r2 = dx*dx + dy*dy + dz*dz
      if (r2 < thr) cycle
      if (r2 < min_r2) min_r2 = r2
   end do

   if (min_r2 >= huge(1.0_wp)) return

   ! Second pass: collect images within tol of min_r2 and insertion-sort into list
   cap = size(list)
   nkept = 0
   do itr = 1, size(trans, 2)
      dx = rij(1) - trans(1, itr)
      dy = rij(2) - trans(2, itr)
      dz = rij(3) - trans(3, itr)
      r2 = dx*dx + dy*dy + dz*dz
      if (r2 < thr) cycle
      if (r2 - min_r2 > tol) cycle
      nimg_total = nimg_total + 1
      if (cap <= 0) cycle

      pos = nkept + 1
      do while (pos > 1)
         prev = list(pos - 1)
         dx = rij(1) - trans(1, prev)
         dy = rij(2) - trans(2, prev)
         dz = rij(3) - trans(3, prev)
         r2_prev = dx*dx + dy*dy + dz*dz
         if (r2_prev <= r2) exit
         if (pos <= cap) list(pos) = prev
         pos = pos - 1
      end do
      if (pos <= cap) then
         list(pos) = itr
         if (nkept < cap) nkept = nkept + 1
      end if
   end do

end subroutine find_wsc_pairs


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

   ! Evaluate unnormalized switching weights relative to the closest image refidx
   do img = 1, nimg
      idx = self%tridx(img, jat, iat)
      vec = rij - self%trans(:, idx)
      delta = max(0.0_wp, dot_product(vec, vec) - dot_product(refvec, refvec))
      weight(img) = smooth_image_weight(delta)
      sum_shape = sum_shape + weight(img)
   end do

   weight(:nimg) = weight(:nimg) / sum_shape
   if (.not.present(dwdr) .and. .not.present(dwdL)) return

   ! Accumulate denominator derivatives for quotient-rule differentiation
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

   ! Evaluate normalized weight derivatives dw_k / dr and dw_k / dL
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
