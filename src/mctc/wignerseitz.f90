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
!> Periodic cyclic cluster Wigner-Seitz model for DFT-D4 and related models following:
!> - E. Caldeweyher, J.-M. Mewes, S. Ehlert and S. Grimme, *Phys. Chem. Chem. Phys.*,
!>   **2020**, 22, 8499-8512. DOI: [10.1039/D0CP00502A](https://doi.org/10.1039/D0CP00502A)
module mctc_wignerseitz
   use, intrinsic :: iso_fortran_env, only : error_unit
   use mctc_cutoff, only : get_inverse_metric, get_lattice_points, reduce_basis
   use mctc_env, only : wp, i8, error_type, fatal_error
   use mctc_io, only : structure_type
   use mctc_io_math, only : outerprod
   implicit none
   private

   public :: new_wignerseitz_cell, get_wignerseitz_weights, wignerseitz_cell, &
      & get_wignerseitz_translations, find_wsc_pairs

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
      !> Translation indices for each atom pair (nimg_max, nat, nat)
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


   !> Squared distance below which an image is the atom itself
   real(wp), parameter :: thr = sqrt(epsilon(0.0_wp))

   !> Squared-distance interval for smoothly averaging competing nearest images
   real(wp), parameter :: tol = 0.3_wp

   !> Maximum number of candidate translations, limits the memory and the cost
   !> of the image search for every atom pair
   integer, parameter :: max_wsc_trans = 100000


contains


!> Construct a Wigner-Seitz cell for a molecular structure
subroutine new_wignerseitz_cell(self, mol, error)

   !> Wigner-Seitz cell instance
   type(wignerseitz_cell), intent(out) :: self

   !> Molecular structure data
   type(structure_type), intent(in) :: mol

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   integer :: iat, jat, nimg, empty(0)
   real(wp) :: vec(3), min_r2
   real(wp), allocatable :: trans(:, :)

   call get_wignerseitz_translations(mol, trans, error)
   if (allocated(error)) return

   ! Count the images of every pair first, such that the image indices are
   ! only stored up to the largest number of images found
   allocate(self%nimg(mol%nat, mol%nat))
   !$omp parallel do default(none) schedule(runtime) collapse(2) &
   !$omp& shared(mol, trans, self) private(iat, jat, vec, nimg, empty, min_r2)
   do iat = 1, mol%nat
      do jat = 1, mol%nat
         vec(:) = mol%xyz(:, iat) - mol%xyz(:, jat)
         call find_wsc_pairs(trans, vec, nimg, empty, min_r2)
         self%nimg(jat, iat) = nimg
      end do
   end do

   if (mol%nat > 0) self%nimg_max = maxval(self%nimg)

   allocate(self%tridx(self%nimg_max, mol%nat, mol%nat), source=0)
   !$omp parallel do default(none) schedule(runtime) collapse(2) &
   !$omp& shared(mol, trans, self) private(iat, jat, vec, nimg, min_r2)
   do iat = 1, mol%nat
      do jat = 1, mol%nat
         vec(:) = mol%xyz(:, iat) - mol%xyz(:, jat)
         call find_wsc_pairs(trans, vec, nimg, self%tridx(:, jat, iat), min_r2)
      end do
   end do

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


!> Generate lattice translations containing all competing nearest images.
!>
!> With the periodic lattice vectors A, the metric G = A^T A, and the fractional
!> coordinates s = G^-1 A^T r of a pair vector r, the image translated by A n
!> has the squared distance |r_perp|^2 + (s - n)^T G (s - n). Any admissible
!> candidate image with parallel squared distance d0^2 bounds the nearest image,
!> hence all images within the switching interval satisfy
!> (s - n)^T G (s - n) <= d0^2 + tol. By the Cauchy-Schwarz inequality, this
!> ellipsoid is confined to |n_k - s_k| <= sqrt((d0^2 + tol) G^-1_kk).
!>
!> The bound holds for any basis of the lattice. A reduced basis keeps it small
!> for skewed cells, the translations are generated in this basis.
subroutine get_wignerseitz_translations(mol, trans, error)

   !> Molecular structure data
   type(structure_type), intent(in) :: mol

   !> Lattice translation vectors
   real(wp), allocatable, intent(out) :: trans(:, :)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   integer :: iat, jat, k, kmin
   real(wp) :: basis(3, 3), lat(3, 3), ginv(3, 3), length(3), bound(3)
   real(wp) :: sij(3), diff(3), perp2, par2, step
   real(wp), allocatable :: frac(:, :), perp(:, :)

   if (.not. any(mol%periodic)) then
      allocate(trans(3, 1), source=0.0_wp)
      return
   end if

   call get_inverse_metric(mol%periodic, mol%lattice, basis, ginv, error)
   if (allocated(error)) return

   call reduce_basis(basis)
   call get_inverse_metric(mol%periodic, basis, lat, ginv, error)
   if (allocated(error)) return

   ! Shortest periodic lattice vector, used to step away from self images
   length(:) = norm2(lat, dim=1)
   kmin = minloc(length, dim=1, mask=length > 0.0_wp)

   ! Fractional coordinates in the periodic subspace and perpendicular part
   ! of all atoms, the pair quantities are their differences
   allocate(frac(3, mol%nat), perp(3, mol%nat))
   do iat = 1, mol%nat
      frac(:, iat) = matmul(ginv, matmul(transpose(lat), mol%xyz(:, iat)))
      perp(:, iat) = mol%xyz(:, iat) - matmul(lat, frac(:, iat))
   end do

   bound(:) = 0.0_wp
   !$omp parallel do default(none) schedule(runtime) reduction(max:bound) &
   !$omp& shared(mol, frac, perp, lat, ginv, length, kmin) &
   !$omp& private(iat, jat, k, sij, diff, perp2, par2, step)
   do iat = 1, mol%nat
      do jat = 1, iat
         sij(:) = frac(:, iat) - frac(:, jat)
         perp2 = sum((perp(:, iat) - perp(:, jat))**2)

         ! Rounding gives a nearby lattice point as candidate image
         diff(:) = sij - anint(sij)
         par2 = sum(matmul(lat, diff)**2)

         ! The image search skips the atom itself, in this case step along the
         ! shortest lattice vector until the candidate is clearly not a self image
         if (perp2 + par2 < 4.0_wp * thr) then
            step = aint((2.0_wp * sqrt(thr) + sqrt(par2)) / length(kmin)) + 1.0_wp
            diff(kmin) = diff(kmin) + step
            par2 = sum(matmul(lat, diff)**2)
         end if

         do k = 1, 3
            bound(k) = max(bound(k), abs(sij(k)) + sqrt((par2 + tol) * ginv(k, k)))
         end do
      end do
   end do

   ! Widen the bound for round-off, which otherwise can lose images on the edge
   ! of the switching interval for distant unwrapped atoms
   bound(:) = bound + sqrt(epsilon(1.0_wp)) * (1.0_wp + bound)

   if (.not. (product(2.0_wp * aint(bound) + 1.0_wp) <= real(max_wsc_trans, wp))) then
      call fatal_error(error, &
         & "Too many candidate translations for the Wigner-Seitz cell")
      return
   end if

   call get_lattice_points(lat, int(bound), .true., trans, error)

end subroutine get_wignerseitz_translations


!> Find the competing nearest images of an interatomic vector.
!>
!> Counts the images within the switching interval of the nearest image,
!> excluding the atom itself, in `nimg` and returns the minimum squared
!> distance in `min_r2`. If `list` is empty, the images are only counted.
!> Otherwise, `list` must hold all images (size(list) >= nimg) and receives
!> their translation indices sorted by distance. A list of size(trans, 2)
!> is always sufficient.
pure subroutine find_wsc_pairs(trans, rij, nimg, list, min_r2)

   !> Translation vectors
   real(wp), intent(in) :: trans(:, :)

   !> Interatomic vector
   real(wp), intent(in) :: rij(3)

   !> Number of images within the switching interval
   integer, intent(out) :: nimg

   !> Indices of the nearest translation images
   integer, intent(inout) :: list(:)

   !> Minimum squared distance found
   real(wp), intent(out) :: min_r2

   integer :: itr, pos, nkept
   real(wp) :: r2

   nimg = 0
   min_r2 = huge(1.0_wp)

   ! First pass, distance to the nearest image other than the atom itself
   do itr = 1, size(trans, 2)
      r2 = sum((rij - trans(:, itr))**2)
      if (r2 < thr) cycle
      if (r2 < min_r2) min_r2 = r2
   end do

   ! Second pass, collect all images within the interval, sorted by insertion
   nkept = 0
   do itr = 1, size(trans, 2)
      r2 = sum((rij - trans(:, itr))**2)
      if (r2 < thr .or. r2 - min_r2 > tol) cycle
      nimg = nimg + 1

      pos = nkept + 1
      do while (pos > 1)
         if (sum((rij - trans(:, list(pos - 1)))**2) <= r2) exit
         if (pos <= size(list)) list(pos) = list(pos - 1)
         pos = pos - 1
      end do
      if (pos <= size(list)) then
         list(pos) = itr
         nkept = min(nkept + 1, size(list))
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
