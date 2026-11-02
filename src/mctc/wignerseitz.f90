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
   use mctc_env, only : wp, i8
   use mctc_io, only : structure_type
   use mctc_io_math, only : crossprod, outerprod
   implicit none
   private

   public :: new_wignerseitz_cell, get_wignerseitz_weights, wignerseitz_cell, &
      & get_pairs

   !> Wigner-Seitz cell and its nearest image translations
   type :: wignerseitz_cell
      !> Maximum number of images per atom pair
      integer :: nimg_max = 0
      !> Number of images for each atom pair
      integer, allocatable :: nimg(:, :)
      !> Translation indices for each atom pair, first dimension sized nimg_max
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


contains


!> Construct a Wigner-Seitz cell for a molecular structure
subroutine new_wignerseitz_cell(self, mol)

   !> Wigner-Seitz cell instance
   type(wignerseitz_cell), intent(out) :: self

   !> Molecular structure data
   type(structure_type), intent(in) :: mol

   integer :: iat, jat, ntr, nimg, rep(3)
   integer, allocatable :: tridx(:)
   real(wp) :: vec(3)
   real(wp), allocatable :: trans(:, :)

   call get_wsc_rep(mol, rep)
   if (all(rep == 0)) then
      allocate(trans(3, 1))
      trans(:, :) = 0.0_wp
   else
      call get_lattice_points(mol%lattice, rep, .true., trans)
   end if
   ntr = size(trans, 2)
   allocate(self%nimg(mol%nat, mol%nat), tridx(ntr))

   !$omp parallel do default(none) schedule(runtime) collapse(2) &
   !$omp& shared(mol, trans, self) private(iat, jat, vec, nimg, tridx)
   do iat = 1, mol%nat
      do jat = 1, mol%nat
         vec(:) = mol%xyz(:, iat) - mol%xyz(:, jat)
         call get_pairs(nimg, trans, vec, tridx)
         self%nimg(jat, iat) = nimg
      end do
   end do

   if (mol%nat > 0) self%nimg_max = maxval(self%nimg)

   ! Allocate only the actual nearest images. Counting first avoids the
   ! much larger ntranslations * nat^2 temporary, especially in supercells.
   allocate(self%tridx(self%nimg_max, mol%nat, mol%nat), source=0)
   !$omp parallel do default(none) schedule(runtime) collapse(2) &
   !$omp& shared(mol, trans, self) private(iat, jat, vec, nimg, tridx)
   do iat = 1, mol%nat
      do jat = 1, mol%nat
         vec(:) = mol%xyz(:, iat) - mol%xyz(:, jat)
         call get_pairs(nimg, trans, vec, tridx)
         self%tridx(:nimg, jat, iat) = tridx(:nimg)
      end do
   end do

   call move_alloc(trans, self%trans)

end subroutine new_wignerseitz_cell


!> Determine lattice repetition bounds needed to cover all Wigner-Seitz nearest
!> images, accounting for unwrapped coordinates and skewed periodic subspaces.
pure subroutine get_wsc_rep(mol, rep)

   !> Molecular structure data
   type(structure_type), intent(in) :: mol

   !> Repetition bounds along each lattice vector
   integer, intent(out) :: rep(3)

   integer :: i, iat, jat, iter, ndim, p_axes(3), mu
   integer :: u_mat(2, 2), tmp_u(2), n_shift(2), m_red(2)
   real(wp) :: a1(3), a2(3), a3(3), b1(3), b2(3), tmp_v(3)
   real(wp) :: len1, len2, len3, area, vol
   real(wp) :: g11, g12, g22, det2, proj1, proj2, h1, h2, rcov
   real(wp) :: s_span, s_span1, s_span2
   real(wp), allocatable :: s1d(:), s2d(:, :)

   rep(:) = 0
   if (mol%nat <= 0) return
   if (.not. allocated(mol%periodic) .or. .not. allocated(mol%lattice)) return
   if (.not. any(mol%periodic)) return
   if (size(mol%lattice, 1) < 3 .or. size(mol%lattice, 2) < 3) return

   ndim = 0
   p_axes(:) = 0
   do i = 1, min(3, size(mol%periodic))
      if (mol%periodic(i)) then
         ndim = ndim + 1
         p_axes(ndim) = i
      end if
   end do

   select case (ndim)
   case (1)
      a1 = mol%lattice(:, p_axes(1))
      len1 = norm2(a1)
      if (len1 <= tol_deg) return
      allocate(s1d(mol%nat))
      do iat = 1, mol%nat
         s1d(iat) = dot_product(mol%xyz(:, iat), a1) / (len1 * len1)
      end do
      s_span = maxval(s1d) - minval(s1d)
      if (s_span <= 1.4_wp) then
         rep(p_axes(1)) = 1
      else
         rep(p_axes(1)) = max(1, ceiling(s_span - 0.5_wp + sqrt(tol) / len1))
      end if

   case (2)
      a1 = mol%lattice(:, p_axes(1))
      a2 = mol%lattice(:, p_axes(2))
      len1 = norm2(a1)
      len2 = norm2(a2)
      if (len1 <= tol_deg .or. len2 <= tol_deg) return
      area = norm2(crossprod(a1, a2))
      if (area <= tol_deg * len1 * len2) return

      ! 2D Gauss/Lagrange basis reduction: [b1, b2] = [a1, a2] * U
      b1 = a1
      b2 = a2
      u_mat = reshape([1, 0, 0, 1], [2, 2])
      do iter = 1, 100
         if (dot_product(b1, b1) > dot_product(b2, b2)) then
            tmp_v = b1; b1 = b2; b2 = tmp_v
            tmp_u = u_mat(:, 1); u_mat(:, 1) = u_mat(:, 2); u_mat(:, 2) = tmp_u
         end if
         g11 = dot_product(b1, b1)
         if (g11 <= tol_deg**2) exit
         mu = nint(dot_product(b1, b2) / g11)
         if (mu == 0) exit
         b2 = b2 - real(mu, wp) * b1
         u_mat(:, 2) = u_mat(:, 2) - mu * u_mat(:, 1)
      end do

      g11 = dot_product(b1, b1)
      g12 = dot_product(b1, b2)
      g22 = dot_product(b2, b2)
      det2 = g11 * g22 - g12 * g12
      if (det2 <= (tol_deg**2) * g11 * g22) return

      allocate(s2d(2, mol%nat))
      do iat = 1, mol%nat
         proj1 = dot_product(mol%xyz(:, iat), b1)
         proj2 = dot_product(mol%xyz(:, iat), b2)
         s2d(1, iat) = (g22 * proj1 - g12 * proj2) / det2
         s2d(2, iat) = (-g12 * proj1 + g11 * proj2) / det2
      end do
      s_span1 = maxval(s2d(1, :)) - minval(s2d(1, :))
      s_span2 = maxval(s2d(2, :)) - minval(s2d(2, :))

      if (abs(u_mat(1, 1)) + abs(u_mat(1, 2)) == 1 &
         & .and. abs(u_mat(2, 1)) + abs(u_mat(2, 2)) == 1 &
         & .and. s_span1 <= 1.4_wp .and. s_span2 <= 1.4_wp) then
         rep(p_axes(1)) = 1
         rep(p_axes(2)) = 1
      else
         h1 = sqrt(det2 / g22)
         h2 = sqrt(det2 / g11)
         rcov = 0.5_wp * (sqrt(g11) + sqrt(g22)) + sqrt(tol)
         m_red(1) = max(1, ceiling(rcov / h1))
         m_red(2) = max(1, ceiling(rcov / h2))
         rep(p_axes(1)) = 1
         rep(p_axes(2)) = 1
         do iat = 1, mol%nat
            do jat = 1, mol%nat
               n_shift(1) = nint(s2d(1, jat) - s2d(1, iat))
               n_shift(2) = nint(s2d(2, jat) - s2d(2, iat))
               do i = 1, 2
                  rep(p_axes(i)) = max(rep(p_axes(i)), &
                     & abs(u_mat(i, 1) * n_shift(1) + u_mat(i, 2) * n_shift(2)) &
                     & + abs(u_mat(i, 1)) * m_red(1) + abs(u_mat(i, 2)) * m_red(2))
               end do
            end do
         end do
      end if

   case (3)
      a1 = mol%lattice(:, 1)
      a2 = mol%lattice(:, 2)
      a3 = mol%lattice(:, 3)
      len1 = norm2(a1)
      len2 = norm2(a2)
      len3 = norm2(a3)
      if (len1 <= tol_deg .or. len2 <= tol_deg .or. len3 <= tol_deg) return
      vol = abs(dot_product(a1, crossprod(a2, a3)))
      if (vol <= tol_deg * len1 * len2 * len3) return
      rep(:) = 1
   case default
      continue
   end select

end subroutine get_wsc_rep

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
