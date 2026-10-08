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

!> Declaration of base class for coordination number evaluations
module mctc_ncoord_type
   use mctc_csrlist, only : csr_list
   use mctc_cutoff, only : get_lattice_points
   use mctc_env, only : wp, i8
   use mctc_io, only : structure_type

   implicit none
   private

   !> Abstract base class for coordination number evaluator
   type, public, abstract :: ncoord_type
      !> Radial cutoff for the coordination number
      real(wp) :: cutoff
      !> Steepness of counting function
      real(wp) :: kcn
      !> Factor determining whether the CN is evaluated with direction
      !> if +1 the CN contribution is added equally to both partners
      !> if -1 (i.e. with the EN-dep.) it is added to one and subtracted from the other
      real(wp) :: directed_factor
      !> Cutoff for the maximum coordination number (negative value, no cutoff)
      real(wp)  :: cut = -1.0_wp
   contains
      !> Obtains lattice information and calls get_coordination number
      procedure :: get_cn
      !> Decides whether the energy or gradient is calculated
      procedure :: get_coordination_number
      !> Evaluates the CN from the specific counting function
      procedure :: ncoord
      !> Evaluates derivative of the CN from the specific counting function
      procedure :: ncoord_d
      !>
      !> Evaluates pairwise electronegativity factor
      procedure :: get_en_factor
      !> Add CN derivative of an arbitrary function
      procedure :: add_coordination_number_derivs
      !> Add CN derivative of an arbitrary function using the CSR list
      procedure :: add_coordination_number_derivs_list
      !> Add dE/dCN contracted with the Cartesian CN Hessian
      procedure :: add_coordination_number_hessian
      !> Add dE/dCN contracted with the Cartesian CN Hessian using the CSR list
      procedure :: add_coordination_number_hessian_list
      !> Evaluates the counting function (exp, dexp, erf, ...)
      procedure(ncoord_count),  deferred :: ncoord_count
      !> Evaluates the derivative of the counting function (exp, dexp, erf, ...)
      procedure(ncoord_dcount), deferred :: ncoord_dcount
      !> Evaluates the second derivative of the counting function
      !> Analytic implementations can override the numerical fallback.
      procedure :: ncoord_d2count
   end type ncoord_type

   abstract interface

      !> Abstract counting function
      elemental function ncoord_count(self, izp, jzp, r) result(count)
         import :: ncoord_type, wp
         !> Instance of coordination number container
         class(ncoord_type), intent(in) :: self
         !> Atom i index
         integer, intent(in)  :: izp
         !> Atom j index
         integer, intent(in)  :: jzp
         !> Current distance.
         real(wp), intent(in) :: r

         real(wp) :: count
      end function ncoord_count

      !> Abstract derivative of the counting function w.r.t. the distance.
      elemental function ncoord_dcount(self, izp, jzp, r) result(count)
         import :: ncoord_type, wp
         !> Instance of coordination number container
         class(ncoord_type), intent(in) :: self
         !> Atom i index
         integer, intent(in)  :: izp
         !> Atom j index
         integer, intent(in)  :: jzp
         !> Current distance.
         real(wp), intent(in) :: r

         real(wp) :: count
      end function ncoord_dcount

   end interface

contains

   !> Numerical fallback for the second derivative of the counting function
   !> w.r.t. the distance. Built-in counting functions override this routine
   !> with analytic expressions.
   elemental function ncoord_d2count(self, izp, jzp, r) result(count)
      !> Instance of coordination number container
      class(ncoord_type), intent(in) :: self
      !> Atom i index
      integer, intent(in) :: izp
      !> Atom j index
      integer, intent(in) :: jzp
      !> Current distance
      real(wp), intent(in) :: r

      real(wp) :: count, step
      real(wp), parameter :: eps = epsilon(1.0_wp)**(1.0_wp/3.0_wp)

      step = min(0.5_wp*r, eps*max(abs(r), 1.0_wp))
      count = (self%ncoord_dcount(izp, jzp, r + step) &
         & - self%ncoord_dcount(izp, jzp, r - step))/(2.0_wp*step)

   end function ncoord_d2count


   !> Wrapper for CN using the CN cutoff for the lattice
   subroutine get_cn(self, mol, cn, dcndr, dcndL, list, dcndrlist)
      !> Coordination number container
      class(ncoord_type), intent(in) :: self
      !> Molecular structure data
      type(structure_type), intent(in) :: mol
      !> CSR list for neighbourlist-based CN evaluation
      type(csr_list), intent(in), optional :: list
      !> Error function coordination number.
      real(wp), intent(out) :: cn(:)
      !> Derivative of the CN with respect to the Cartesian coordinates.
      real(wp), intent(out), optional :: dcndr(:, :, :)
      !> Derivative of the CN with respect to strain deformations.
      real(wp), intent(out), optional :: dcndL(:, :, :)
      !> Derivative of the CN with respect to the Cartesian coordinates
      !> in the sparsity pattern of the CSR list
      real(wp), intent(out), optional :: dcndrlist(:, :)

      real(wp), allocatable :: lattr(:, :)

      call get_lattice_points(mol%periodic, mol%lattice, self%cutoff, lattr)
      call get_coordination_number(self, mol, lattr, cn, dcndr, dcndL, list, &
         & dcndrlist)
   end subroutine get_cn

   !> Geometric fractional coordination number
   subroutine get_coordination_number(self, mol, trans, cn, dcndr, dcndL, list, &
      & dcndrlist)

      !> Coordination number container
      class(ncoord_type), intent(in) :: self

      !> Molecular structure data
      type(structure_type), intent(in) :: mol

      !> Lattice points
      real(wp), intent(in) :: trans(:, :)

      !> Coordination number
      real(wp), intent(out) :: cn(:)

      !> Derivative of the CN with respect to Cartesian coordinates
      real(wp), intent(out), optional :: dcndr(:, :, :)

      !> Derivative of the CN with respect to strain deformations
      real(wp), intent(out), optional :: dcndL(:, :, :)

      !> CSR list
      type(csr_list), intent(in), optional :: list

      !> Derivative of the CN with respect to the Cartesian coordinates
      real(wp), intent(out), optional :: dcndrlist(:, :)

      if (present(list)) then
         if (present(dcndrlist) .and. present(dcndL)) then
            call ncoord_d_list(self, mol, trans, cn, dcndrlist, dcndL, list)
         else
            call ncoord_list(self, mol, trans, cn, list)
         end if
      else
         if (present(dcndr) .and. present(dcndL)) then
            call ncoord_d(self, mol, trans, cn, dcndr, dcndL)
         else
            call ncoord(self, mol, trans, cn)
         end if
      end if

      if (self%cut > 0.0_wp) then
         call cut_coordination_number(self%cut, cn, dcndr, dcndL, dcndrlist, list)
      end if

   end subroutine get_coordination_number

   !> Evaluates coordination numbers
   subroutine ncoord(self, mol, trans, cn)
      !> Coordination number container
      class(ncoord_type), intent(in) :: self
      !> Molecular structure data
      type(structure_type), intent(in) :: mol
      !> Lattice points
      real(wp), intent(in) :: trans(:, :)
      !> Error function coordination number.
      real(wp), intent(out) :: cn(:)

      integer :: iat, jat, izp, jzp, itr
      real(wp) :: r2, r1, rij(3), countf, cutoff2, den

      ! Thread-private array for reduction
      real(wp), allocatable :: cn_local(:)

      cn(:) = 0.0_wp
      cutoff2 = self%cutoff**2

      !$omp parallel default(none) &
      !$omp shared(self, mol, trans, cutoff2, cn) &
      !$omp private(jat, itr, izp, jzp, r2, rij, r1, den, countf) &
      !$omp private(cn_local)
      allocate(cn_local, source=cn)
      !$omp do schedule(runtime)
      do iat = 1, mol%nat
         izp = mol%id(iat)
         do jat = 1, iat
            jzp = mol%id(jat)
            den = self%get_en_factor(izp, jzp)

            do itr = 1, size(trans, dim=2)
               rij = mol%xyz(:, iat) - (mol%xyz(:, jat) + trans(:, itr))
               r2 = sum(rij**2)
               if (r2 > cutoff2 .or. r2 < 1.0e-12_wp) cycle
               r1 = sqrt(r2)

               countf = den * self%ncoord_count(izp, jzp, r1)

               cn_local(iat) = cn_local(iat) + countf
               if (iat /= jat) then
                  cn_local(jat) = cn_local(jat) + countf * self%directed_factor
               end if

            end do
         end do
      end do
      !$omp end do
      !$omp critical (ncoord_)
      cn(:) = cn(:) + cn_local(:)
      !$omp end critical (ncoord_)
      deallocate(cn_local)
      !$omp end parallel

   end subroutine ncoord

   !> Evaluates coordination numbers using a CSR neighbour list
   subroutine ncoord_list(self, mol, trans, cn, list)
      !> Coordination number container
      class(ncoord_type), intent(in) :: self
      !> Molecular structure data
      type(structure_type), intent(in) :: mol
      !> Lattice points
      real(wp), intent(in) :: trans(:, :)
      !> Error function coordination number.
      real(wp), intent(out) :: cn(:)
      !> CSR list for neighbourlist-based CN evaluation
      type(csr_list), intent(in) :: list

      integer :: iat, jat, izp, jzp, itr, itrst, itrfin
      integer(i8) :: kat
      real(wp) :: r2, r1, rij(3), countf, cutoff2, den
      logical :: trlist, half
      real(wp), allocatable :: lattr(:, :)

      ! Thread-private array for reduction
      real(wp), allocatable :: cn_local(:)

      cn(:) = 0.0_wp
      cutoff2 = self%cutoff**2
      trlist = allocated(list%nltr)
      half = .not. list%complete

      if (trlist) then
         lattr = list%trans
      else
         lattr = trans
      end if

      !$omp parallel default(none) &
      !$omp shared(self, mol, list, lattr, cutoff2, cn, trlist, half) &
      !$omp private(jat, kat, itr, itrst, itrfin, izp, jzp, r2, rij, r1, den) &
      !$omp private(countf, cn_local)
      allocate(cn_local, source=cn)
      !$omp do schedule(runtime)
      do iat = 1, mol%nat
         izp = mol%id(iat)
         do kat = list%inl(iat), list%inl(iat + 1) - 1
            jat = list%nlat(kat)
            jzp = mol%id(jat)
            den = self%get_en_factor(izp, jzp)

            itrst = 1
            itrfin = size(lattr, dim=2)
            if (trlist) then
               itrst = list%nltr(kat)
               itrfin = itrst
            end if

            do itr = itrst, itrfin
               rij = mol%xyz(:, iat) - (mol%xyz(:, jat) + lattr(:, itr))
               r2 = sum(rij**2)
               if (r2 > cutoff2 .or. r2 < 1.0e-12_wp) cycle
               r1 = sqrt(r2)

               countf = den * self%ncoord_count(izp, jzp, r1)

               cn_local(iat) = cn_local(iat) + countf
               ! Upper triangular list, add the mirrored contribution to atom j
               if (half .and. iat /= jat) then
                  cn_local(jat) = cn_local(jat) + countf * self%directed_factor
               end if

            end do
         end do
      end do
      !$omp end do
      !$omp critical (ncoord_list_)
      cn(:) = cn(:) + cn_local(:)
      !$omp end critical (ncoord_list_)
      deallocate(cn_local)
      !$omp end parallel

   end subroutine ncoord_list

   !> Evaluates coordination numbers and their derivatives
   subroutine ncoord_d(self, mol, trans, cn, dcndr, dcndL)
      !> Coordination number container
      class(ncoord_type), intent(in) :: self
      !> Molecular structure data
      type(structure_type), intent(in) :: mol
      !> Lattice points
      real(wp), intent(in) :: trans(:, :)
      !> Error function coordination number.
      real(wp), intent(out) :: cn(:)
      !> Derivative of the CN with respect to the Cartesian coordinates.
      real(wp), intent(out) :: dcndr(:, :, :)
      !> Derivative of the CN with respect to strain deformations.
      real(wp), intent(out) :: dcndL(:, :, :)

      integer :: iat, jat, izp, jzp, itr
      real(wp) :: r2, r1, rij(3), countf, countd(3), sigma(3, 3), cutoff2, den

      ! Thread-private arrays for reduction
      real(wp), allocatable :: cn_local(:)
      real(wp), allocatable :: dcndr_local(:, :, :), dcndL_local(:, :, :)

      cn(:) = 0.0_wp
      dcndr(:, :, :) = 0.0_wp
      dcndL(:, :, :) = 0.0_wp
      cutoff2 = self%cutoff**2

      !$omp parallel default(none) &
      !$omp shared(self, mol, trans, cutoff2, cn, dcndr, dcndL) &
      !$omp private(jat, itr, izp, jzp, r2, rij, r1, den, countf, countd) &
      !$omp private(sigma, cn_local, dcndr_local, dcndL_local)
      allocate(cn_local, source=cn)
      allocate(dcndr_local, source=dcndr)
      allocate(dcndL_local, source=dcndL)
      !$omp do schedule(runtime)
      do iat = 1, mol%nat
         izp = mol%id(iat)
         do jat = 1, iat
            jzp = mol%id(jat)
            den = self%get_en_factor(izp, jzp)

            do itr = 1, size(trans, dim=2)
               rij = mol%xyz(:, iat) - (mol%xyz(:, jat) + trans(:, itr))
               r2 = sum(rij**2)
               if (r2 > cutoff2 .or. r2 < 1.0e-12_wp) cycle
               r1 = sqrt(r2)

               countf = den * self%ncoord_count(izp, jzp, r1)
               countd = den * self%ncoord_dcount(izp, jzp, r1) * rij/r1

               cn_local(iat) = cn_local(iat) + countf
               if (iat /= jat) then
                  cn_local(jat) = cn_local(jat) + countf * self%directed_factor
               end if

               dcndr_local(:, iat, iat) = dcndr_local(:, iat, iat) + countd
               dcndr_local(:, jat, jat) = dcndr_local(:, jat, jat) &
                  & - countd * self%directed_factor
               dcndr_local(:, iat, jat) = dcndr_local(:, iat, jat) &
                  & + countd * self%directed_factor
               dcndr_local(:, jat, iat) = dcndr_local(:, jat, iat) - countd

               sigma = spread(countd, 1, 3) * spread(rij, 2, 3)

               dcndL_local(:, :, iat) = dcndL_local(:, :, iat) + sigma
               if (iat /= jat) then
                  dcndL_local(:, :, jat) = dcndL_local(:, :, jat) &
                     & + sigma * self%directed_factor
               end if

            end do
         end do
      end do
      !$omp end do
      !$omp critical (ncoord_d_)
      cn(:) = cn(:) + cn_local(:)
      dcndr(:, :, :) = dcndr(:, :, :) + dcndr_local(:, :, :)
      dcndL(:, :, :) = dcndL(:, :, :) + dcndL_local(:, :, :)
      !$omp end critical (ncoord_d_)
      deallocate(cn_local, dcndr_local, dcndL_local)
      !$omp end parallel

   end subroutine ncoord_d

   !> Evaluates coordination numbers and derivatives using a CSR neighbour list
   subroutine ncoord_d_list(self, mol, trans, cn, dcndrlist, dcndL, list)
      !> Coordination number container
      class(ncoord_type), intent(in) :: self
      !> Molecular structure data
      type(structure_type), intent(in) :: mol
      !> Lattice points
      real(wp), intent(in) :: trans(:, :)
      !> Error function coordination number.
      real(wp), intent(out) :: cn(:)
      !> Derivative of the CN in CSR format
      real(wp), intent(out) :: dcndrlist(:, :)
      !> Derivative of the CN with respect to strain deformations.
      real(wp), intent(out) :: dcndL(:, :, :)
      !> CSR list for neighbourlist-based CN evaluation
      type(csr_list), intent(in) :: list

      integer :: iat, jat, izp, jzp, itr, itrst, itrfin
      integer(i8) :: kat
      real(wp) :: r2, r1, rij(3), countf, countd(3), sigma(3, 3), cutoff2, den
      logical :: trlist, half
      real(wp), allocatable :: lattr(:, :)

      real(wp), allocatable :: cn_local(:)
      real(wp), allocatable :: dcndrdiag_local(:, :), dcndL_local(:, :, :)

      cn(:) = 0.0_wp
      dcndrlist(:, :) = 0.0_wp
      dcndL(:, :, :) = 0.0_wp
      cutoff2 = self%cutoff**2
      half = .not. list%complete
      trlist = allocated(list%nltr)

      ! Periodic lists store every image as its own entry, indexing the list translations
      if (trlist) then
         lattr = list%trans
      else
         lattr = trans
      end if

      !$omp parallel default(none) &
      !$omp shared(self, mol, list, lattr, cutoff2, cn, dcndrlist, dcndL, trlist, half) &
      !$omp private(iat, jat, kat, itr, itrst, itrfin, izp, jzp, r2, rij, r1) &
      !$omp private(den, countf, countd, sigma) &
      !$omp private(cn_local, dcndrdiag_local, dcndL_local)
      allocate(cn_local, source=cn)
      allocate(dcndrdiag_local(3, mol%nat), source=0.0_wp)
      allocate(dcndL_local, source=dcndL)
      !$omp do schedule(runtime)
      do iat = 1, mol%nat
         izp = mol%id(iat)
         do kat = list%inl(iat), list%inl(iat + 1) - 1
            jat = list%nlat(kat)
            jzp = mol%id(jat)
            den = self%get_en_factor(izp, jzp)

            itrst = 1
            itrfin = size(lattr, dim=2)
            if (trlist) then
               itrst = list%nltr(kat)
               itrfin = itrst
            end if

            do itr = itrst, itrfin
               rij = mol%xyz(:, iat) - (mol%xyz(:, jat) + lattr(:, itr))
               r2 = sum(rij**2)
               if (r2 > cutoff2 .or. r2 < 1.0e-12_wp) cycle
               r1 = sqrt(r2)

               countf = den * self%ncoord_count(izp, jzp, r1)
               countd = den * self%ncoord_dcount(izp, jzp, r1) * rij/r1
               sigma = spread(countd, 1, 3) * spread(rij, 2, 3)

               cn_local(iat) = cn_local(iat) + countf
               dcndL_local(:, :, iat) = dcndL_local(:, :, iat) + sigma

               ! Self-images move rigidly with the atom, no Cartesian derivative
               if (iat == jat) cycle

               ! Derivatives of CN(j) and CN(i) with respect to the position of atom i
               dcndrlist(:, kat) = dcndrlist(:, kat) + countd * self%directed_factor
               dcndrdiag_local(:, iat) = dcndrdiag_local(:, iat) + countd

               ! Upper triangular list, add the mirrored contributions to atom j
               if (half) then
                  cn_local(jat) = cn_local(jat) + countf * self%directed_factor
                  dcndL_local(:, :, jat) = dcndL_local(:, :, jat) &
                     & + sigma * self%directed_factor
                  dcndrdiag_local(:, jat) = dcndrdiag_local(:, jat) &
                     & - countd * self%directed_factor
               end if

            end do
         end do
      end do
      !$omp end do
      !$omp critical (ncoord_d_list_)
      cn(:) = cn(:) + cn_local(:)
      dcndL(:, :, :) = dcndL(:, :, :) + dcndL_local(:, :, :)
      do iat = 1, mol%nat
         dcndrlist(:, list%inl(iat)) = dcndrlist(:, list%inl(iat)) + dcndrdiag_local(:, iat)
      end do
      !$omp end critical (ncoord_d_list_)
      deallocate(cn_local, dcndrdiag_local, dcndL_local)
      !$omp end parallel

   end subroutine ncoord_d_list

   !> Adds coordination number derivatives to a gradient and stress tensor
   subroutine add_coordination_number_derivs(self, mol, trans, dEdcn, gradient, sigma)
      !> Coordination number container
      class(ncoord_type), intent(in) :: self

      !> Molecular structure data
      type(structure_type), intent(in) :: mol

      !> Lattice points
      real(wp), intent(in) :: trans(:, :)

      !> Derivative of expression with respect to the coordination number
      real(wp), intent(in) :: dEdcn(:)

      !> Derivative of the CN with respect to the Cartesian coordinates
      real(wp), intent(inout) :: gradient(:, :)

      !> Derivative of the CN with respect to strain deformations
      real(wp), intent(inout) :: sigma(:, :)

      integer :: iat, jat, izp, jzp, itr
      real(wp) :: r2, r1, rij(3), countd(3), ds(3, 3), cutoff2, den
      real(wp) :: idamp, jdamp, dEdcnij

      real(wp), allocatable :: gradient_local(:, :), sigma_local(:, :)
      real(wp), allocatable :: cn(:)

      cutoff2 = self%cutoff**2

      if (self%cut > 0.0_wp) then
         allocate(cn(mol%nat), source=0.0_wp)
         call ncoord(self, mol, trans, cn)
      end if

      !$omp parallel default(none) &
      !$omp shared(self, mol, trans, cn, cutoff2, dEdcn, gradient, sigma) &
      !$omp private(iat, jat, itr, izp, jzp, r2, rij, r1, countd, ds, den) &
      !$omp private(gradient_local, sigma_local, idamp, jdamp, dEdcnij)

      allocate(gradient_local(size(gradient, 1), size(gradient, 2)), source=0.0_wp)
      allocate(sigma_local(size(sigma, 1), size(sigma, 2)), source=0.0_wp)

      !$omp do schedule(runtime)
      do iat = 1, mol%nat
         izp = mol%id(iat)

         idamp = 1.0_wp
         if (self%cut > 0.0_wp) idamp = dlog_cn_cut(cn(iat), self%cut)

         do jat = 1, iat
            jzp = mol%id(jat)
            den = self%get_en_factor(izp, jzp)

            jdamp = 1.0_wp
            if (self%cut > 0.0_wp) jdamp = dlog_cn_cut(cn(jat), self%cut)

            dEdcnij = dEdcn(iat) * idamp &
            & + dEdcn(jat) * self%directed_factor * jdamp

            do itr = 1, size(trans, dim=2)
               rij = mol%xyz(:, iat) - (mol%xyz(:, jat) + trans(:, itr))
               r2 = sum(rij**2)
               if (r2 > cutoff2 .or. r2 < 1.0e-12_wp) cycle
               r1 = sqrt(r2)

               countd = den * self%ncoord_dcount(izp, jzp, r1) * rij/r1

               gradient_local(:, iat) = gradient_local(:, iat) + countd * dEdcnij
               gradient_local(:, jat) = gradient_local(:, jat) - countd * dEdcnij

               ds = spread(countd, 1, 3) * spread(rij, 2, 3)

               sigma_local(:, :) = sigma_local(:, :) &
                  & + ds * (dEdcn(iat) * idamp + &
                  & merge(dEdcn(jat) * self%directed_factor * jdamp, 0.0_wp, jat /= iat))
            end do
         end do
      end do
      !$omp end do

      !$omp critical (add_coordination_number_derivs_)
      gradient(:, :) = gradient(:, :) + gradient_local(:, :)
      sigma(:, :) = sigma(:, :) + sigma_local(:, :)
      !$omp end critical (add_coordination_number_derivs_)

      deallocate(gradient_local, sigma_local)
      !$omp end parallel
   end subroutine add_coordination_number_derivs

   !> Adds coordination number derivatives using a CSR neighbour list
   subroutine add_coordination_number_derivs_list(self, mol, trans, dEdcn, gradient, &
      & sigma, list)

      !> Coordination number container
      class(ncoord_type), intent(in) :: self

      !> Molecular structure data
      type(structure_type), intent(in) :: mol

      !> Lattice points
      real(wp), intent(in) :: trans(:, :)

      !> Derivative of the expression with respect to the coordination number
      real(wp), intent(in) :: dEdcn(:)

      !> Derivative of the CN with respect to the Cartesian coordinates
      real(wp), intent(inout) :: gradient(:, :)

      !> Derivative of the CN with respect to strain deformations
      real(wp), intent(inout) :: sigma(:, :)

      !> CSR list for neighbourlist-based CN evaluation
      type(csr_list), intent(in) :: list

      integer :: iat, jat, izp, jzp, itr, itrst, itrfin
      integer(i8) :: kat
      logical :: trlist, half
      real(wp) :: r2, r1, rij(3), countd(3), ds(3, 3), cutoff2, den
      real(wp) :: idamp, jdamp, dEdcnij, dEdcns
      real(wp), allocatable :: cn(:), lattr(:, :)

      ! Thread-private arrays for reduction
      ! Set to zero explicitly as the shared variants are potentially non-zero (inout)
      real(wp), allocatable :: gradient_local(:, :), sigma_local(:, :)

      cutoff2 = self%cutoff**2
      trlist = allocated(list%nltr)
      half = .not. list%complete

      if (self%cut > 0.0_wp) then
         allocate(cn(mol%nat), source=0.0_wp)
         call ncoord_list(self, mol, trans, cn, list)
      end if

      if (trlist) then
         lattr = list%trans
      else
         lattr = trans
      end if

      !$omp parallel default(none) &
      !$omp shared(self, mol, list, lattr, cutoff2, dEdcn, gradient, sigma, cn) &
      !$omp shared(trlist, half) &
      !$omp private(iat, jat, kat, itr, itrst, itrfin, izp, jzp, r2, rij, r1) &
      !$omp private(countd, ds, den, gradient_local, sigma_local, idamp, jdamp) &
      !$omp private(dEdcnij, dEdcns)
      allocate(gradient_local(size(gradient, 1), size(gradient, 2)), source=0.0_wp)
      allocate(sigma_local(size(sigma, 1), size(sigma, 2)), source=0.0_wp)
      !$omp do schedule(runtime)
      do iat = 1, mol%nat
         izp = mol%id(iat)

         idamp = 1.0_wp
         if (self%cut > 0.0_wp) idamp = dlog_cn_cut(cn(iat), self%cut)

         do kat = list%inl(iat), list%inl(iat + 1) - 1
            jat = list%nlat(kat)
            jzp = mol%id(jat)
            den = self%get_en_factor(izp, jzp)

            jdamp = 1.0_wp
            if (self%cut > 0.0_wp) jdamp = dlog_cn_cut(cn(jat), self%cut)

            dEdcnij = dEdcn(iat) * idamp &
               & + dEdcn(jat) * self%directed_factor * jdamp

            if (half .and. jat /= iat) then
               dEdcns = dEdcnij
            else
               dEdcns = dEdcn(iat) * idamp
            end if

            itrst = 1
            itrfin = size(lattr, dim=2)
            if (trlist) then
               itrst = list%nltr(kat)
               itrfin = itrst
            end if

            do itr = itrst, itrfin
               rij = mol%xyz(:, iat) - (mol%xyz(:, jat) + lattr(:, itr))
               r2 = sum(rij**2)
               if (r2 > cutoff2 .or. r2 < 1.0e-12_wp) cycle
               r1 = sqrt(r2)

               countd = den * self%ncoord_dcount(izp, jzp, r1) * rij/r1

               ds = spread(countd, 1, 3) * spread(rij, 2, 3)
               sigma_local(:, :) = sigma_local(:, :) + ds * dEdcns

               if (iat == jat) cycle

               gradient_local(:, iat) = gradient_local(:, iat) + countd * dEdcnij
               if (half) then
                  gradient_local(:, jat) = gradient_local(:, jat) - countd * dEdcnij
               end if
            end do
         end do
      end do
      !$omp end do
      !$omp critical (add_coordination_number_derivs_list_)
      gradient(:, :) = gradient(:, :) + gradient_local(:, :)
      sigma(:, :) = sigma(:, :) + sigma_local(:, :)
      !$omp end critical (add_coordination_number_derivs_list_)
      deallocate(gradient_local, sigma_local)
      !$omp end parallel

   end subroutine add_coordination_number_derivs_list

   !> Add dE/dCN contracted with the Cartesian Hessian of the
   !> coordination numbers.
   subroutine add_coordination_number_hessian(self, mol, trans, dEdcn, hessian)

      !> Coordination number container
      class(ncoord_type), intent(in) :: self

      !> Molecular structure data
      type(structure_type), intent(in) :: mol

      !> Lattice points
      real(wp), intent(in) :: trans(:, :)

      !> Derivative of expression with respect to the coordination number
      real(wp), intent(in) :: dEdcn(:)

      !> Cartesian Hessian in flattened (3*nat, 3*nat) representation
      real(wp), intent(inout) :: hessian(:, :)

      integer :: ipair, npair, iat, jat, izp, jzp, itr
      integer :: ic, jc, ii, jj
      real(wp) :: r2, r1, rij(3), cutoff2, den, dEdcnij
      real(wp) :: countd, countd2, box(3, 3), pair_box(3, 3)
      real(wp) :: idamp, jdamp
      real(wp), allocatable :: cn(:)

      ! Only diagonal Cartesian boxes receive contributions from more than one
      ! unordered atom pair. Keep those thread-private and write off-diagonal
      ! boxes directly from the thread owning the pair.
      real(wp), allocatable :: diagonal_local(:, :, :)

      cutoff2 = self%cutoff**2
      npair = mol%nat*(mol%nat - 1)/2

      if (self%cut > 0.0_wp) then
         allocate(cn(mol%nat), source=0.0_wp)
         call ncoord(self, mol, trans, cn)
      end if

      !$omp parallel default(none) &
      !$omp shared(self, mol, trans, cn, cutoff2, dEdcn, hessian, npair) &
      !$omp private(ipair, iat, jat, izp, jzp, itr, ic, jc, ii, jj, r2, r1, rij, &
      !$omp& den, dEdcnij, countd, countd2, box, pair_box, diagonal_local, &
      !$omp& idamp, jdamp)
      allocate(diagonal_local(3, 3, mol%nat), source=0.0_wp)

      !$omp do schedule(runtime)
      do ipair = 1, npair
         iat = int(0.5_wp*(1.0_wp + sqrt(8.0_wp*real(ipair, wp) + 1.0_wp)))
         if (iat*(iat - 1)/2 < ipair) iat = iat + 1
         jat = ipair - (iat - 1)*(iat - 2)/2

         izp = mol%id(iat)
         jzp = mol%id(jat)
         den = self%get_en_factor(izp, jzp)

         ! Pre-calculate damping for the atoms i and j
         idamp = 1.0_wp
         jdamp = 1.0_wp
         if (self%cut > 0.0_wp) then
            idamp = dlog_cn_cut(cn(iat), self%cut)
            jdamp = dlog_cn_cut(cn(jat), self%cut)
         end if

         ! Combined chain-rule factor for the pair contribution
         dEdcnij = dEdcn(iat)*idamp + dEdcn(jat)*self%directed_factor*jdamp

         pair_box(:, :) = 0.0_wp
         do itr = 1, size(trans, dim=2)
            rij = mol%xyz(:, iat) - (mol%xyz(:, jat) + trans(:, itr))
            r2 = sum(rij**2)
            if (r2 > cutoff2 .or. r2 < 1.0e-12_wp) cycle
            r1 = sqrt(r2)

            countd = den*self%ncoord_dcount(izp, jzp, r1)
            countd2 = den*self%ncoord_d2count(izp, jzp, r1)

            do ic = 1, 3
               do jc = 1, 3
                  box(ic, jc) = dEdcnij * ( &
                     & countd2*rij(ic)*rij(jc)/r2 &
                     & - countd*rij(ic)*rij(jc)/(r2*r1))
               end do
               box(ic, ic) = box(ic, ic) + dEdcnij*countd/r1
            end do
            pair_box(:, :) = pair_box(:, :) + box(:, :)
         end do

         diagonal_local(:, :, iat) = diagonal_local(:, :, iat) + pair_box(:, :)
         diagonal_local(:, :, jat) = diagonal_local(:, :, jat) + pair_box(:, :)

         do ic = 1, 3
            ii = 3*(iat - 1) + ic
            do jc = 1, 3
               jj = 3*(jat - 1) + jc
               hessian(ii, jj) = hessian(ii, jj) - pair_box(ic, jc)
               hessian(jj, ii) = hessian(jj, ii) - pair_box(jc, ic)
            end do
         end do
      end do
      !$omp end do nowait

      !$omp critical (add_coordination_number_hessian_)
      do iat = 1, mol%nat
         do ic = 1, 3
            ii = 3*(iat - 1) + ic
            do jc = 1, 3
               jj = 3*(iat - 1) + jc
               hessian(ii, jj) = hessian(ii, jj) + diagonal_local(ic, jc, iat)
            end do
         end do
      end do
      !$omp end critical (add_coordination_number_hessian_)

      deallocate(diagonal_local)
      !$omp end parallel

   end subroutine add_coordination_number_hessian

   !> Add dE/dCN contracted with the Cartesian Hessian of the
   !> coordination numbers using the CSR-based neighbour list.
   subroutine add_coordination_number_hessian_list(self, mol, trans, dEdcn, hessian, list)

      !> Coordination number container
      class(ncoord_type), intent(in) :: self

      !> Molecular structure data
      type(structure_type), intent(in) :: mol

      !> Lattice points
      real(wp), intent(in) :: trans(:, :)

      !> Derivative of expression with respect to the coordination number
      real(wp), intent(in) :: dEdcn(:)

      !> Cartesian Hessian in flattened (3*nat, 3*nat) representation
      real(wp), intent(inout) :: hessian(:, :)

      !> CSR list for neighbourlist-based CN evaluation
      type(csr_list), intent(in) :: list

      integer :: iat, jat, izp, jzp, itr, itrst, itrfin
      integer(i8) :: kat
      integer :: ic, jc, ii, jj
      logical :: trlist, half
      real(wp) :: r2, r1, rij(3), cutoff2, den, dEdcnij
      real(wp) :: countd, countd2, box(3, 3), pair_box(3, 3)
      real(wp) :: idamp, jdamp
      real(wp), allocatable :: cn(:), lattr(:, :)

      real(wp), allocatable :: diagonal_local(:, :, :)

      cutoff2 = self%cutoff**2
      trlist = allocated(list%nltr)
      half = .not. list%complete

      if (self%cut > 0.0_wp) then
         allocate(cn(mol%nat), source=0.0_wp)
         call ncoord_list(self, mol, trans, cn, list)
      end if

      if (trlist) then
         lattr = list%trans
      else
         lattr = trans
      end if

      !$omp parallel default(none) &
      !$omp shared(self, mol, lattr, cn, cutoff2, dEdcn, hessian, list, trlist, half) &
      !$omp private(iat, jat, kat, izp, jzp, itr, itrst, itrfin, ic, jc, ii, jj, &
      !$omp& r2, r1, rij, den, dEdcnij, countd, countd2, box, pair_box, &
      !$omp& diagonal_local, idamp, jdamp)
      allocate(diagonal_local(3, 3, mol%nat), source=0.0_wp)

      !$omp do schedule(runtime)
      do iat = 1, mol%nat
         izp = mol%id(iat)

         idamp = 1.0_wp
         if (self%cut > 0.0_wp) idamp = dlog_cn_cut(cn(iat), self%cut)

         do kat = list%inl(iat) + 1, list%inl(iat + 1) - 1
            jat = list%nlat(kat)
            jzp = mol%id(jat)
            den = self%get_en_factor(izp, jzp)

            jdamp = 1.0_wp
            if (self%cut > 0.0_wp) jdamp = dlog_cn_cut(cn(jat), self%cut)

            dEdcnij = dEdcn(iat)*idamp + dEdcn(jat)*self%directed_factor*jdamp

            itrst = 1
            itrfin = size(lattr, dim=2)
            if (trlist) then
               itrst = list%nltr(kat)
               itrfin = itrst
            end if

            pair_box(:, :) = 0.0_wp
            do itr = itrst, itrfin
               rij = mol%xyz(:, iat) - (mol%xyz(:, jat) + lattr(:, itr))
               r2 = sum(rij**2)
               if (r2 > cutoff2 .or. r2 < 1.0e-12_wp) cycle
               r1 = sqrt(r2)

               countd = den*self%ncoord_dcount(izp, jzp, r1)
               countd2 = den*self%ncoord_d2count(izp, jzp, r1)

               do ic = 1, 3
                  do jc = 1, 3
                     box(ic, jc) = dEdcnij * ( &
                        & countd2*rij(ic)*rij(jc)/r2 &
                        & - countd*rij(ic)*rij(jc)/(r2*r1))
                  end do
                  box(ic, ic) = box(ic, ic) + dEdcnij*countd/r1
               end do
               pair_box(:, :) = pair_box(:, :) + box(:, :)
            end do

            diagonal_local(:, :, iat) = diagonal_local(:, :, iat) + pair_box(:, :)
            if (half) then
               diagonal_local(:, :, jat) = diagonal_local(:, :, jat) + pair_box(:, :)
            end if

            do ic = 1, 3
               ii = 3*(iat - 1) + ic
               do jc = 1, 3
                  jj = 3*(jat - 1) + jc
                  hessian(jj, ii) = hessian(jj, ii) - pair_box(jc, ic)
                  if (half) hessian(ii, jj) = hessian(ii, jj) - pair_box(ic, jc)
               end do
            end do
         end do
      end do
      !$omp end do nowait

      !$omp critical (add_coordination_number_hessian_list_)
      do iat = 1, mol%nat
         do ic = 1, 3
            ii = 3*(iat - 1) + ic
            do jc = 1, 3
               jj = 3*(iat - 1) + jc
               hessian(ii, jj) = hessian(ii, jj) + diagonal_local(ic, jc, iat)
            end do
         end do
      end do
      !$omp end critical (add_coordination_number_hessian_list_)
      deallocate(diagonal_local)
      !$omp end parallel

   end subroutine add_coordination_number_hessian_list


   !> Evaluates the pairwise electronegativity factor
   elemental function get_en_factor(self, izp, jzp) result(en_factor)
      !> Coordination number container
      class(ncoord_type), intent(in) :: self
      !> Atom i index
      integer, intent(in)  :: izp
      !> Atom j index
      integer, intent(in)  :: jzp

      real(wp) :: en_factor

      en_factor = 1.0_wp

   end function get_en_factor


   !> Cutoff function for large coordination numbers
   pure subroutine cut_coordination_number(cn_max, cn, dcndr, dcndL, &
      & dcndrlist, list)

      !> Maximum CN (not strictly obeyed)
      real(wp), intent(in) :: cn_max

      !> On input coordination number, on output modified CN
      real(wp), intent(inout) :: cn(:)

      !> On input derivative of CN w.r.t. cartesian coordinates,
      !> on output derivative of modified CN
      real(wp), intent(inout), optional :: dcndr(:, :, :)

      !> On input derivative of CN in CSR format,
      !> on output derivative of modified CN
      real(wp), intent(inout), optional :: dcndrlist(:, :)

      !> On input derivative of CN w.r.t. strain deformation,
      !> on output derivative of modified CN
      real(wp), intent(inout), optional :: dcndL(:, :, :)

      !> CSR list
      type(csr_list), intent(in), optional :: list

      real(wp) :: dcnpdcn
      integer :: iat
      integer(i8) :: kat

      if (present(dcndL)) then
         do iat = 1, size(cn)
            dcnpdcn = dlog_cn_cut(cn(iat), cn_max)
            dcndL(:, :, iat) = dcnpdcn*dcndL(:, :, iat)
         end do
      end if

      if (present(dcndrlist) .and. present(list)) then
         do kat = 1_i8, size(dcndrlist, 2, kind=i8)
            dcnpdcn = dlog_cn_cut(cn(list%nlat(kat)), cn_max)
            dcndrlist(:, kat) = dcnpdcn*dcndrlist(:, kat)
         end do
      end if
      if (present(dcndr)) then
         do iat = 1, size(cn)
            dcnpdcn = dlog_cn_cut(cn(iat), cn_max)
            dcndr(:, :, iat) = dcnpdcn*dcndr(:, :, iat)
         end do
      end if

      do iat = 1, size(cn)
         cn(iat) = log_cn_cut(cn(iat), cn_max)
      end do

   end subroutine cut_coordination_number

   !> Applies the smooth coordination number cutoff
   elemental function log_cn_cut(cn, cnmax) result(cnp)
      !> Coordination number
      real(wp), intent(in) :: cn

      !> Maximum coordination number
      real(wp), intent(in) :: cnmax

      real(wp) :: cnp
      cnp = log(1.0_wp + exp(cnmax)) - log(1.0_wp + exp(cnmax - cn))
   end function log_cn_cut

   !> Evaluates the derivative of the smooth coordination number cutoff
   elemental function dlog_cn_cut(cn, cnmax) result(dcnpdcn)
      !> Coordination number
      real(wp), intent(in) :: cn

      !> Maximum coordination number
      real(wp), intent(in) :: cnmax

      real(wp) :: dcnpdcn
      dcnpdcn = exp(cnmax)/(exp(cnmax) + exp(cn))
   end function dlog_cn_cut

end module mctc_ncoord_type
