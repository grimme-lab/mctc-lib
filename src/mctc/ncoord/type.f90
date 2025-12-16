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

!> Declaration of base class for coordination number evalulations
module mctc_ncoord_type
   use mctc_env, only : wp
   use mctc_io, only : structure_type
   use mctc_cutoff, only : get_lattice_points

   implicit none
   private

   !> Abstract base class for coordination number evaluator
   type, public, abstract :: ncoord_type
      !> Radial cutoff for the coordination number
      real(wp)  :: cutoff
      !> Steepness of counting function
      real(wp)  :: kcn 
      !> Factor determining whether the CN is evaluated with direction
      !> if +1 the CN contribution is added equally to both partners
      !> if -1 (i.e. with the EN-dep.) it is added to one and subtracted from the other
      real(wp)  :: directed_factor
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
      !> Evaluates pairwise electronegativity factor
      procedure :: get_en_factor
      !> Add CN derivative of an arbitrary function
      procedure :: add_coordination_number_derivs
      !> Evaluates the counting function (exp, dexp, erf, ...)
      procedure(ncoord_count),  deferred :: ncoord_count
      !> Evaluates the derivative of the counting function (exp, dexp, erf, ...)
      procedure(ncoord_dcount), deferred :: ncoord_dcount
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

   !> Wrapper for CN using the CN cutoff for the lattice
   subroutine get_cn(self, mol, cn, dcndr, dcndL)
      !> Coordination number container
      class(ncoord_type), intent(in) :: self
      !> Molecular structure data
      type(structure_type), intent(in) :: mol
      !> Error function coordination number.
      real(wp), intent(out) :: cn(:)
      !> Derivative of the CN with respect to the Cartesian coordinates.
      real(wp), intent(out), optional :: dcndr(:, :, :)
      !> Derivative of the CN with respect to strain deformations.
      real(wp), intent(out), optional :: dcndL(:, :, :)

      real(wp), allocatable :: lattr(:, :)

      call get_lattice_points(mol%periodic, mol%lattice, self%cutoff, lattr)
      call get_coordination_number(self, mol, lattr, cn, dcndr, dcndL)
   end subroutine get_cn

   !> Geometric fractional coordination number
   subroutine get_coordination_number(self, mol, trans, cn, dcndr, dcndL)

      !> Coordination number container
      class(ncoord_type), intent(in) :: self

      !> Molecular structure data
      type(structure_type), intent(in) :: mol

      !> Lattice points
      real(wp), intent(in) :: trans(:, :)

      !> Error function coordination number.
      real(wp), intent(out) :: cn(:)

      !> Derivative of the CN with respect to the Cartesian coordinates.
      real(wp), intent(out), optional :: dcndr(:, :, :)

      !> Derivative of the CN with respect to strain deformations.
      real(wp), intent(out), optional :: dcndL(:, :, :)

      if (present(dcndr) .and. present(dcndL)) then
         call ncoord_d(self, mol, trans, cn, dcndr, dcndL)
      else
         call ncoord(self, mol, trans, cn)
      end if

      if (self%cut > 0.0_wp) then
         call cut_coordination_number(self%cut, cn, dcndr, dcndL)
      end if

   end subroutine get_coordination_number


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
               dcndr_local(:, jat, jat) = dcndr_local(:, jat, jat) - countd * self%directed_factor
               dcndr_local(:, iat, jat) = dcndr_local(:, iat, jat) + countd * self%directed_factor
               dcndr_local(:, jat, iat) = dcndr_local(:, jat, iat) - countd

               sigma = spread(countd, 1, 3) * spread(rij, 2, 3)

               dcndL_local(:, :, iat) = dcndL_local(:, :, iat) + sigma
               if (iat /= jat) then
                  dcndL_local(:, :, jat) = dcndL_local(:, :, jat) + sigma * self%directed_factor
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

      ! Thread-private arrays for reduction
      ! Set to zero explicitly as the shared variants are potentially non-zero (inout)
      real(wp), allocatable :: gradient_local(:, :), sigma_local(:, :)
   
      cutoff2 = self%cutoff**2

      !$omp parallel default(none) &
      !$omp shared(self, mol, trans, cutoff2, dEdcn, gradient, sigma) &
      !$omp private(iat, jat, itr, izp, jzp, r2, rij, r1, countd, ds, den) &
      !$omp private(gradient_local, sigma_local)
      allocate(gradient_local(size(gradient, 1), size(gradient, 2)), source=0.0_wp)
      allocate(sigma_local(size(sigma, 1), size(sigma, 2)), source=0.0_wp)
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

               countd = den * self%ncoord_dcount(izp, jzp, r1) * rij/r1

               gradient_local(:, iat) = gradient_local(:, iat) + countd &
                  & * (dEdcn(iat) + dEdcn(jat) * self%directed_factor)
               gradient_local(:, jat) = gradient_local(:, jat) - countd &
                  & * (dEdcn(iat) + dEdcn(jat) * self%directed_factor)
   
               ds = spread(countd, 1, 3) * spread(rij, 2, 3)
   
               sigma_local(:, :) = sigma_local(:, :) &
                  & + ds * (dEdcn(iat) + &
                  & merge(dEdcn(jat) * self%directed_factor, 0.0_wp, jat /= iat))
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


   !> Evaluates pairwise electronegativity factor if non applies
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
   pure subroutine cut_coordination_number(cn_max, cn, dcndr, dcndL)

      !> Maximum CN (not strictly obeyed)
      real(wp), intent(in) :: cn_max

      !> On input coordination number, on output modified CN
      real(wp), intent(inout) :: cn(:)

      !> On input derivative of CN w.r.t. cartesian coordinates,
      !> on output derivative of modified CN
      real(wp), intent(inout), optional :: dcndr(:, :, :)

      !> On input derivative of CN w.r.t. strain deformation,
      !> on output derivative of modified CN
      real(wp), intent(inout), optional :: dcndL(:, :, :)

      real(wp) :: dcnpdcn
      integer  :: iat

      if (present(dcndL)) then
         do iat = 1, size(cn)
            dcnpdcn = dlog_cn_cut(cn(iat), cn_max)
            dcndL(:, :, iat) = dcnpdcn*dcndL(:, :, iat)
         enddo
      endif

      if (present(dcndr)) then
         do iat = 1, size(cn)
            dcnpdcn = dlog_cn_cut(cn(iat), cn_max)
            dcndr(:, :, iat) = dcnpdcn*dcndr(:, :, iat)
         enddo
      endif

      do iat = 1, size(cn)
         cn(iat) = log_cn_cut(cn(iat), cn_max)
      enddo

   end subroutine cut_coordination_number

   elemental function log_cn_cut(cn, cnmax) result(cnp)
      real(wp), intent(in) :: cn
      real(wp), intent(in) :: cnmax
      real(wp) :: cnp
      cnp = log(1.0_wp + exp(cnmax)) - log(1.0_wp + exp(cnmax - cn))
   end function log_cn_cut

   elemental function dlog_cn_cut(cn, cnmax) result(dcnpdcn)
      real(wp), intent(in) :: cn
      real(wp), intent(in) :: cnmax
      real(wp) :: dcnpdcn
      dcnpdcn = exp(cnmax)/(exp(cnmax) + exp(cn))
   end function dlog_cn_cut

end module mctc_ncoord_type
