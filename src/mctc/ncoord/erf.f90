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

!> @dir mctc/ncoord/erf
!> Provides variations (EN) of the error function based coordination number

!> @file mctc/ncoord/erf.f90
!> Provides error function based coordination number

!> Coordination number implementation with single error function
module mctc_ncoord_erf
   use mctc_env, only : wp
   use mctc_io, only : structure_type
   use mctc_io_constants, only : pi
   use mctc_data_covrad, only : get_covalent_rad
   use mctc_ncoord_type, only : ncoord_type
   implicit none
   private

   public :: new_erf_ncoord

   !> Coordination number evaluator
   type, public, extends(ncoord_type) :: erf_ncoord_type
      real(wp), allocatable :: rcov(:)
   contains
      !> Evaluates the error counting function
      procedure :: ncoord_count
      !> Evaluates the derivative of the error counting function
      procedure :: ncoord_dcount
   end type erf_ncoord_type

   !> Steepness of counting function (CEH)
   real(wp), parameter :: default_kcn = 3.15_wp

   real(wp), parameter :: default_cutoff = 25.0_wp

contains


   subroutine new_erf_ncoord(self, mol, kcn, cutoff, rcov, cut)
      !> Coordination number container
      type(erf_ncoord_type), intent(out) :: self
      !> Molecular structure data
      type(structure_type), intent(in) :: mol
      !> Steepness of counting function
      real(wp), optional :: kcn
      !> Real space cutoff
      real(wp), intent(in), optional :: cutoff
      !> Covalent radii
      real(wp), intent(in), optional :: rcov(:)
      !> Cutoff for the maximum coordination number
      real(wp), intent(in), optional :: cut

      if(present(kcn)) then
         self%kcn = kcn
      else
         self%kcn = default_kcn
      end if

      if (present(cutoff)) then
         self%cutoff = cutoff
      else
         self%cutoff = default_cutoff
      end if

      allocate(self%rcov(mol%nid))
      if (present(rcov)) then
         self%rcov(:) = rcov
      else
         self%rcov(:) = get_covalent_rad(mol%num)
      end if

      self%directed_factor = 1.0_wp

      if (present(cut)) then
         self%cut = cut
      else
         ! Negative value deactivates the cutoff
         self%cut = -1.0_wp
      end if

   end subroutine new_erf_ncoord


   !> Error counting function for coordination number contributions.
   elemental function ncoord_count(self, izp, jzp, r) result(count)
      !> Coordination number container
      class(erf_ncoord_type), intent(in) :: self
      !> Atom i index
      integer, intent(in)  :: izp
      !> Atom j index
      integer, intent(in)  :: jzp
      !> Current distance.
      real(wp), intent(in) :: r

      real(wp) :: rc, count
      
      rc = (self%rcov(izp) + self%rcov(jzp))

      count = 0.5_wp * (1.0_wp + erf(-self%kcn*(r-rc)/rc))
      
   end function ncoord_count

   !> Derivative of the error counting function w.r.t. the distance.
   elemental function ncoord_dcount(self, izp, jzp, r) result(count)
      !> Coordination number container
      class(erf_ncoord_type), intent(in) :: self
      !> Atom i index
      integer, intent(in)  :: izp
      !> Atom j index
      integer, intent(in)  :: jzp
      !> Current distance.
      real(wp), intent(in) :: r

      real(wp), parameter :: sqrtpi = sqrt(pi)
      real(wp) :: rc, exponent, expterm, count

      rc = self%rcov(izp) + self%rcov(jzp)

      exponent = self%kcn*(r-rc)/rc
      expterm = exp(-exponent**2)
      count = -(self%kcn*expterm)/(rc*sqrtpi)

   end function ncoord_dcount

end module mctc_ncoord_erf
