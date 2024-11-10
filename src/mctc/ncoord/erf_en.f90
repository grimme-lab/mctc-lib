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

!> @file mctc/ncoord/erf_en.f90
!> Provides an implementation for the electronegativity-weighted CN as used in the CEH method

!> Coordination number implementation with single error function and EN-weighting for the CEH method.
module mctc_ncoord_erf_en
   use mctc_env, only : wp
   use mctc_io, only : structure_type
   use mctc_io_constants, only : pi
   use mctc_data_covrad, only : get_covalent_rad
   use mctc_data_paulingen, only : get_pauling_en
   use mctc_ncoord_type, only : ncoord_type
   implicit none
   private

   public :: new_erf_en_ncoord

   !> Coordination number evaluator
   type, public, extends(ncoord_type) :: erf_en_ncoord_type
      real(wp), allocatable :: rcov(:)
      real(wp), allocatable :: en(:)
   contains
      !> Evaluates the EN-weighted error counting function
      procedure :: ncoord_count
      !> Evaluates the derivative of the EN-weighted error counting function
      procedure :: ncoord_dcount
   end type erf_en_ncoord_type

   !> Steepness of counting function (CEH)
   real(wp), parameter :: default_kcn = 2.65_wp

   real(wp), parameter :: default_cutoff = 25.0_wp

contains


   subroutine new_erf_en_ncoord(self, mol, kcn, cutoff, rcov, en)
      !> Coordination number container
      type(erf_en_ncoord_type), intent(out) :: self
      !> Molecular structure data
      type(structure_type), intent(in) :: mol
      !> Steepness of counting function
      real(wp), optional :: kcn
      !> Real space cutoff
      real(wp), intent(in), optional :: cutoff
      !> Covalent radii
      real(wp), intent(in), optional :: rcov(:)
      !> Electronegativity
      real(wp), intent(in), optional :: en(:)
      
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

      ! Default are always the Pyykko radii 
      ! but in the CEH use case they will be overwritten
      allocate(self%rcov(mol%nid))
      if (present(rcov)) then
         self%rcov(:) = rcov
      else   
         self%rcov(:) = get_covalent_rad(mol%num)
      end if

      ! Default are always the Pauling EN 
      ! but in the CEH use case they will be overwritten
      allocate(self%en(mol%nid))
      if (present(en)) then
         self%en(:) = en
      else
         self%en(:) = get_pauling_en(mol%num)
      end if

      !> CN is directed due to the EN contribution
      !> i.e. contribution added to higher EN and removed from lower EN partner
      self%directed_factor = -1.0_wp

   end subroutine new_erf_en_ncoord


   !> Error counting function for coordination number contributions.
   elemental function ncoord_count(self, izp, jzp, r) result(count)
      !> Coordination number container
      class(erf_en_ncoord_type), intent(in) :: self
      !> Atom i index
      integer, intent(in)  :: izp
      !> Atom j index
      integer, intent(in)  :: jzp
      !> Current distance.
      real(wp), intent(in) :: r

      real(wp) :: rc, diff_en, count

      rc = self%rcov(izp) + self%rcov(jzp)
      diff_en = self%en(jzp) - self%en(izp)
      ! error function based counting function with EN 
      count = 0.5_wp * diff_en * (1.0_wp + erf(-self%kcn*(r-rc)/rc))

   end function ncoord_count

   !> Derivative of the error counting function w.r.t. the distance.
   elemental function ncoord_dcount(self, izp, jzp, r) result(count)
      !> Coordination number container
      class(erf_en_ncoord_type), intent(in) :: self
      !> Atom i index
      integer, intent(in)  :: izp
      !> Atom j index
      integer, intent(in)  :: jzp
      !> Current distance.
      real(wp), intent(in) :: r

      real(wp) :: rc, diff_en, exponent, expterm, count

      rc = self%rcov(izp) + self%rcov(jzp)
      diff_en = self%en(jzp) - self%en(izp)
      ! error function based counting function with EN derivative
      exponent = self%kcn*(r-rc)/rc
      expterm = exp(-exponent**2)
      count = - diff_en * (self%kcn*expterm)/(rc*sqrt(pi))

   end function ncoord_dcount

end module mctc_ncoord_erf_en
