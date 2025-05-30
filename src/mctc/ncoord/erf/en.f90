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

!> @file mctc/ncoord/erf/en.f90
!> Provides an implementation for the electronegativity-weighted CN

!> Coordination number implementation with single error function and EN-weighting.
module mctc_ncoord_erf_en
   use mctc_env, only : wp
   use mctc_io, only : structure_type
   use mctc_data_covrad, only : get_covalent_rad
   use mctc_data_paulingen, only : get_pauling_en
   use mctc_ncoord_erf, only : erf_ncoord_type
   implicit none
   private

   public :: new_erf_en_ncoord

   !> Coordination number evaluator
   type, public, extends(erf_ncoord_type) :: erf_en_ncoord_type
      !> Electronegativity
      real(wp), allocatable :: en(:)
   contains
      !> Evaluates pairwise electronegativity factor
      procedure :: get_en_factor
   end type erf_en_ncoord_type

   !> Steepness of counting function
   real(wp), parameter :: default_kcn = 2.65_wp
   !> Exponent of distance normalization 
   real(wp), parameter :: default_norm_exp = 1.0_wp
   !> Real-space cutoff for coordination number
   real(wp), parameter :: default_cutoff = 25.0_wp

contains


   subroutine new_erf_en_ncoord(self, mol, kcn, cutoff, rcov, en, cut, norm_exp)
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
      !> Electronegativity (normalized to F)
      real(wp), intent(in), optional :: en(:)
      !> Cutoff for the maximum coordination number
      real(wp), intent(in), optional :: cut
      !> Exponent of the distance normalization
      real(wp), intent(in), optional :: norm_exp

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

      allocate(self%en(mol%nid))
      if (present(en)) then
         self%en(:) = en
      else
         self%en(:) = get_pauling_en(mol%num)
      end if

      ! CN is directed due to the EN contribution
      ! i.e. added to higher EN and removed from lower EN species
      self%directed_factor = -1.0_wp

      if (present(cut)) then
         self%cut = cut
      else
         ! Negative value deactivates the cutoff
         self%cut = -1.0_wp
      end if

      if (present(norm_exp)) then
         self%norm_exp = norm_exp
      else
         self%norm_exp = default_norm_exp
      end if

   end subroutine new_erf_en_ncoord


   !> Evaluates pairwise electronegativity factor
   elemental function get_en_factor(self, izp, jzp) result(en_factor)
      !> Coordination number container
      class(erf_en_ncoord_type), intent(in) :: self
      !> Atom i index
      integer, intent(in)  :: izp
      !> Atom j index
      integer, intent(in)  :: jzp

      real(wp) :: en_factor

      en_factor = self%en(jzp) - self%en(izp)
      
   end function get_en_factor

end module mctc_ncoord_erf_en
