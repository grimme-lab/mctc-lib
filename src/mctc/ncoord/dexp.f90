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

!> @file mctc/ncoord/dexp.f90
!> Provides a coordination number implementation with double exponential counting function

!> Coordination number implementation using a double exponential counting function as in GFN2-xTB
module mctc_ncoord_dexp
   use mctc_env, only : wp
   use mctc_io, only : structure_type
   use mctc_data_covrad, only : get_covalent_rad
   use mctc_ncoord_type, only : ncoord_type
   implicit none
   private

   public :: new_dexp_ncoord
   

   !> Coordination number evaluator
   type, public, extends(ncoord_type) :: dexp_ncoord_type
      real(wp), allocatable :: rcov(:)
   contains
      !> Evaluates the dexp counting function 
      procedure :: ncoord_count
      !> Evaluates the derivative of the dexp counting function
      procedure :: ncoord_dcount
   end type dexp_ncoord_type

   !> Steepness of first counting function
   real(wp),parameter :: ka = 10.0_wp
   !> Steepness of second counting function
   real(wp),parameter :: kb = 20.0_wp
   !> Offset of the second counting function
   real(wp),parameter :: r_shift = 2.0_wp

   real(wp), parameter :: default_cutoff = 25.0_wp


contains


subroutine new_dexp_ncoord(self, mol, cutoff, rcov)
   !> Coordination number container
   type(dexp_ncoord_type), intent(out) :: self
   !> Molecular structure data
   type(structure_type), intent(in) :: mol
   !> Real space cutoff
   real(wp), intent(in), optional :: cutoff
   !> Covalent radii
   real(wp), intent(in), optional :: rcov(:)

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

end subroutine new_dexp_ncoord


!> Double-exponential counting function for coordination number contributions.
elemental function ncoord_count(self, izp, jzp, r) result(count)
   !> Coordination number container
   class(dexp_ncoord_type), intent(in) :: self
   !> Atom i index
   integer, intent(in)  :: izp
   !> Atom j index
   integer, intent(in)  :: jzp
   !> Current distance.
   real(wp), intent(in) :: r

   real(wp) :: rc, count

   rc = self%rcov(izp) + self%rcov(jzp)

   count = exp_count(ka, r, rc) * exp_count(kb, r, rc + r_shift)

end function ncoord_count

!> Derivative of the double-exponential counting function w.r.t. the distance.
elemental function ncoord_dcount(self, izp, jzp, r) result(count)
   !> Coordination number container
   class(dexp_ncoord_type), intent(in) :: self
   !> Atom i index
   integer, intent(in)  :: izp
   !> Atom j index
   integer, intent(in)  :: jzp
   !> Current distance.
   real(wp), intent(in) :: r

   real(wp) :: rc, count
   
   rc = self%rcov(izp) + self%rcov(jzp)

   count = (exp_dcount(ka, r, rc) * exp_count(kb, r, rc + r_shift) &
               & + exp_count(ka, r, rc) * exp_dcount(kb, r, rc + r_shift))

end function ncoord_dcount


!> Mono-exponential counting function for coordination number contributions.
elemental function exp_count(k, r, r0) result(count)
   !> Steepness of the counting function.
   real(wp), intent(in) :: k
   !> Current distance.
   real(wp), intent(in) :: r
   !> Cutoff radius.
   real(wp), intent(in) :: r0

   real(wp) :: count

   count = 1.0_wp/(1.0_wp+exp(-k*(r0/r-1.0_wp)))

end function exp_count

!> Derivative of the mono-exponential counting function w.r.t. the distance.
elemental function exp_dcount(k, r, r0) result(count)
   !> Steepness of the counting function.
   real(wp), intent(in) :: k
   !> Current distance.
   real(wp), intent(in) :: r
   !> Cutoff radius.
   real(wp), intent(in) :: r0

   real(wp) :: count
   real(wp) :: expterm

   expterm = exp(-k*(r0/r-1._wp))
   count = (-k*r0*expterm)/(r**2*((expterm+1._wp)**2))

end function exp_dcount

end module mctc_ncoord_dexp
