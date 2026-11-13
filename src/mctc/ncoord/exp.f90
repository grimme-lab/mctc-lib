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

!> Coordination number implementation using an exponential counting function as in dftd3.
module mctc_ncoord_exp
   use mctc_data_covrad, only : get_covalent_rad
   use mctc_env, only : wp
   use mctc_io, only : structure_type
   use mctc_ncoord_type, only : ncoord_type
   implicit none
   private

   public :: new_exp_ncoord

   !> Coordination number evaluator
   type, public, extends(ncoord_type) :: exp_ncoord_type
      !> Covalent radii
      real(wp), allocatable :: rcov(:)
   contains
      !> Evaluates the exponential counting function
      procedure :: ncoord_count
      !> Evaluates the derivative of the exponential counting function
      procedure :: ncoord_dcount
      !> Evaluates the second derivative of the exponential counting function
      procedure :: ncoord_d2count
      !> Evaluates both the counting function and its first derivative
      procedure :: ncoord_count_dcount
      !> Evaluates both the first and second derivatives of the counting function
      procedure :: ncoord_dcount_d2count
   end type exp_ncoord_type

   !> Steepness of counting function
   real(wp), parameter :: default_kcn = 16.0_wp
   !> Real-space cutoff for coordination number
   real(wp), parameter :: default_cutoff = 25.0_wp

contains


   subroutine new_exp_ncoord(self, mol, kcn, cutoff, rcov, cut)
      !> Coordination number container
      type(exp_ncoord_type), intent(out) :: self
      !> Molecular structure data
      type(structure_type), intent(in) :: mol
      !> Steepness of counting function
      real(wp), intent(in), optional :: kcn
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

   end subroutine new_exp_ncoord


   !> Exponential counting function for coordination number contributions.
   elemental function ncoord_count(self, izp, jzp, r) result(count)
      !> Coordination number container
      class(exp_ncoord_type), intent(in) :: self
      !> Atom i index
      integer, intent(in)  :: izp
      !> Atom j index
      integer, intent(in)  :: jzp
      !> Current distance.
      real(wp), intent(in) :: r

      real(wp) :: rc, inv_r, count

      rc = self%rcov(izp) + self%rcov(jzp)
      inv_r = 1.0_wp / r

      count = 1.0_wp / (1.0_wp + exp(-self%kcn * (rc * inv_r - 1.0_wp)))

   end function ncoord_count

   !> Derivative of the exponential counting function w.r.t. the distance.
   elemental function ncoord_dcount(self, izp, jzp, r) result(count)
      !> Coordination number container
      class(exp_ncoord_type), intent(in) :: self
      !> Atom i index
      integer, intent(in)  :: izp
      !> Atom j index
      integer, intent(in)  :: jzp
      !> Current distance.
      real(wp), intent(in) :: r

      real(wp) :: rc, inv_r, expterm, cf, count

      rc = self%rcov(izp) + self%rcov(jzp)
      inv_r = 1.0_wp / r

      expterm = exp(-self%kcn * (rc * inv_r - 1.0_wp))
      cf = 1.0_wp / (1.0_wp + expterm)
      count = -self%kcn * rc * (expterm * cf * cf) * (inv_r * inv_r)

   end function ncoord_dcount


   !> Second derivative of the exponential counting function w.r.t. the distance.
   elemental function ncoord_d2count(self, izp, jzp, r) result(count)
      !> Coordination number container
      class(exp_ncoord_type), intent(in) :: self
      !> Atom i index
      integer, intent(in) :: izp
      !> Atom j index
      integer, intent(in) :: jzp
      !> Current distance.
      real(wp), intent(in) :: r

      real(wp) :: rc, inv_r, inv_r2, krc, expterm, cf, tmp, count

      rc = self%rcov(izp) + self%rcov(jzp)
      inv_r = 1.0_wp / r
      inv_r2 = inv_r * inv_r
      krc = self%kcn * rc
      expterm = exp(-self%kcn * (rc * inv_r - 1.0_wp))
      cf = 1.0_wp / (1.0_wp + expterm)
      tmp = expterm * cf * cf

      count = tmp * krc * inv_r2 * inv_r * ((1.0_wp - 2.0_wp * cf) * krc * inv_r + 2.0_wp)

   end function ncoord_d2count


   !> Fused evaluation of exponential counting function and its first derivative.
   elemental subroutine ncoord_count_dcount(self, izp, jzp, r, countf, countd)
      !> Coordination number container
      class(exp_ncoord_type), intent(in) :: self
      !> Atom i index
      integer, intent(in) :: izp
      !> Atom j index
      integer, intent(in) :: jzp
      !> Current distance
      real(wp), intent(in) :: r
      !> Counting function value
      real(wp), intent(out) :: countf
      !> First derivative of counting function w.r.t. distance
      real(wp), intent(out) :: countd

      real(wp) :: rc, inv_r, expterm

      rc = self%rcov(izp) + self%rcov(jzp)
      inv_r = 1.0_wp / r
      expterm = exp(-self%kcn * (rc * inv_r - 1.0_wp))
      countf = 1.0_wp / (1.0_wp + expterm)
      countd = -self%kcn * rc * (expterm * countf * countf) * (inv_r * inv_r)

   end subroutine ncoord_count_dcount


   !> Fused evaluation of first and second derivatives of exponential counting function.
   elemental subroutine ncoord_dcount_d2count(self, izp, jzp, r, countd, countd2)
      !> Coordination number container
      class(exp_ncoord_type), intent(in) :: self
      !> Atom i index
      integer, intent(in) :: izp
      !> Atom j index
      integer, intent(in) :: jzp
      !> Current distance
      real(wp), intent(in) :: r
      !> First derivative of counting function w.r.t. distance
      real(wp), intent(out) :: countd
      !> Second derivative of counting function w.r.t. distance
      real(wp), intent(out) :: countd2

      real(wp) :: rc, inv_r, inv_r2, krc, expterm, cf, tmp

      rc = self%rcov(izp) + self%rcov(jzp)
      inv_r = 1.0_wp / r
      inv_r2 = inv_r * inv_r
      krc = self%kcn * rc
      expterm = exp(-self%kcn * (rc * inv_r - 1.0_wp))
      cf = 1.0_wp / (1.0_wp + expterm)
      tmp = expterm * cf * cf

      countd = -krc * tmp * inv_r2
      countd2 = tmp * krc * inv_r2 * inv_r * ((1.0_wp - 2.0_wp * cf) * krc * inv_r + 2.0_wp)

   end subroutine ncoord_dcount_d2count

end module mctc_ncoord_exp
