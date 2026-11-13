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

!> Coordination number implementation using a double exponential counting function as in GFN2-xTB
module mctc_ncoord_dexp
   use mctc_data_covrad, only : get_covalent_rad
   use mctc_env, only : wp
   use mctc_io, only : structure_type
   use mctc_ncoord_type, only : ncoord_type
   implicit none
   private

   public :: new_dexp_ncoord


   !> Coordination number evaluator
   type, public, extends(ncoord_type) :: dexp_ncoord_type
      !> Covalent radii
      real(wp), allocatable :: rcov(:)
   contains
      !> Evaluates the dexp counting function
      procedure :: ncoord_count
      !> Evaluates the derivative of the dexp counting function
      procedure :: ncoord_dcount
      !> Evaluates the second derivative of the dexp counting function
      procedure :: ncoord_d2count
      !> Evaluates both the counting function and its first derivative
      procedure :: ncoord_count_dcount
      !> Evaluates both the first and second derivatives of the counting function
      procedure :: ncoord_dcount_d2count
   end type dexp_ncoord_type

   !> Steepness of first counting function
   real(wp),parameter :: ka = 10.0_wp
   !> Steepness of second counting function
   real(wp),parameter :: kb = 20.0_wp
   !> Offset of the second counting function
   real(wp),parameter :: r_shift = 2.0_wp
   !> Real-space cutoff for coordination number
   real(wp), parameter :: default_cutoff = 25.0_wp


contains


subroutine new_dexp_ncoord(self, mol, cutoff, rcov, cut)
   !> Coordination number container
   type(dexp_ncoord_type), intent(out) :: self
   !> Molecular structure data
   type(structure_type), intent(in) :: mol
   !> Real space cutoff
   real(wp), intent(in), optional :: cutoff
   !> Covalent radii
   real(wp), intent(in), optional :: rcov(:)
   !> Cutoff for the maximum coordination number
   real(wp), intent(in), optional :: cut

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

   real(wp) :: rc, inv_r, ea, eb, count

   rc = self%rcov(izp) + self%rcov(jzp)
   inv_r = 1.0_wp / r

   ea = exp(-ka * (rc * inv_r - 1.0_wp))
   eb = exp(-kb * ((rc + r_shift) * inv_r - 1.0_wp))
   count = (1.0_wp / (1.0_wp + ea)) * (1.0_wp / (1.0_wp + eb))

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

   real(wp) :: rc, rcb, inv_r, inv_r2, ea, eb, fa, fb, dfa, dfb, count

   rc = self%rcov(izp) + self%rcov(jzp)
   rcb = rc + r_shift
   inv_r = 1.0_wp / r
   inv_r2 = inv_r * inv_r

   ea = exp(-ka * (rc * inv_r - 1.0_wp))
   fa = 1.0_wp / (1.0_wp + ea)
   dfa = -ka * rc * (ea * fa * fa) * inv_r2

   eb = exp(-kb * (rcb * inv_r - 1.0_wp))
   fb = 1.0_wp / (1.0_wp + eb)
   dfb = -kb * rcb * (eb * fb * fb) * inv_r2

   count = dfa * fb + fa * dfb

end function ncoord_dcount

!> Second derivative of the double-exponential counting function w.r.t. the distance.
elemental function ncoord_d2count(self, izp, jzp, r) result(count)
   !> Coordination number container
   class(dexp_ncoord_type), intent(in) :: self
   !> Atom i index
   integer, intent(in) :: izp
   !> Atom j index
   integer, intent(in) :: jzp
   !> Current distance.
   real(wp), intent(in) :: r

   real(wp) :: rc, rcb, inv_r, inv_r2, krc_a, krc_b
   real(wp) :: ea, fa, tmp_a, dfa, d2fa
   real(wp) :: eb, fb, tmp_b, dfb, d2fb, count

   rc = self%rcov(izp) + self%rcov(jzp)
   rcb = rc + r_shift
   inv_r = 1.0_wp / r
   inv_r2 = inv_r * inv_r

   krc_a = ka * rc
   ea = exp(-ka * (rc * inv_r - 1.0_wp))
   fa = 1.0_wp / (1.0_wp + ea)
   tmp_a = ea * fa * fa
   dfa = -krc_a * tmp_a * inv_r2
   d2fa = tmp_a * krc_a * inv_r2 * inv_r * ((1.0_wp - 2.0_wp * fa) * krc_a * inv_r + 2.0_wp)

   krc_b = kb * rcb
   eb = exp(-kb * (rcb * inv_r - 1.0_wp))
   fb = 1.0_wp / (1.0_wp + eb)
   tmp_b = eb * fb * fb
   dfb = -krc_b * tmp_b * inv_r2
   d2fb = tmp_b * krc_b * inv_r2 * inv_r * ((1.0_wp - 2.0_wp * fb) * krc_b * inv_r + 2.0_wp)

   count = d2fa * fb + 2.0_wp * dfa * dfb + fa * d2fb

end function ncoord_d2count

!> Fused evaluation of double-exponential counting function and its first derivative.
elemental subroutine ncoord_count_dcount(self, izp, jzp, r, countf, countd)
   !> Coordination number container
   class(dexp_ncoord_type), intent(in) :: self
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

   real(wp) :: rc, rcb, inv_r, inv_r2, ea, eb, fa, fb, dfa, dfb

   rc = self%rcov(izp) + self%rcov(jzp)
   rcb = rc + r_shift
   inv_r = 1.0_wp / r
   inv_r2 = inv_r * inv_r

   ea = exp(-ka * (rc * inv_r - 1.0_wp))
   fa = 1.0_wp / (1.0_wp + ea)
   dfa = -ka * rc * (ea * fa * fa) * inv_r2

   eb = exp(-kb * (rcb * inv_r - 1.0_wp))
   fb = 1.0_wp / (1.0_wp + eb)
   dfb = -kb * rcb * (eb * fb * fb) * inv_r2

   countf = fa * fb
   countd = dfa * fb + fa * dfb

end subroutine ncoord_count_dcount

!> Fused evaluation of first and second derivatives of double-exponential counting function.
elemental subroutine ncoord_dcount_d2count(self, izp, jzp, r, countd, countd2)
   !> Coordination number container
   class(dexp_ncoord_type), intent(in) :: self
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

   real(wp) :: rc, rcb, inv_r, inv_r2, krc_a, krc_b
   real(wp) :: ea, fa, tmp_a, dfa, d2fa
   real(wp) :: eb, fb, tmp_b, dfb, d2fb

   rc = self%rcov(izp) + self%rcov(jzp)
   rcb = rc + r_shift
   inv_r = 1.0_wp / r
   inv_r2 = inv_r * inv_r

   krc_a = ka * rc
   ea = exp(-ka * (rc * inv_r - 1.0_wp))
   fa = 1.0_wp / (1.0_wp + ea)
   tmp_a = ea * fa * fa
   dfa = -krc_a * tmp_a * inv_r2
   d2fa = tmp_a * krc_a * inv_r2 * inv_r * ((1.0_wp - 2.0_wp * fa) * krc_a * inv_r + 2.0_wp)

   krc_b = kb * rcb
   eb = exp(-kb * (rcb * inv_r - 1.0_wp))
   fb = 1.0_wp / (1.0_wp + eb)
   tmp_b = eb * fb * fb
   dfb = -krc_b * tmp_b * inv_r2
   d2fb = tmp_b * krc_b * inv_r2 * inv_r * ((1.0_wp - 2.0_wp * fb) * krc_b * inv_r + 2.0_wp)

   countd = dfa * fb + fa * dfb
   countd2 = d2fa * fb + 2.0_wp * dfa * dfb + fa * d2fb

end subroutine ncoord_dcount_d2count

end module mctc_ncoord_dexp
