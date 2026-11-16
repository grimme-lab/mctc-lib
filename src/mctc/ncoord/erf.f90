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

!> Coordination number implementation with single error function
module mctc_ncoord_erf
   use mctc_data_covrad, only : get_covalent_rad
   use mctc_env, only : wp
   use mctc_io, only : structure_type
   use mctc_io_constants, only : pi
   use mctc_ncoord_type, only : ncoord_type
   implicit none
   private

   public :: new_erf_ncoord, init_erf_ncoord

   !> Coordination number evaluator
   type, public, extends(ncoord_type) :: erf_ncoord_type
      ! The pair tables are derived from rcov and norm_exp. All four are private
      ! so they cannot be changed from outside without rebuilding the tables,
      ! use init_erf_ncoord to set them.
      !> Covalent radii, set together with the pair tables by init_erf_ncoord
      real(wp), allocatable, private :: rcov(:)
      !> Exponent of the distance normalization, set by init_erf_ncoord
      real(wp), private :: norm_exp
      !> Precomputed pairwise covalent radii sum (nid, nid)
      real(wp), allocatable, private :: rc_mat(:, :)
      !> Precomputed inverse normalized pairwise covalent radii (nid, nid)
      real(wp), allocatable, private :: inv_rcn_mat(:, :)
   contains
      !> Evaluates the error counting function
      procedure :: ncoord_count
      !> Evaluates the derivative of the error counting function
      procedure :: ncoord_dcount
      !> Evaluates the second derivative of the error counting function
      procedure :: ncoord_d2count
   end type erf_ncoord_type

   !> Steepness of counting function
   real(wp), parameter :: default_kcn = 3.15_wp
   !> Exponent of distance normalization
   real(wp), parameter :: default_norm_exp = 1.0_wp
   !> Real-space cutoff for coordination number
   real(wp), parameter :: default_cutoff = 25.0_wp

contains


   subroutine new_erf_ncoord(self, mol, kcn, cutoff, rcov, cut, norm_exp)
      !> Coordination number container
      type(erf_ncoord_type), intent(out) :: self
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

      self%directed_factor = 1.0_wp

      if (present(cut)) then
         self%cut = cut
      else
         ! Negative value deactivates the cutoff
         self%cut = -1.0_wp
      end if

      if (present(norm_exp)) then
         call init_erf_ncoord(self, mol, norm_exp, rcov)
      else
         call init_erf_ncoord(self, mol, default_norm_exp, rcov)
      end if

   end subroutine new_erf_ncoord


   !> Set the covalent radii and the distance normalization of an erf-type
   !> coordination number and build the pair tables derived from them.
   !> The radii and exponent are only accessible through this routine,
   !> which keeps them consistent with the tables.
   subroutine init_erf_ncoord(self, mol, norm_exp, rcov)

      !> Coordination number container
      class(erf_ncoord_type), intent(inout) :: self

      !> Molecular structure data
      type(structure_type), intent(in) :: mol

      !> Exponent of the distance normalization
      real(wp), intent(in) :: norm_exp

      !> Covalent radii, absent selects the default covalent radii
      real(wp), intent(in), optional :: rcov(:)

      ! Avoid the 0/0 in the derivative expressions for a non-positive radius
      ! sum, the large finite inverse makes the exponentially damped
      ! derivatives evaluate to zero
      real(wp), parameter :: inv_rc_zero = 1.0e30_wp

      integer :: izp, jzp
      real(wp) :: rc

      if (allocated(self%rcov)) deallocate(self%rcov)
      allocate(self%rcov(mol%nid))
      if (present(rcov)) then
         self%rcov(:) = rcov
      else
         self%rcov(:) = get_covalent_rad(mol%num)
      end if

      self%norm_exp = norm_exp

      if (allocated(self%rc_mat)) deallocate(self%rc_mat)
      if (allocated(self%inv_rcn_mat)) deallocate(self%inv_rcn_mat)
      allocate(self%rc_mat(mol%nid, mol%nid), self%inv_rcn_mat(mol%nid, mol%nid))

      do jzp = 1, mol%nid
         do izp = 1, mol%nid
            rc = self%rcov(izp) + self%rcov(jzp)
            self%rc_mat(izp, jzp) = rc
            if (rc > 0.0_wp) then
               self%inv_rcn_mat(izp, jzp) = 1.0_wp / rc**self%norm_exp
            else
               self%inv_rcn_mat(izp, jzp) = inv_rc_zero
            end if
         end do
      end do

   end subroutine init_erf_ncoord


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

      real(wp) :: rc, inv_rcn, count

      rc = self%rc_mat(izp, jzp)
      inv_rcn = self%inv_rcn_mat(izp, jzp)

      count = 0.5_wp * (1.0_wp + erf(-self%kcn * (r - rc) * inv_rcn))

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

      real(wp), parameter :: inv_sqrtpi = 1.0_wp / sqrt(pi)
      real(wp) :: rc, inv_rcn, alpha, exponent, expterm, count

      rc = self%rc_mat(izp, jzp)
      inv_rcn = self%inv_rcn_mat(izp, jzp)

      alpha = self%kcn * inv_rcn
      exponent = alpha * (r - rc)
      expterm = exp(-exponent * exponent)
      count = -inv_sqrtpi * alpha * expterm

   end function ncoord_dcount


   !> Second derivative of the error counting function w.r.t. the distance.
   elemental function ncoord_d2count(self, izp, jzp, r) result(count)
      !> Coordination number container
      class(erf_ncoord_type), intent(in) :: self
      !> Atom i index
      integer, intent(in) :: izp
      !> Atom j index
      integer, intent(in) :: jzp
      !> Current distance.
      real(wp), intent(in) :: r

      real(wp), parameter :: two_inv_sqrtpi = 2.0_wp / sqrt(pi)
      real(wp) :: rc, inv_rcn, alpha, exponent, expterm, count

      rc = self%rc_mat(izp, jzp)
      inv_rcn = self%inv_rcn_mat(izp, jzp)

      alpha = self%kcn * inv_rcn
      exponent = alpha * (r - rc)
      expterm = exp(-exponent * exponent)
      count = two_inv_sqrtpi * alpha * alpha * exponent * expterm

   end function ncoord_d2count

end module mctc_ncoord_erf
