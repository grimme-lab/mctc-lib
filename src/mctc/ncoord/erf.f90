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

   public :: new_erf_ncoord, init_erf_tables

   !> Coordination number evaluator
   type, public, extends(ncoord_type) :: erf_ncoord_type
      !> Covalent radii
      real(wp), allocatable :: rcov(:)
      !> Exponent of the distance normalization
      real(wp)  :: norm_exp
      !> Precomputed pairwise covalent radii sum (nid, nid)
      real(wp), allocatable :: rc_mat(:, :)
      !> Precomputed inverse normalized pairwise covalent radii (nid, nid)
      real(wp), allocatable :: inv_rcn_mat(:, :)
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

      if (present(norm_exp)) then
         self%norm_exp = norm_exp
      else
         self%norm_exp = default_norm_exp
      end if

      call init_erf_tables(self)

   end subroutine new_erf_ncoord


   !> Precompute pairwise covalent radii sum and normalized inverse radii tables.
   !> Must be called again after changing rcov or norm_exp of a constructed instance.
   pure subroutine init_erf_tables(self)
      !> Coordination number container
      class(erf_ncoord_type), intent(inout) :: self

      !> Inverse radius used for non-positive radii sums, keeps the tables finite
      real(wp), parameter :: inv_rc_zero = 1.0e30_wp

      integer :: nid, izp, jzp
      real(wp) :: rc

      nid = size(self%rcov)
      if (allocated(self%rc_mat)) deallocate(self%rc_mat)
      if (allocated(self%inv_rcn_mat)) deallocate(self%inv_rcn_mat)
      allocate(self%rc_mat(nid, nid), self%inv_rcn_mat(nid, nid))

      do jzp = 1, nid
         do izp = 1, nid
            rc = self%rcov(izp) + self%rcov(jzp)
            self%rc_mat(izp, jzp) = rc
            if (rc > 0.0_wp) then
               self%inv_rcn_mat(izp, jzp) = 1.0_wp / rc**self%norm_exp
            else
               self%inv_rcn_mat(izp, jzp) = inv_rc_zero
            end if
         end do
      end do

   end subroutine init_erf_tables


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
