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

!> @dir mctc/ncoord
!> Contains the implementation for the coordination number evaluators.

!> @file mctc/ncoord.f90
!> Reexports the coordination number evaluation modules.

!> Proxy module to expose coordination number containers
module mctc_ncoord
   use mctc_env, only : error_type, fatal_error, wp
   use mctc_io, only : structure_type
   use mctc_ncoord_type, only : ncoord_type
   use mctc_ncoord_dexp, only : dexp_ncoord_type, new_dexp_ncoord
   use mctc_ncoord_exp, only : exp_ncoord_type, new_exp_ncoord
   use mctc_ncoord_erf, only : erf_ncoord_type, new_erf_ncoord
   use mctc_ncoord_erf_en, only : erf_en_ncoord_type, new_erf_en_ncoord
   use mctc_ncoord_erf_dftd4, only : erf_dftd4_ncoord_type, new_erf_dftd4_ncoord
   implicit none
   private

   public :: ncoord_type, new_ncoord, cn_count, get_cn_count_id, get_cn_count_string


   !> Possible coordination number counting functions
   type :: enum_cn_count

      !> Exponential counting function
      integer :: exp = 1

      !> Double-exponential counting function
      integer :: dexp = 2

      !> Error-function-based counting function
      integer :: erf = 3

      !> Electronegativity-weighted error-function-based counting function
      integer :: erf_en = 4

      !> DFT-D4 error-function-based counting function
      integer :: dftd4 = 5

   end type enum_cn_count

   !> Actual enumerator possible coordination numbers
   type(enum_cn_count), parameter :: cn_count = enum_cn_count()

contains

!> Create a new generic coordination number container
subroutine new_ncoord(self, mol, cn_count_type, error, kcn, cutoff, rcov, en, cut, norm_exp)
   !> Instance of the coordination number container
   class(ncoord_type), allocatable, intent(out) :: self
   !> Molecular structure data
   type(structure_type), intent(in) :: mol
   !> Coordination number type
   integer, intent(in) :: cn_count_type
   !> Error handling
   type(error_type), allocatable, intent(out) :: error
   !> Optional steepness of counting function
   real(wp), intent(in), optional :: kcn
   !> Optional real space cutoff
   real(wp), intent(in), optional :: cutoff
   !> Optional set of covalent radii to be used in CN
   real(wp), intent(in), optional :: rcov(:)
   !> Optional set of electronegativities to be used in CN
   real(wp), intent(in), optional :: en(:)
   !> Optional cutoff for the maximum coordination number
   real(wp), intent(in), optional :: cut
   !> Optional exponent of the distance normalization
   real(wp), intent(in), optional :: norm_exp

   select case(cn_count_type)
   case default 
      call fatal_error(error, "Unsupported option for coordination number")
      return 
   case(cn_count%exp)
      block
         type(exp_ncoord_type), allocatable :: tmp
         allocate(tmp)
         call new_exp_ncoord(tmp, mol, kcn=kcn, cutoff=cutoff, rcov=rcov, cut=cut)
         call move_alloc(tmp, self)
      end block
   case(cn_count%dexp)
      block
         type(dexp_ncoord_type), allocatable :: tmp
         allocate(tmp)
         call new_dexp_ncoord(tmp, mol, cutoff=cutoff, rcov=rcov, cut=cut)
         call move_alloc(tmp, self)
      end block
   case(cn_count%erf)
      block
         type(erf_ncoord_type), allocatable :: tmp
         allocate(tmp)
         call new_erf_ncoord(tmp, mol, kcn=kcn, cutoff=cutoff, &
            & rcov=rcov, cut=cut, norm_exp=norm_exp)
         call move_alloc(tmp, self)
      end block
   case(cn_count%erf_en)
      block
         type(erf_en_ncoord_type), allocatable :: tmp
         allocate(tmp)
         call new_erf_en_ncoord(tmp, mol, kcn=kcn, cutoff=cutoff, &
            & rcov=rcov, en=en, cut=cut, norm_exp=norm_exp)
         call move_alloc(tmp, self)
      end block
   case(cn_count%dftd4)
      block
         type(erf_dftd4_ncoord_type), allocatable :: tmp
         allocate(tmp)
         call new_erf_dftd4_ncoord(tmp, mol, kcn=kcn, cutoff=cutoff, &
            & rcov=rcov, en=en, cut=cut, norm_exp=norm_exp)
         call move_alloc(tmp, self)
      end block
   end select

end subroutine new_ncoord

!> Translate string into coordination number type
pure function get_cn_count_id(cn_count_name) result(cn_count_id)
   !> String for coordination number counting function
   character(len=*), intent(in) :: cn_count_name
   !> ID for coordination number counting function
   integer :: cn_count_id

   select case(cn_count_name)
   case default
      ! Indicate unknown CN option
      cn_count_id = -1 
   case("exp")
      cn_count_id = cn_count%exp
   case("dexp")
      cn_count_id = cn_count%dexp
   case("erf")
      cn_count_id = cn_count%erf
   case("erf_en")
      cn_count_id = cn_count%erf_en
   case("dftd4")
      cn_count_id = cn_count%dftd4
   end select

end function get_cn_count_id

!> Translate string into coordination number type
pure function get_cn_count_string(cn_count_id) result(cn_count_name)
   !> ID for coordination number counting function
   integer, intent(in) :: cn_count_id
   !> String for coordination number counting function
   character(len=:), allocatable :: cn_count_name

   select case(cn_count_id)
   case default
      ! Empty string to indicate unknown CN option
      cn_count_name = ""
   case(cn_count%exp)
      cn_count_name = "exp"
   case(cn_count%dexp)
      cn_count_name = "dexp"
   case(cn_count%erf)
      cn_count_name = "erf"
   case(cn_count%erf_en)
      cn_count_name = "erf_en"
   case(cn_count%dftd4)
      cn_count_name = "dftd4"
   end select

end function get_cn_count_string

end module mctc_ncoord
