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

module test_write_cjson
   use mctc_env_testing, only : new_unittest, unittest_type, error_type, check
   use testsuite_structure, only : get_structure
   use mctc_io_write_cjson
   use mctc_io_read_cjson
   use mctc_io_structure
   use mctc_version, only : get_mctc_feature
   implicit none
   private

   public :: collect_write_cjson


contains


!> Collect all exported unit tests
subroutine collect_write_cjson(testsuite)

   !> Collection of tests
   type(unittest_type), allocatable, intent(out) :: testsuite(:)

   logical :: with_json

   with_json = get_mctc_feature("json")

   testsuite = [ &
      & new_unittest("valid1-cjson", test_valid1_cjson, should_fail=.not.with_json), &
      & new_unittest("valid2-cjson", test_valid2_cjson, should_fail=.not.with_json) &
      & ]

end subroutine collect_write_cjson


subroutine test_valid1_cjson(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit, nat, nid

   call get_structure(struc, "mindless01")
   struc%comment = "mindless"
   nat = struc%nat
   nid = struc%nid

   open(status='scratch', newunit=unit)
   call write_cjson(struc, unit)
   rewind(unit)

   call read_cjson(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, struc%comment, "mindless", "Comment no preserved")
   if (allocated(error)) return
   call check(error, struc%nat, nat, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, nid, "Number of species does not match")
   if (allocated(error)) return

end subroutine test_valid1_cjson


subroutine test_valid2_cjson(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit, nat, nid

   call get_structure(struc, "x01")
   nat = struc%nat
   nid = struc%nid

   open(status='scratch', newunit=unit)
   call write_cjson(struc, unit)
   rewind(unit)

   call read_cjson(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, struc%nat, nat, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, nid, "Number of species does not match")
   if (allocated(error)) return

end subroutine test_valid2_cjson


end module test_write_cjson
