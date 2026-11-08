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

module test_write_qcschema
   use mctc_env_accuracy, only : wp
   use mctc_env_testing, only : new_unittest, unittest_type, error_type, check
   use mctc_io_math, only : matdet_3x3
   use mctc_io_read_qcschema, only : read_qcschema
   use mctc_io_structure, only : structure_type
   use mctc_io_write_qcschema, only : write_qcschema
   use mctc_version, only : get_mctc_feature
   use testsuite_structure, only : get_structure
   implicit none
   private

   public :: collect_write_qcschema


contains


!> Collect all exported unit tests
subroutine collect_write_qcschema(testsuite)

   !> Collection of tests
   type(unittest_type), allocatable, intent(out) :: testsuite(:)

   logical :: with_json

   with_json = get_mctc_feature("json")

   testsuite = [ &
      & new_unittest("valid1-qcschema", test_valid1_qcschema, should_fail=.not.with_json), &
      & new_unittest("valid2-qcschema", test_valid2_qcschema, should_fail=.not.with_json), &
      & new_unittest("valid3-qcschema", test_valid3_qcschema, should_fail=.not.with_json) &
      & ]

end subroutine collect_write_qcschema


subroutine test_valid1_qcschema(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit, nat, nid

   call get_structure(struc, "mindless01")
   struc%comment = "mindless"
   nat = struc%nat
   nid = struc%nid

   open(status="scratch", newunit=unit)
   call write_qcschema(struc, unit)
   rewind(unit)

   call read_qcschema(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, struc%comment, "mindless", "Comment no preserved")
   if (allocated(error)) return
   call check(error, struc%nat, nat, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, nid, "Number of species does not match")
   if (allocated(error)) return

end subroutine test_valid1_qcschema


subroutine test_valid2_qcschema(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit, nat, nid

   call get_structure(struc, "x01")
   nat = struc%nat
   nid = struc%nid

   open(status="scratch", newunit=unit)
   call write_qcschema(struc, unit)
   rewind(unit)

   call read_qcschema(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, struc%nat, nat, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, nid, "Number of species does not match")
   if (allocated(error)) return

end subroutine test_valid2_qcschema


subroutine test_valid3_qcschema(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit
   real(wp) :: chirality

   call get_structure(struc, "chiral")
   chirality = matdet_3x3(struc%xyz(:, 2:4) - spread(struc%xyz(:, 1), 2, 3))

   open(status="scratch", newunit=unit)
   call write_qcschema(struc, unit)
   rewind(unit)

   call read_qcschema(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, matdet_3x3(struc%xyz(:, 2:4) - spread(struc%xyz(:, 1), 2, 3)), &
      & chirality, thr=1.0e-8_wp, message="Handedness of the structure is not preserved")
   if (allocated(error)) return

end subroutine test_valid3_qcschema


end module test_write_qcschema
