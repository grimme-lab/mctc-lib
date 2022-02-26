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

module test_write_qchem
   use mctc_env_testing, only : new_unittest, unittest_type, error_type, check
   use testsuite_structure, only : get_structure
   use mctc_io_write_qchem
   use mctc_io_read_qchem
   use mctc_io_structure
   implicit none
   private

   public :: collect_write_qchem


contains


!> Collect all exported unit tests
subroutine collect_write_qchem(testsuite)

   !> Collection of tests
   type(unittest_type), allocatable, intent(out) :: testsuite(:)

   testsuite = [ &
      & new_unittest("valid1-qchem", test_valid1_qchem), &
      & new_unittest("valid2-qchem", test_valid2_qchem) &
      & ]

end subroutine collect_write_qchem


subroutine test_valid1_qchem(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit, nat, nid

   call get_structure(struc, "mindless01")
   nat = struc%nat
   nid = struc%nid

   open(status='scratch', newunit=unit)
   call write_qchem(struc, unit)
   rewind(unit)

   call read_qchem(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, struc%nat, nat, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, nid, "Number of species does not match")
   if (allocated(error)) return

end subroutine test_valid1_qchem


subroutine test_valid2_qchem(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit, nat, nid

   call get_structure(struc, "mindless02")
   nat = struc%nat
   nid = struc%nid

   open(status='scratch', newunit=unit)
   call write_qchem(struc, unit)
   rewind(unit)

   call read_qchem(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, struc%nat, nat, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, nid, "Number of species does not match")
   if (allocated(error)) return

end subroutine test_valid2_qchem


end module test_write_qchem
