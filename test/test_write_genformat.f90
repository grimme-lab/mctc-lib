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

module test_write_genformat
   use mctc_env_testing, only : new_unittest, unittest_type, error_type, check
   use testsuite_structure, only : get_structure
   use mctc_io_write_genformat
   use mctc_io_read_genformat
   use mctc_io_structure
   use mctc_io_structure_info
   implicit none
   private

   public :: collect_write_genformat


contains


!> Collect all exported unit tests
subroutine collect_write_genformat(testsuite)

   !> Collection of tests
   type(unittest_type), allocatable, intent(out) :: testsuite(:)

   testsuite = [ &
      & new_unittest("valid1-gen", test_valid1_gen), &
      & new_unittest("valid2-gen", test_valid2_gen), &
      & new_unittest("valid3-gen", test_valid3_gen) &
      & ]

end subroutine collect_write_genformat


subroutine test_valid1_gen(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit, nat, nid

   call get_structure(struc, "mindless01")
   nat = struc%nat
   nid = struc%nid

   open(status='scratch', newunit=unit)
   call write_genformat(struc, unit)
   rewind(unit)

   call read_genformat(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, struc%nat, nat, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, nid, "Number of species does not match")
   if (allocated(error)) return

end subroutine test_valid1_gen


subroutine test_valid2_gen(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit, nat, nid

   call get_structure(struc, "x01")
   nat = struc%nat
   nid = struc%nid

   open(status='scratch', newunit=unit)
   call write_genformat(struc, unit)
   rewind(unit)

   call read_genformat(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, struc%nat, nat, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, nid, "Number of species does not match")
   if (allocated(error)) return

end subroutine test_valid2_gen


subroutine test_valid3_gen(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   type(structure_info) :: info
   integer :: unit, nat, nid

   call get_structure(struc, "x02")
   nat = struc%nat
   nid = struc%nid
   info = structure_info(cartesian=.false.)
   struc%info = info

   open(status='scratch', newunit=unit)
   call write_genformat(struc, unit)
   rewind(unit)

   call read_genformat(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, struc%nat, nat, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, nid, "Number of species does not match")
   if (allocated(error)) return

end subroutine test_valid3_gen


end module test_write_genformat
