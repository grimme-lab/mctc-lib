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

module test_read_gaussian
   use mctc_env_testing, only : new_unittest, unittest_type, error_type, check
   use mctc_io_read_gaussian
   use mctc_io_structure
   implicit none
   private

   public :: collect_read_gaussian


contains


!> Collect all exported unit tests
subroutine collect_read_gaussian(testsuite)

   !> Collection of tests
   type(unittest_type), allocatable, intent(out) :: testsuite(:)

   testsuite = [ &
      & new_unittest("valid1-ein", test_valid1_ein), &
      & new_unittest("invalid1-ein", test_invalid1_ein, should_fail=.true.), &
      & new_unittest("invalid2-ein", test_invalid2_ein, should_fail=.true.), &
      & new_unittest("invalid3-ein", test_invalid3_ein, should_fail=.true.), &
      & new_unittest("invalid4-ein", test_invalid4_ein, should_fail=.true.) &
      & ]

end subroutine collect_read_gaussian


subroutine test_valid1_ein(error)

   use mctc_env_accuracy, only : wp
   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "         4         1         0         1", &
      "         7      0.000000000000      0.000000000000     -0.114091591161      0.000000000000 ", &
      "         1     -1.817280998039      0.000000000000      0.528409372569      0.000000000000 ", &
      "         1      0.908640499019     -1.573811509290      0.528409372569      0.000000000000 ", &
      "         1      0.908640499019      1.573811509290      0.528409372569      0.000000000000 ", &
      " 1 2 1.000 3 1.000 4 1.000", &
      " 2 1 1.000", &
      " 3 1 1.000", &
      " 4 1 1.000"
   rewind(unit)

   call read_gaussian_external(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, struc%nat, 4, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, 2, "Number of species does not match")
   if (allocated(error)) return
   
   call check(error, struc%xyz(1,1), 0.000000000000_wp, thr=1.0e-10_wp, message="Coordinates do not match")
   if (allocated(error)) return
  
   call check(error, struc%xyz(3,2), 0.528409372569_wp, thr=1.0e-10_wp,message="Coordinates do not match")
   if (allocated(error)) return
   
   call check(error, struc%xyz(2,3), -1.573811509290_wp, thr=1.0e-10_wp, message="Coordinates do not match")
  if (allocated(error)) return

end subroutine test_valid1_ein


subroutine test_invalid1_ein(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "         4         1      zero       one", &
      "         7      0.000000000000      0.000000000000     -0.114091591161      0.000000000000 ", &
      "         1     -1.817280998039      0.000000000000      0.528409372569      0.000000000000 ", &
      "         1      0.908640499019     -1.573811509290      0.528409372569      0.000000000000 ", &
      "         1      0.908640499019      1.573811509290      0.528409372569      0.000000000000 ", &
      " 1 2 1.000 3 1.000 4 1.000", &
      " 2 1 1.000", &
      " 3 1 1.000", &
      " 4 1 1.000"
   rewind(unit)

   call read_gaussian_external(struc, unit, error)
   close(unit)

end subroutine test_invalid1_ein


subroutine test_invalid2_ein(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "        -4         1         0         1", &
      "         7      0.000000000000      0.000000000000     -0.114091591161      0.000000000000 ", &
      "         1     -1.817280998039      0.000000000000      0.528409372569      0.000000000000 ", &
      "         1      0.908640499019     -1.573811509290      0.528409372569      0.000000000000 ", &
      "         1      0.908640499019      1.573811509290      0.528409372569      0.000000000000 ", &
      " 1 2 1.000 3 1.000 4 1.000", &
      " 2 1 1.000", &
      " 3 1 1.000", &
      " 4 1 1.000"
   rewind(unit)

   call read_gaussian_external(struc, unit, error)
   close(unit)

end subroutine test_invalid2_ein


subroutine test_invalid3_ein(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "         4         1         0         1", &
      "         7      0.000000000000      0.000000000000     -0.114091591161      0.000000000000 ", &
      "         1     -1.817280998039      0.000000000000      0.528409372569      0.000000000000 ", &
      "         1      0.908640499019     -1.573811509290      0.528409372569      0.000000000000 ", &
      "         1      abcd.efgh-jklm      1.573811509290      0.528409372569      0.000000000000 ", &
      " 1 2 1.000 3 1.000 4 1.000", &
      " 2 1 1.000", &
      " 3 1 1.000", &
      " 4 1 1.000"
   rewind(unit)

   call read_gaussian_external(struc, unit, error)
   close(unit)

end subroutine test_invalid3_ein


subroutine test_invalid4_ein(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "         4         1         0         1", &
      "         7      0.000000000000      0.000000000000     -0.114091591161      0.000000000000 ", &
      "         1     -1.817280998039      0.000000000000      0.528409372569      0.000000000000 ", &
      "         1      0.908640499019     -1.573811509290      0.528409372569      0.000000000000 ", &
      "        -1      0.908640499019      1.573811509290      0.528409372569      0.000000000000 ", &
      " 1 2 1.000 3 1.000 4 1.000", &
      " 2 1 1.000", &
      " 3 1 1.000", &
      " 4 1 1.000"
   rewind(unit)

   call read_gaussian_external(struc, unit, error)
   close(unit)

end subroutine test_invalid4_ein


end module test_read_gaussian
