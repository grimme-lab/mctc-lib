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

module test_write_pdb
   use mctc_env_accuracy, only : wp
   use mctc_env_testing, only : new_unittest, unittest_type, error_type, check
   use testsuite_structure, only : get_structure
   use mctc_io_write_pdb
   use mctc_io_read_pdb
   use mctc_io_structure
   implicit none
   private

   public :: collect_write_pdb


contains


!> Collect all exported unit tests
subroutine collect_write_pdb(testsuite)

   !> Collection of tests
   type(unittest_type), allocatable, intent(out) :: testsuite(:)

   testsuite = [ &
      & new_unittest("valid1-pdb", test_valid1_pdb), &
      & new_unittest("valid2-pdb", test_valid2_pdb), &
      & new_unittest("invalid1-pdb", test_invalid1_pdb, should_fail=.true.), &
      & new_unittest("invalid2-pdb", test_invalid2_pdb, should_fail=.true.) &
      & ]

end subroutine collect_write_pdb


subroutine test_valid1_pdb(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit, nat, nid

   call get_structure(struc, "mindless01")
   nat = struc%nat
   nid = struc%nid

   open(status='scratch', newunit=unit)
   call write_pdb(struc, unit)
   rewind(unit)

   call read_pdb(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, struc%nat, nat, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, nid, "Number of species does not match")
   if (allocated(error)) return

end subroutine test_valid1_pdb


subroutine test_valid2_pdb(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit
   real(wp) :: xyz(3, 3)
   character(len=120) :: line

   xyz(:, 1) = [0.0_wp, 0.0_wp, 0.0_wp]
   xyz(:, 2) = [1.0_wp, 0.0_wp, 0.0_wp]
   xyz(:, 3) = [2.0_wp, 0.0_wp, 0.0_wp]

   call new(struc, [character(len=2) :: "N ", "C ", "C "], xyz)
   allocate(struc%pdb(struc%nat))

   struc%pdb(:)%residue = "SER"
   struc%pdb(:)%chains = "A"
   struc%pdb(:)%residue_number = 1

   struc%pdb(1)%name = " N  "
   struc%pdb(1)%occupancy = 1.00_wp

   struc%pdb(2)%name = " CA "
   struc%pdb(2)%loc = "A"
   struc%pdb(2)%occupancy = 0.60_wp

   struc%pdb(3)%name = " CA "
   struc%pdb(3)%loc = "B"
   struc%pdb(3)%occupancy = 0.40_wp

   open(status='scratch', newunit=unit)
   call write_pdb(struc, unit)
   rewind(unit)

   read(unit, '(a)') line
   read(unit, '(a)') line
   call check(error, line(17:17), "A", "Alternative location indicator was not written")
   if (allocated(error)) return
   call check(error, line(55:60), "  0.60", "Partial occupancy for conformer A was not written")
   if (allocated(error)) return

   read(unit, '(a)') line
   call check(error, line(17:17), "B", "Alternative location indicator was not written")
   if (allocated(error)) return
   call check(error, line(55:60), "  0.40", "Partial occupancy for conformer B was not written")
   if (allocated(error)) return

   close(unit)

end subroutine test_valid2_pdb


subroutine test_invalid1_pdb(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit
   real(wp) :: xyz(3, 1)

   xyz(:, 1) = [0.0_wp, 0.0_wp, 0.0_wp]

   call new(struc, [character(len=2) :: "C "], xyz)
   allocate(struc%pdb(struc%nat))

   struc%pdb(1)%name = " C  "
   struc%pdb(1)%residue = "SER"
   struc%pdb(1)%chains = "A"
   struc%pdb(1)%residue_number = 1
   struc%pdb(1)%occupancy = 1.50_wp

   open(status='scratch', newunit=unit)
   call write_pdb(struc, unit, error=error)
   close(unit)

end subroutine test_invalid1_pdb


subroutine test_invalid2_pdb(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit
   real(wp) :: xyz(3, 1)

   xyz(:, 1) = [0.0_wp, 0.0_wp, 0.0_wp]

   call new(struc, [character(len=2) :: "N "], xyz)
   allocate(struc%pdb(struc%nat))

   struc%pdb(1)%residue = "SER"
   struc%pdb(1)%chains = "A"
   struc%pdb(1)%residue_number = 1
   struc%pdb(1)%occupancy = -0.10_wp

   open(status='scratch', newunit=unit)
   call write_pdb(struc, unit, error=error)
   close(unit)

end subroutine test_invalid2_pdb


end module test_write_pdb
