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

module test_write_vasp
   use mctc_env_testing, only : new_unittest, unittest_type, error_type, check
   use testsuite_structure, only : get_structure
   use mctc_io_write_vasp
   use mctc_io_read_vasp
   use mctc_io_structure
   use mctc_io_structure_info
   implicit none
   private

   public :: collect_write_vasp


contains


!> Collect all exported unit tests
subroutine collect_write_vasp(testsuite)

   !> Collection of tests
   type(unittest_type), allocatable, intent(out) :: testsuite(:)

   testsuite = [ &
      & new_unittest("valid1-poscar", test_valid1_poscar), &
      & new_unittest("valid2-poscar", test_valid2_poscar), &
      & new_unittest("valid3-poscar", test_valid3_poscar), &
      & new_unittest("valid4-poscar", test_valid4_poscar) &
      & ]

end subroutine collect_write_vasp


subroutine test_valid1_poscar(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit, nat, nid

   call get_structure(struc, "x01")
   nat = struc%nat
   nid = struc%nid

   open(status='scratch', newunit=unit)
   call write_vasp(struc, unit)
   rewind(unit)

   call read_vasp(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, struc%nat, nat, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, nid, "Number of species does not match")
   if (allocated(error)) return

end subroutine test_valid1_poscar


subroutine test_valid2_poscar(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   type(structure_info) :: info
   integer :: unit, nat, nid

   call get_structure(struc, "x02")
   nat = struc%nat
   nid = struc%nid
   info = structure_info(selective=.true., cartesian=.false.)
   struc%info = info

   open(status='scratch', newunit=unit)
   call write_vasp(struc, unit, "x02")
   rewind(unit)

   call read_vasp(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, struc%nat, nat, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, nid, "Number of species does not match")
   if (allocated(error)) return

end subroutine test_valid2_poscar


subroutine test_valid3_poscar(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   type(structure_info) :: info
   integer :: unit, nat, nid

   call get_structure(struc, "x03")
   nat = struc%nat
   nid = struc%nid
   info = structure_info(scale=0.5291772105638411)
   struc%info = info

   open(status='scratch', newunit=unit)
   call write_vasp(struc, unit)
   rewind(unit)

   call read_vasp(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, struc%nat, nat, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, nid, "Number of species does not match")
   if (allocated(error)) return

end subroutine test_valid3_poscar


subroutine test_valid4_poscar(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit, nat, nid

   call get_structure(struc, "mindless01")
   nat = struc%nat
   nid = struc%nid

   open(status='scratch', newunit=unit)
   call write_vasp(struc, unit)
   rewind(unit)

   call read_vasp(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, struc%nat, nat, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, nid, "Number of species does not match")
   if (allocated(error)) return

end subroutine test_valid4_poscar


end module test_write_vasp
