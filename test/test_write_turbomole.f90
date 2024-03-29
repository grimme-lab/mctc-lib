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

module test_write_turbomole
   use mctc_env_testing, only : new_unittest, unittest_type, error_type, check
   use testsuite_structure, only : get_structure
   use mctc_io_write_turbomole
   use mctc_io_read_turbomole
   use mctc_io_structure
   use mctc_io_convert, only : autoaa
   implicit none
   private

   public :: collect_write_turbomole


contains


!> Collect all exported unit tests
subroutine collect_write_turbomole(testsuite)

   !> Collection of tests
   type(unittest_type), allocatable, intent(out) :: testsuite(:)

   testsuite = [ &
      & new_unittest("valid1-coord", test_valid1_coord), &
      & new_unittest("valid2-coord", test_valid2_coord), &
      & new_unittest("valid1-coord-angs", test_valid1_coord_angs), &
      & new_unittest("valid2-coord-angs", test_valid2_coord_angs) &
      & ]

end subroutine collect_write_turbomole


subroutine test_valid1_coord(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit, nat, nid

   call get_structure(struc, "mindless01")
   nat = struc%nat
   nid = struc%nid

   open(status='scratch', newunit=unit)
   call write_coord(struc, unit)
   rewind(unit)

   call read_coord(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, struc%nat, nat, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, nid, "Number of species does not match")
   if (allocated(error)) return

end subroutine test_valid1_coord


subroutine test_valid2_coord(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit, nat, nid

   call get_structure(struc, "x01")
   nat = struc%nat
   nid = struc%nid

   open(status='scratch', newunit=unit)
   call write_coord(struc, unit)
   rewind(unit)

   call read_coord(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, struc%nat, nat, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, nid, "Number of species does not match")
   if (allocated(error)) return

end subroutine test_valid2_coord

subroutine test_valid1_coord_angs(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit, nat, nid

   call get_structure(struc, "mindless01")
   nat = struc%nat
   nid = struc%nid

   struc%xyz = struc%xyz * autoaa
   struc%info%angs_coord = .true.

   open(status='scratch', newunit=unit)
   call write_coord(struc, unit)
   rewind(unit)

   call read_coord(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, struc%nat, nat, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, nid, "Number of species does not match")
   if (allocated(error)) return
   call check(error, struc%info%angs_coord, .true., "Coordinates are not written in angstrom.")
   if (allocated(error)) return

end subroutine test_valid1_coord_angs

subroutine test_valid2_coord_angs(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit, nat, nid

   call get_structure(struc, "x01")
   nat = struc%nat
   nid = struc%nid

   struc%xyz = struc%xyz * autoaa
   struc%lattice = struc%lattice * autoaa
   struc%info%angs_coord = .true.

   open(status='scratch', newunit=unit)
   call write_coord(struc, unit)
   rewind(unit)

   call read_coord(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, struc%nat, nat, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, nid, "Number of species does not match")
   if (allocated(error)) return
   call check(error, struc%info%angs_coord, .true., "Coordinates are not written in angstrom.")
   if (allocated(error)) return

end subroutine test_valid2_coord_angs

end module test_write_turbomole
