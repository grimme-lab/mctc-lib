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

module test_write_xyz
   use mctc_env_accuracy, only : wp
   use mctc_env_testing, only : new_unittest, unittest_type, error_type, check
   use mctc_io_read_xyz, only : read_xyz
   use mctc_io_structure, only : structure_type, new
   use mctc_io_write_xyz, only : write_xyz
   use testsuite_structure, only : get_structure
   implicit none
   private

   public :: collect_write_xyz


contains


!> Collect all exported unit tests
subroutine collect_write_xyz(testsuite)

   !> Collection of tests
   type(unittest_type), allocatable, intent(out) :: testsuite(:)

   testsuite = [ &
      & new_unittest("valid1-xyz", test_valid1_xyz), &
      & new_unittest("valid2-periodic-xyz", test_valid2_periodic_xyz) &
      & ]

end subroutine collect_write_xyz


subroutine test_valid1_xyz(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit, nat, nid

   call get_structure(struc, "mindless01")
   nat = struc%nat
   nid = struc%nid

   open(status="scratch", newunit=unit)
   call write_xyz(struc, unit)
   rewind(unit)

   call read_xyz(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, struc%nat, nat, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, nid, "Number of species does not match")
   if (allocated(error)) return

end subroutine test_valid1_xyz


subroutine test_valid2_periodic_xyz(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit
   character(len=1), parameter :: sym(2) = ["H", "O"]
   real(wp), parameter :: xyz(3, 2) = reshape([ &
      & 1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, 6.0_wp], [3, 2])
   real(wp), parameter :: lattice(3, 3) = reshape([ &
      & 10.0_wp, 0.0_wp, 0.0_wp, &
      & 1.0_wp, 11.0_wp, 0.0_wp, &
      & 2.0_wp, 3.0_wp, 12.0_wp], [3, 3])
   logical, parameter :: periodic(3) = [.true., .false., .true.]

   call new(struc, sym, xyz, lattice=lattice, periodic=periodic)
   struc%comment = 'roundtrip "test"'

   open(status="scratch", newunit=unit)
   call write_xyz(struc, unit)
   rewind(unit)

   call read_xyz(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, count(struc%periodic), 2, "Periodicity does not match")
   if (allocated(error)) return
   call check(error, maxval(abs(struc%lattice-lattice)), 0.0_wp, &
      & "Lattice does not match", thr=1.0e-12_wp)
   if (allocated(error)) return
   call check(error, maxval(abs(struc%xyz-xyz)), 0.0_wp, &
      & "Coordinates do not match", thr=1.0e-12_wp)
   if (allocated(error)) return
   call check(error, struc%comment, 'roundtrip "test"')

end subroutine test_valid2_periodic_xyz


end module test_write_xyz
