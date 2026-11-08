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

module test_write_cif
   use mctc_env_accuracy, only : wp
   use mctc_env_testing, only : new_unittest, unittest_type, error_type, check
   use mctc_io_math, only : matdet_3x3, matinv_3x3
   use mctc_io_read_cif, only : read_cif
   use mctc_io_structure, only : structure_type
   use mctc_io_write_cif, only : write_cif
   use testsuite_structure, only : get_structure
   implicit none
   private

   public :: collect_write_cif


contains


!> Collect all exported unit tests
subroutine collect_write_cif(testsuite)

   !> Collection of tests
   type(unittest_type), allocatable, intent(out) :: testsuite(:)

   testsuite = [ &
      & new_unittest("valid1-cif", test_valid1_cif), &
      & new_unittest("valid2-cif", test_valid2_cif), &
      & new_unittest("valid3-cif", test_valid3_cif), &
      & new_unittest("valid4-cif", test_valid4_cif), &
      & new_unittest("invalid1-cif", test_invalid1_cif, should_fail=.true.), &
      & new_unittest("invalid2-cif", test_invalid2_cif, should_fail=.true.) &
      & ]

end subroutine collect_write_cif


subroutine test_valid1_cif(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit, nat
   integer, allocatable :: num(:)
   real(wp), allocatable :: gram(:, :), frac(:, :)

   call get_structure(struc, "x01")
   nat = struc%nat
   num = struc%num(struc%id)
   gram = matmul(transpose(struc%lattice), struc%lattice)
   frac = matmul(matinv_3x3(struc%lattice), struc%xyz)

   open(status="scratch", newunit=unit)
   call write_cif(struc, unit, error)
   if (allocated(error)) then
      close(unit)
      return
   end if
   rewind(unit)

   call read_cif(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, struc%nat, nat, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, all(struc%num(struc%id) == num), "Atomic numbers do not match")
   if (allocated(error)) return
   call check(error, maxval(abs(matmul(transpose(struc%lattice), struc%lattice) - gram)), &
      & 0.0_wp, thr=1.0e-8_wp, message="Cell metric does not match")
   if (allocated(error)) return
   call check(error, maxval(abs(matmul(matinv_3x3(struc%lattice), struc%xyz) - frac)), &
      & 0.0_wp, thr=1.0e-10_wp, message="Fractional coordinates do not match")
   if (allocated(error)) return
   call check(error, struc%comment, "structure")
   if (allocated(error)) return

end subroutine test_valid1_cif


subroutine test_valid2_cif(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit, nat

   call get_structure(struc, "x02")
   struc%comment = "x02 test"
   nat = struc%nat

   open(status="scratch", newunit=unit)
   call write_cif(struc, unit, error)
   if (allocated(error)) then
      close(unit)
      return
   end if
   rewind(unit)

   call read_cif(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, struc%nat, nat, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%comment, "x02_test")
   if (allocated(error)) return

end subroutine test_valid2_cif


subroutine test_valid3_cif(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit
   real(wp) :: chirality

   call get_structure(struc, "chiral")
   chirality = matdet_3x3(struc%xyz(:, 2:4) - spread(struc%xyz(:, 1), 2, 3))

   open(status="scratch", newunit=unit)
   call write_cif(struc, unit, error)
   if (allocated(error)) then
      close(unit)
      return
   end if
   rewind(unit)

   call read_cif(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, matdet_3x3(struc%xyz(:, 2:4) - spread(struc%xyz(:, 1), 2, 3)), &
      & chirality, thr=1.0e-8_wp, message="Handedness of the structure is not preserved")
   if (allocated(error)) return

end subroutine test_valid3_cif


subroutine test_valid4_cif(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit
   real(wp), allocatable :: frac(:, :)

   call get_structure(struc, "nacl")
   ! atoms far outside the unit cell, like in an unwrapped trajectory
   struc%xyz(:, 1) = struc%xyz(:, 1) + matmul(struc%lattice, [-150.0_wp, 0.0_wp, 0.0_wp])
   struc%xyz(:, 2) = struc%xyz(:, 2) + matmul(struc%lattice, [0.0_wp, 1234.0_wp, -99.0_wp])
   frac = matmul(matinv_3x3(struc%lattice), struc%xyz)

   open(status="scratch", newunit=unit)
   call write_cif(struc, unit, error)
   if (allocated(error)) then
      close(unit)
      return
   end if
   rewind(unit)

   call read_cif(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, maxval(abs(matmul(matinv_3x3(struc%lattice), struc%xyz) - frac)), 0.0_wp, &
      & thr=1.0e-10_wp, message="Fractional coordinates do not match")
   if (allocated(error)) return

end subroutine test_valid4_cif



subroutine test_invalid1_cif(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   call get_structure(struc, "mindless01")

   open(status="scratch", newunit=unit)
   call write_cif(struc, unit, error)
   close(unit)

end subroutine test_invalid1_cif


subroutine test_invalid2_cif(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   call get_structure(struc, "nacl")
   struc%lattice(:, 3) = struc%lattice(:, 1) + struc%lattice(:, 2)

   open(status="scratch", newunit=unit)
   call write_cif(struc, unit, error)
   close(unit)

end subroutine test_invalid2_cif


end module test_write_cif
