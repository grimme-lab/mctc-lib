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

module test_write_ctfile
   use mctc_env_accuracy, only : wp
   use mctc_env_testing, only : new_unittest, unittest_type, error_type, check
   use testsuite_structure, only : get_structure
   use mctc_io_write_ctfile
   use mctc_io_read_ctfile
   use mctc_io_structure
   implicit none
   private

   public :: collect_write_ctfile


contains


!> Collect all exported unit tests
subroutine collect_write_ctfile(testsuite)

   !> Collection of tests
   type(unittest_type), allocatable, intent(out) :: testsuite(:)

   testsuite = [ &
      & new_unittest("valid1-mol", test_valid1_mol), &
      & new_unittest("valid1-sdf", test_valid1_sdf), &
      & new_unittest("v3k-large-mol", test_v3k_large_mol), &
      & new_unittest("v3k-large-sdf", test_v3k_large_sdf), &
      & new_unittest("v3k-with-bonds", test_v3k_with_bonds) &
      & ]

end subroutine collect_write_ctfile


subroutine test_valid1_mol(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit, nat, nid

   call get_structure(struc, "mindless01")
   nat = struc%nat
   nid = struc%nid

   open(status='scratch', newunit=unit)
   call write_molfile(struc, unit)
   rewind(unit)

   call read_molfile(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, struc%nat, nat, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, nid, "Number of species does not match")
   if (allocated(error)) return

end subroutine test_valid1_mol


subroutine test_valid1_sdf(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit, nat, nid

   call get_structure(struc, "mindless01")
   nat = struc%nat
   nid = struc%nid

   open(status='scratch', newunit=unit)
   call write_sdf(struc, unit)
   rewind(unit)

   call read_sdf(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, struc%nat, nat, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, nid, "Number of species does not match")
   if (allocated(error)) return

end subroutine test_valid1_sdf


subroutine test_v3k_large_mol(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc, struc_read
   integer :: unit, nat, i
   character(len=2), allocatable :: sym(:)
   real(wp), allocatable :: xyz(:, :)

   ! Create a structure with 1001 atoms to trigger V3000 format
   nat = 1001
   allocate(sym(nat), xyz(3, nat))

   ! Create alternating H and C atoms on a line
   do i = 1, nat
      if (mod(i, 2) == 0) then
         sym(i) = "C"
      else
         sym(i) = "H"
      end if
      xyz(1, i) = real(i, wp)
      xyz(2, i) = 0.0_wp
      xyz(3, i) = 0.0_wp
   end do

   call new(struc, sym, xyz)

   open(status='scratch', newunit=unit)
   call write_molfile(struc, unit)
   rewind(unit)

   call read_molfile(struc_read, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, struc_read%nat, nat, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc_read%nid, 2, "Number of species does not match")
   if (allocated(error)) return

end subroutine test_v3k_large_mol


subroutine test_v3k_large_sdf(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc, struc_read
   integer :: unit, nat, i
   character(len=2), allocatable :: sym(:)
   real(wp), allocatable :: xyz(:, :)

   ! Create a structure with 1001 atoms to trigger V3000 format
   nat = 1001
   allocate(sym(nat), xyz(3, nat))

   ! Create alternating H and C atoms on a line
   do i = 1, nat
      if (mod(i, 2) == 0) then
         sym(i) = "C"
      else
         sym(i) = "H"
      end if
      xyz(1, i) = real(i, wp)
      xyz(2, i) = 0.0_wp
      xyz(3, i) = 0.0_wp
   end do

   call new(struc, sym, xyz)

   open(status='scratch', newunit=unit)
   call write_sdf(struc, unit)
   rewind(unit)

   call read_sdf(struc_read, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, struc_read%nat, nat, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc_read%nid, 2, "Number of species does not match")
   if (allocated(error)) return

end subroutine test_v3k_large_sdf


subroutine test_v3k_with_bonds(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc, struc_read
   integer :: unit, nat, nbd, i
   character(len=2), allocatable :: sym(:)
   real(wp), allocatable :: xyz(:, :)
   integer, allocatable :: bond(:, :)

   ! Create a structure with 1001 atoms and 1000 bonds to trigger V3000 format
   nat = 1001
   nbd = 1000
   allocate(sym(nat), xyz(3, nat), bond(3, nbd))

   ! Create a linear chain of alternating H and C atoms
   do i = 1, nat
      if (mod(i, 2) == 0) then
         sym(i) = "C"
      else
         sym(i) = "H"
      end if
      xyz(1, i) = real(i, wp)
      xyz(2, i) = 0.0_wp
      xyz(3, i) = 0.0_wp
   end do

   ! Create bonds connecting each consecutive atom pair
   do i = 1, nbd
      bond(1, i) = i
      bond(2, i) = i + 1
      bond(3, i) = 1  ! single bond
   end do

   call new(struc, sym, xyz, bond=bond)

   open(status='scratch', newunit=unit)
   call write_molfile(struc, unit)
   rewind(unit)

   call read_molfile(struc_read, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, struc_read%nat, nat, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc_read%nid, 2, "Number of species does not match")
   if (allocated(error)) return
   call check(error, struc_read%nbd, nbd, "Number of bonds does not match")
   if (allocated(error)) return

end subroutine test_v3k_with_bonds


end module test_write_ctfile
