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

module test_write
   use mctc_env_accuracy, only : wp
   use mctc_env_testing, only : new_unittest, unittest_type, error_type, check
   use mctc_io_write
   use mctc_io_read
   use mctc_io_structure, only : structure_type
   use mctc_version, only : get_mctc_feature
   use testsuite_structure, only : get_structure
   implicit none
   private

   public :: collect_write


contains


!> Collect all exported unit tests
subroutine collect_write(testsuite)

   !> Collection of tests
   type(unittest_type), allocatable, intent(out) :: testsuite(:)

   testsuite = [ &
      & new_unittest("valid-cjson", test_cjson, should_fail=.not.get_mctc_feature("json")), &
      & new_unittest("valid-mol", test_mol), &
      & new_unittest("valid-sdf", test_sdf), &
      & new_unittest("valid-gen", test_gen), &
      & new_unittest("valid-pdb", test_pdb), &
      & new_unittest("valid-qchem", test_qchem), &
      & new_unittest("valid-qcschema", test_qcschema, should_fail=.not.get_mctc_feature("json")), &
      & new_unittest("valid-vasp", test_vasp), &
      & new_unittest("valid-coord", test_coord), &
      & new_unittest("valid-xyz", test_xyz) &
      & ]

end subroutine collect_write


subroutine test_mol(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   character(len=:), allocatable :: name
   integer :: unit

   name = get_name() // ".mol"

   call get_structure(struc, "mindless01")

   call write_structure(struc, name, error)
   if (.not.allocated(error)) then
      call read_structure(struc, name, error)
   end if

   open(file=name, newunit=unit)
   close(unit, status='delete')

end subroutine test_mol


subroutine test_sdf(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   character(len=:), allocatable :: name
   integer :: unit

   name = get_name() // ".sdf"

   call get_structure(struc, "mindless02")

   call write_structure(struc, name, error)
   if (.not.allocated(error)) then
      call read_structure(struc, name, error)
   end if

   open(file=name, newunit=unit)
   close(unit, status='delete')

end subroutine test_sdf


subroutine test_pdb(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   character(len=:), allocatable :: name
   integer :: unit

   name = get_name() // ".pdb"

   call get_structure(struc, "mindless03")

   call write_structure(struc, name, error)
   if (.not.allocated(error)) then
      call read_structure(struc, name, error)
   end if

   open(file=name, newunit=unit)
   close(unit, status='delete')

end subroutine test_pdb


subroutine test_qchem(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   character(len=:), allocatable :: name
   integer :: unit

   name = get_name() // ".qchem"

   call get_structure(struc, "mindless04")

   call write_structure(struc, name, error)
   if (.not.allocated(error)) then
      call read_structure(struc, name, error)
   end if

   open(file=name, newunit=unit)
   close(unit, status='delete')

end subroutine test_qchem


subroutine test_gen(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   character(len=:), allocatable :: name
   integer :: unit

   name = get_name() // ".gen"

   call get_structure(struc, "x01")

   call write_structure(struc, name, error)
   if (.not.allocated(error)) then
      call read_structure(struc, name, error)
   end if

   open(file=name, newunit=unit)
   close(unit, status='delete')

end subroutine test_gen


subroutine test_coord(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   character(len=:), allocatable :: name
   integer :: unit

   name = get_name() // ".coord"

   call get_structure(struc, "x02")

   call write_structure(struc, name, error)
   if (.not.allocated(error)) then
      call read_structure(struc, name, error)
   end if

   open(file=name, newunit=unit)
   close(unit, status='delete')

end subroutine test_coord


subroutine test_vasp(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   character(len=:), allocatable :: name
   integer :: unit

   name = get_name() // ".poscar"

   call get_structure(struc, "x03")

   call write_structure(struc, name, error)
   if (.not.allocated(error)) then
      call read_structure(struc, name, error)
   end if

   open(file=name, newunit=unit)
   close(unit, status='delete')

end subroutine test_vasp


subroutine test_xyz(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   character(len=:), allocatable :: name
   integer :: unit

   name = get_name() // ".xyz"

   call get_structure(struc, "mindless04")

   call write_structure(struc, name, error)
   if (.not.allocated(error)) then
      call read_structure(struc, name, error)
   end if

   open(file=name, newunit=unit)
   close(unit, status='delete')

end subroutine test_xyz


subroutine test_qcschema(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   character(len=:), allocatable :: name
   integer :: unit

   name = get_name() // ".json"

   call get_structure(struc, "mindless05")

   call write_structure(struc, name, error)
   if (.not.allocated(error)) then
      call read_structure(struc, name, error)
   end if

   open(file=name, newunit=unit)
   close(unit, status='delete')

end subroutine test_qcschema


subroutine test_cjson(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   character(len=:), allocatable :: name
   integer :: unit

   name = get_name() // ".cjson"

   call get_structure(struc, "mindless06")

   call write_structure(struc, name, error)
   if (.not.allocated(error)) then
      call read_structure(struc, name, error)
   end if

   open(file=name, newunit=unit)
   close(unit, status='delete')

end subroutine test_cjson


function get_name() result(name)

   character(len=18) :: name

   real :: val

   call random_number(val)
   write(name, '(a, z8.8)') "mctc-test-", int(val*1.0e9)

end function get_name


end module test_write
