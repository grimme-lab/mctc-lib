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

module test_read_cjson
   use mctc_env_testing, only : new_unittest, unittest_type, error_type, check
   use mctc_io_read_cjson
   use mctc_io_structure
   use mctc_version, only : get_mctc_feature
   implicit none
   private

   public :: collect_read_cjson


contains


!> Collect all exported unit tests
subroutine collect_read_cjson(testsuite)

   !> Collection of tests
   type(unittest_type), allocatable, intent(out) :: testsuite(:)

   logical :: with_json

   with_json = get_mctc_feature("json")

   testsuite = [ &
      & new_unittest("valid1-cjson", test_valid1_cjson, should_fail=.not.with_json), &
      & new_unittest("valid2-cjson", test_valid2_cjson, should_fail=.not.with_json), &
      & new_unittest("invalid1-cjson", test_invalid1_cjson, should_fail=.true.), &
      & new_unittest("invalid2-cjson", test_invalid2_cjson, should_fail=.true.), &
      & new_unittest("invalid3-cjson", test_invalid3_cjson, should_fail=.true.) &
      & ]

end subroutine collect_read_cjson


subroutine test_valid1_cjson(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      '{', &
      '  "chemical json": 0,', &
      '  "name": "ethane",', &
      '  "inchi": "1/C2H6/c1-2/h1-2H3",', &
      '  "formula": "C 2 H 6",', &
      '  "atoms": {', &
      '    "elements": {', &
      '      "number": [  1,   6,   1,   1,   6,   1,   1,   1 ]', &
      '    },', &
      '    "coords": {', &
      '      "3d": [  1.185080, -0.003838,  0.987524,', &
      '               0.751621, -0.022441, -0.020839,', &
      '               1.166929,  0.833015, -0.569312,', &
      '               1.115519, -0.932892, -0.514525,', &
      '              -0.751587,  0.022496,  0.020891,', &
      '              -1.166882, -0.833372,  0.568699,', &
      '              -1.115691,  0.932608,  0.515082,', &
      '              -1.184988,  0.004424, -0.987522 ]', &
      '    }', &
      '  },', &
      '  "bonds": {', &
      '    "connections": {', &
      '      "index": [ 0, 1,', &
      '                 1, 2,', &
      '                 1, 3,', &
      '                 1, 4,', &
      '                 4, 5,', &
      '                 4, 6,', &
      '                 4, 7 ]', &
      '    },', &
      '    "order": [ 1, 1, 1, 1, 1, 1, 1 ]', &
      '  },', &
      '  "properties": {', &
      '    "molecular mass": 30.0690,', &
      '    "melting point": -172,', &
      '    "boiling point": -88', &
      '  }', &
      '}'
   rewind(unit)

   call read_cjson(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, allocated(struc%comment), "Comment line should be preserved")
   if (allocated(error)) return
   call check(error, struc%comment, "ethane")
   if (allocated(error)) return
   call check(error, struc%nat, 8, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, 2, "Number of species does not match")
   if (allocated(error)) return
   call check(error, struc%nbd, 7, "Number of bonds does not match")
   if (allocated(error)) return

end subroutine test_valid1_cjson


subroutine test_valid2_cjson(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      '{', &
      '  "chemical json": 0,', &
      '  "name": "TiO2 rutile",', &
      '  "formula": "Ti 2 O 4",', &
      '  "unit cell": {', &
      '    "a": 2.95812,', &
      '    "b": 4.59373,', &
      '    "c": 4.59373,', &
      '    "alpha": 90.0,', &
      '    "beta":  90.0,', &
      '    "gamma": 90.0', &
      '  },', &
      '  "atoms": {', &
      '    "elements": {', &
      '      "number": [ 22, 22, 8, 8, 8, 8 ]', &
      '    },', &
      '    "coords": {', &
      '      "3d fractional": [ 0.00000, 0.00000, 0.00000,', &
      '                         0.50000, 0.50000, 0.50000,', &
      '                         0.00000, 0.30530, 0.30530,', &
      '                         0.00000, 0.69470, 0.69470,', &
      '                         0.50000, 0.19470, 0.80530,', &
      '                         0.50000, 0.80530, 0.19470 ]', &
      '    }', &
      '  }', &
      '}'
   rewind(unit)

   call read_cjson(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, allocated(struc%comment), "Comment line should be preserved")
   if (allocated(error)) return
   call check(error, struc%comment, "TiO2 rutile")
   if (allocated(error)) return
   call check(error, struc%nat, 6, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, 2, "Number of species does not match")
   if (allocated(error)) return

end subroutine test_valid2_cjson


subroutine test_invalid1_cjson(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      '{', &
      '  "chemicalJson": -1,', &
      '  "atoms": {', &
      '    "elements": {', &
      '      "number": [  1,   6,   1,   1,   6,   1,   1,   1 ]', &
      '    },', &
      '    "coords": {', &
      '      "3d": [  1.185080, -0.003838,  0.987524,', &
      '               0.751621, -0.022441, -0.020839,', &
      '               1.166929,  0.833015, -0.569312,', &
      '               1.115519, -0.932892, -0.514525,', &
      '              -0.751587,  0.022496,  0.020891,', &
      '              -1.166882, -0.833372,  0.568699,', &
      '              -1.115691,  0.932608,  0.515082,', &
      '              -1.184988,  0.004424, -0.987522 ]', &
      '    }', &
      '  }', &
      '}'
   rewind(unit)

   call read_cjson(struc, unit, error)
   close(unit)
   if (allocated(error)) return

end subroutine test_invalid1_cjson


subroutine test_invalid2_cjson(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      '{', &
      '  "chemicalJson": 1,', &
      '  "atoms": {', &
      '    "elements": {', &
      '      "number": [  1,   6,   1,   1,   6,   1,   1 ]', &
      '    },', &
      '    "coords": {', &
      '      "3d": [  1.185080, -0.003838,  0.987524,', &
      '               0.751621, -0.022441, -0.020839,', &
      '               1.166929,  0.833015, -0.569312,', &
      '               1.115519, -0.932892, -0.514525,', &
      '              -0.751587,  0.022496,  0.020891,', &
      '              -1.166882, -0.833372,  0.568699,', &
      '              -1.115691,  0.932608,  0.515082,', &
      '              -1.184988,  0.004424, -0.987522 ]', &
      '    }', &
      '  }', &
      '}'
   rewind(unit)

   call read_cjson(struc, unit, error)
   close(unit)
   if (allocated(error)) return

end subroutine test_invalid2_cjson


subroutine test_invalid3_cjson(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      '{', &
      '  "chemicalJson": 1,', &
      '  "atoms": {', &
      '    "elements": {', &
      '      "number": [  1,   6,   1,   1,   6,   1,   1,   1 ]', &
      '    },', &
      '    "coords": {', &
      '      "3d": null,', &
      '    }', &
      '  }', &
      '}'
   rewind(unit)

   call read_cjson(struc, unit, error)
   close(unit)
   if (allocated(error)) return

end subroutine test_invalid3_cjson


end module test_read_cjson
