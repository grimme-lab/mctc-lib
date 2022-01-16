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

module test_read_qcschema
   use mctc_env_testing, only : new_unittest, unittest_type, error_type, check
   use mctc_io_read_qcschema
   use mctc_io_structure
   use mctc_version, only : get_mctc_feature
   implicit none
   private

   public :: collect_read_qcschema


contains


!> Collect all exported unit tests
subroutine collect_read_qcschema(testsuite)

   !> Collection of tests
   type(unittest_type), allocatable, intent(out) :: testsuite(:)

   logical :: with_json

   with_json = get_mctc_feature("json")

   testsuite = [ &
      & new_unittest("valid1-qcschema", test_valid1_qcschema, should_fail=.not.with_json), &
      & new_unittest("valid2-qcschema", test_valid2_qcschema, should_fail=.not.with_json), &
      & new_unittest("invalid1-qcschema", test_invalid1_qcschema, should_fail=.true.), &
      & new_unittest("invalid2-qcschema", test_invalid2_qcschema, should_fail=.true.), &
      & new_unittest("invalid3-qcschema", test_invalid3_qcschema, should_fail=.true.), &
      & new_unittest("invalid4-qcschema", test_invalid4_qcschema, should_fail=.true.), &
      & new_unittest("invalid5-qcschema", test_invalid5_qcschema, should_fail=.true.), &
      & new_unittest("invalid6-qcschema", test_invalid6_qcschema, should_fail=.true.) &
      & ]

end subroutine collect_read_qcschema


subroutine test_valid1_qcschema(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      '{', &
      '  "schema_version": 1,', &
      '  "molecule": {', &
      '    "geometry": [', &
      '      0.0,  0.0000, -0.1294,', &
      '      0.0, -1.4941,  1.0274,', &
      '      0.0,  1.4941,  1.0274', &
      '    ],', &
      '    "symbols": ["O", "H", "H"],', &
      '    "comment": "Water molecule"', &
      '  }', &
      '}'
   rewind(unit)

   call read_qcschema(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, allocated(struc%comment), "Comment line should be preserved")
   if (allocated(error)) return
   call check(error, struc%comment, "Water molecule")
   if (allocated(error)) return
   call check(error, struc%nat, 3, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, 2, "Number of species does not match")
   if (allocated(error)) return

end subroutine test_valid1_qcschema


subroutine test_valid2_qcschema(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      '{', &
      '  "schema_version": 1,', &
      '  "schema_name": "qcschema_input",', &
      '  "driver": "energy",', &
      '  "model": {', &
      '    "method": "xtb",', &
      '    "basis": null', &
      '  },', &
      '  "molecule": {', &
      '    "schema_version": 2,', &
      '    "schema_name": "qcschema_molecule",', &
      '    "provenance": {', &
      '      "creator": "mctc-lib",', &
      '      "version": "0.2.3",', &
      '      "routine": "mctc_io_write_qcschema::write_qcschema"', &
      '    },', &
      '    "symbols": [', &
      '      "C", "C", "C", "C", "C", "C", "H", "H", "H", "H", "H", "H",', &
      '      "H", "H", "H", "H", "H", "C", "C", "C", "C", "C", "C", "H",', &
      '      "H", "H", "H", "H", "H", "H", "H", "H", "H", "H", "H", "H",', &
      '      "H", "H"', &
      '    ],', &
      '    "geometry": [', &
      '       1.1910941154998063E+00, 8.0445623507578545E-01, 0.0000000000000000E+00,', &
      '       3.6246828858324265E+00,-7.2565467293657882E-01, 0.0000000000000000E+00,', &
      '       6.0068711168320394E+00, 8.8306882464391478E-01, 0.0000000000000000E+00,', &
      '       8.4393260517381972E+00,-6.4779797365275837E-01, 0.0000000000000000E+00,', &
      '       1.0824537843875046E+01, 9.5827990793265394E-01, 0.0000000000000000E+00,', &
      '       1.3237906549102401E+01,-5.9564154403544178E-01, 0.0000000000000000E+00,', &
      '       1.4916738870552548E+01, 5.9753126974621407E-01, 0.0000000000000000E+00,', &
      '       1.3341085572910570E+01,-1.8126249017728293E+00,-1.6605019820556557E+00,', &
      '       1.3341085572910570E+01,-1.8126249017728293E+00, 1.6605019820556557E+00,', &
      '       1.0802428053059009E+01, 2.2054988770423987E+00,-1.6484077375067128E+00,', &
      '       1.0802428053059009E+01, 2.2054988770423987E+00, 1.6484077375067128E+00,', &
      '       8.4618137876963875E+00,-1.8965287233311212E+00, 1.6491636277910218E+00,', &
      '       8.4618137876963875E+00,-1.8965287233311212E+00,-1.6491636277910218E+00,', &
      '       5.9874069420110843E+00, 2.1312326566090456E+00,-1.6493526003620989E+00,', &
      '       5.9874069420110843E+00, 2.1312326566090456E+00, 1.6493526003620989E+00,', &
      '       3.6452808960798451E+00,-1.9740074774727869E+00, 1.6491636277910218E+00,', &
      '       3.6452808960798451E+00,-1.9740074774727869E+00,-1.6491636277910218E+00,', &
      '      -1.1910941154998063E+00,-8.0445623507578545E-01, 0.0000000000000000E+00,', &
      '      -3.6246828858324265E+00, 7.2565467293657882E-01, 0.0000000000000000E+00,', &
      '      -6.0068711168320394E+00,-8.8306882464391478E-01, 0.0000000000000000E+00,', &
      '      -8.4393260517381972E+00, 6.4779797365275837E-01, 0.0000000000000000E+00,', &
      '      -1.0824537843875046E+01,-9.5827990793265394E-01, 0.0000000000000000E+00,', &
      '      -1.3237906549102401E+01, 5.9564154403544178E-01, 0.0000000000000000E+00,', &
      '      -1.4916738870552548E+01,-5.9753126974621407E-01, 0.0000000000000000E+00,', &
      '      -1.3341085572910570E+01, 1.8126249017728293E+00, 1.6605019820556557E+00,', &
      '      -1.3341085572910570E+01, 1.8126249017728293E+00,-1.6605019820556557E+00,', &
      '      -1.0802428053059009E+01,-2.2054988770423987E+00,-1.6484077375067128E+00,', &
      '      -1.0802428053059009E+01,-2.2054988770423987E+00, 1.6484077375067128E+00,', &
      '      -8.4618137876963875E+00, 1.8965287233311212E+00, 1.6491636277910218E+00,', &
      '      -8.4618137876963875E+00, 1.8965287233311212E+00,-1.6491636277910218E+00,', &
      '      -5.9874069420110843E+00,-2.1312326566090456E+00,-1.6493526003620989E+00,', &
      '      -5.9874069420110843E+00,-2.1312326566090456E+00, 1.6493526003620989E+00,', &
      '      -3.6452808960798451E+00, 1.9740074774727869E+00,-1.6491636277910218E+00,', &
      '      -3.6452808960798451E+00, 1.9740074774727869E+00, 1.6491636277910218E+00,', &
      '      -1.1706850778234652E+00,-2.0526200670409165E+00, 1.6491636277910218E+00,', &
      '      -1.1706850778234652E+00,-2.0526200670409165E+00,-1.6491636277910218E+00,', &
      '       1.1706850778234652E+00, 2.0526200670409165E+00,-1.6491636277910218E+00,', &
      '       1.1706850778234652E+00, 2.0526200670409165E+00, 1.6491636277910218E+00', &
      '    ],', &
      '    "molecular_charge": 0,', &
      '    "connectivity": [', &
      '      [ 0, 1, 1],  [ 1, 2, 1],  [ 2, 3, 1],  [ 3, 4, 1],  [ 4, 5, 1],', &
      '      [ 5, 6, 1],  [ 5, 7, 1],  [ 5, 8, 1],  [ 4, 9, 1],  [ 4,10, 1],', &
      '      [ 3,11, 1],  [ 3,12, 1],  [ 2,13, 1],  [ 2,14, 1],  [ 1,15, 1],', &
      '      [ 1,16, 1],  [ 0,17, 1],  [17,18, 1],  [18,19, 1],  [19,20, 1],', &
      '      [20,21, 1],  [21,22, 1],  [22,23, 1],  [22,24, 1],  [22,25, 1],', &
      '      [21,26, 1],  [21,27, 1],  [20,28, 1],  [20,29, 1],  [19,30, 1],', &
      '      [19,31, 1],  [18,32, 1],  [18,33, 1],  [17,34, 1],  [17,35, 1],', &
      '      [ 0,36, 1],  [ 0,37, 1]', &
      '    ]', &
      '  }', &
      '}'
   rewind(unit)

   call read_qcschema(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, .not.allocated(struc%comment), "Empty comment line should not be saved")
   if (allocated(error)) return
   call check(error, struc%nat, 38, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, 2, "Number of species does not match")
   if (allocated(error)) return

end subroutine test_valid2_qcschema


subroutine test_valid4_qcschema(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "3", &
      "WATER27, H2O", &
      "O     1.1847029    1.1150792   -0.0344641 ", &
      "H     0.4939088    0.9563767    0.6340089 ", &
      "H     2.0242676    1.0811246    0.4301417 ", &
      "3", &
      "WATER27, H2O", &
      "O    -1.1469443    0.0697649    1.1470196 ", &
      "H    -1.2798308   -0.5232169    1.8902833 ", &
      "H    -1.0641398   -0.4956693    0.3569250 ", &
      "3", &
      "WATER27, H2O", &
      "O    -0.1633508   -1.0289346   -1.2401808 ", &
      "H     0.4914771   -0.3248733   -1.0784838 ", &
      "H    -0.5400907   -0.8496512   -2.1052499 "
   rewind(unit)

   call read_qcschema(struc, unit, error)
   if (.not.allocated(error)) then
      call read_qcschema(struc, unit, error)
   end if
   close(unit)
   if (allocated(error)) return

   call check(error, struc%nat, 3, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, 2, "Number of species does not match")
   if (allocated(error)) return

end subroutine test_valid4_qcschema


subroutine test_valid5_qcschema(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "3", &
      "WATER27, H2O", &
      "8     1.1847029    1.1150792   -0.0344641 ", &
      "1     0.4939088    0.9563767    0.6340089 ", &
      "1     2.0242676    1.0811246    0.4301417 ", &
      "3", &
      "WATER27, H2O", &
      "8    -1.1469443    0.0697649    1.1470196 ", &
      "1    -1.2798308   -0.5232169    1.8902833 ", &
      "1    -1.0641398   -0.4956693    0.3569250 "
   rewind(unit)

   call read_qcschema(struc, unit, error)
   if (.not.allocated(error)) then
      call read_qcschema(struc, unit, error)
   end if
   close(unit)
   if (allocated(error)) return

   call check(error, struc%nat, 3, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, 2, "Number of species does not match")
   if (allocated(error)) return

end subroutine test_valid5_qcschema


subroutine test_invalid1_qcschema(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      '{', &
      '  "schema_version": 2,', &
      '  "schema_name": "qcschema_molecule",', &
      '  "molecule": {', &
      '    "geometry": [', &
      '      0.0,  0.0000, -0.1294,', &
      '      0.0, -1.4941,  1.0274,', &
      '      0.0,  1.4941,  1.0274', &
      '    ],', &
      '    "symbols": ["O", "H", "H"],', &
      '    "comment": "Water molecule"', &
      '  }', &
      '}'
   rewind(unit)

   call read_qcschema(struc, unit, error)
   close(unit)
   if (allocated(error)) return

end subroutine test_invalid1_qcschema


subroutine test_invalid2_qcschema(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      '{', &
      '  "schema_version": 0,', &
      '  "schema_name": "qcschema_molecule",', &
      '  "molecule": {', &
      '    "geometry": [', &
      '      0.0,  0.0000, -0.1294,', &
      '      0.0, -1.4941,  1.0274,', &
      '      0.0,  1.4941,  1.0274', &
      '    ],', &
      '    "symbols": ["O", "H", "H"],', &
      '    "comment": "Water molecule"', &
      '  }', &
      '}'
   rewind(unit)

   call read_qcschema(struc, unit, error)
   close(unit)
   if (allocated(error)) return

end subroutine test_invalid2_qcschema


subroutine test_invalid3_qcschema(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      '{', &
      '  "schema_version": 1,', &
      '  "schema_name": "qcschema_molecule",', &
      '  "molecule": {', &
      '    "geometry": [', &
      '      0.0,  0.0000, -0.1294,', &
      '      0.0, -1.4941,  1.0274,', &
      '      0.0,  1.4941,  1.0274', &
      '    ],', &
      '    "symbols": ["O", "H", "H", "H"],', &
      '  }', &
      '}'
   rewind(unit)

   call read_qcschema(struc, unit, error)
   close(unit)
   if (allocated(error)) return

end subroutine test_invalid3_qcschema


subroutine test_invalid4_qcschema(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      '{', &
      '  "schema_version": 1,', &
      '  "schema_name": "qcschema_molecule",', &
      '  "molecule": {', &
      '    "geometry": [', &
      '      0.0,  0.0000, -0.1294,', &
      '      0.0, -1.4941,  1.0274,', &
      '      0.0,  1.4941,  1.0274', &
      '    ],', &
      '    "atomic_numbers": [8, 1, 1],', &
      '  }', &
      '}'
   rewind(unit)

   call read_qcschema(struc, unit, error)
   close(unit)
   if (allocated(error)) return

end subroutine test_invalid4_qcschema


subroutine test_invalid5_qcschema(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      '{', &
      '  "schema_version": 1,', &
      '  "schema_name": "qcschema_molecule",', &
      '  "molecule": {', &
      '    "geometry": [', &
      '      0.0,  0.0000, -0.1294,', &
      '      0.0, -1.4941,  1.0274,'
   rewind(unit)

   call read_qcschema(struc, unit, error)
   close(unit)
   if (allocated(error)) return

end subroutine test_invalid5_qcschema


subroutine test_invalid6_qcschema(error)

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
      '  }', &
      '}'
   rewind(unit)

   call read_qcschema(struc, unit, error)
   close(unit)
   if (allocated(error)) return

end subroutine test_invalid6_qcschema


end module test_read_qcschema
