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
      & new_unittest("valid3-qcschema", test_valid3_qcschema, should_fail=.not.with_json), &
      & new_unittest("valid4-qcschema", test_valid4_qcschema, should_fail=.not.with_json), &
      & new_unittest("valid5-qcschema", test_valid5_qcschema, should_fail=.not.with_json), &
      & new_unittest("extras1-qcschema", test_extras1_qcschema, should_fail=.not.with_json), &
      & new_unittest("incomplete", test_incomplete, should_fail=.true.), &
      & new_unittest("mismatch-schema", test_mismatch_schema, should_fail=.true.), &
      & new_unittest("mismatch-geometry-symbols", test_mismatch_geometry_symbols, should_fail=.true.), &
      & new_unittest("missing-symbols", test_missing_symbols, should_fail=.true.), &
      & new_unittest("invalid-molecule", test_invalid_molecule, should_fail=.true.), &
      & new_unittest("invalid-symbols", test_invalid_symbols, should_fail=.true.), &
      & new_unittest("invalid-geometry", test_invalid_geometry, should_fail=.true.), &
      & new_unittest("invalid-connectivity1", test_invalid_connectivity1, should_fail=.true.), &
      & new_unittest("invalid-connectivity2", test_invalid_connectivity2, should_fail=.true.), &
      & new_unittest("invalid-connectivity3", test_invalid_connectivity3, should_fail=.true.), &
      & new_unittest("invalid-comment", test_invalid_comment, should_fail=.true.), &
      & new_unittest("invalid-charge", test_invalid_charge, should_fail=.true.), &
      & new_unittest("invalid-multiplicity", test_invalid_multiplicity, should_fail=.true.), &
      & new_unittest("invalid-schema-version-value1", test_invalid_schema_version_value1, should_fail=.true.), &
      & new_unittest("invalid-schema-version-value2", test_invalid_schema_version_value2, should_fail=.true.), &
      & new_unittest("invalid-schema-version-value3", test_invalid_schema_version_value3, should_fail=.true.), &
      & new_unittest("invalid-schema-version-type1", test_invalid_schema_version_type1, should_fail=.true.), &
      & new_unittest("invalid-schema-version-type2", test_invalid_schema_version_type2, should_fail=.true.), &
      & new_unittest("invalid-schema-name-value1", test_invalid_schema_name_value1, should_fail=.true.), &
      & new_unittest("invalid-schema-name-value2", test_invalid_schema_name_value2, should_fail=.true.), &
      & new_unittest("invalid-schema-name-type1", test_invalid_schema_name_type1, should_fail=.true.), &
      & new_unittest("invalid-schema-name-type2", test_invalid_schema_name_type2, should_fail=.true.), &
      & new_unittest("invalid-root-data", test_invalid_root_data, should_fail=.true.), &
      & new_unittest("extras-incomplete-lattice", test_extras_incomplete_lattice, should_fail=.true.), &
      & new_unittest("extras-incompatible-lattice1", test_extras_incompatible_lattice1, should_fail=.true.), &
      & new_unittest("extras-incompatible-lattice2", test_extras_incompatible_lattice2, should_fail=.true.), &
      & new_unittest("extras-incompatible-periodic", test_extras_incompatible_periodic, should_fail=.true.), &
      & new_unittest("extras-type-mismatch", test_extras_type_mismatch, should_fail=.true.), &
      & new_unittest("cjson", test_cjson_qcschema, should_fail=.true.) &
      & ]

end subroutine collect_read_qcschema


subroutine test_valid1_qcschema(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: filename = ".test-valid1-qcschema.json"
   type(structure_type) :: struc
   integer :: unit

   open(file=filename, newunit=unit)
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
   close(unit, status='delete')
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

   character(len=*), parameter :: filename = ".test-valid2-qcschema.json"
   type(structure_type) :: struc
   integer :: unit

   open(file=filename, newunit=unit)
   write(unit, '(a)') &
      '{', &
      '  "schema_version": 1,', &
      '  "schema_name": "qcschema_input",', &
      '  "driver": "energy",', &
      '  "model": {', &
      '    "method": "xtb",', &
      '    "basis": ""', &
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
   close(unit, status='delete')
   if (allocated(error)) return

   call check(error, .not.allocated(struc%comment), "Empty comment line should not be saved")
   if (allocated(error)) return
   call check(error, struc%nat, 38, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, 2, "Number of species does not match")
   if (allocated(error)) return

end subroutine test_valid2_qcschema


subroutine test_valid3_qcschema(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: filename = ".test-valid3-qcschema.json"
   type(structure_type) :: struc
   integer :: unit

   open(file=filename, newunit=unit)
   write(unit, '(a)') &
      '{', &
      '  "schema_version": 2,', &
      '  "geometry": [', &
      '    0.0,  0.0000, -0.1294,', &
      '    0.0, -1.4941,  1.0274,', &
      '    0.0,  1.4941,  1.0274', &
      '  ],', &
      '  "symbols": ["O", "H", "H"],', &
      '  "comment": "Water molecule"', &
      '}'
   rewind(unit)

   call read_qcschema(struc, unit, error)
   close(unit, status='delete')
   if (allocated(error)) return

   call check(error, allocated(struc%comment), "Comment line should be preserved")
   if (allocated(error)) return
   call check(error, struc%comment, "Water molecule")
   if (allocated(error)) return
   call check(error, struc%nat, 3, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, 2, "Number of species does not match")
   if (allocated(error)) return

end subroutine test_valid3_qcschema


subroutine test_valid4_qcschema(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: filename = ".test-valid4-qcschema.json"
   type(structure_type) :: struc
   integer :: unit

   open(file=filename, newunit=unit)
   write(unit, '(a)') &
      '{', &
      '  "schema_version": 1,', &
      '  "schema_name": "qcschema_input",', &
      '  "driver": "gradient",', &
      '  "model": {', &
      '    "method": "r2scan",', &
      '    "basis": "def2-svp"', &
      '  },', &
      '  "molecule": {', &
      '    "schema_version": 1,', &
      '    "schema_name": "qcschema_molecule",', &
      '    "molecule": {', &
      '      "symbols": [', &
      '        "C", "C", "C", "C", "C", "C", "H", "H", "H", "H", "H", "H",', &
      '        "H", "H", "H", "H", "H", "C", "C", "C", "C", "C", "C", "H",', &
      '        "H", "H", "H", "H", "H", "H", "H", "H", "H", "H", "H", "H",', &
      '        "H", "H"', &
      '      ],', &
      '      "geometry": [', &
      '         1.1910941154998063E+00, 8.0445623507578545E-01, 0.0000000000000000E+00,', &
      '         3.6246828858324265E+00,-7.2565467293657882E-01, 0.0000000000000000E+00,', &
      '         6.0068711168320394E+00, 8.8306882464391478E-01, 0.0000000000000000E+00,', &
      '         8.4393260517381972E+00,-6.4779797365275837E-01, 0.0000000000000000E+00,', &
      '         1.0824537843875046E+01, 9.5827990793265394E-01, 0.0000000000000000E+00,', &
      '         1.3237906549102401E+01,-5.9564154403544178E-01, 0.0000000000000000E+00,', &
      '         1.4916738870552548E+01, 5.9753126974621407E-01, 0.0000000000000000E+00,', &
      '         1.3341085572910570E+01,-1.8126249017728293E+00,-1.6605019820556557E+00,', &
      '         1.3341085572910570E+01,-1.8126249017728293E+00, 1.6605019820556557E+00,', &
      '         1.0802428053059009E+01, 2.2054988770423987E+00,-1.6484077375067128E+00,', &
      '         1.0802428053059009E+01, 2.2054988770423987E+00, 1.6484077375067128E+00,', &
      '         8.4618137876963875E+00,-1.8965287233311212E+00, 1.6491636277910218E+00,', &
      '         8.4618137876963875E+00,-1.8965287233311212E+00,-1.6491636277910218E+00,', &
      '         5.9874069420110843E+00, 2.1312326566090456E+00,-1.6493526003620989E+00,', &
      '         5.9874069420110843E+00, 2.1312326566090456E+00, 1.6493526003620989E+00,', &
      '         3.6452808960798451E+00,-1.9740074774727869E+00, 1.6491636277910218E+00,', &
      '         3.6452808960798451E+00,-1.9740074774727869E+00,-1.6491636277910218E+00,', &
      '        -1.1910941154998063E+00,-8.0445623507578545E-01, 0.0000000000000000E+00,', &
      '        -3.6246828858324265E+00, 7.2565467293657882E-01, 0.0000000000000000E+00,', &
      '        -6.0068711168320394E+00,-8.8306882464391478E-01, 0.0000000000000000E+00,', &
      '        -8.4393260517381972E+00, 6.4779797365275837E-01, 0.0000000000000000E+00,', &
      '        -1.0824537843875046E+01,-9.5827990793265394E-01, 0.0000000000000000E+00,', &
      '        -1.3237906549102401E+01, 5.9564154403544178E-01, 0.0000000000000000E+00,', &
      '        -1.4916738870552548E+01,-5.9753126974621407E-01, 0.0000000000000000E+00,', &
      '        -1.3341085572910570E+01, 1.8126249017728293E+00, 1.6605019820556557E+00,', &
      '        -1.3341085572910570E+01, 1.8126249017728293E+00,-1.6605019820556557E+00,', &
      '        -1.0802428053059009E+01,-2.2054988770423987E+00,-1.6484077375067128E+00,', &
      '        -1.0802428053059009E+01,-2.2054988770423987E+00, 1.6484077375067128E+00,', &
      '        -8.4618137876963875E+00, 1.8965287233311212E+00, 1.6491636277910218E+00,', &
      '        -8.4618137876963875E+00, 1.8965287233311212E+00,-1.6491636277910218E+00,', &
      '        -5.9874069420110843E+00,-2.1312326566090456E+00,-1.6493526003620989E+00,', &
      '        -5.9874069420110843E+00,-2.1312326566090456E+00, 1.6493526003620989E+00,', &
      '        -3.6452808960798451E+00, 1.9740074774727869E+00,-1.6491636277910218E+00,', &
      '        -3.6452808960798451E+00, 1.9740074774727869E+00, 1.6491636277910218E+00,', &
      '        -1.1706850778234652E+00,-2.0526200670409165E+00, 1.6491636277910218E+00,', &
      '        -1.1706850778234652E+00,-2.0526200670409165E+00,-1.6491636277910218E+00,', &
      '         1.1706850778234652E+00, 2.0526200670409165E+00,-1.6491636277910218E+00,', &
      '         1.1706850778234652E+00, 2.0526200670409165E+00, 1.6491636277910218E+00', &
      '      ],', &
      '      "molecular_charge": 0,', &
      '      "connectivity": [', &
      '        [ 0, 1, 1],  [ 1, 2, 1],  [ 2, 3, 1],  [ 3, 4, 1],  [ 4, 5, 1],', &
      '        [ 5, 6, 1],  [ 5, 7, 1],  [ 5, 8, 1],  [ 4, 9, 1],  [ 4,10, 1],', &
      '        [ 3,11, 1],  [ 3,12, 1],  [ 2,13, 1],  [ 2,14, 1],  [ 1,15, 1],', &
      '        [ 1,16, 1],  [ 0,17, 1],  [17,18, 1],  [18,19, 1],  [19,20, 1],', &
      '        [20,21, 1],  [21,22, 1],  [22,23, 1],  [22,24, 1],  [22,25, 1],', &
      '        [21,26, 1],  [21,27, 1],  [20,28, 1],  [20,29, 1],  [19,30, 1],', &
      '        [19,31, 1],  [18,32, 1],  [18,33, 1],  [17,34, 1],  [17,35, 1],', &
      '        [ 0,36, 1],  [ 0,37, 1]', &
      '      ]', &
      '    }', &
      '  }', &
      '}'
   rewind(unit)

   call read_qcschema(struc, unit, error)
   close(unit, status='delete')
   if (allocated(error)) return

   call check(error, .not.allocated(struc%comment), "Empty comment line should not be saved")
   if (allocated(error)) return
   call check(error, struc%nat, 38, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, 2, "Number of species does not match")
   if (allocated(error)) return

end subroutine test_valid4_qcschema


subroutine test_valid5_qcschema(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: filename = ".test-valid5-qcschema.json"
   type(structure_type) :: struc
   integer :: unit

   open(file=filename, newunit=unit)
   write(unit, '(a)') &
      '{', &
      '  "schema_version": 2,', &
      '  "geometry": [', &
      '    0.0,  0.0000, -0.1294,', &
      '    0.0, -1.4941,  1.0274,', &
      '    0.0,  1.4941,  1.0274', &
      '  ],', &
      '  "symbols": ["O", "H", "H"],', &
      '  "connectivity": [[ 0, 1, 1],  [ 0, 2, 1]],', &
      '  "molecular_charge": 0.0,', &
      '  "comment": "Water molecule",', &
      '  "extras": null', &
      '}'
   rewind(unit)

   call read_qcschema(struc, unit, error)
   close(unit, status='delete')
   if (allocated(error)) return

   call check(error, allocated(struc%comment), "Comment line should be preserved")
   if (allocated(error)) return
   call check(error, struc%comment, "Water molecule")
   if (allocated(error)) return
   call check(error, struc%nat, 3, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, 2, "Number of species does not match")
   if (allocated(error)) return

end subroutine test_valid5_qcschema


subroutine test_extras1_qcschema(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: filename = ".test-valid5-qcschema.json"
   type(structure_type) :: struc
   integer :: unit

   open(file=filename, newunit=unit)
   write(unit, '(a)') &
      '{', &
      '  "schema_version": 2,', &
      '  "schema_name": "qcschema_molecule",', &
      '  "provenance": {', &
      '    "creator": "mctc-lib",', &
      '    "version": "0.4.2",', &
      '    "routine": "mctc_io_write_qcschema::write_qcschema"', &
      '  },', &
      '  "comment": "TiO2 rutile",', &
      '  "symbols": ["Ti", "Ti", "O", "O", "O", "O"],', &
      '  "atomic_numbers": [22, 22, 8, 8, 8, 8],', &
      '  "geometry": [', &
      '     0.0000000000000000E+00, 0.0000000000000000E+00, 0.0000000000000000E+00,', &
      '     5.2818191416515159E+00, 8.2022538117381334E+00, 8.2022538117381334E+00,', &
      '     6.1333938828927657E-16, 5.0082961774473045E+00, 5.0082961774473045E+00,', &
      '     1.3956333869785798E-15, 1.1396211446028962E+01, 1.1396211446028962E+01,', &
      '     5.2818191416515150E+00, 3.1939576342908298E+00, 1.3210549989185438E+01,', &
      '     5.2818191416515150E+00, 1.3210549989185438E+01, 3.1939576342908289E+00', &
      '  ],', &
      '  "molecular_charge": 0,', &
      '  "extras": {', &
      '    "periodic": {', &
      '      "lattice": [', &
      '         5.5900366437622173E+00, 0.0000000000000000E+00, 0.0000000000000000E+00,', &
      '         5.3155130499965102E-16, 8.6808915904526547E+00, 0.0000000000000000E+00,', &
      '         5.3155130499965102E-16, 5.3155130499965102E-16, 8.6808915904526547E+00', &
      '      ]', &
      '    }', &
      '  }', &
      '}'
   rewind(unit)

   call read_qcschema(struc, unit, error)
   close(unit, status='delete')
   if (allocated(error)) return

   call check(error, allocated(struc%comment), "Comment line should be preserved")
   if (allocated(error)) return
   call check(error, struc%comment, "TiO2 rutile")
   if (allocated(error)) return
   call check(error, struc%nat, 6, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, 2, "Number of species does not match")
   if (allocated(error)) return
   call check(error, all(struc%periodic), .true., "Structure should be periodic")
   if (allocated(error)) return

end subroutine test_extras1_qcschema


subroutine test_mismatch_schema(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: filename = ".test-schema-mismatch-qcschema.json"
   type(structure_type) :: struc
   integer :: unit

   open(file=filename, newunit=unit)
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
   close(unit, status='delete')

end subroutine test_mismatch_schema


subroutine test_invalid_molecule(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: filename = ".test-invalid-molecule-qcschema.json"
   type(structure_type) :: struc
   integer :: unit

   open(file=filename, newunit=unit)
   write(unit, '(a)') &
      '{', &
      '  "schema_version": 1,', &
      '  "schema_name": "qcschema_molecule",', &
      '  "molecule": []', &
      '}'
   rewind(unit)

   call read_qcschema(struc, unit, error)
   close(unit, status='delete')

end subroutine test_invalid_molecule


subroutine test_invalid_symbols(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: filename = ".test-invalid-symbols-qcschema.json"
   type(structure_type) :: struc
   integer :: unit

   open(file=filename, newunit=unit)
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
      '    "symbols": [8, 1,  1],', &
      '    "comment": "Water molecule"', &
      '  }', &
      '}'
   rewind(unit)

   call read_qcschema(struc, unit, error)
   close(unit, status='delete')

end subroutine test_invalid_symbols


subroutine test_invalid_geometry(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: filename = ".test-invalid-geometry-qcschema.json"
   type(structure_type) :: struc
   integer :: unit

   open(file=filename, newunit=unit)
   write(unit, '(a)') &
      '{', &
      '  "schema_version": 1,', &
      '  "schema_name": "qcschema_molecule",', &
      '  "molecule": {', &
      '    "geometry": [', &
      '      [0.0,  0.0000, -0.1294],', &
      '      [0.0, -1.4941,  1.0274],', &
      '      [0.0,  1.4941,  1.0274]', &
      '    ],', &
      '    "symbols": ["O", "H",  "H"],', &
      '    "comment": "Water molecule"', &
      '  }', &
      '}'
   rewind(unit)

   call read_qcschema(struc, unit, error)
   close(unit, status='delete')

end subroutine test_invalid_geometry


subroutine test_invalid_connectivity1(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: filename = ".test-invalid-connectivity1-qcschema.json"
   type(structure_type) :: struc
   integer :: unit

   open(file=filename, newunit=unit)
   write(unit, '(a)') &
      '{', &
      '  "schema_version": 1,', &
      '  "schema_name": "qcschema_molecule",', &
      '  "molecule": {', &
      '    "connectivity": {"bonds": [[ 0, 1, 1],  [ 0, 2, 1]]},', &
      '    "geometry": [', &
      '      0.0,  0.0000, -0.1294,', &
      '      0.0, -1.4941,  1.0274,', &
      '      0.0,  1.4941,  1.0274', &
      '    ],', &
      '    "symbols": ["O", "H",  "H"],', &
      '    "comment": "Water molecule"', &
      '  }', &
      '}'
   rewind(unit)

   call read_qcschema(struc, unit, error)
   close(unit, status='delete')

end subroutine test_invalid_connectivity1


subroutine test_invalid_connectivity2(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: filename = ".test-invalid-connectivity-qcschema.json"
   type(structure_type) :: struc
   integer :: unit

   open(file=filename, newunit=unit)
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
      '    "connectivity": [[ 0, 1, "single"],  [ 0, 2, "single"]],', &
      '    "symbols": ["O", "H",  "H"],', &
      '    "comment": "Water molecule"', &
      '  }', &
      '}'
   rewind(unit)

   call read_qcschema(struc, unit, error)
   close(unit, status='delete')

end subroutine test_invalid_connectivity2


subroutine test_invalid_connectivity3(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: filename = ".test-invalid-connectivity3-qcschema.json"
   type(structure_type) :: struc
   integer :: unit

   open(file=filename, newunit=unit)
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
      '    "symbols": ["O", "H",  "H"],', &
      '    "connectivity": ["0-1", "0-2"],', &
      '    "comment": "Water molecule"', &
      '  }', &
      '}'
   rewind(unit)

   call read_qcschema(struc, unit, error)
   close(unit, status='delete')

end subroutine test_invalid_connectivity3


subroutine test_invalid_comment(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: filename = ".test-invalid-comment-qcschema.json"
   type(structure_type) :: struc
   integer :: unit

   open(file=filename, newunit=unit)
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
      '    "symbols": ["O", "H",  "H"],', &
      '    "comment": 100', &
      '  }', &
      '}'
   rewind(unit)

   call read_qcschema(struc, unit, error)
   close(unit, status='delete')

end subroutine test_invalid_comment


subroutine test_invalid_charge(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: filename = ".test-invalid-charge-qcschema.json"
   type(structure_type) :: struc
   integer :: unit

   open(file=filename, newunit=unit)
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
      '    "symbols": ["O", "H",  "H"],', &
      '    "comment": "Water molecule",', &
      '    "molecular_charge": "neutral"', &
      '  }', &
      '}'
   rewind(unit)

   call read_qcschema(struc, unit, error)
   close(unit, status='delete')

end subroutine test_invalid_charge


subroutine test_invalid_multiplicity(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: filename = ".test-invalid-multiplicity-qcschema.json"
   type(structure_type) :: struc
   integer :: unit

   open(file=filename, newunit=unit)
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
      '    "symbols": ["O", "H",  "H"],', &
      '    "comment": "Water molecule",', &
      '    "molecular_multiplicity": "singlet"', &
      '  }', &
      '}'
   rewind(unit)

   call read_qcschema(struc, unit, error)
   close(unit, status='delete')

end subroutine test_invalid_multiplicity


subroutine test_invalid_schema_version_value1(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: filename = ".test-invalid-schema-version-value1-qcschema.json"
   type(structure_type) :: struc
   integer :: unit

   open(file=filename, newunit=unit)
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
   close(unit, status='delete')

end subroutine test_invalid_schema_version_value1


subroutine test_invalid_schema_version_value2(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: filename = ".test-invalid-schema-version-value2-qcschema.json"
   type(structure_type) :: struc
   integer :: unit

   open(file=filename, newunit=unit)
   write(unit, '(a)') &
      '{', &
      '  "schema_version": 1,', &
      '  "schema_name": "qcschema_input",', &
      '  "molecule": {', &
      '    "schema_version": 0,', &
      '    "schema_name": "qcschema_molecule",', &
      '    "geometry": [', &
      '      0.0,  0.0000, -0.1294,', &
      '      0.0, -1.4941,  1.0274,', &
      '      0.0,  1.4941,  1.0274', &
      '    ],', &
      '    "symbols": ["O", "H", "H"],', &
      '    "comment": "Water molecule"', &
      '  },', &
      '  "driver": "energy",', &
      '  "model": {', &
      '    "method": "r2scan",', &
      '    "basis": "def2-tzvp"', &
      '  }', &
      '}'
   rewind(unit)

   call read_qcschema(struc, unit, error)
   close(unit, status='delete')

end subroutine test_invalid_schema_version_value2

subroutine test_invalid_schema_version_value3(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: filename = ".test-invalid-schema-version-value3-qcschema.json"
   type(structure_type) :: struc
   integer :: unit

   open(file=filename, newunit=unit)
   write(unit, '(a)') &
      '{', &
      '  "schema_version": 2,', &
      '  "schema_name": "qcschema_input",', &
      '  "molecule": {', &
      '    "schema_version": 1,', &
      '    "schema_name": "qcschema_molecule",', &
      '    "geometry": [', &
      '      0.0,  0.0000, -0.1294,', &
      '      0.0, -1.4941,  1.0274,', &
      '      0.0,  1.4941,  1.0274', &
      '    ],', &
      '    "symbols": ["O", "H", "H"],', &
      '    "comment": "Water molecule"', &
      '  },', &
      '  "driver": "energy",', &
      '  "model": {', &
      '    "method": "r2scan",', &
      '    "basis": "def2-tzvp"', &
      '  }', &
      '}'
   rewind(unit)

   call read_qcschema(struc, unit, error)
   close(unit, status='delete')

end subroutine test_invalid_schema_version_value3


subroutine test_invalid_schema_version_type1(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: filename = ".test-invalid-schema-version-type1-qcschema.json"
   type(structure_type) :: struc
   integer :: unit

   open(file=filename, newunit=unit)
   write(unit, '(a)') &
      '{', &
      '  "schema_version": "one",', &
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
   close(unit, status='delete')

end subroutine test_invalid_schema_version_type1


subroutine test_invalid_schema_version_type2(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: filename = ".test-invalid-schema-version-type2-qcschema.json"
   type(structure_type) :: struc
   integer :: unit

   open(file=filename, newunit=unit)
   write(unit, '(a)') &
      '{', &
      '  "schema_version": 1,', &
      '  "schema_name": "qcschema_input",', &
      '  "molecule": {', &
      '    "schema_version": "one",', &
      '    "schema_name": "qcschema_molecule",', &
      '    "geometry": [', &
      '      0.0,  0.0000, -0.1294,', &
      '      0.0, -1.4941,  1.0274,', &
      '      0.0,  1.4941,  1.0274', &
      '    ],', &
      '    "symbols": ["O", "H", "H"],', &
      '    "comment": "Water molecule"', &
      '  },', &
      '  "driver": "energy",', &
      '  "model": {', &
      '    "method": "r2scan",', &
      '    "basis": "def2-tzvp"', &
      '  }', &
      '}'
   rewind(unit)

   call read_qcschema(struc, unit, error)
   close(unit, status='delete')

end subroutine test_invalid_schema_version_type2


subroutine test_invalid_schema_name_value1(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: filename = ".test-invalid-schema-name-value1-qcschema.json"
   type(structure_type) :: struc
   integer :: unit

   open(file=filename, newunit=unit)
   write(unit, '(a)') &
      '{', &
      '  "schema_version": 1,', &
      '  "schema_name": "qcschema_geometry",', &
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
   close(unit, status='delete')

end subroutine test_invalid_schema_name_value1


subroutine test_invalid_schema_name_value2(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: filename = ".test-invalid-schema-name-value2-qcschema.json"
   type(structure_type) :: struc
   integer :: unit

   open(file=filename, newunit=unit)
   write(unit, '(a)') &
      '{', &
      '  "schema_version": 1,', &
      '  "schema_name": "qcschema_input",', &
      '  "molecule": {', &
      '    "schema_version": 2,', &
      '    "schema_name": "qcschema_structure",', &
      '    "geometry": [', &
      '      0.0,  0.0000, -0.1294,', &
      '      0.0, -1.4941,  1.0274,', &
      '      0.0,  1.4941,  1.0274', &
      '    ],', &
      '    "symbols": ["O", "H", "H"],', &
      '    "comment": "Water molecule"', &
      '  },', &
      '  "driver": "energy",', &
      '  "model": {', &
      '    "method": "r2scan",', &
      '    "basis": "def2-tzvp"', &
      '  }', &
      '}'
   rewind(unit)

   call read_qcschema(struc, unit, error)
   close(unit, status='delete')

end subroutine test_invalid_schema_name_value2


subroutine test_invalid_schema_name_type1(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: filename = ".test-invalid-schema-name1-qcschema.json"
   type(structure_type) :: struc
   integer :: unit

   open(file=filename, newunit=unit)
   write(unit, '(a)') &
      '{', &
      '  "schema_version": 1,', &
      '  "schema_name": 1,', &
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
   close(unit, status='delete')

end subroutine test_invalid_schema_name_type1


subroutine test_invalid_schema_name_type2(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: filename = ".test-invalid-schema-name2-qcschema.json"
   type(structure_type) :: struc
   integer :: unit

   open(file=filename, newunit=unit)
   write(unit, '(a)') &
      '{', &
      '  "schema_version": 1,', &
      '  "schema_name": "qcschema_input",', &
      '  "molecule": {', &
      '    "schema_version": 2,', &
      '    "schema_name": 1,', &
      '    "geometry": [', &
      '      0.0,  0.0000, -0.1294,', &
      '      0.0, -1.4941,  1.0274,', &
      '      0.0,  1.4941,  1.0274', &
      '    ],', &
      '    "symbols": ["O", "H", "H"],', &
      '    "comment": "Water molecule"', &
      '  },', &
      '  "driver": "energy",', &
      '  "model": {', &
      '    "method": "r2scan",', &
      '    "basis": "def2-tzvp"', &
      '  }', &
      '}'
   rewind(unit)

   call read_qcschema(struc, unit, error)
   close(unit, status='delete')

end subroutine test_invalid_schema_name_type2


subroutine test_invalid_root_data(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: filename = ".test-invalid-root-data-qcschema.json"
   type(structure_type) :: struc
   integer :: unit

   open(file=filename, newunit=unit)
   write(unit, '(a)') &
      '[{', &
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
      '}]'
   rewind(unit)

   call read_qcschema(struc, unit, error)
   close(unit, status='delete')

end subroutine test_invalid_root_data


subroutine test_mismatch_geometry_symbols(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: filename = ".test-mismatch-geometry-symbols-qcschema.json"
   type(structure_type) :: struc
   integer :: unit

   open(file=filename, newunit=unit)
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
      '    "symbols": ["O", "H", "H", "H"]', &
      '  }', &
      '}'
   rewind(unit)

   call read_qcschema(struc, unit, error)
   close(unit, status='delete')

end subroutine test_mismatch_geometry_symbols


subroutine test_missing_symbols(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: filename = ".test-missing-symbols.json"
   type(structure_type) :: struc
   integer :: unit

   open(file=filename, newunit=unit)
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
      '    "atomic_numbers": [8, 1, 1]', &
      '  }', &
      '}'
   rewind(unit)

   call read_qcschema(struc, unit, error)
   close(unit, status='delete')

end subroutine test_missing_symbols


subroutine test_incomplete(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: filename = ".test-incomplete-qcschema.json"
   type(structure_type) :: struc
   integer :: unit

   open(file=filename, newunit=unit)
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
   close(unit, status='delete')

end subroutine test_incomplete


subroutine test_cjson_qcschema(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: filename = ".test-cjson-qcschema.json"
   type(structure_type) :: struc
   integer :: unit

   open(file=filename, newunit=unit)
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
   close(unit, status='delete')

end subroutine test_cjson_qcschema


subroutine test_extras_incomplete_lattice(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: filename = ".test-valid5-qcschema.json"
   type(structure_type) :: struc
   integer :: unit

   open(file=filename, newunit=unit)
   write(unit, '(a)') &
      '{', &
      '  "schema_version": 2,', &
      '  "schema_name": "qcschema_molecule",', &
      '  "provenance": {', &
      '    "creator": "mctc-lib",', &
      '    "version": "0.4.2",', &
      '    "routine": "mctc_io_write_qcschema::write_qcschema"', &
      '  },', &
      '  "comment": "TiO2 rutile",', &
      '  "symbols": ["Ti", "Ti", "O", "O", "O", "O"],', &
      '  "atomic_numbers": [22, 22, 8, 8, 8, 8],', &
      '  "geometry": [', &
      '     0.0000000000000000E+00, 0.0000000000000000E+00, 0.0000000000000000E+00,', &
      '     5.2818191416515159E+00, 8.2022538117381334E+00, 8.2022538117381334E+00,', &
      '     6.1333938828927657E-16, 5.0082961774473045E+00, 5.0082961774473045E+00,', &
      '     1.3956333869785798E-15, 1.1396211446028962E+01, 1.1396211446028962E+01,', &
      '     5.2818191416515150E+00, 3.1939576342908298E+00, 1.3210549989185438E+01,', &
      '     5.2818191416515150E+00, 1.3210549989185438E+01, 3.1939576342908289E+00', &
      '  ],', &
      '  "molecular_charge": 0,', &
      '  "extras": {', &
      '    "periodic": {', &
      '      "lattice": [', &
      '         5.5900366437622173E+00, 8.6808915904526547E+00, 8.6808915904526547E+00', &
      '      ]', &
      '    }', &
      '  }', &
      '}'
   rewind(unit)

   call read_qcschema(struc, unit, error)
   close(unit, status='delete')
   if (allocated(error)) return

end subroutine test_extras_incomplete_lattice


subroutine test_extras_incompatible_lattice1(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: filename = ".test-valid5-qcschema.json"
   type(structure_type) :: struc
   integer :: unit

   open(file=filename, newunit=unit)
   write(unit, '(a)') &
      '{', &
      '  "schema_version": 2,', &
      '  "schema_name": "qcschema_molecule",', &
      '  "provenance": {', &
      '    "creator": "mctc-lib",', &
      '    "version": "0.4.2",', &
      '    "routine": "mctc_io_write_qcschema::write_qcschema"', &
      '  },', &
      '  "comment": "TiO2 rutile",', &
      '  "symbols": ["Ti", "Ti", "O", "O", "O", "O"],', &
      '  "atomic_numbers": [22, 22, 8, 8, 8, 8],', &
      '  "geometry": [', &
      '     0.0000000000000000E+00, 0.0000000000000000E+00, 0.0000000000000000E+00,', &
      '     5.2818191416515159E+00, 8.2022538117381334E+00, 8.2022538117381334E+00,', &
      '     6.1333938828927657E-16, 5.0082961774473045E+00, 5.0082961774473045E+00,', &
      '     1.3956333869785798E-15, 1.1396211446028962E+01, 1.1396211446028962E+01,', &
      '     5.2818191416515150E+00, 3.1939576342908298E+00, 1.3210549989185438E+01,', &
      '     5.2818191416515150E+00, 1.3210549989185438E+01, 3.1939576342908289E+00', &
      '  ],', &
      '  "molecular_charge": 0,', &
      '  "extras": {', &
      '    "periodic": {', &
      '      "lattice": [', &
      '         [5.5900366437622173E+00, 0.0000000000000000E+00, 0.0000000000000000E+00],', &
      '         [5.3155130499965102E-16, 8.6808915904526547E+00, 0.0000000000000000E+00],', &
      '         [5.3155130499965102E-16, 5.3155130499965102E-16, 8.6808915904526547E+00]', &
      '      ]', &
      '    }', &
      '  }', &
      '}'
   rewind(unit)

   call read_qcschema(struc, unit, error)
   close(unit, status='delete')
   if (allocated(error)) return

end subroutine test_extras_incompatible_lattice1


subroutine test_extras_incompatible_lattice2(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: filename = ".test-valid5-qcschema.json"
   type(structure_type) :: struc
   integer :: unit

   open(file=filename, newunit=unit)
   write(unit, '(a)') &
      '{', &
      '  "schema_version": 2,', &
      '  "schema_name": "qcschema_molecule",', &
      '  "provenance": {', &
      '    "creator": "mctc-lib",', &
      '    "version": "0.4.2",', &
      '    "routine": "mctc_io_write_qcschema::write_qcschema"', &
      '  },', &
      '  "comment": "TiO2 rutile",', &
      '  "symbols": ["Ti", "Ti", "O", "O", "O", "O"],', &
      '  "atomic_numbers": [22, 22, 8, 8, 8, 8],', &
      '  "geometry": [', &
      '     0.0000000000000000E+00, 0.0000000000000000E+00, 0.0000000000000000E+00,', &
      '     5.2818191416515159E+00, 8.2022538117381334E+00, 8.2022538117381334E+00,', &
      '     6.1333938828927657E-16, 5.0082961774473045E+00, 5.0082961774473045E+00,', &
      '     1.3956333869785798E-15, 1.1396211446028962E+01, 1.1396211446028962E+01,', &
      '     5.2818191416515150E+00, 3.1939576342908298E+00, 1.3210549989185438E+01,', &
      '     5.2818191416515150E+00, 1.3210549989185438E+01, 3.1939576342908289E+00', &
      '  ],', &
      '  "molecular_charge": 0,', &
      '  "extras": {', &
      '    "periodic": {', &
      '      "lattice": {', &
      '         "a": 5.5900366437622173E+00,', &
      '         "b": 8.6808915904526547E+00,', &
      '         "c": 8.6808915904526547E+00,', &
      '         "alpha": 90.0,', &
      '         "beta":  90.0,', &
      '         "gamma": 90.0', &
      '      ]', &
      '    }', &
      '  }', &
      '}'
   rewind(unit)

   call read_qcschema(struc, unit, error)
   close(unit, status='delete')
   if (allocated(error)) return

end subroutine test_extras_incompatible_lattice2


subroutine test_extras_incompatible_periodic(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: filename = ".test-valid5-qcschema.json"
   type(structure_type) :: struc
   integer :: unit

   open(file=filename, newunit=unit)
   write(unit, '(a)') &
      '{', &
      '  "schema_version": 2,', &
      '  "schema_name": "qcschema_molecule",', &
      '  "provenance": {', &
      '    "creator": "mctc-lib",', &
      '    "version": "0.4.2",', &
      '    "routine": "mctc_io_write_qcschema::write_qcschema"', &
      '  },', &
      '  "comment": "TiO2 rutile",', &
      '  "symbols": ["Ti", "Ti", "O", "O", "O", "O"],', &
      '  "atomic_numbers": [22, 22, 8, 8, 8, 8],', &
      '  "geometry": [', &
      '     0.0000000000000000E+00, 0.0000000000000000E+00, 0.0000000000000000E+00,', &
      '     5.2818191416515159E+00, 8.2022538117381334E+00, 8.2022538117381334E+00,', &
      '     6.1333938828927657E-16, 5.0082961774473045E+00, 5.0082961774473045E+00,', &
      '     1.3956333869785798E-15, 1.1396211446028962E+01, 1.1396211446028962E+01,', &
      '     5.2818191416515150E+00, 3.1939576342908298E+00, 1.3210549989185438E+01,', &
      '     5.2818191416515150E+00, 1.3210549989185438E+01, 3.1939576342908289E+00', &
      '  ],', &
      '  "molecular_charge": 0,', &
      '  "extras": {', &
      '    "periodic": 3', &
      '  }', &
      '}'
   rewind(unit)

   call read_qcschema(struc, unit, error)
   close(unit, status='delete')
   if (allocated(error)) return

end subroutine test_extras_incompatible_periodic


subroutine test_extras_type_mismatch(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: filename = ".test-valid5-qcschema.json"
   type(structure_type) :: struc
   integer :: unit

   open(file=filename, newunit=unit)
   write(unit, '(a)') &
      '{', &
      '  "schema_version": 2,', &
      '  "schema_name": "qcschema_molecule",', &
      '  "provenance": {', &
      '    "creator": "mctc-lib",', &
      '    "version": "0.4.2",', &
      '    "routine": "mctc_io_write_qcschema::write_qcschema"', &
      '  },', &
      '  "comment": "TiO2 rutile",', &
      '  "symbols": ["Ti", "Ti", "O", "O", "O", "O"],', &
      '  "atomic_numbers": [22, 22, 8, 8, 8, 8],', &
      '  "geometry": [', &
      '     0.0000000000000000E+00, 0.0000000000000000E+00, 0.0000000000000000E+00,', &
      '     5.2818191416515159E+00, 8.2022538117381334E+00, 8.2022538117381334E+00,', &
      '     6.1333938828927657E-16, 5.0082961774473045E+00, 5.0082961774473045E+00,', &
      '     1.3956333869785798E-15, 1.1396211446028962E+01, 1.1396211446028962E+01,', &
      '     5.2818191416515150E+00, 3.1939576342908298E+00, 1.3210549989185438E+01,', &
      '     5.2818191416515150E+00, 1.3210549989185438E+01, 3.1939576342908289E+00', &
      '  ],', &
      '  "molecular_charge": 0,', &
      '  "extras": true', &
      '}'
   rewind(unit)

   call read_qcschema(struc, unit, error)
   close(unit, status='delete')
   if (allocated(error)) return

end subroutine test_extras_type_mismatch

end module test_read_qcschema
