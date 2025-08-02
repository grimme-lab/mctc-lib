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

module test_read_json
   use mctc_env_testing, only : new_unittest, unittest_type, error_type, check
   use mctc_io_read_json
   use mctc_io_structure
   use mctc_version, only : get_mctc_feature
   implicit none
   private

   public :: collect_read_json


contains


!> Collect all exported unit tests
subroutine collect_read_json(testsuite)

   !> Collection of tests
   type(unittest_type), allocatable, intent(out) :: testsuite(:)

   logical :: with_json

   with_json = get_mctc_feature("json")

   testsuite = [ &
      & new_unittest("valid-pymatgen-mol", test_valid_mol1, should_fail=.not.with_json), &
      & new_unittest("valid-cjson-mol", test_valid_mol2, should_fail=.not.with_json), &
      & new_unittest("valid-qcschema-mol", test_valid_mol3, should_fail=.not.with_json), &
      & new_unittest("valid-pymatgen-sol", test_valid_sol1, should_fail=.not.with_json), &
      & new_unittest("valid-cjson-sol", test_valid_sol2, should_fail=.not.with_json), &
      & new_unittest("valid-qcschema-sol", test_valid_sol3, should_fail=.not.with_json) &
      & ]

end subroutine collect_read_json


subroutine test_valid_mol1(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: filename = ".test-general-pmg-mol1.json"
   type(structure_type) :: struc
   integer :: unit

   open(file=filename, newunit=unit)
   write(unit, '(a)') &
      '{', &
      '  "@module": "pymatgen.core.structure",', &
      '  "@class": "Molecule",', &
      '  "charge": 0,', &
      '  "spin_multiplicity": 1,', &
      '  "sites": [', &
      '    {', &
      '      "name": "O",', &
      '      "species": [{"element": "O", "occu": 1}],', &
      '      "xyz": [1.1847029, 1.1150792, -0.0344641],', &
      '      "properties": {},', &
      '      "label": "O"', &
      '    },', &
      '    {', &
      '      "name": "H",', &
      '      "species": [{"element": "H", "occu": 1}],', &
      '      "xyz": [0.4939088, 0.9563767, 0.6340089],', &
      '      "properties": {},', &
      '      "label": "H"', &
      '    },', &
      '    {', &
      '      "name": "H",', &
      '      "species": [{"element": "H", "occu": 1}],', &
      '      "xyz": [2.0242676, 1.0811246, 0.4301417],', &
      '      "properties": {},', &
      '      "label": "H"', &
      '    },', &
      '    {', &
      '      "name": "O",', &
      '      "species": [{"element": "O", "occu": 1}],', &
      '      "xyz": [-1.1469443, 0.0697649, 1.1470196],', &
      '      "properties": {},', &
      '      "label": "O"', &
      '    },', &
      '    {', &
      '      "name": "H",', &
      '      "species": [{"element": "H", "occu": 1}],', &
      '      "xyz": [-1.2798308, -0.5232169, 1.8902833],', &
      '      "properties": {},', &
      '      "label": "H"', &
      '    },', &
      '    {', &
      '      "name": "H",', &
      '      "species": [{"element": "H", "occu": 1}],', &
      '      "xyz": [-1.0641398, -0.4956693, 0.356925],', &
      '      "properties": {},', &
      '      "label": "H"', &
      '    },', &
      '    {', &
      '      "name": "O",', &
      '      "species": [{"element": "O", "occu": 1}],', &
      '      "xyz": [-0.1633508, -1.0289346, -1.2401808],', &
      '      "properties": {},', &
      '      "label": "O"', &
      '    },', &
      '    {', &
      '      "name": "H",', &
      '      "species": [{"element": "H", "occu": 1}],', &
      '      "xyz": [0.4914771, -0.3248733, -1.0784838],', &
      '      "properties": {},', &
      '      "label": "H"', &
      '    },', &
      '    {', &
      '      "name": "H",', &
      '      "species": [{"element": "H", "occu": 1}],', &
      '      "xyz": [-0.5400907, -0.8496512, -2.1052499],', &
      '      "properties": {},', &
      '      "label": "H"', &
      '    }', &
      '  ],', &
      '  "properties": {}', &
      '}'
   rewind(unit)

   call read_json(struc, unit, error)
   close(unit, status='delete')
   if (allocated(error)) return

   call check(error, struc%nat, 9, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, 2, "Number of species does not match")
   if (allocated(error)) return

end subroutine test_valid_mol1

subroutine test_valid_mol2(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: filename = ".test-general-cjson-mol2.json"
   type(structure_type) :: struc
   integer :: unit

   open(file=filename, newunit=unit)
   write(unit, '(a)') &
      '{', &
      '  "chemicalJson": 1,', &
      '  "atoms": {', &
      '    "elements": {', &
      '      "number": [', &
      '        8,', &
      '        1', &
      '      ]', &
      '    },', &
      '    "coords": {', &
      '      "3d": [', &
      '         1.2358341722502633E+00,', &
      '        -9.1774253284895344E-02,', &
      '        -6.7936144993384059E-02,', &
      '         1.5475582000473165E+00,', &
      '         5.7192830956765273E-01,', &
      '         5.5691301045614838E-01', &
      '      ]', &
      '    },', &
      '    "formalCharges": [ -1, 0 ]', &
      '  }', &
      '}'
   rewind(unit)

   call read_json(struc, unit, error)
   close(unit, status='delete')
   if (allocated(error)) return
   call check(error, struc%nat, 2, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, 2, "Number of species does not match")
   if (allocated(error)) return
   call check(error, nint(struc%charge), -1, "Total charge does not match")
   if (allocated(error)) return

end subroutine test_valid_mol2

subroutine test_valid_mol3(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: filename = ".test-general-qcschema-mol3.json"
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

   call read_json(struc, unit, error)
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

end subroutine test_valid_mol3

subroutine test_valid_sol1(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: filename = ".test-general-pmg-sol1.json"
   type(structure_type) :: struc
   integer :: unit

   open(file=filename, newunit=unit)
   write(unit, '(a)') &
      '{', &
      '  "@module": "pymatgen.core.structure",', &
      '  "@class": "Structure",', &
      '  "charge": 0.0,', &
      '  "lattice": {', &
      '    "matrix": [', &
      '      [5.59003664376222, 0.0, 0.0],', &
      '      [0.0, 8.68089159045265, 0.0],', &
      '      [0.0, 0.0, 8.68089159045265]', &
      '    ],', &
      '    "pbc": [true, true, true],', &
      '    "a": 5.59003664376222,', &
      '    "b": 8.68089159045265,', &
      '    "c": 8.68089159045265,', &
      '    "alpha": 90.0,', &
      '    "beta": 90.0,', &
      '    "gamma": 90.0,', &
      '    "volume": 421.253303917213', &
      '  },', &
      '  "properties": {},', &
      '  "sites": [', &
      '    {', &
      '      "species": [{"element": "Ti", "occu": 1}],', &
      '      "abc": [0.0, 0.0, 0.0],', &
      '      "properties": {},', &
      '      "label": "Ti",', &
      '      "xyz": [0.0, 0.0, 0.0]', &
      '    },', &
      '    {', &
      '      "species": [{"element": "Ti", "occu": 1}],', &
      '      "abc": [0.5, 0.5000000000000007, 0.5000000000000007],', &
      '      "properties": {},', &
      '      "label": "Ti",', &
      '      "xyz": [2.79501832188111, 4.340445795226331, 4.340445795226331]', &
      '    },', &
      '    {', &
      '      "species": [{"element": "O", "occu": 1}], ', &
      '      "abc": [0.0, 0.30530000000000074, 0.30530000000000074],', &
      '      "properties": {},', &
      '      "label": "O",', &
      '      "xyz": [0.0, 2.6502762025652005, 2.6502762025652005]', &
      '    },', &
      '    {', &
      '      "species": [{"element": "O", "occu": 1}], ', &
      '      "abc": [0.0, 0.6947000000000005, 0.6947000000000005],', &
      '      "properties": {},', &
      '      "label": "O",', &
      '      "xyz": [0.0, 6.03061538788746, 6.03061538788746]', &
      '    },', &
      '    {', &
      '      "species": [{"element": "O", "occu": 1}], ', &
      '      "abc": [0.5, 0.1946999999999999, 0.8053000000000002],', &
      '      "properties": {},', &
      '      "label": "O",', &
      '      "xyz": [2.79501832188111, 1.69016959266113, 6.99072199779152]', &
      '    },', &
      '    {', &
      '      "species": [{"element": "O", "occu": 1}], ', &
      '      "abc": [0.5, 0.8053000000000002, 0.1946999999999999],', &
      '      "properties": {},', &
      '      "label": "O",', &
      '      "xyz": [2.79501832188111, 6.99072199779152, 1.69016959266113]', &
      '    }', &
      '  ]', &
      '}'
   rewind(unit)

   call read_json(struc, unit, error)
   close(unit, status='delete')
   if (allocated(error)) return

   call check(error, struc%nat, 6, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, 2, "Number of species does not match")
   if (allocated(error)) return
   call check(error, allocated(struc%lattice), .true., "Lattice is not allocated")
   if (allocated(error)) return

end subroutine test_valid_sol1

subroutine test_valid_sol2(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: filename = ".test-general-cjson-sol2.json"
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
      '  },', &
      '  "properties": {', &
      '    "molecular mass": 30.0690,', &
      '    "melting point": -172,', &
      '    "boiling point": -88', &
      '  }', &
      '}'
   rewind(unit)

   call read_json(struc, unit, error)
   close(unit, status='delete')
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

end subroutine test_valid_sol2

subroutine test_valid_sol3(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: filename = ".test-general-qcsk-sol3.json"
   type(structure_type) :: struc
   integer :: unit

   open(file=filename, newunit=unit)
   write(unit, '(a)') &
      '{', &
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

   call read_json(struc, unit, error)
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

end subroutine test_valid_sol3

end module test_read_json
