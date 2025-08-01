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

module test_read_pymatgen
   use mctc_env_testing, only : new_unittest, unittest_type, error_type, check
   use mctc_io_read_pymatgen
   use mctc_io_structure
   use mctc_version, only : get_mctc_feature
   implicit none
   private

   public :: collect_read_pymatgen


contains


!> Collect all exported unit tests
subroutine collect_read_pymatgen(testsuite)

   !> Collection of tests
   type(unittest_type), allocatable, intent(out) :: testsuite(:)

   logical :: with_json

   with_json = get_mctc_feature("json")

   testsuite = [ &
      & new_unittest("valid-pymatgen-mol1", test_valid_mol1, should_fail=.not.with_json), &
      & new_unittest("valid-pymatgen-sol1", test_valid_sol1, should_fail=.not.with_json) &
      & ]

end subroutine collect_read_pymatgen


subroutine test_valid_mol1(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: filename = ".test-valid-pmg-mol1.json"
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

   call read_pymatgen(struc, unit, error)
   close(unit, status='delete')
   if (allocated(error)) return

   call check(error, struc%nat, 9, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, 2, "Number of species does not match")
   if (allocated(error)) return

end subroutine test_valid_mol1


subroutine test_valid_sol1(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: filename = ".test-valid-pmg-sol1.json"
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

   call read_pymatgen(struc, unit, error)
   close(unit, status='delete')
   if (allocated(error)) return

   call check(error, struc%nat, 6, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, 2, "Number of species does not match")
   if (allocated(error)) return
   call check(error, allocated(struc%lattice), .true., "Lattice is not allocated")
   if (allocated(error)) return

end subroutine test_valid_sol1


end module test_read_pymatgen