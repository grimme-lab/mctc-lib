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
      & new_unittest("valid-pymatgen-sol1", test_valid_sol1, should_fail=.not.with_json), &
      & new_unittest("missing-module", test_missing_module, should_fail=.true.), &
      & new_unittest("incorrect-module", test_incorrect_module, should_fail=.true.), &
      & new_unittest("missing-class", test_missing_class, should_fail=.true.), &
      & new_unittest("incorrect-class", test_incorrect_class, should_fail=.true.), &
      & new_unittest("incorrect-charge", test_incorrect_charge, should_fail=.true.), &
      & new_unittest("incorrect-multiplicity", test_incorrect_multiplicity, should_fail=.true.), &
      & new_unittest("unphysical-multiplicity", test_unphysical_multiplicity, should_fail=.true.), &
      & new_unittest("incorrect-sites", test_incorrect_sites, should_fail=.true.), &
      & new_unittest("incorrect-sites-entry", test_incorrect_sites_entry, should_fail=.true.), &
      & new_unittest("incorrect-label", test_incorrect_label, should_fail=.true.), &
      & new_unittest("incorrect-xyz", test_incorrect_xyz, should_fail=.true.), &
      & new_unittest("incorrect-xyz-value", test_incorrect_xyz_value, should_fail=.true.), &
      & new_unittest("incorrect-xyz-size", test_incorrect_xyz_size, should_fail=.true.), &
      & new_unittest("incorrect-root", test_incorrect_root, should_fail=.true.), &
      & new_unittest("incorrect-lattice", test_incorrect_lattice, should_fail=.true.), &
      & new_unittest("incorrect-lattice-table", test_incorrect_lattice_table, should_fail=.true.), &
      & new_unittest("incorrect-lattice-value", test_incorrect_lattice_value, should_fail=.true.), &
      & new_unittest("incorrect-lattice-size", test_incorrect_lattice_size, should_fail=.true.), &
      & new_unittest("incorrect-lattice-dim", test_incorrect_lattice_dim, should_fail=.true.), &
      & new_unittest("incorrect-lattice-rank", test_incorrect_lattice_rank, should_fail=.true.) &
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


subroutine test_missing_module(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: filename = ".test-invalid-pmg-mol1.json"
   type(structure_type) :: struc
   integer :: unit

   open(file=filename, newunit=unit)
   write(unit, '(a)') &
      '{', &
      '  "@class": "Molecule",', &
      '  "charge": 0,', &
      '  "spin_multiplicity": 1,', &
      '  "sites": [', &
      '    {', &
      '      "name": "O", "label": "O",', &
      '      "species": [{"element": "O", "occu": 1}],', &
      '      "xyz": [1.1847029, 1.1150792, -0.0344641],', &
      '      "properties": {}', &
      '    },', &
      '    {', &
      '      "name": "H", "label": "H",', &
      '      "species": [{"element": "H", "occu": 1}],', &
      '      "xyz": [0.4939088, 0.9563767, 0.6340089],', &
      '      "properties": {}', &
      '    },', &
      '    {', &
      '      "name": "H", "label": "H",', &
      '      "species": [{"element": "H", "occu": 1}],', &
      '      "xyz": [2.0242676, 1.0811246, 0.4301417],', &
      '      "properties": {}', &
      '    }', &
      '  ],', &
      '  "properties": {}', &
      '}'
   rewind(unit)

   call read_pymatgen(struc, unit, error)
   close(unit, status='delete')

end subroutine test_missing_module


subroutine test_incorrect_module(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: filename = ".test-invalid-pmg-mol2.json"
   type(structure_type) :: struc
   integer :: unit

   open(file=filename, newunit=unit)
   write(unit, '(a)') &
      '{', &
      '  "@module": "mctc.io.structure",', &
      '  "@class": "Molecule",', &
      '  "charge": 0,', &
      '  "spin_multiplicity": 1,', &
      '  "sites": [', &
      '    {', &
      '      "name": "O", "label": "O",', &
      '      "species": [{"element": "O", "occu": 1}],', &
      '      "xyz": [1.1847029, 1.1150792, -0.0344641],', &
      '      "properties": {}', &
      '    },', &
      '    {', &
      '      "name": "H", "label": "H",', &
      '      "species": [{"element": "H", "occu": 1}],', &
      '      "xyz": [0.4939088, 0.9563767, 0.6340089],', &
      '      "properties": {}', &
      '    },', &
      '    {', &
      '      "name": "H", "label": "H",', &
      '      "species": [{"element": "H", "occu": 1}],', &
      '      "xyz": [2.0242676, 1.0811246, 0.4301417],', &
      '      "properties": {}', &
      '    }', &
      '  ],', &
      '  "properties": {}', &
      '}'
   rewind(unit)

   call read_pymatgen(struc, unit, error)
   close(unit, status='delete')

end subroutine test_incorrect_module

subroutine test_missing_class(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: filename = ".test-invalid-pmg-mol3.json"
   type(structure_type) :: struc
   integer :: unit

   open(file=filename, newunit=unit)
   write(unit, '(a)') &
      '{', &
      '  "@module": "pymatgen.core.structure",', &
      '  "charge": 0,', &
      '  "spin_multiplicity": 1,', &
      '  "sites": [', &
      '    {', &
      '      "name": "O", "label": "O",', &
      '      "species": [{"element": "O", "occu": 1}],', &
      '      "xyz": [1.1847029, 1.1150792, -0.0344641],', &
      '      "properties": {}', &
      '    },', &
      '    {', &
      '      "name": "H", "label": "H",', &
      '      "species": [{"element": "H", "occu": 1}],', &
      '      "xyz": [0.4939088, 0.9563767, 0.6340089],', &
      '      "properties": {}', &
      '    },', &
      '    {', &
      '      "name": "H", "label": "H",', &
      '      "species": [{"element": "H", "occu": 1}],', &
      '      "xyz": [2.0242676, 1.0811246, 0.4301417],', &
      '      "properties": {}', &
      '    }', &
      '  ],', &
      '  "properties": {}', &
      '}'
   rewind(unit)

   call read_pymatgen(struc, unit, error)
   close(unit, status='delete')

end subroutine test_missing_class


subroutine test_incorrect_class(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: filename = ".test-invalid-pmg-mol4.json"
   type(structure_type) :: struc
   integer :: unit

   open(file=filename, newunit=unit)
   write(unit, '(a)') &
      '{', &
      '  "@module": "pymatgen.core.structure",', &
      '  "@class": "Geometry",', &
      '  "charge": 0,', &
      '  "spin_multiplicity": 1,', &
      '  "sites": [', &
      '    {', &
      '      "name": "O", "label": "O",', &
      '      "species": [{"element": "O", "occu": 1}],', &
      '      "xyz": [1.1847029, 1.1150792, -0.0344641],', &
      '      "properties": {}', &
      '    },', &
      '    {', &
      '      "name": "H", "label": "H",', &
      '      "species": [{"element": "H", "occu": 1}],', &
      '      "xyz": [0.4939088, 0.9563767, 0.6340089],', &
      '      "properties": {}', &
      '    },', &
      '    {', &
      '      "name": "H", "label": "H",', &
      '      "species": [{"element": "H", "occu": 1}],', &
      '      "xyz": [2.0242676, 1.0811246, 0.4301417],', &
      '      "properties": {}', &
      '    }', &
      '  ],', &
      '  "properties": {}', &
      '}'
   rewind(unit)

   call read_pymatgen(struc, unit, error)
   close(unit, status='delete')

end subroutine test_incorrect_class

subroutine test_incorrect_charge(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: filename = ".test-invalid-pmg-mol5.json"
   type(structure_type) :: struc
   integer :: unit

   open(file=filename, newunit=unit)
   write(unit, '(a)') &
      '{', &
      '  "@module": "pymatgen.core.structure",', &
      '  "@class": "Molecule",', &
      '  "charge": "neutral",', &
      '  "spin_multiplicity": 1,', &
      '  "sites": [', &
      '    {', &
      '      "name": "O", "label": "O",', &
      '      "species": [{"element": "O", "occu": 1}],', &
      '      "xyz": [1.1847029, 1.1150792, -0.0344641],', &
      '      "properties": {}', &
      '    },', &
      '    {', &
      '      "name": "H", "label": "H",', &
      '      "species": [{"element": "H", "occu": 1}],', &
      '      "xyz": [0.4939088, 0.9563767, 0.6340089],', &
      '      "properties": {}', &
      '    },', &
      '    {', &
      '      "name": "H", "label": "H",', &
      '      "species": [{"element": "H", "occu": 1}],', &
      '      "xyz": [2.0242676, 1.0811246, 0.4301417],', &
      '      "properties": {}', &
      '    }', &
      '  ],', &
      '  "properties": {}', &
      '}'
   rewind(unit)

   call read_pymatgen(struc, unit, error)
   close(unit, status='delete')

end subroutine test_incorrect_charge

subroutine test_incorrect_multiplicity(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: filename = ".test-invalid-pmg-mol5.json"
   type(structure_type) :: struc
   integer :: unit

   open(file=filename, newunit=unit)
   write(unit, '(a)') &
      '{', &
      '  "@module": "pymatgen.core.structure",', &
      '  "@class": "Molecule",', &
      '  "charge": 0,', &
      '  "spin_multiplicity": "singlet",', &
      '  "sites": [', &
      '    {', &
      '      "name": "O", "label": "O",', &
      '      "species": [{"element": "O", "occu": 1}],', &
      '      "xyz": [1.1847029, 1.1150792, -0.0344641],', &
      '      "properties": {}', &
      '    },', &
      '    {', &
      '      "name": "H", "label": "H",', &
      '      "species": [{"element": "H", "occu": 1}],', &
      '      "xyz": [0.4939088, 0.9563767, 0.6340089],', &
      '      "properties": {}', &
      '    },', &
      '    {', &
      '      "name": "H", "label": "H",', &
      '      "species": [{"element": "H", "occu": 1}],', &
      '      "xyz": [2.0242676, 1.0811246, 0.4301417],', &
      '      "properties": {}', &
      '    }', &
      '  ],', &
      '  "properties": {}', &
      '}'
   rewind(unit)

   call read_pymatgen(struc, unit, error)
   close(unit, status='delete')

end subroutine test_incorrect_multiplicity

subroutine test_unphysical_multiplicity(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: filename = ".test-invalid-pmg-mol6.json"
   type(structure_type) :: struc
   integer :: unit

   open(file=filename, newunit=unit)
   write(unit, '(a)') &
      '{', &
      '  "@module": "pymatgen.core.structure",', &
      '  "@class": "Molecule",', &
      '  "charge": 0,', &
      '  "spin_multiplicity": 0,', &
      '  "sites": [', &
      '    {', &
      '      "name": "O", "label": "O",', &
      '      "species": [{"element": "O", "occu": 1}],', &
      '      "xyz": [1.1847029, 1.1150792, -0.0344641],', &
      '      "properties": {}', &
      '    },', &
      '    {', &
      '      "name": "H", "label": "H",', &
      '      "species": [{"element": "H", "occu": 1}],', &
      '      "xyz": [0.4939088, 0.9563767, 0.6340089],', &
      '      "properties": {}', &
      '    },', &
      '    {', &
      '      "name": "H", "label": "H",', &
      '      "species": [{"element": "H", "occu": 1}],', &
      '      "xyz": [2.0242676, 1.0811246, 0.4301417],', &
      '      "properties": {}', &
      '    }', &
      '  ],', &
      '  "properties": {}', &
      '}'
   rewind(unit)

   call read_pymatgen(struc, unit, error)
   close(unit, status='delete')

end subroutine test_unphysical_multiplicity

subroutine test_incorrect_sites(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: filename = ".test-invalid-pmg-mol7.json"
   type(structure_type) :: struc
   integer :: unit

   open(file=filename, newunit=unit)
   write(unit, '(a)') &
      '{', &
      '  "@module": "pymatgen.core.structure",', &
      '  "@class": "Molecule",', &
      '  "charge": 0,', &
      '  "spin_multiplicity": 1,', &
      '  "sites": "missing",', &
      '  "properties": {}', &
      '}'
   rewind(unit)

   call read_pymatgen(struc, unit, error)
   close(unit, status='delete')

end subroutine test_incorrect_sites

subroutine test_incorrect_sites_entry(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: filename = ".test-invalid-pmg-mol8.json"
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
      '    ["O", 1.1847029, 1.1150792,-0.0344641],', &
      '    ["H", 0.4939088, 0.9563767, 0.6340089],', &
      '    ["H", 2.0242676, 1.0811246, 0.4301417]', &
      '  ],', &
      '  "properties": {}', &
      '}'
   rewind(unit)

   call read_pymatgen(struc, unit, error)
   close(unit, status='delete')

end subroutine test_incorrect_sites_entry

subroutine test_incorrect_label(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: filename = ".test-invalid-pmg-mol9.json"
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
      '      "name": 8, "label": 8', &
      '      "species": [{"element": "O", "occu": 1}],', &
      '      "xyz": [1.1847029, 1.1150792, -0.0344641],', &
      '      "properties": {}', &
      '    },', &
      '    {', &
      '      "name": 1, "label": 1', &
      '      "species": [{"element": "H", "occu": 1}],', &
      '      "xyz": [0.4939088, 0.9563767, 0.6340089],', &
      '      "properties": {}', &
      '    },', &
      '    {', &
      '      "name": 1, "label": 1', &
      '      "species": [{"element": "H", "occu": 1}],', &
      '      "xyz": [2.0242676, 1.0811246, 0.4301417],', &
      '      "properties": {}', &
      '    }', &
      '  ],', &
      '  "properties": {}', &
      '}'
   rewind(unit)

   call read_pymatgen(struc, unit, error)
   close(unit, status='delete')

end subroutine test_incorrect_label

subroutine test_incorrect_xyz(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: filename = ".test-invalid-pmg-mol10.json"
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
      '      "name": "O", "label": "O",', &
      '      "species": [{"element": "O", "occu": 1}],', &
      '      "xyz": "1.1847029, 1.1150792, -0.0344641",', &
      '      "properties": {}', &
      '    },', &
      '    {', &
      '      "name": "H", "label": "H",', &
      '      "species": [{"element": "H", "occu": 1}],', &
      '      "xyz": "0.4939088, 0.9563767, 0.6340089",', &
      '      "properties": {}', &
      '    },', &
      '    {', &
      '      "name": "H", "label": "H",', &
      '      "species": [{"element": "H", "occu": 1}],', &
      '      "xyz": "2.0242676, 1.0811246, 0.4301417",', &
      '      "properties": {}', &
      '    }', &
      '  ],', &
      '  "properties": {}', &
      '}'
   rewind(unit)

   call read_pymatgen(struc, unit, error)
   close(unit, status='delete')

end subroutine test_incorrect_xyz

subroutine test_incorrect_xyz_size(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: filename = ".test-invalid-pmg-mol11.json"
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
      '      "name": "O", "label": "O",', &
      '      "species": [{"element": "O", "occu": 1}],', &
      '      "xyz": [1.1847029, 1.1150792],', &
      '      "properties": {}', &
      '    },', &
      '    {', &
      '      "name": "H", "label": "H",', &
      '      "species": [{"element": "H", "occu": 1}],', &
      '      "xyz": [0.4939088, 0.9563767, 0.6340089],', &
      '      "properties": {}', &
      '    },', &
      '    {', &
      '      "name": "H", "label": "H",', &
      '      "species": [{"element": "H", "occu": 1}],', &
      '      "xyz": [2.0242676, 1.0811246, 0.4301417],', &
      '      "properties": {}', &
      '    }', &
      '  ],', &
      '  "properties": {}', &
      '}'
   rewind(unit)

   call read_pymatgen(struc, unit, error)
   close(unit, status='delete')

end subroutine test_incorrect_xyz_size

subroutine test_incorrect_xyz_value(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: filename = ".test-invalid-pmg-mol12.json"
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
      '      "name": "O", "label": "O",', &
      '      "species": [{"element": "O", "occu": 1}],', &
      '      "xyz": [1.1847029, "1.1150792", -0.0344641],', &
      '      "properties": {}', &
      '    },', &
      '    {', &
      '      "name": "H", "label": "H",', &
      '      "species": [{"element": "H", "occu": 1}],', &
      '      "xyz": [0.4939088, 0.9563767, 0.6340089],', &
      '      "properties": {}', &
      '    },', &
      '    {', &
      '      "name": "H", "label": "H",', &
      '      "species": [{"element": "H", "occu": 1}],', &
      '      "xyz": [2.0242676, 1.0811246, 0.4301417],', &
      '      "properties": {}', &
      '    }', &
      '  ],', &
      '  "properties": {}', &
      '}'
   rewind(unit)

   call read_pymatgen(struc, unit, error)
   close(unit, status='delete')

end subroutine test_incorrect_xyz_value

subroutine test_incorrect_root(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: filename = ".test-invalid-pmg-mol13.json"
   type(structure_type) :: struc
   integer :: unit

   open(file=filename, newunit=unit)
   write(unit, '(a)') &
      '[{', &
      '  "@module": "pymatgen.core.structure",', &
      '  "@class": "Molecule",', &
      '  "charge": 0,', &
      '  "spin_multiplicity": 1,', &
      '  "sites": [', &
      '    {', &
      '      "name": "O", "label": "O",', &
      '      "species": [{"element": "O", "occu": 1}],', &
      '      "xyz": [1.1847029, 1.1150792, -0.0344641],', &
      '      "properties": {}', &
      '    },', &
      '    {', &
      '      "name": "H", "label": "H",', &
      '      "species": [{"element": "H", "occu": 1}],', &
      '      "xyz": [0.4939088, 0.9563767, 0.6340089],', &
      '      "properties": {}', &
      '    },', &
      '    {', &
      '      "name": "H", "label": "H",', &
      '      "species": [{"element": "H", "occu": 1}],', &
      '      "xyz": [2.0242676, 1.0811246, 0.4301417],', &
      '      "properties": {}', &
      '    }', &
      '  ],', &
      '  "properties": {}', &
      '}]'
   rewind(unit)

   call read_pymatgen(struc, unit, error)
   close(unit, status='delete')

end subroutine test_incorrect_root

subroutine test_incorrect_lattice(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: filename = ".test-invalid-pmg-sol1.json"
   type(structure_type) :: struc
   integer :: unit

   open(file=filename, newunit=unit)
   write(unit, '(a)') &
      '{', &
      '  "@module": "pymatgen.core.structure",', &
      '  "@class": "Structure",', &
      '  "charge": 0.0,', &
      '  "lattice": {', &
      '    "matrix": {}', &
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

end subroutine test_incorrect_lattice

subroutine test_incorrect_lattice_value(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: filename = ".test-invalid-pmg-sol2.json"
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
      '      ["5.59003664376222", 0.0, 0.0],', &
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

end subroutine test_incorrect_lattice_value

subroutine test_incorrect_lattice_size(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: filename = ".test-invalid-pmg-sol3.json"
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
      '      [5.59003664376222],', &
      '      [8.68089159045265],', &
      '      [8.68089159045265]', &
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

end subroutine test_incorrect_lattice_size

subroutine test_incorrect_lattice_dim(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: filename = ".test-invalid-pmg-sol4.json"
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

end subroutine test_incorrect_lattice_dim

subroutine test_incorrect_lattice_rank(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: filename = ".test-invalid-pmg-sol5.json"
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
      '      5.59003664376222,', &
      '      8.68089159045265,', &
      '      8.68089159045265', &
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

end subroutine test_incorrect_lattice_rank

subroutine test_incorrect_lattice_table(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: filename = ".test-invalid-pmg-sol6.json"
   type(structure_type) :: struc
   integer :: unit

   open(file=filename, newunit=unit)
   write(unit, '(a)') &
      '{', &
      '  "@module": "pymatgen.core.structure",', &
      '  "@class": "Structure",', &
      '  "charge": 0.0,', &
      '  "lattice": [],', &
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

end subroutine test_incorrect_lattice_table

end module test_read_pymatgen