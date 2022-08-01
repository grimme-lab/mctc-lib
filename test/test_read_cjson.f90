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
      & new_unittest("valid3-cjson", test_valid3_cjson, should_fail=.not.with_json), &
      & new_unittest("valid4-cjson", test_valid4_cjson, should_fail=.not.with_json), &
      & new_unittest("valid5-cjson", test_valid5_cjson, should_fail=.not.with_json), &
      & new_unittest("valid6-cjson", test_valid6_cjson, should_fail=.not.with_json), &
      & new_unittest("invalid1-cjson", test_invalid1_cjson, should_fail=.true.), &
      & new_unittest("invalid2-cjson", test_invalid2_cjson, should_fail=.true.), &
      & new_unittest("invalid3-cjson", test_invalid3_cjson, should_fail=.true.), &
      & new_unittest("invalid4-cjson", test_invalid4_cjson, should_fail=.true.), &
      & new_unittest("invalid5-cjson", test_invalid5_cjson, should_fail=.true.), &
      & new_unittest("invalid6-cjson", test_invalid6_cjson, should_fail=.true.) &
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


subroutine test_valid3_cjson(error)

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
      '    }', &
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

end subroutine test_valid3_cjson


subroutine test_valid4_cjson(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      '{', &
      '  "chemical json": 1,', &
      '  "atoms": {', &
      '    "elements": {', &
      '      "number": [', &
      '        6, 7, 6, 7, 6, 6, 6, 8, 7, 6, 8, 7, 6, 6, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1', &
      '      ]', &
      '    },', &
      '    "coords": {', &
      '      "3d": [', &
      '         1.0731997649702911E+00,  4.8899989290949721E-02, -7.5699983421776973E-02,', &
      '         2.5136994495022558E+00,  1.2599997240612813E-02, -7.5799983399877077E-02,', &
      '         3.3519992659154081E+00,  1.0958997599990143E+00, -7.5299983509376570E-02,', &
      '         4.6189989884436962E+00,  7.3029984006504256E-01, -7.5499983465576764E-02,', &
      '         4.5790989971817559E+00, -6.3139986172404194E-01, -7.5299983509376570E-02,', &
      '         3.3012992770186567E+00, -1.1025997585317211E+00, -7.5199983531276451E-02,', &
      '         2.9806993472297307E+00, -2.4868994553714288E+00, -7.3799983837875047E-02,', &
      '         1.8252996002611557E+00, -2.9003993648153492E+00, -7.5799983399877077E-02,', &
      '         4.1143990989505834E+00, -3.3042992763616597E+00, -6.9399984801470568E-02,', &
      '         5.4516988060832432E+00, -2.8561993744951040E+00, -7.2399984144473614E-02,', &
      '         6.3892986007497967E+00, -3.6596991985294207E+00, -7.2299984166373524E-02,', &
      '         5.6623987599401575E+00, -1.4767996765823013E+00, -7.4899983596976152E-02,', &
      '         7.0094984649266268E+00, -9.3649979490745228E-01, -7.5199983531276451E-02,', &
      '         3.9205991413925863E+00, -4.7408989617477202E+00, -6.1599986509662634E-02,', &
      '         7.3399983925474632E-01,  1.0878997617510062E+00, -7.4999983575076257E-02,', &
      '         7.1239984398512435E-01, -4.5699989991746470E-01,  8.2339981967623732E-01,', &
      '         7.1239984398512435E-01, -4.5579990018026340E-01, -9.7549978636649193E-01,', &
      '         2.9929993445360430E+00,  2.1175995362477531E+00, -7.4799983618876062E-02,', &
      '         7.7652982994071955E+00, -1.7262996219420552E+00, -7.5899983377977168E-02,', &
      '         7.1485984344638682E+00, -3.2179992952612718E-01,  8.1969982048653345E-01,', &
      '         7.1479984345952676E+00, -3.2079992974512617E-01, -9.6949978768048573E-01,', &
      '         2.8649993725679135E+00, -5.0231988999243073E+00, -5.8299987232359275E-02,', &
      '         4.4022990359007768E+00, -5.1591988701404459E+00,  8.2839981858124223E-01,', &
      '         4.4001990363606742E+00, -5.1692988679285561E+00, -9.4779979243276369E-01', &
      '      ]', &
      '    }', &
      '  },', &
      '  "bonds": {', &
      '    "connections": {', &
      '      "index": [', &
      '        0, 1,', &
      '        1, 2,', &
      '        2, 3,', &
      '        3, 4,', &
      '        1, 5,', &
      '        4, 5,', &
      '        5, 6,', &
      '        6, 7,', &
      '        6, 8,', &
      '        8, 9,', &
      '        9, 10,', &
      '        4, 11,', &
      '        9, 11,', &
      '        11, 12,', &
      '        8, 13,', &
      '        0, 14,', &
      '        0, 15,', &
      '        0, 16,', &
      '        2, 17,', &
      '        12, 18,', &
      '        12, 19,', &
      '        12, 20,', &
      '        13, 21,', &
      '        13, 22,', &
      '        13, 23', &
      '      ]', &
      '    },', &
      '    "order": [', &
      '      1, 4, 4, 4, 1, 4, 1, 2, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1', &
      '    ]', &
      '  }', &
      '}'
   rewind(unit)

   call read_cjson(struc, unit, error)
   close(unit)
   if (allocated(error)) return
   call check(error, struc%nat, 24, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, 4, "Number of species does not match")
   if (allocated(error)) return
   call check(error, struc%nbd, 25, "Number of bonds does not match")
   if (allocated(error)) return

end subroutine test_valid4_cjson


subroutine test_valid5_cjson(error)

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

   call read_cjson(struc, unit, error)
   close(unit)
   if (allocated(error)) return
   call check(error, struc%nat, 2, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, 2, "Number of species does not match")
   if (allocated(error)) return
   call check(error, nint(struc%charge), -1, "Total charge does not match")
   if (allocated(error)) return

end subroutine test_valid5_cjson


subroutine test_valid6_cjson(error)

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
      '      "number": [', &
      '        7,', &
      '        7,', &
      '        7', &
      '      ]', &
      '    },', &
      '    "coords": {', &
      '      "3d": [', &
      '         3.6361808414857721E-01,', &
      '         1.9287266130863627E+00,', &
      '        -1.7850498831821635E+00,', &
      '         8.2217629145179161E-01,', &
      '         2.4066501990670561E+00,', &
      '        -2.7896663819784173E+00,', &
      '        -9.4568423260748616E-02,', &
      '         1.4516946870018026E+00,', &
      '        -7.7682289506097102E-01', &
      '      ]', &
      '    }', &
      '  },', &
      '  "properties": {', &
      '    "totalCharge": -1', &
      '  }', &
      '}'
   rewind(unit)

   call read_cjson(struc, unit, error)
   close(unit)
   if (allocated(error)) return
   call check(error, struc%nat, 3, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, 1, "Number of species does not match")
   if (allocated(error)) return
   call check(error, nint(struc%charge), -1, "Total charge does not match")
   if (allocated(error)) return

end subroutine test_valid6_cjson


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


subroutine test_invalid4_cjson(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      '{', &
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

end subroutine test_invalid4_cjson


subroutine test_invalid5_cjson(error)

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
      '      "numbers": [  1,   6,   1,   1,   6,   1,   1,   1 ]', &
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

end subroutine test_invalid5_cjson


subroutine test_invalid6_cjson(error)

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
      '    "gama":  90.0', &
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

end subroutine test_invalid6_cjson


end module test_read_cjson
