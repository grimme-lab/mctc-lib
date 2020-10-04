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

module test_read
   use mctc_env_accuracy, only : wp
   use mctc_env_testing, only : new_unittest, unittest_type, error_type, check
   use mctc_io_read
   use mctc_io_structure, only : structure_type
   use mctc_io_filetype, only : get_filetype
   implicit none
   private

   public :: collect_read


contains


!> Collect all exported unit tests
subroutine collect_read(testsuite)

   !> Collection of tests
   type(unittest_type), allocatable, intent(out) :: testsuite(:)

   testsuite = [ &
      & new_unittest("valid-mol", test_mol), &
      & new_unittest("valid-sdf", test_sdf), &
      & new_unittest("valid-gen", test_gen), &
      & new_unittest("valid-pdb", test_pdb), &
      & new_unittest("valid-vasp", test_vasp), &
      & new_unittest("valid-coord", test_coord), &
      & new_unittest("valid-xyz", test_xyz) &
      & ]

end subroutine collect_read


subroutine test_mol(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   character(len=:), allocatable :: name
   integer :: unit

   name = get_name() // ".mol"

   open(file=name, newunit=unit)
   write(unit, '(a)') &
      "", &
      "  xtb     10012013503D", &
      "", &
      " 24 25  0     0  0            999 V2000", &
      "    1.0732    0.0489   -0.0757 C   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    2.5137    0.0126   -0.0758 N   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    3.3520    1.0959   -0.0753 C   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    4.6190    0.7303   -0.0755 N   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    4.5791   -0.6314   -0.0753 C   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    3.3013   -1.1026   -0.0752 C   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    2.9807   -2.4869   -0.0738 C   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    1.8253   -2.9004   -0.0758 O   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    4.1144   -3.3043   -0.0694 N   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    5.4517   -2.8562   -0.0724 C   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    6.3893   -3.6597   -0.0723 O   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    5.6624   -1.4768   -0.0749 N   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    7.0095   -0.9365   -0.0752 C   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    3.9206   -4.7409   -0.0616 C   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    0.7340    1.0879   -0.0750 H   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    0.7124   -0.4570    0.8234 H   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    0.7124   -0.4558   -0.9755 H   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    2.9930    2.1176   -0.0748 H   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    7.7653   -1.7263   -0.0759 H   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    7.1486   -0.3218    0.8197 H   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    7.1480   -0.3208   -0.9695 H   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    2.8650   -5.0232   -0.0583 H   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    4.4023   -5.1592    0.8284 H   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    4.4002   -5.1693   -0.9478 H   0  0  0  0  0  0  0  0  0  0  0  0", &
      "  1  2  1  0  0  0  0", &
      "  2  3  4  0  0  0  0", &
      "  3  4  4  0  0  0  0", &
      "  4  5  4  0  0  0  0", &
      "  2  6  1  0  0  0  0", &
      "  5  6  4  0  0  0  0", &
      "  6  7  1  0  0  0  0", &
      "  7  8  2  0  0  0  0", &
      "  7  9  1  0  0  0  0", &
      "  9 10  1  0  0  0  0", &
      " 10 11  2  0  0  0  0", &
      "  5 12  1  0  0  0  0", &
      " 10 12  1  0  0  0  0", &
      " 12 13  1  0  0  0  0", &
      "  9 14  1  0  0  0  0", &
      "  1 15  1  0  0  0  0", &
      "  1 16  1  0  0  0  0", &
      "  1 17  1  0  0  0  0", &
      "  3 18  1  0  0  0  0", &
      " 13 19  1  0  0  0  0", &
      " 13 20  1  0  0  0  0", &
      " 13 21  1  0  0  0  0", &
      " 14 22  1  0  0  0  0", &
      " 14 23  1  0  0  0  0", &
      " 14 24  1  0  0  0  0", &
      "M  END"
   close(unit)

   call read_structure(struc, name, error)

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

   open(file=name, newunit=unit)
   write(unit, '(a)') &
      "", &
      "  xtb     08072014173D", &
      "", &
      " 13 13  0     0  0            999 V2000", &
      "    1.4896   -2.2438   -0.0275 O   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    0.8475   -1.2058   -0.0075 C   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    1.4981    0.0466    0.2360 C   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    0.7744    1.2240    0.2564 C   0  0  0  0  0  0  0  0  0  0  0  0", &
      "   -0.5681    1.2354    0.0512 C   0  0  0  0  0  0  0  0  0  0  0  0", &
      "   -1.3469   -0.0099   -0.2052 C   0  0  0  0  0  0  0  0  0  0  0  0", &
      "   -0.5125   -1.2225   -0.2193 N   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    2.5680    0.0344    0.3998 H   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    1.2958    2.1567    0.4406 H   0  0  0  0  0  0  0  0  0  0  0  0", &
      "   -1.0960    2.1819    0.0742 H   0  0  0  0  0  0  0  0  0  0  0  0", &
      "   -1.8599    0.0606   -1.1755 H   0  0  0  0  0  0  0  0  0  0  0  0", &
      "   -2.1168   -0.1374    0.5699 H   0  0  0  0  0  0  0  0  0  0  0  0", &
      "   -0.9728   -2.1202   -0.3930 H   0  0  0  0  0  0  0  0  0  0  0  0", &
      "  1  2  2  0  0  0  0", &
      "  2  3  1  0  0  0  0", &
      "  3  4  2  0  0  0  0", &
      "  3  8  1  0  0  0  0", &
      "  4  5  1  0  0  0  0", &
      "  4  9  1  0  0  0  0", &
      "  5  6  1  0  0  0  0", &
      "  5 10  1  0  0  0  0", &
      "  6  7  1  0  0  0  0", &
      "  6 11  1  0  0  0  0", &
      "  6 12  1  0  0  0  0", &
      "  7  2  1  0  0  0  0", &
      "  7 13  1  0  0  0  0", &
      "M  CHG  1  5  1", &
      "M  END", &
      ">  <total energy / Eh>", &
      "-18.421705869411", &
      "", &
      ">  <gradient norm / Eh/a0>", &
      "0.000695317397", &
      "", &
      "$$$$"
   close(unit)

   call read_structure(struc, name, error)

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

   open(file=name, newunit=unit)
   write(unit, '(a)') &
      "HETATM    1  O   HOH     1      -4.900  22.628  -5.720  1.00  0.00           O  ", &
      "HETATM    2  O   HOH     2      -3.391  28.399  -5.286  1.00  0.00           O  ", &
      "HETATM    3  O   HOH     3      -1.344  27.910  -2.140  1.00  0.00           O  ", &
      "HETATM    4  O   HOH     4      -3.412  29.606  -2.541  1.00  0.00           O  ", &
      "HETATM    5  O   HOH     5      -1.321  28.109  -8.687  1.00  0.00           O  ", &
      "HETATM    6  O   HOH     6      -3.810  29.129  -8.232  1.00  0.00           O  ", &
      "HETATM    7  H   HOH     0      -4.922  23.438  -5.175  1.00  0.00           H  ", &
      "HETATM    8  H   HOH     0      -5.691  22.647  -6.276  1.00  0.00           H  ", &
      "HETATM    9  H   HOH     0      -2.824  28.827  -5.944  1.00  0.00           H  ", &
      "HETATM   10  H   HOH     0      -4.277  28.836  -5.406  1.00  0.00           H  ", &
      "HETATM   11  H   HOH     0      -1.979  28.651  -2.242  1.00  0.00           H  ", &
      "HETATM   12  H   HOH     0      -1.885  27.145  -2.375  1.00  0.00           H  ", &
      "HETATM   13  H   HOH     0      -3.380  30.352  -3.142  1.00  0.00           H  ", &
      "HETATM   14  H   HOH     0      -4.045  28.971  -2.911  1.00  0.00           H  ", &
      "HETATM   15  H   HOH     0      -0.902  28.921  -8.995  1.00  0.00           H  ", &
      "HETATM   16  H   HOH     0      -2.254  28.344  -8.517  1.00  0.00           H  ", &
      "HETATM   17  H   HOH     0      -4.487  29.278  -7.546  1.00  0.00           H  ", &
      "HETATM   18  H   HOH     0      -3.960  29.804  -8.896  1.00  0.00           H  ", &
      "END"
   close(unit)

   call read_structure(struc, name, error)

   open(file=name, newunit=unit)
   close(unit, status='delete')

end subroutine test_pdb


subroutine test_gen(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   character(len=:), allocatable :: name
   integer :: unit

   name = get_name() // ".gen"

   open(file=name, newunit=unit)
   write(unit, '(a)') &
      "25 C", &
      "C I H N", &
      "     1   1   1.0144755100E+00   4.6020000000E-04  -5.7394848000E-01", &
      "     2   1  -2.0032898000E-01  -4.5965000000E-04   1.1414560000E-01", &
      "     3   1  -2.0231545000E-01  -7.5279000000E-04   1.5088935800E+00", &
      "     4   1   9.9778745000E-01  -1.4728000000E-04   2.2196682500E+00", &
      "     5   1   2.2098457600E+00   7.7203000000E-04   1.5267653600E+00", &
      "     6   1   2.2204190100E+00   1.0915900000E-03   1.3029999000E-01", &
      "     7   2  -2.0632490900E+00  -1.7735200000E-03   2.5681651400E+00", &
      "     8   3   9.9055013000E-01  -3.9128000000E-04   3.3058222100E+00", &
      "     9   3   3.1477611200E+00   1.2445900000E-03   2.0779530500E+00", &
      "    10   3   3.1655004500E+00   1.8314400000E-03  -4.0749435000E-01", &
      "    11   3   1.0197382500E+00   6.8937000000E-04  -1.6618052000E+00", &
      "    12   3  -1.1377934900E+00  -9.4582000000E-04  -4.3447764000E-01", &
      "    13   4  -4.5879963100E+00  -1.7316200000E-03   4.0080783300E+00", &
      "    14   1  -5.4215082100E+00   1.0537153000E+00   3.4165937900E+00", &
      "    15   1  -5.1864905500E+00  -1.3318644400E+00   3.8317758400E+00", &
      "    16   1  -4.2778535800E+00   2.7405573000E-01   5.4173867200E+00", &
      "    17   3  -5.1820921200E+00   2.9428169000E-01   6.0466815900E+00", &
      "    18   3  -3.6095075800E+00  -4.9753141000E-01   5.8038290000E+00", &
      "    19   3  -3.7811344200E+00   1.2423401200E+00   5.5011568400E+00", &
      "    20   3  -6.4066973900E+00   1.1287638900E+00   3.9043224100E+00", &
      "    21   3  -4.9175136300E+00   2.0169453400E+00   3.5136058800E+00", &
      "    22   3  -5.5795960400E+00   8.4572812000E-01   2.3568167300E+00", &
      "    23   3  -6.1551031700E+00  -1.4245890400E+00   4.3488506700E+00", &
      "    24   3  -5.3462169300E+00  -1.5245333100E+00   2.7693575100E+00", &
      "    25   3  -4.5125813600E+00  -2.0930893200E+00   4.2288284200E+00"
   close(unit)

   call read_structure(struc, name, error)

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

   open(file=name, newunit=unit)
   write(unit, '(a)') &
      "$coord frac", &
      "    0.00000000000000      0.00000000000000      0.00000000000000      mg", &
      "    0.50000000000000      0.50000000000000      0.50000000000000      o", &
      "$periodic 3", &
      "$cell", &
      " 5.798338236 5.798338236 5.798338236 60. 60. 60.", &
      "$end"
   close(unit)

   call read_structure(struc, name, error)

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

   open(file=name, newunit=unit)
   write(unit, '(a)') &
      "Na Cl ", &
      " 1.0000000000000000", &
      "     5.6405599999999998    0.0000000000000000    0.0000000000000000", &
      "     0.0000000000000000    5.6405599999999998    0.0000000000000000", &
      "     0.0000000000000000    0.0000000000000000    5.6405599999999998", &
      "   4   4", &
      "Cartesian", &
      "  0.0000000000000000  0.0000000000000000  0.0000000000000000", &
      "  0.0000000000000000  2.8202799999999999  2.8202799999999999", &
      "  2.8202799999999999  0.0000000000000000  2.8202799999999999", &
      "  2.8202799999999999  2.8202799999999999  0.0000000000000000", &
      "  2.8202799999999999  2.8202799999999999  2.8202799999999999", &
      "  2.8202799999999999  0.0000000000000000  0.0000000000000000", &
      "  0.0000000000000000  2.8202799999999999  0.0000000000000000", &
      "  0.0000000000000000  0.0000000000000000  2.8202799999999999"
   close(unit)

   call read_structure(struc, name, error)

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

   open(file=name, newunit=unit)
   write(unit, '(a)') &
      "47", &
      "comment='Rivaroxaban (7)'", &
      "Cl        10.37060000      0.04000000      0.09340000", &
      "C          8.67250000      0.18330000      0.27690000", &
      "C          7.94790000      1.31690000      0.57310000", &
      "C          6.54940000      1.05420000      0.64450000", &
      "C          6.29630000     -0.25050000      0.40050000", &
      "S          7.66770000     -1.19010000      0.08600000", &
      "C          4.98660000     -0.91730000      0.38250000", &
      "O          4.86140000     -2.11520000      0.12170000", &
      "N          3.87770000     -0.10620000      0.64850000", &
      "C          2.53500000     -0.60710000      0.63610000", &
      "C          1.91560000     -0.48810000     -0.74950000", &
      "C          0.49590000     -1.00870000     -0.81480000", &
      "N         -0.29810000      0.18230000     -0.67240000", &
      "C          0.49690000      1.30240000     -0.90140000", &
      "O          0.17750000      2.48080000     -0.97180000", &
      "O          1.80070000      0.90110000     -1.08790000", &
      "C         -1.70220000      0.15010000     -0.43730000", &
      "C         -2.43380000      1.33750000     -0.41950000", &
      "C         -3.80920000      1.30600000     -0.18920000", &
      "C         -4.44670000      0.08710000      0.02210000", &
      "C         -3.72140000     -1.10060000      0.00540000", &
      "C         -2.34600000     -1.06890000     -0.22480000", &
      "H         -1.81830000     -2.01710000     -0.22660000", &
      "H         -4.17020000     -2.06550000      0.21590000", &
      "N         -5.86990000      0.05450000      0.26060000", &
      "C         -6.65860000     -1.04450000     -0.12130000", &
      "O         -6.28170000     -2.01920000     -0.77460000", &
      "C         -8.10170000     -1.01510000      0.37010000", &
      "O         -8.65500000      0.25530000      0.62130000", &
      "C         -7.80770000      0.99800000      1.48360000", &
      "C         -6.51980000      1.28190000      0.73820000", &
      "H         -5.85050000      1.82830000      1.41150000", &
      "H         -6.73450000      1.89190000     -0.14750000", &
      "H         -8.31270000      1.93460000      1.73940000", &
      "H         -7.63220000      0.44280000      2.41290000", &
      "H         -8.72500000     -1.49560000     -0.39170000", &
      "H         -8.16270000     -1.62590000      1.27870000", &
      "H         -4.34420000      2.25050000     -0.20770000", &
      "H         -1.99920000      2.31460000     -0.58100000", &
      "H          0.30010000     -1.74920000     -0.03410000", &
      "H          0.28380000     -1.44110000     -1.79930000", &
      "H          2.54530000     -0.98390000     -1.49690000", &
      "H          2.53380000     -1.65030000      0.97000000", &
      "H          1.97700000     -0.01220000      1.36770000", &
      "H          3.99640000      0.88090000      0.84660000", &
      "H          5.81840000      1.81990000      0.86610000", &
      "H          8.39390000      2.29070000      0.73180000"
   close(unit)

   call read_structure(struc, name, error)

   open(file=name, newunit=unit)
   close(unit, status='delete')

end subroutine test_xyz


function get_name() result(name)

   character(len=18) :: name

   real :: val

   call random_number(val)
   write(name, '(a, z8.8)') "mctc-test-", int(val*1.0e9)

end function get_name


end module test_read
