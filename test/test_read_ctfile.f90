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

module test_read_ctfile
   use mctc_env_testing, only : new_unittest, unittest_type, error_type, check
   use mctc_io_read_ctfile
   use mctc_io_structure
   implicit none
   private

   public :: collect_read_ctfile


contains


!> Collect all exported unit tests
subroutine collect_read_ctfile(testsuite)

   !> Collection of tests
   type(unittest_type), allocatable, intent(out) :: testsuite(:)

   testsuite = [ &
      & new_unittest("valid1-mol", test_valid1_mol), &
      & new_unittest("valid2-mol", test_valid2_mol), &
      & new_unittest("valid3-mol", test_valid3_mol), &
      & new_unittest("valid4-mol", test_valid4_mol), &
      & new_unittest("invalid1-mol", test_invalid1_mol, should_fail=.true.), &
      & new_unittest("invalid2-mol", test_invalid2_mol, should_fail=.true.), &
      & new_unittest("invalid3-mol", test_invalid3_mol, should_fail=.true.), &
      & new_unittest("invalid4-mol", test_invalid4_mol, should_fail=.true.), &
      & new_unittest("invalid5-mol", test_invalid5_mol, should_fail=.true.), &
      & new_unittest("invalid6-mol", test_invalid6_mol, should_fail=.true.), &
      & new_unittest("invalid7-mol", test_invalid7_mol, should_fail=.true.), &
      & new_unittest("invalid8-mol", test_invalid8_mol, should_fail=.true.), &
      & new_unittest("invalid9-mol", test_invalid9_mol, should_fail=.true.), &
      & new_unittest("maestro-mol", test_maestro_mol), &
      & new_unittest("valid1-sdf", test_valid1_sdf), &
      & new_unittest("valid2-sdf", test_valid2_sdf), &
      & new_unittest("valid3-sdf", test_valid3_sdf), &
      & new_unittest("invalid1-sdf", test_invalid1_sdf, should_fail=.true.), &
      & new_unittest("invalid2-sdf", test_invalid2_sdf, should_fail=.true.), &
      & new_unittest("invalid3-sdf", test_invalid3_sdf, should_fail=.true.), &
      & new_unittest("invalid4-sdf", test_invalid4_sdf, should_fail=.true.), &
      & new_unittest("invalid5-sdf", test_invalid5_sdf, should_fail=.true.), &
      & new_unittest("invalid6-sdf", test_invalid6_sdf, should_fail=.true.), &
      & new_unittest("invalid7-sdf", test_invalid7_sdf, should_fail=.true.), &
      & new_unittest("unsupported1-sdf", test_unsupported1_sdf, should_fail=.true.), &
      & new_unittest("unsupported2-sdf", test_unsupported2_sdf, should_fail=.true.), &
      & new_unittest("unsupported3-sdf", test_unsupported3_sdf, should_fail=.true.) &
      & ]

end subroutine collect_read_ctfile


subroutine test_valid1_mol(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "", &
      "  Mrv1823 10191918163D          ", &
      "", &
      " 12 12  0  0  0  0            999 V2000", &
      "   -0.0090   -0.0157   -0.0000 C   0  0  0  0  0  0  0  0  0  0  0  0", &
      "   -0.7131    1.2038   -0.0000 C   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    1.3990   -0.0157   -0.0000 C   0  0  0  0  0  0  0  0  0  0  0  0", &
      "   -0.0090    2.4232   -0.0000 C   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    2.1031    1.2038   -0.0000 C   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    1.3990    2.4232    0.0000 C   0  0  0  0  0  0  0  0  0  0  0  0", &
      "   -0.5203   -0.9011   -0.0000 H   0  0  0  0  0  0  0  0  0  0  0  0", &
      "   -1.7355    1.2038    0.0000 H   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    1.9103   -0.9011    0.0000 H   0  0  0  0  0  0  0  0  0  0  0  0", &
      "   -0.5203    3.3087    0.0000 H   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    3.1255    1.2038    0.0000 H   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    1.9103    3.3087   -0.0000 H   0  0  0  0  0  0  0  0  0  0  0  0", &
      "  2  1  4  0  0  0  0", &
      "  3  1  4  0  0  0  0", &
      "  4  2  4  0  0  0  0", &
      "  5  3  4  0  0  0  0", &
      "  6  4  4  0  0  0  0", &
      "  6  5  4  0  0  0  0", &
      "  1  7  1  0  0  0  0", &
      "  2  8  1  0  0  0  0", &
      "  3  9  1  0  0  0  0", &
      "  4 10  1  0  0  0  0", &
      "  5 11  1  0  0  0  0", &
      "  6 12  1  0  0  0  0", &
      "M  END"
   rewind(unit)

   call read_molfile(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, .not.allocated(struc%comment), "Empty comment line should not be saved")
   if (allocated(error)) return
   call check(error, struc%nat, 12, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, 2, "Number of species does not match")
   if (allocated(error)) return
   call check(error, struc%nbd, 12, "Number of bonds does not match")
   if (allocated(error)) return

end subroutine test_valid1_mol


subroutine test_valid2_mol(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "", &
      "  xtb     09072013503D", &
      " xtb: 6.3.2 (b5103a3)", &
      " 24 25  0     0  0            999 V2000", &
      "    1.0732    0.0488   -0.0757 C   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    2.5137    0.0126   -0.0758 N   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    3.3520    1.0959   -0.0753 C*  0  0  0  0  0  0  0  0  0  0  0  0", &
      "    4.6190    0.7303   -0.0755 N   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    4.5791   -0.6314   -0.0753 C*  0  0  0  0  0  0  0  0  0  0  0  0", &
      "    3.3013   -1.1026   -0.0752 C   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    2.9807   -2.4869   -0.0738 C   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    1.8253   -2.9004   -0.0758 18O 0  0  0  0  0  0  0  0  0  0  0  0", &
      "    4.1144   -3.3043   -0.0694 N   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    5.4517   -2.8562   -0.0723 C*  0  0  0  0  0  0  0  0  0  0  0  0", &
      "    6.3893   -3.6597   -0.0723 O   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    5.6624   -1.4768   -0.0749 N   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    7.0095   -0.9365   -0.0752 C   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    3.9206   -4.7409   -0.0616 C   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    0.7340    1.0879   -0.0750 D   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    0.7124   -0.4570    0.8234 D   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    0.7124   -0.4558   -0.9755 D   0  0  0  0  0  0  0  0  0  0  0  0", &
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
   rewind(unit)

   call read_molfile(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, .not.allocated(struc%comment), "Empty comment line should not be saved")
   if (allocated(error)) return
   call check(error, struc%nat, 24, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, 7, "Number of species does not match")
   if (allocated(error)) return
   call check(error, struc%nbd, 25, "Number of bonds does not match")
   if (allocated(error)) return

end subroutine test_valid2_mol


subroutine test_valid3_mol(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "Compound 1017", &
      "     RDKit          3D", &
      "", &
      "  0  0  0  0  0  0  0  0  0  0999 V3000", &
      "M  V30 BEGIN CTAB", &
      "M  V30 COUNTS 28 29 0 0 0", &
      "M  V30 BEGIN ATOM", &
      "M  V30 1 O -0.139802 -1.830034 0.709675 0", &
      "M  V30 2 C -0.316756 -0.419558 0.542247 0 CFG=2", &
      "M  V30 3 C -1.799960 -0.109911 0.488996 0", &
      "M  V30 4 C 0.386061 0.062191 -0.739099 0", &
      "M  V30 5 C -2.697868 -0.951923 -0.184715 0", &
      "M  V30 6 C -2.294606 1.060665 1.085325 0", &
      "M  V30 7 C 1.894644 0.013015 -0.632932 0", &
      "M  V30 8 C -4.055760 -0.636028 -0.254512 0", &
      "M  V30 9 C -3.653160 1.375870 1.014385 0", &
      "M  V30 10 N 2.442088 -0.971958 0.125897 0", &
      "M  V30 11 C 2.672560 0.914435 -1.356061 0", &
      "M  V30 12 C -4.532743 0.527936 0.344354 0", &
      "M  V30 13 C 3.791786 -1.035730 0.183623 0", &
      "M  V30 14 C 4.059439 0.822550 -1.276192 0", &
      "M  V30 15 C 4.633339 -0.168766 -0.490541 0", &
      "M  V30 16 H 0.832285 -1.959785 0.673224 0", &
      "M  V30 17 H 0.117435 0.060361 1.428520 0", &
      "M  V30 18 H 0.082070 1.094331 -0.955596 0", &
      "M  V30 19 H 0.094415 -0.560551 -1.594197 0", &
      "M  V30 20 H -2.337706 -1.867971 -0.649441 0", &
      "M  V30 21 H -1.623344 1.733954 1.613637 0", &
      "M  V30 22 H -4.740022 -1.302611 -0.772469 0", &
      "M  V30 23 H -4.025651 2.281920 1.484883 0", &
      "M  V30 24 H 2.212518 1.677511 -1.976085 0", &
      "M  V30 25 H -5.590493 0.771200 0.292658 0", &
      "M  V30 26 H 4.192367 -1.831268 0.805865 0", &
      "M  V30 27 H 4.686889 1.517092 -1.827730 0", &
      "M  V30 28 H 5.709975 -0.266939 -0.409811 0", &
      "M  V30 END ATOM", &
      "M  V30 BEGIN BOND", &
      "M  V30 1 1 2 1", &
      "M  V30 2 1 2 3", &
      "M  V30 3 1 2 4", &
      "M  V30 4 2 3 5", &
      "M  V30 5 1 3 6", &
      "M  V30 6 1 4 7", &
      "M  V30 7 1 5 8", &
      "M  V30 8 2 6 9", &
      "M  V30 9 2 7 10", &
      "M  V30 10 1 7 11", &
      "M  V30 11 2 8 12", &
      "M  V30 12 1 10 13", &
      "M  V30 13 2 11 14", &
      "M  V30 14 2 13 15", &
      "M  V30 15 1 9 12", &
      "M  V30 16 1 14 15", &
      "M  V30 17 1 1 16", &
      "M  V30 18 1 2 17 CFG=1", &
      "M  V30 19 1 4 18", &
      "M  V30 20 1 4 19", &
      "M  V30 21 1 5 20", &
      "M  V30 22 1 6 21", &
      "M  V30 23 1 8 22", &
      "M  V30 24 1 9 23", &
      "M  V30 25 1 11 24", &
      "M  V30 26 1 12 25", &
      "M  V30 27 1 13 26", &
      "M  V30 28 1 14 27", &
      "M  V30 29 1 15 28", &
      "M  V30 END BOND", &
      "M  V30 BEGIN COLLECTION", &
      "M  V30 MDLV30/STERAC1 ATOMS=(1 2)", &
      "M  V30 END COLLECTION", &
      "M  V30 END CTAB", &
      "M  END"
   rewind(unit)

   call read_molfile(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   if (allocated(error)) return
   call check(error, struc%nat, 28, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, 4, "Number of species does not match")
   if (allocated(error)) return
   call check(error, struc%nbd, 29, "Number of bonds does not match")
   if (allocated(error)) return

end subroutine test_valid3_mol


subroutine test_valid4_mol(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "Compound 1016", &
      "     RDKit          3D", &
      "", &
      "  0  0  0  0  0  0  0  0  0  0999 V3000", &
      "M  V30 BEGIN CTAB", &
      "M  V30 COUNTS 21 21 0 0 0", &
      "M  V30 BEGIN ATOM", &
      "M  V30 1 O -1.686687 0.989923 -1.309588 0", &
      "M  V30 2 C -1.641906 0.550463 0.052070 0 CFG=2", &
      "M  V30 3 C -0.870884 -0.770712 0.137052 0", &
      "M  V30 4 C -3.076970 0.377330 0.535110 0", &
      "M  V30 5 C 0.620384 -0.546719 0.078757 0", &
      "M  V30 6 N 1.280723 -0.500634 1.262879 0", &
      "M  V30 7 C 1.265020 -0.375370 -1.143956 0", &
      "M  V30 8 C 2.615192 -0.292093 1.219648 0", &
      "M  V30 9 C 2.639873 -0.159926 -1.155509 0", &
      "M  V30 10 C 3.332076 -0.118715 0.048112 0", &
      "M  V30 11 H -2.097713 1.871989 -1.311606 0", &
      "M  V30 12 H -1.148607 1.330233 0.644439 0", &
      "M  V30 13 H -1.164397 -1.449978 -0.672900 0", &
      "M  V30 14 H -1.097610 -1.279061 1.082626 0", &
      "M  V30 15 H -3.629187 1.318763 0.441977 0", &
      "M  V30 16 H -3.109338 0.058726 1.581416 0", &
      "M  V30 17 H -3.609000 -0.361105 -0.074891 0", &
      "M  V30 18 H 0.702849 -0.400424 -2.072665 0", &
      "M  V30 19 H 3.109786 -0.266610 2.186219 0", &
      "M  V30 20 H 3.163550 -0.022841 -2.096933 0", &
      "M  V30 21 H 4.402843 0.046761 0.073357 0", &
      "M  V30 END ATOM", &
      "M  V30 BEGIN BOND", &
      "M  V30 1 1 2 1", &
      "M  V30 2 1 2 3", &
      "M  V30 3 1 2 4", &
      "M  V30 4 1 3 5", &
      "M  V30 5 2 5 6", &
      "M  V30 6 1 5 7", &
      "M  V30 7 1 6 8", &
      "M  V30 8 2 7 9", &
      "M  V30 9 2 8 10", &
      "M  V30 10 1 9 10", &
      "M  V30 11 1 1 11", &
      "M  V30 12 1 2 12 CFG=1", &
      "M  V30 13 1 3 13", &
      "M  V30 14 1 3 14", &
      "M  V30 15 1 4 15", &
      "M  V30 16 1 4 16", &
      "M  V30 17 1 4 17", &
      "M  V30 18 1 7 18", &
      "M  V30 19 1 8 19", &
      "M  V30 20 1 9 20", &
      "M  V30 21 1 10 21", &
      "M  V30 END BOND", &
      "M  V30 BEGIN COLLECTION", &
      "M  V30 MDLV30/STERAC1 ATOMS=(1 2)", &
      "M  V30 END COLLECTION", &
      "M  V30 END CTAB", &
      "M  END"
   rewind(unit)

   call read_molfile(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   if (allocated(error)) return
   call check(error, struc%nat, 21, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, 4, "Number of species does not match")
   if (allocated(error)) return
   call check(error, struc%nbd, 21, "Number of bonds does not match")
   if (allocated(error)) return

end subroutine test_valid4_mol


subroutine test_invalid1_mol(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      " OpenBabel10191918023D", &
      "", &
      " 12 12  0  0  0  0  0  0  0  0999 V2000", &
      "   -0.0090   -0.0157    0.0000 C   0  0  0  0  0  0  0  0  0  0  0  0", &
      "   -0.7131    1.2038    0.0000 C   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    1.3990   -0.0157    0.0000 C   0  0  0  0  0  0  0  0  0  0  0  0", &
      "   -0.0090    2.4232    0.0000 C   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    2.1031    1.2038    0.0000 C   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    1.3990    2.4232    0.0000 C   0  0  0  0  0  0  0  0  0  0  0  0", &
      "   -0.5203   -0.9011    0.0000 H   0  0  0  0  0  0  0  0  0  0  0  0", &
      "   -1.7355    1.2038    0.0000 H   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    1.9103   -0.9011    0.0000 H   0  0  0  0  0  0  0  0  0  0  0  0", &
      "   -0.5203    3.3087    0.0000 H   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    3.1255    1.2038    0.0000 H   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    1.9103    3.3087    0.0000 H   0  0  0  0  0  0  0  0  0  0  0  0", &
      "  1  2  2  0  0  0  0", &
      "  1  3  1  0  0  0  0", &
      "  1  7  1  0  0  0  0", &
      "  2  4  1  0  0  0  0", &
      "  2  8  1  0  0  0  0", &
      "  3  5  2  0  0  0  0", &
      "  3  9  1  0  0  0  0", &
      "  4  6  2  0  0  0  0", &
      "  4 10  1  0  0  0  0", &
      "  5  6  1  0  0  0  0", &
      "  5 11  1  0  0  0  0", &
      "  6 12  1  0  0  0  0", &
      "M  END"
   rewind(unit)

   call read_molfile(struc, unit, error)
   close(unit)

end subroutine test_invalid1_mol


subroutine test_invalid2_mol(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "", &
      "          10191918023D", &
      "", &
      " 12 12  0  0  0  0  0  0  0  0999 V2000", &
      "   -0.0090   -0.0157    0.0000 C   0  0  0  0  0  0  0  0  0  0  0  0", &
      "   -0.7131    1.2038    0.0000 C   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    1.3990   -0.0157    0.0000 C   0  0  0  0  0  0  0  0  0  0  0  0", &
      "   -0.0090    2.4232    0.0000 C   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    2.1031    1.2038    0.0000 C   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    1.3990    2.4232    0.0000 C   0  0  0  0  0  0  0  0  0  0  0  0", &
      "M  END"
   rewind(unit)

   call read_molfile(struc, unit, error)
   close(unit)

end subroutine test_invalid2_mol


subroutine test_invalid3_mol(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "", &
      "          10191918163D          ", &
      "", &
      " 12 12  0  0  0  0            999 V2000", &
      "   -0.0090   -0.0157   -0.0000 *** 0  0  0  0  0  0  0  0  0  0  0  0", &
      "   -0.7131    1.2038   -0.0000 *** 0  0  0  0  0  0  0  0  0  0  0  0", &
      "    1.3990   -0.0157   -0.0000 *** 0  0  0  0  0  0  0  0  0  0  0  0", &
      "   -0.0090    2.4232   -0.0000 *** 0  0  0  0  0  0  0  0  0  0  0  0", &
      "    2.1031    1.2038   -0.0000 *** 0  0  0  0  0  0  0  0  0  0  0  0", &
      "    1.3990    2.4232    0.0000 *** 0  0  0  0  0  0  0  0  0  0  0  0", &
      "   -0.5203   -0.9011   -0.0000 H   0  0  0  0  0  0  0  0  0  0  0  0", &
      "   -1.7355    1.2038    0.0000 H   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    1.9103   -0.9011    0.0000 H   0  0  0  0  0  0  0  0  0  0  0  0", &
      "   -0.5203    3.3087    0.0000 H   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    3.1255    1.2038    0.0000 H   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    1.9103    3.3087   -0.0000 H   0  0  0  0  0  0  0  0  0  0  0  0", &
      "  2  1  4  0  0  0  0", &
      "  3  1  4  0  0  0  0", &
      "  4  2  4  0  0  0  0", &
      "  5  3  4  0  0  0  0", &
      "  6  4  4  0  0  0  0", &
      "  6  5  4  0  0  0  0", &
      "  1  7  1  0  0  0  0", &
      "  2  8  1  0  0  0  0", &
      "  3  9  1  0  0  0  0", &
      "  4 10  1  0  0  0  0", &
      "  5 11  1  0  0  0  0", &
      "  6 12  1  0  0  0  0", &
      "M  END"
   rewind(unit)

   call read_molfile(struc, unit, error)
   close(unit)

end subroutine test_invalid3_mol


subroutine test_invalid4_mol(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "", &
      "  Mrv1823 10191918163D          ", &
      "", &
      " 12 12  0  0  0  0            999 V2000", &
      "   -0.0090   -0.0157   -0.0000 C   0  0  0  0  0  0  0  0  0  0  0  0", &
      "   -0.7131    1.2038   -0.0000 C   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    1.3990   -0.0157   -0.0000 C   0  0  0  0  0  0  0  0  0  0  0  0", &
      "   -0.0090    2.4232   -0.0000 C   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    2.1031    1.2038   -0.0000 C   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    1.3990    2.4232    0.0000 C   0  0  0  0  0  0  0  0  0  0  0  0", &
      "   -0.5203   -0.9011   -0.0000 H   0  0  0  0  0  0  0  0  0  0  0  0", &
      "   -1.7355    1.2038    0.0000 H   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    1.9103   -0.9011    0.0000 H   0  0  0  0  0  0  0  0  0  0  0  0", &
      "   -0.5203    3.3087    0.0000 H   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    3.1255    1.2038    0.0000 H   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    1.9103    3.3087   -0.0000 H   0  0  0  0  0  0  0  0  0  0  0  0", &
      "  2  1  4  0  0  0  0", &
      "  3  1  4  0  0  0  0", &
      "  4  2  4  0  0  0  0", &
      "  5  3  4  0  0  0  0", &
      "  6  4  4  0  0  0  0", &
      "  6  5  4  0  0  0  0", &
      "  1  7  1  0  0  0  0", &
      "  2  8  1  0  0  0  0", &
      "  3  9  1  0  0  0  0", &
      "  4 10  1  0  0  0  0", &
      "  5 11  1  0  0  0  0", &
      "  6 12  1  0  0  0  0", &
      "M  CHG  3   1   1   3   b   2  -1", &
      "M  END"
   rewind(unit)

   call read_molfile(struc, unit, error)
   close(unit)

end subroutine test_invalid4_mol


subroutine test_invalid5_mol(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "", &
      "  Mrv1823 10191918163D          ", &
      "", &
      "  0 12  0  0  0  0            999 V2000", &
      "  2  1  4  0  0  0  0", &
      "  3  1  4  0  0  0  0", &
      "  4  2  4  0  0  0  0", &
      "  5  3  4  0  0  0  0", &
      "  6  4  4  0  0  0  0", &
      "  6  5  4  0  0  0  0", &
      "  1  7  1  0  0  0  0", &
      "  2  8  1  0  0  0  0", &
      "  3  9  1  0  0  0  0", &
      "  4 10  1  0  0  0  0", &
      "  5 11  1  0  0  0  0", &
      "  6 12  1  0  0  0  0", &
      "M  END"
   rewind(unit)

   call read_molfile(struc, unit, error)
   close(unit)

end subroutine test_invalid5_mol


subroutine test_invalid6_mol(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "Compound 11", &
      "     RDKit          3D", &
      "", &
      "  0  0  0  0  0  0  0  0  0  0999 V3000", &
      "M  V30 BEGIN CTAB", &
      "M  V30 COUNTS 16 16 0 0 0", &
      "M  V30 BEGIN ATOM", &
      "M  V30 1 O -2.821131 -0.276238 -0.753131 0", &
      "M  V30 2 C -2.076407 0.000289 0.175864 0", &
      "M  V30 3 O -2.469860 0.872693 1.126516 0", &
      "M  V30 4 C -0.648307 -0.508439 0.384207 0", &
      "M  V30 5 N -0.553725 -1.908221 -0.092137 0", &
      "M  V30 6 C 0.306640 0.448659 -0.352110 0", &
      "M  V30 7 C 1.764852 0.167437 -0.097096 0", &
      "M  V30 8 C 2.575104 0.984442 0.587951 0", &
      "M  V30 9 H -3.391314 1.091514 0.873612 0", &
      "M  V30 10 H -0.438887 -0.513473 1.460318 0", &
      "M  V30 11 H 0.421206 -2.197466 -0.111774 0", &
      "M  V30 12 H -0.893195 -1.946576 -1.055443 0", &
      "M  V30 13 H 0.073377 1.483235 -0.066299 0", &
      "M  V30 14 H 0.129860 0.396798 -1.434760 0", &
      "M  V30 15 H 2.179323 -0.745345 -0.519251 0", &
      "M  V30 16 H 2.219589 1.914253 1.021797 0", &
      "M  V30 17 H 3.622875 0.736438 0.730780 0", &
      "M  V30 END ATOM", &
      "M  V30 BEGIN BOND", &
      "M  V30 1 2 1 2", &
      "M  V30 2 1 2 3", &
      "M  V30 3 1 4 2", &
      "M  V30 4 1 4 5", &
      "M  V30 5 1 4 6", &
      "M  V30 6 1 6 7", &
      "M  V30 7 2 7 8 CFG=2", &
      "M  V30 8 1 3 9", &
      "M  V30 9 1 4 10 CFG=1", &
      "M  V30 10 1 5 11", &
      "M  V30 11 1 5 12", &
      "M  V30 12 1 6 13", &
      "M  V30 13 1 6 14", &
      "M  V30 14 1 7 15", &
      "M  V30 15 1 8 16", &
      "M  V30 16 1 8 17", &
      "M  V30 END BOND", &
      "M  V30 END CTAB", &
      "M  END"
   rewind(unit)

   call read_molfile(struc, unit, error)
   close(unit)

end subroutine test_invalid6_mol


subroutine test_invalid7_mol(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "Compound 11", &
      "     RDKit          3D", &
      "", &
      "  0  0  0  0  0  0  0  0  0  0999 V3000", &
      "M  V30 BEGIN CTAB", &
      "M  V30 COUNTS 17 16 0 0 0", &
      "M  V30 BEGIN ATOM", &
      "M  V30 1 O -2.821131 -0.276238 -0.753131 0", &
      "M  V30 2 C -2.076407 0.000289 0.175864 0", &
      "M  V30 3 O -2.469860 0.872693 1.126516 0", &
      "M  V30 4 C -0.648307 -0.508439 0.384207 0", &
      "M  V30 5 N -0.553725 -1.908221 -0.092137 0", &
      "M  V30 6 C 0.306640 0.448659 -0.352110 0", &
      "M  V30 7 C 1.764852 0.167437 -0.097096 0", &
      "M  V30 8 C 2.575104 0.984442 0.587951 0", &
      "M  V30 9 H -3.391314 1.091514 0.873612 0", &
      "M  V30 10 H -0.438887 -0.513473 1.460318 0", &
      "M  V30 11 H 0.421206 -2.197466 -0.111774 0", &
      "M  V30 12 H -0.893195 -1.946576 -1.055443 0", &
      "M  V30 13 H 0.073377 1.483235 -0.066299 0", &
      "M  V30 14 H 0.129860 0.396798 -1.434760 0", &
      "M  V30 15 H 2.179323 -0.745345 -0.519251 0", &
      "M  V30 16 H 2.219589 1.914253 1.021797 0", &
      "M  V30 17 H 3.622875 0.736438 0.730780 0", &
      "M  V30 END ATOM", &
      "M  V30 BEGIN BOND", &
      "M  V30 1 2 1 2", &
      "M  V30 2 1 2 3", &
      "M  V30 3 1 4 2", &
      "M  V30 4 1 4 5", &
      "M  V30 5 1 4 6", &
      "M  V30 6 1 6 7", &
      "M  V30 7 2 7 8 CFG=2", &
      "M  V30 8 1 3 9", &
      "M  V30 9 1 4 10 CFG=1", &
      "M  V30 10 1 5 11", &
      "M  V30 11 1 5 12", &
      "M  V30 12 1 6 13", &
      "M  V30 13 1 6 14", &
      "M  V30 14 1 7 15", &
      "M  V30 15 1 8 16", &
      "M  V30 16 1 8 17", &
      "M  V30 END BOND", &
      "M  V30 BEGIN INVALID", &
      "M  V30 END INVALID", &
      "M  V30 END CTAB", &
      "M  END"
   rewind(unit)

   call read_molfile(struc, unit, error)
   close(unit)

end subroutine test_invalid7_mol


subroutine test_invalid8_mol(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "Compound 11", &
      "     RDKit          3D", &
      "", &
      "  0  0  0  0  0  0  0  0  0  0999 V3000", &
      "M  V30 BEGIN CTAB", &
      "M  V30 COUNTS 17 15 0 0 0", &
      "M  V30 BEGIN ATOM", &
      "M  V30 1 O -2.821131 -0.276238 -0.753131 0", &
      "M  V30 2 C -2.076407 0.000289 0.175864 0", &
      "M  V30 3 O -2.469860 0.872693 1.126516 0", &
      "M  V30 4 C -0.648307 -0.508439 0.384207 0", &
      "M  V30 5 N -0.553725 -1.908221 -0.092137 0", &
      "M  V30 6 C 0.306640 0.448659 -0.352110 0", &
      "M  V30 7 C 1.764852 0.167437 -0.097096 0", &
      "M  V30 8 C 2.575104 0.984442 0.587951 0", &
      "M  V30 9 H -3.391314 1.091514 0.873612 0", &
      "M  V30 10 H -0.438887 -0.513473 1.460318 0", &
      "M  V30 11 H 0.421206 -2.197466 -0.111774 0", &
      "M  V30 12 H -0.893195 -1.946576 -1.055443 0", &
      "M  V30 13 H 0.073377 1.483235 -0.066299 0", &
      "M  V30 14 H 0.129860 0.396798 -1.434760 0", &
      "M  V30 15 H 2.179323 -0.745345 -0.519251 0", &
      "M  V30 16 H 2.219589 1.914253 1.021797 0", &
      "M  V30 17 H 3.622875 0.736438 0.730780 0", &
      "M  V30 END ATOM", &
      "M  V30 BEGIN BOND", &
      "M  V30 1 2 1 2", &
      "M  V30 2 1 2 3", &
      "M  V30 3 1 4 2", &
      "M  V30 4 1 4 5", &
      "M  V30 5 1 4 6", &
      "M  V30 6 1 6 7", &
      "M  V30 7 2 7 8 CFG=2", &
      "M  V30 8 1 3 9", &
      "M  V30 9 1 4 10 CFG=1", &
      "M  V30 10 1 5 11", &
      "M  V30 11 1 5 12", &
      "M  V30 12 1 6 13", &
      "M  V30 13 1 6 14", &
      "M  V30 14 1 7 15", &
      "M  V30 15 1 8 16", &
      "M  V30 16 1 8 17", &
      "M  V30 END BOND", &
      "M  V30 END CTAB", &
      "M  END"
   rewind(unit)

   call read_molfile(struc, unit, error)
   close(unit)

end subroutine test_invalid8_mol


subroutine test_invalid9_mol(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "Compound 11", &
      "     RDKit          3D", &
      "", &
      "  0  0  0  0  0  0  0  0  0  0999 V3000", &
      "M  V30 BEGIN CTAB", &
      "M  V30 COUNTS 17 16 0 0 0", &
      "M  V30 BEGIN ATOM", &
      "M  V30 1 O -2.821131 -0.276238 -0.753131 0", &
      "M  V30 2 C -2.076407 0.000289 0.175864 0", &
      "M  V30 3 O -2.469860 0.872693 1.126516 0", &
      "M  V30 4 C -0.648307 -0.508439 0.384207 0", &
      "M  V30 5 N -0.553725 -1.908221 -0.092137 0", &
      "M  V30 6 C 0.306640 0.448659 -0.352110 0", &
      "M  V30 7 C 1.764852 0.167437 -0.097096 0", &
      "M  V30 8 C 2.575104 0.984442 0.587951 0", &
      "M  V30 9 H -3.391314 1.091514 0.873612 0", &
      "M  V30 10 H -0.438887 -0.513473 1.460318 0", &
      "M  V30 11 H 0.421206 -2.197466 -0.111774 0", &
      "M  V30 12 H -0.893195 -1.946576 -1.055443 0", &
      "M  V30 13 H 0.073377 1.483235 -0.066299 0", &
      "M  V30 14 H 0.129860 0.396798 -1.434760 0", &
      "M  V30 15 H 2.179323 -0.745345 -0.519251 0", &
      "M  V30 16 H 2.219589 1.914253 1.021797 0", &
      "M  V30 17 H 3.622875 0.736438 0.730780 0", &
      "M  V30 END ATOM", &
      "M  V30 BEGIN BOND", &
      "M  V30 1 2 1 2", &
      "M  V30 2 1 2 3", &
      "M  V30 3 1 4 2", &
      "M  V30 4 1 4 5", &
      "M  V30 5 1 4 6", &
      "M  V30 6 1 6 7", &
      "M  V30 7 2 7 8 CFG=2", &
      "M  V30 8 1 3 9", &
      "M  V30 9 1 4 10 CFG=1", &
      "M  V30 10 1 5 11", &
      "M  V30 11 1 5 12", &
      "M  V30 12 1 6 13", &
      "M  V30 13 1 6 14", &
      "M  V30 14 1 7 15", &
      "M  V30 15 1 8 16", &
      "M  V30 16 1 8 17", &
      "M  V30 END BOND", &
      "M  END"
   rewind(unit)

   call read_molfile(struc, unit, error)
   close(unit)

end subroutine test_invalid9_mol

subroutine test_maestro_mol(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "  2244", &
      "                   3D", &
      " Schrodinger Suite 2022-1.", &
      " 21 21  0  0  1  0            999 V2000", &
      "    1.2333    0.5540    0.7792 O   0  0  0  0  0  0", &
      "   -0.6952   -2.7148   -0.7502 O   0  0  0  0  0  0", &
      "    0.7958   -2.1843    0.8685 O   0  0  0  0  0  0", &
      "    1.7813    0.8105   -1.4821 O   0  0  0  0  0  0", &
      "   -0.0857    0.6088    0.4403 C   0  0  0  0  0  0", &
      "   -0.7927   -0.5515    0.1244 C   0  0  0  0  0  0", &
      "   -0.7288    1.8464    0.4133 C   0  0  0  0  0  0", &
      "   -2.1426   -0.4741   -0.2184 C   0  0  0  0  0  0", &
      "   -2.0787    1.9238    0.0706 C   0  0  0  0  0  0", &
      "   -2.7855    0.7636   -0.2453 C   0  0  0  0  0  0", &
      "   -0.1409   -1.8536    0.1477 C   0  0  0  0  0  0", &
      "    2.1094    0.6715   -0.3113 C   0  0  0  0  0  0", &
      "    3.5305    0.5996    0.1635 C   0  0  0  0  0  0", &
      "   -0.1851    2.7545    0.6593 H   0  0  0  0  0  0", &
      "   -2.7247   -1.3605   -0.4564 H   0  0  0  0  0  0", &
      "   -2.5797    2.8872    0.0506 H   0  0  0  0  0  0", &
      "   -3.8374    0.8238   -0.5090 H   0  0  0  0  0  0", &
      "    3.7290    1.4184    0.8593 H   0  0  0  0  0  0", &
      "    4.2045    0.6969   -0.6924 H   0  0  0  0  0  0", &
      "    3.7105   -0.3659    0.6426 H   0  0  0  0  0  0", &
      "   -0.2555   -3.5916   -0.7337 H   0  0  0  0  0  0", &
      "  1  5  1  0  0  0", &
      "  1 12  1  0  0  0", &
      "  2 11  1  0  0  0", &
      "  2 21  1  0  0  0", &
      "  3 11  2  0  0  0", &
      "  4 12  2  0  0  0", &
      "  5  6  1  0  0  0", &
      "  5  7  2  0  0  0", &
      "  6  8  2  0  0  0", &
      "  6 11  1  0  0  0", &
      "  7  9  1  0  0  0", &
      "  7 14  1  0  0  0", &
      "  8 10  1  0  0  0", &
      "  8 15  1  0  0  0", &
      "  9 10  2  0  0  0", &
      "  9 16  1  0  0  0", &
      " 10 17  1  0  0  0", &
      " 12 13  1  0  0  0", &
      " 13 18  1  0  0  0", &
      " 13 19  1  0  0  0", &
      " 13 20  1  0  0  0", &
      "M  END"
   rewind(unit)

   call read_molfile(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   if (allocated(error)) return
   call check(error, struc%nat, 21, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, 3, "Number of species does not match")
   if (allocated(error)) return
   call check(error, struc%nbd, 21, "Number of bonds does not match")
   if (allocated(error)) return

end subroutine test_maestro_mol


subroutine test_valid1_sdf(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "", &
      " OpenBabel10191918023D", &
      "", &
      " 12 12  0  0  0  0  0  0  0  0999 V2000", &
      "   -0.0090   -0.0157    0.0000 C   0  0  0  0  0  0  0  0  0  0  0  0", &
      "   -0.7131    1.2038    0.0000 C   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    1.3990   -0.0157    0.0000 C   0  0  0  0  0  0  0  0  0  0  0  0", &
      "   -0.0090    2.4232    0.0000 C   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    2.1031    1.2038    0.0000 C   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    1.3990    2.4232    0.0000 C   0  0  0  0  0  0  0  0  0  0  0  0", &
      "   -0.5203   -0.9011    0.0000 H   0  0  0  0  0  0  0  0  0  0  0  0", &
      "   -1.7355    1.2038    0.0000 H   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    1.9103   -0.9011    0.0000 H   0  0  0  0  0  0  0  0  0  0  0  0", &
      "   -0.5203    3.3087    0.0000 H   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    3.1255    1.2038    0.0000 H   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    1.9103    3.3087    0.0000 H   0  0  0  0  0  0  0  0  0  0  0  0", &
      "  1  2  2  0  0  0  0", &
      "  1  3  1  0  0  0  0", &
      "  1  7  1  0  0  0  0", &
      "  2  4  1  0  0  0  0", &
      "  2  8  1  0  0  0  0", &
      "  3  5  2  0  0  0  0", &
      "  3  9  1  0  0  0  0", &
      "  4  6  2  0  0  0  0", &
      "  4 10  1  0  0  0  0", &
      "  5  6  1  0  0  0  0", &
      "  5 11  1  0  0  0  0", &
      "  6 12  1  0  0  0  0", &
      "M  END", &
      "$$$$"
   rewind(unit)

   call read_sdf(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, .not.allocated(struc%comment), "Empty comment line should not be saved")
   if (allocated(error)) return
   call check(error, struc%nat, 12, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, 2, "Number of species does not match")
   if (allocated(error)) return
   call check(error, struc%nbd, 12, "Number of bonds does not match")
   if (allocated(error)) return

end subroutine test_valid1_sdf


subroutine test_valid2_sdf(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "962", &
      "  Marvin  12300703363D          ", &
      "", &
      "  3  2  0  0  0  0            999 V2000", &
      "   -0.2309   -0.3265    0.0000 O   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    0.7484   -0.2843    0.0000 H   0  0  0  0  0  0  0  0  0  0  0  0", &
      "   -0.5175    0.6108    0.0000 H   0  0  0  0  0  0  0  0  0  0  0  0", &
      "  1  2  1  0  0  0  0", &
      "  1  3  1  0  0  0  0", &
      "M  END", &
      "", &
      "> <StdInChI>", &
      "InChI=1S/H2O/h1H2", &
      "", &
      "> <StdInChIKey>", &
      "XLYOFNOQVPJJNP-UHFFFAOYSA-N", &
      "", &
      "> <AuxInfo>", &
      "1/0/N:1/rA:3nOHH/rB:s1;s1;/rC:-.2309,-.3265,0;.7484,-.2843,0;-.5175,.6108,0;", &
      "", &
      "> <Formula>", &
      "H2 O", &
      "", &
      "> <Mw>", &
      "18.01528", &
      "", &
      "> <SMILES>", &
      "O([H])[H]", &
      "", &
      "> <CSID>", &
      "937", &
      "", &
      "$$$$", &
      "", &
      " OpenBabel10191919063D", &
      "", &
      "  3  2  0  0  0  0  0  0  0  0999 V2000", &
      "    0.9688   -0.1102    0.0277 H   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    1.9367   -0.0652    0.0164 O   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    2.2155   -0.9652    0.2426 H   0  0  0  0  0  0  0  0  0  0  0  0", &
      "  1  2  1  0  0  0  0", &
      "  2  3  1  0  0  0  0", &
      "M  END", &
      "$$$$"
   rewind(unit)

   call read_sdf(struc, unit, error)
   if (.not.allocated(error)) then
      call check(error, allocated(struc%comment), "Comment line should be saved")
      if (.not.allocated(error)) then
         call read_sdf(struc, unit, error)
      end if
   end if
   close(unit)
   if (allocated(error)) return

   call check(error, struc%nat, 3, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, 2, "Number of species does not match")
   if (allocated(error)) return
   call check(error, struc%nbd, 2, "Number of bonds does not match")
   if (allocated(error)) return

end subroutine test_valid2_sdf


subroutine test_valid3_sdf(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "Compound 11", &
      "     RDKit          3D", &
      "", &
      "  0  0  0  0  0  0  0  0  0  0999 V3000", &
      "M  V30 BEGIN CTAB", &
      "M  V30 COUNTS 17 16 0 0 0", &
      "M  V30 BEGIN ATOM", &
      "M  V30 1 O -2.821131 -0.276238 -0.753131 0", &
      "M  V30 2 C -2.076407 0.000289 0.175864 0", &
      "M  V30 3 O -2.469860 0.872693 1.126516 0", &
      "M  V30 4 C -0.648307 -0.508439 0.384207 0 CFG=1", &
      "M  V30 5 N -0.553725 -1.908221 -0.092137 0", &
      "M  V30 6 C 0.306640 0.448659 -0.352110 0", &
      "M  V30 7 C 1.764852 0.167437 -0.097096 0", &
      "M  V30 8 C 2.575104 0.984442 0.587951 0", &
      "M  V30 9 H -3.391314 1.091514 0.873612 0", &
      "M  V30 10 H -0.438887 -0.513473 1.460318 0", &
      "M  V30 11 H 0.421206 -2.197466 -0.111774 0", &
      "M  V30 12 H -0.893195 -1.946576 -1.055443 0", &
      "M  V30 13 H 0.073377 1.483235 -0.066299 0", &
      "M  V30 14 H 0.129860 0.396798 -1.434760 0", &
      "M  V30 15 H 2.179323 -0.745345 -0.519251 0", &
      "M  V30 16 H 2.219589 1.914253 1.021797 0", &
      "M  V30 17 H 3.622875 0.736438 0.730780 0", &
      "M  V30 END ATOM", &
      "M  V30 BEGIN BOND", &
      "M  V30 1 2 1 2", &
      "M  V30 2 1 2 3", &
      "M  V30 3 1 4 2", &
      "M  V30 4 1 4 5", &
      "M  V30 5 1 4 6", &
      "M  V30 6 1 6 7", &
      "M  V30 7 2 7 8 CFG=2", &
      "M  V30 8 1 3 9", &
      "M  V30 9 1 4 10 CFG=1", &
      "M  V30 10 1 5 11", &
      "M  V30 11 1 5 12", &
      "M  V30 12 1 6 13", &
      "M  V30 13 1 6 14", &
      "M  V30 14 1 7 15", &
      "M  V30 15 1 8 16", &
      "M  V30 16 1 8 17", &
      "M  V30 END BOND", &
      "M  V30 BEGIN COLLECTION", &
      "M  V30 MDLV30/STERAC1 ATOMS=(1 4)", &
      "M  V30 END COLLECTION", &
      "M  V30 END CTAB", &
      "M  END", &
      ">  <smiles>  (1) ", &
      "NC(CC=C)C(=O)O", &
      "", &
      "$$$$"
   rewind(unit)

   call read_molfile(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   if (allocated(error)) return
   call check(error, struc%nat, 17, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, 4, "Number of species does not match")
   if (allocated(error)) return
   call check(error, struc%nbd, 16, "Number of bonds does not match")
   if (allocated(error)) return

end subroutine test_valid3_sdf


subroutine test_invalid1_sdf(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "", &
      "  Mrv1823 10191918163D          ", &
      "", &
      " 12 12  0  0  0  0            999 V2000", &
      "   -0.0090   -0.0157   -0.0000 C   0  0  0  0  0  0  0  0  0  0  0  0", &
      "   -0.7131    1.2038   -0.0000 C   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    1.3990   -0.0157   -0.0000 C   0  0  0  0  0  0  0  0  0  0  0  0", &
      "   -0.0090    2.4232   -0.0000 C   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    2.1031    1.2038   -0.0000 C   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    1.3990    2.4232    0.0000 C   0  0  0  0  0  0  0  0  0  0  0  0", &
      "   -0.5203   -0.9011   -0.0000 H   0  0  0  0  0  0  0  0  0  0  0  0", &
      "   -1.7355    1.2038    0.0000 H   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    1.9103   -0.9011    0.0000 H   0  0  0  0  0  0  0  0  0  0  0  0", &
      "   -0.5203    3.3087    0.0000 H   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    3.1255    1.2038    0.0000 H   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    1.9103    3.3087   -0.0000 H   0  0  0  0  0  0  0  0  0  0  0  0", &
      "  2  1  4  0  0  0  0", &
      "  3  1  4  0  0  0  0", &
      "  4  2  4  0  0  0  0", &
      "  5  3  4  0  0  0  0", &
      "  6  4  4  0  0  0  0", &
      "  6  5  4  0  0  0  0", &
      "  1  7  1  0  0  0  0", &
      "  2  8  1  0  0  0  0", &
      "  3  9  1  0  0  0  0", &
      "  4 10  1  0  0  0  0", &
      "  5 11  1  0  0  0  0", &
      "  6 12  1  0  0  0  0", &
      "M  END"
   rewind(unit)

   call read_sdf(struc, unit, error)
   close(unit)

end subroutine test_invalid1_sdf


subroutine test_invalid2_sdf(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "", &
      "  Mrv1823 10191918163D          ", &
      "", &
      " 12 18  0  0  0  0            999 V2000", &
      "   -0.0090   -0.0157   -0.0000 C   0  0  0  0  0  0  0  0  0  0  0  0", &
      "   -0.7131    1.2038   -0.0000 C   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    1.3990   -0.0157   -0.0000 C   0  0  0  0  0  0  0  0  0  0  0  0", &
      "   -0.0090    2.4232   -0.0000 C   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    2.1031    1.2038   -0.0000 C   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    1.3990    2.4232    0.0000 C   0  0  0  0  0  0  0  0  0  0  0  0", &
      "   -0.5203   -0.9011   -0.0000 H   0  0  0  0  0  0  0  0  0  0  0  0", &
      "   -1.7355    1.2038    0.0000 H   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    1.9103   -0.9011    0.0000 H   0  0  0  0  0  0  0  0  0  0  0  0", &
      "   -0.5203    3.3087    0.0000 H   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    3.1255    1.2038    0.0000 H   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    1.9103    3.3087   -0.0000 H   0  0  0  0  0  0  0  0  0  0  0  0", &
      "  2  1  4  0  0  0  0", &
      "  3  1  4  0  0  0  0", &
      "  4  2  4  0  0  0  0", &
      "  5  3  4  0  0  0  0", &
      "  6  4  4  0  0  0  0", &
      "  6  5  4  0  0  0  0", &
      "  1  7  1  0  0  0  0", &
      "  2  8  1  0  0  0  0", &
      "  3  9  1  0  0  0  0", &
      "  4 10  1  0  0  0  0", &
      "  5 11  1  0  0  0  0", &
      "  6 12  1  0  0  0  0", &
      "M  END", &
      "$$$$"
   rewind(unit)

   call read_sdf(struc, unit, error)
   close(unit)

end subroutine test_invalid2_sdf


subroutine test_invalid3_sdf(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "962", &
      "          12300703363D          ", &
      "", &
      "  3  2  0  0  0  0            999 V2000", &
      "   -0.2309   -0.3265    0.0000 O   0  0  0  0  0  0  0  0  0  0  0  0", &
      "    0.7484   -0.2843    0.0000 H   0  0  0  0  0  0  0  0  0  0  0  0", &
      "   -0.5175    0.6108    0.0000 H   0  0  0  0  0  0  0  0  0  0  0  0", &
      "  1  2  1  0  0  0  0", &
      "  1  3  1  0  0  0  0", &
      "", &
      "> <StdInChI>", &
      "InChI=1S/H2O/h1H2", &
      "", &
      "> <StdInChIKey>", &
      "XLYOFNOQVPJJNP-UHFFFAOYSA-N", &
      "", &
      "> <AuxInfo>", &
      "1/0/N:1/rA:3nOHH/rB:s1;s1;/rC:-.2309,-.3265,0;.7484,-.2843,0;-.5175,.6108,0;", &
      "", &
      "> <Formula>", &
      "H2 O", &
      "", &
      "> <Mw>", &
      "18.01528", &
      "", &
      "> <SMILES>", &
      "O([H])[H]", &
      "", &
      "> <CSID>", &
      "937", &
      "", &
      "$$$$"
   rewind(unit)

   call read_sdf(struc, unit, error)
   if (.not.allocated(error)) then
      call read_sdf(struc, unit, error)
   end if
   close(unit)
   if (allocated(error)) return

   call check(error, struc%nat, 3, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, 2, "Number of species does not match")
   if (allocated(error)) return

end subroutine test_invalid3_sdf


subroutine test_invalid4_sdf(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
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
      "M  CHG  1 5 1", &
      "M  END", &
      ">  <total energy / Eh>", &
      "-18.421705869411", &
      "", &
      ">  <gradient norm / Eh/a0>", &
      "0.000695317397", &
      "", &
      "$$$$"
   rewind(unit)

   call read_sdf(struc, unit, error)

end subroutine test_invalid4_sdf


subroutine test_invalid5_sdf(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "Compound 11", &
      "     RDKit          3D", &
      "", &
      "  0  0  0  0  0  0  0  0  0  0999 V3000", &
      "M  V30 BEGIN CTAB", &
      "M  V30 COUNTS 17 16 0 0 0", &
      "M  V30 BEGIN ATOM", &
      "M  V30 1 O -2.821131 -0.276238 -0.753131 0", &
      "M  V30 2 C -2.076407 0.000289 0.175864 0", &
      "M  V30 3 O -2.469860 0.872693 1.126516 0", &
      "M  V30 4 C -0.648307 -0.508439 0.384207 0 CFG=1", &
      "M  V30 5 N -0.553725 -1.908221 -0.092137 0 CHG=b", &
      "M  V30 6 C 0.306640 0.448659 -0.352110 0", &
      "M  V30 7 C 1.764852 0.167437 -0.097096 0", &
      "M  V30 8 C 2.575104 0.984442 0.587951 0", &
      "M  V30 9 H -3.391314 1.091514 0.873612 0", &
      "M  V30 10 H -0.438887 -0.513473 1.460318 0", &
      "M  V30 11 H 0.421206 -2.197466 -0.111774 0", &
      "M  V30 12 H -0.893195 -1.946576 -1.055443 0", &
      "M  V30 13 H 0.073377 1.483235 -0.066299 0", &
      "M  V30 14 H 0.129860 0.396798 -1.434760 0", &
      "M  V30 15 H 2.179323 -0.745345 -0.519251 0", &
      "M  V30 16 H 2.219589 1.914253 1.021797 0", &
      "M  V30 17 H 3.622875 0.736438 0.730780 0", &
      "M  V30 END ATOM", &
      "M  V30 BEGIN BOND", &
      "M  V30 1 2 1 2", &
      "M  V30 2 1 2 3", &
      "M  V30 3 1 4 2", &
      "M  V30 4 1 4 5", &
      "M  V30 5 1 4 6", &
      "M  V30 6 1 6 7", &
      "M  V30 7 2 7 8 CFG=2", &
      "M  V30 8 1 3 9", &
      "M  V30 9 1 4 10 CFG=1", &
      "M  V30 10 1 5 11", &
      "M  V30 11 1 5 12", &
      "M  V30 12 1 6 13", &
      "M  V30 13 1 6 14", &
      "M  V30 14 1 7 15", &
      "M  V30 15 1 8 16", &
      "M  V30 16 1 8 17", &
      "M  V30 END BOND", &
      "M  V30 BEGIN COLLECTION", &
      "M  V30 MDLV30/STERAC1 ATOMS=(1 4)", &
      "M  V30 END COLLECTION", &
      "M  V30 END CTAB", &
      "M  END", &
      ">  <smiles>  (1) ", &
      "NC(CC=C)C(=O)O", &
      "", &
      "$$$$"
   rewind(unit)

   call read_molfile(struc, unit, error)
   close(unit)

end subroutine test_invalid5_sdf


subroutine test_invalid6_sdf(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "Compound 11", &
      "     RDKit          3D", &
      "", &
      "  0  0  0  0  0  0  0  0  0  0999 V3000", &
      "M  V30 BEGIN CTAB", &
      "M  V30 COUNTS 17 16 0 0 0", &
      "M  V30 BEGIN ATOM", &
      "M  V30 1 O -2.821131 -0.276238 -0.753131 0", &
      "M  V30 2 C -2.076407 0.000289 0.175864 0", &
      "M  V30 3 O -2.469860 0.872693 1.126516 0", &
      "M  V30 4 C -0.648307 -0.508439 0.384207 0 CFG=1", &
      "M  V30 5 N -0.553725 -1.908221 -0.092137 0", &
      "M  V30 6 C 0.306640 0.448659 -0.352110 0", &
      "M  V30 7 C 1.764852 0.167437 -0.097096 0", &
      "M  V30 8 * 2.575104 0.984442 0.587951 0", &
      "M  V30 9 H -3.391314 1.091514 0.873612 0", &
      "M  V30 10 H -0.438887 -0.513473 1.460318 0", &
      "M  V30 11 H 0.421206 -2.197466 -0.111774 0", &
      "M  V30 12 H -0.893195 -1.946576 -1.055443 0", &
      "M  V30 13 H 0.073377 1.483235 -0.066299 0", &
      "M  V30 14 H 0.129860 0.396798 -1.434760 0", &
      "M  V30 15 H 2.179323 -0.745345 -0.519251 0", &
      "M  V30 16 H 2.219589 1.914253 1.021797 0", &
      "M  V30 17 H 3.622875 0.736438 0.730780 0", &
      "M  V30 END ATOM", &
      "M  V30 BEGIN BOND", &
      "M  V30 1 2 1 2", &
      "M  V30 2 1 2 3", &
      "M  V30 3 1 4 2", &
      "M  V30 4 1 4 5", &
      "M  V30 5 1 4 6", &
      "M  V30 6 1 6 7", &
      "M  V30 7 2 7 8 CFG=2", &
      "M  V30 8 1 3 9", &
      "M  V30 9 1 4 10 CFG=1", &
      "M  V30 10 1 5 11", &
      "M  V30 11 1 5 12", &
      "M  V30 12 1 6 13", &
      "M  V30 13 1 6 14", &
      "M  V30 14 1 7 15", &
      "M  V30 15 1 8 16", &
      "M  V30 16 1 8 17", &
      "M  V30 END BOND", &
      "M  V30 BEGIN COLLECTION", &
      "M  V30 MDLV30/STERAC1 ATOMS=(1 4)", &
      "M  V30 END COLLECTION", &
      "M  V30 END CTAB", &
      "M  END", &
      ">  <smiles>  (1) ", &
      "NC(CC=C)C(=O)O", &
      "", &
      "$$$$"
   rewind(unit)

   call read_molfile(struc, unit, error)
   close(unit)

end subroutine test_invalid6_sdf


subroutine test_invalid7_sdf(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "Compound 11", &
      "     RDKit          3D", &
      "", &
      "  0  0  0  0  0  0  0  0  0  0999 V3000", &
      "M  V30 BEGIN CTAB", &
      "M  V30 COUNTS 17 16 a 0 0", &
      "M  V30 BEGIN ATOM", &
      "M  V30 1 O -2.821131 -0.276238 -0.753131 0", &
      "M  V30 2 C -2.076407 0.000289 0.175864 0", &
      "M  V30 3 O -2.469860 0.872693 1.126516 0", &
      "M  V30 4 C -0.648307 -0.508439 0.384207 0 CFG=1", &
      "M  V30 5 N -0.553725 -1.908221 -0.092137 0", &
      "M  V30 6 C 0.306640 0.448659 -0.352110 0", &
      "M  V30 7 C 1.764852 0.167437 -0.097096 0", &
      "M  V30 8 C 2.575104 0.984442 0.587951 0", &
      "M  V30 9 H -3.391314 1.091514 0.873612 0", &
      "M  V30 10 H -0.438887 -0.513473 1.460318 0", &
      "M  V30 11 H 0.421206 -2.197466 -0.111774 0", &
      "M  V30 12 H -0.893195 -1.946576 -1.055443 0", &
      "M  V30 13 H 0.073377 1.483235 -0.066299 0", &
      "M  V30 14 H 0.129860 0.396798 -1.434760 0", &
      "M  V30 15 H 2.179323 -0.745345 -0.519251 0", &
      "M  V30 16 H 2.219589 1.914253 1.021797 0", &
      "M  V30 17 H 3.622875 0.736438 0.730780 0", &
      "M  V30 END ATOM", &
      "M  V30 BEGIN BOND", &
      "M  V30 1 2 1 2", &
      "M  V30 2 1 2 3", &
      "M  V30 3 1 4 2", &
      "M  V30 4 1 4 5", &
      "M  V30 5 1 4 6", &
      "M  V30 6 1 6 7", &
      "M  V30 7 2 7 8 CFG=2", &
      "M  V30 8 1 3 9", &
      "M  V30 9 1 4 10 CFG=1", &
      "M  V30 10 1 5 11", &
      "M  V30 11 1 5 12", &
      "M  V30 12 1 6 13", &
      "M  V30 13 1 6 14", &
      "M  V30 14 1 7 15", &
      "M  V30 15 1 8 16", &
      "M  V30 16 1 8 17", &
      "M  V30 END BOND", &
      "M  V30 BEGIN COLLECTION", &
      "M  V30 MDLV30/STERAC1 ATOMS=(1 4)", &
      "M  V30 END COLLECTION", &
      "M  V30 END CTAB", &
      "M  END", &
      ">  <smiles>  (1) ", &
      "NC(CC=C)C(=O)O", &
      "", &
      "$$$$"
   rewind(unit)

   call read_molfile(struc, unit, error)
   close(unit)

end subroutine test_invalid7_sdf


subroutine test_unsupported1_sdf(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "Compound 11", &
      "     RDKit          3D", &
      "", &
      "  0  0  0  0  0  0  0  0  0  0999 V3000", &
      "M  V30 BEGIN CTAB", &
      "M  V30 COUNTS 17 16 0 0 0", &
      "M  V30 BEGIN ATOM", &
      "M  V30 1 O -2.821131 -0.276238 -0.753131 0", &
      "M  V30 2 C -2.076407 0.000289 0.175864 0", &
      "M  V30 3 O -2.469860 0.872693 1.126516 1", &
      "M  V30 4 C -0.648307 -0.508439 0.384207 0 CFG=1", &
      "M  V30 5 N -0.553725 -1.908221 -0.092137 0", &
      "M  V30 6 C 0.306640 0.448659 -0.352110 0", &
      "M  V30 7 C 1.764852 0.167437 -0.097096 0", &
      "M  V30 8 * 2.575104 0.984442 0.587951 0", &
      "M  V30 9 H -3.391314 1.091514 0.873612 0", &
      "M  V30 10 H -0.438887 -0.513473 1.460318 0", &
      "M  V30 11 H 0.421206 -2.197466 -0.111774 0", &
      "M  V30 12 H -0.893195 -1.946576 -1.055443 0", &
      "M  V30 13 H 0.073377 1.483235 -0.066299 0", &
      "M  V30 14 H 0.129860 0.396798 -1.434760 0", &
      "M  V30 15 H 2.179323 -0.745345 -0.519251 0", &
      "M  V30 16 H 2.219589 1.914253 1.021797 0", &
      "M  V30 17 H 3.622875 0.736438 0.730780 0", &
      "M  V30 END ATOM", &
      "M  V30 BEGIN BOND", &
      "M  V30 1 2 1 2", &
      "M  V30 2 1 2 3", &
      "M  V30 3 1 4 2", &
      "M  V30 4 1 4 5", &
      "M  V30 5 1 4 6", &
      "M  V30 6 1 6 7", &
      "M  V30 7 2 7 8 CFG=2", &
      "M  V30 8 1 3 9", &
      "M  V30 9 1 4 10 CFG=1", &
      "M  V30 10 1 5 11", &
      "M  V30 11 1 5 12", &
      "M  V30 12 1 6 13", &
      "M  V30 13 1 6 14", &
      "M  V30 14 1 7 15", &
      "M  V30 15 1 8 16", &
      "M  V30 16 1 8 17", &
      "M  V30 END BOND", &
      "M  V30 BEGIN COLLECTION", &
      "M  V30 MDLV30/STERAC1 ATOMS=(1 4)", &
      "M  V30 END COLLECTION", &
      "M  V30 END CTAB", &
      "M  END", &
      ">  <smiles>  (1) ", &
      "NC(CC=C)C(=O)O", &
      "", &
      "$$$$"
   rewind(unit)

   call read_molfile(struc, unit, error)
   close(unit)

end subroutine test_unsupported1_sdf


subroutine test_unsupported2_sdf(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "Compound 11", &
      "     RDKit          3D", &
      "", &
      "  0  0  0  0  0  0  0  0  0  0999 V3000", &
      "M  V30 BEGIN CTAB", &
      "M  V30 COUNTS 0 0 0 0 0", &
      "M  V30 END CTAB", &
      "M  END", &
      ">  <smiles>  (1) ", &
      "NC(CC=C)C(=O)O", &
      "", &
      "$$$$"
   rewind(unit)

   call read_molfile(struc, unit, error)
   close(unit)

end subroutine test_unsupported2_sdf


subroutine test_unsupported3_sdf(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "Compound 11", &
      "     RDKit          3D", &
      "", &
      "  0  0  0  0  0  0  0  0  0  0999 V3000", &
      "M  END", &
      ">  <smiles>  (1) ", &
      "NC(CC=C)C(=O)O", &
      "", &
      "$$$$"
   rewind(unit)

   call read_molfile(struc, unit, error)
   close(unit)

end subroutine test_unsupported3_sdf

end module test_read_ctfile
