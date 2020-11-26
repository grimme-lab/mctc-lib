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
      & new_unittest("invalid1-mol", test_invalid1_mol, should_fail=.true.), &
      & new_unittest("invalid2-mol", test_invalid2_mol, should_fail=.true.), &
      & new_unittest("invalid3-mol", test_invalid3_mol, should_fail=.true.), &
      & new_unittest("valid1-sdf", test_valid1_sdf), &
      & new_unittest("valid2-sdf", test_valid2_sdf), &
      & new_unittest("invalid1-sdf", test_invalid1_sdf, should_fail=.true.), &
      & new_unittest("invalid2-sdf", test_invalid2_sdf, should_fail=.true.), &
      & new_unittest("invalid3-sdf", test_invalid3_sdf, should_fail=.true.) &
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


end module test_read_ctfile
