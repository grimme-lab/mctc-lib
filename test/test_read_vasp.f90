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

module test_read_vasp
   use mctc_env_testing, only : new_unittest, unittest_type, error_type, check, &
      & test_failed
   use mctc_io_read_vasp
   use mctc_io_structure
   implicit none
   private

   public :: collect_read_vasp


contains


!> Collect all exported unit tests
subroutine collect_read_vasp(testsuite)

   !> Collection of tests
   type(unittest_type), allocatable, intent(out) :: testsuite(:)

   testsuite = [ &
      & new_unittest("valid1-poscar", test_valid1_poscar), &
      & new_unittest("valid2-poscar", test_valid2_poscar), &
      & new_unittest("valid3-poscar", test_valid3_poscar), &
      & new_unittest("valid4-poscar", test_valid4_poscar), &
      & new_unittest("valid5-poscar", test_valid5_poscar), &
      & new_unittest("invalid1-poscar", test_invalid1_poscar, should_fail=.true.), &
      & new_unittest("invalid2-poscar", test_invalid2_poscar, should_fail=.true.), &
      & new_unittest("invalid3-poscar", test_invalid3_poscar, should_fail=.true.), &
      & new_unittest("invalid4-poscar", test_invalid4_poscar, should_fail=.true.), &
      & new_unittest("invalid5-poscar", test_invalid5_poscar, should_fail=.true.), &
      & new_unittest("invalid6-poscar", test_invalid6_poscar, should_fail=.true.), &
      & new_unittest("invalid7-poscar", test_invalid7_poscar, should_fail=.true.) &
      & ]

end subroutine collect_read_vasp


subroutine test_valid1_poscar(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "Ti  O ", &
      " 1.0000000000000000", &
      "     4.59373    0.00000    0.00000", &
      "     0.00000    4.59373    0.00000", &
      "     0.00000    0.00000    2.95812", &
      "   2   4", &
      "Cartesian", &
      "  0.000000000  0.000000000  0.000000000", &
      "  2.296865000  2.296865000  1.479060000", &
      "  1.402465769  1.402465769  0.000000000", &
      "  3.191264231  3.191264231  0.000000000", &
      "  3.699330769  0.894399231  1.479060000", &
      "  0.894399231  3.699330769  1.479060000"
   rewind(unit)

   call read_vasp(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, .not.allocated(struc%comment), "Pre-Vasp5 comment line is used to store symbols")
   if (allocated(error)) return
   call check(error, struc%nat, 6, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, 2, "Number of species does not match")
   if (allocated(error)) return

end subroutine test_valid1_poscar


subroutine test_valid2_poscar(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "Anatase", &
      " 1.0000000000000000", &
      "     3.7850000000000000    0.0000000000000000    0.0000000000000000", &
      "     0.0000000000000000    3.7850000000000000    0.0000000000000000", &
      "     0.0000000000000000    0.0000000000000000    9.5140000000000000", &
      "   Ti  O ", &
      "   4   8", &
      "Selective", &
      "Cartesian", &
      "  0.0000000000000000  0.0000000000000000  0.0000000000000000", &
      "  1.8925000000000000  1.8925000000000000  4.7570000000000000", &
      "  0.0000000000000000  1.8925000000000000  2.3785000000000000", &
      "  1.8925000000000000  0.0000000000000000  7.1355000000000000", &
      "  0.0000000000000000  0.0000000000000000  1.9655924000000000", &
      "  1.8925000000000000  1.8925000000000000  6.7225924000000000", &
      "  0.0000000000000000  1.8925000000000000  4.3440924000000000", &
      "  1.8925000000000000  0.0000000000000000  9.1010924000000000", &
      "  1.8925000000000000  1.8925000000000000  2.7914076000000000", &
      "  0.0000000000000000  0.0000000000000000  7.5484076000000000", &
      "  1.8925000000000000  0.0000000000000000  5.1699076000000000", &
      "  0.0000000000000000  1.8925000000000000  0.4129076000000000"
   rewind(unit)

   call read_vasp(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, allocated(struc%comment), "Comment line should be preserved")
   if (allocated(error)) return
   call check(error, struc%comment, "Anatase")
   if (allocated(error)) return
   call check(error, struc%nat, 12, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, 2, "Number of species does not match")
   if (allocated(error)) return

end subroutine test_valid2_poscar


subroutine test_valid3_poscar(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "cubic diamond", &
      "  3.7", &
      "    0.5 0.5 0.0", &
      "    0.0 0.5 0.5", &
      "    0.5 0.0 0.5", &
      "   C", &
      "   2", &
      "Direct", &
      "  0.0 0.0 0.0", &
      "  0.25 0.25 0.25"
   rewind(unit)

   call read_vasp(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, struc%comment, "cubic diamond")
   if (allocated(error)) return
   call check(error, struc%nat, 2, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, 1, "Number of species does not match")
   if (allocated(error)) return

end subroutine test_valid3_poscar


subroutine test_valid4_poscar(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "C2 F2", &
      "1.0", &
      "  1.291         2.23608        +0.0000000000", &
      " -1.291         2.23608        +0.0000000000", &
      " +0.0000000000  +0.0000000000   5.75", &
      " 2 2", &
      "cartesian", &
      "   0.00000000  0.00000000  1.37627335", &
      "   0.00000000  2.98144198  1.86702665", &
      "   0.00000000  0.00000000  0.00394701", &
      "   0.00000000  2.98144198  3.23935299"
   rewind(unit)

   call read_vasp(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, struc%nat, 4, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, 2, "Number of species does not match")
   if (allocated(error)) return

end subroutine test_valid4_poscar


subroutine test_valid5_poscar(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      " O  C  H ", &
      " 1.0000000000000000", &
      "     6.4411018522600001    0.0492571261505000    0.2192046129910000", &
      "     0.0462076831739000    6.6435057067500001    0.1670513770770000", &
      "     0.2262248220170000   -0.9573234940220000    6.7608039126200001", &
      "   4  12  16", &
      "Cartesian", &
      "  4.5853168464880421  4.9392326929575878  4.1894081210748118", &
      "  5.8862267152491423  1.1425258978245871  6.5015058204768126", &
      "  1.4284279616220412  4.6017511285540875  3.1465884436348119", &
      "  2.3323404704521411  1.4471801154820869  0.7121932185858125", &
      "  4.7333543155561415  2.7747291872305868  3.1951352976178122", &
      "  5.6617754419101418  2.2133191164485870  2.1235404838618126", &
      "  5.3107598618381422  2.6902988056185868  0.7384466319968125", &
      "  4.4947071071761426  3.9530790692635867  0.6801776747598124", &
      "  4.8005171923760424  4.9185102874975870  1.8186363449528122", &
      "  4.6951362687070421  4.2781752812835867  3.1816411821728123", &
      "  1.3838574419160412  5.2817805008910863  4.1482702947948136", &
      "  1.0268974195990415  4.7234752637800881  5.4989995400388123", &
      "  2.0852659694760409  5.0956317453800875  6.5351699846458127", &
      "  2.3344644666691412 -0.0736561690909131  6.4245628001158135", &
      "  2.4894017448231409  0.6213510313930869  5.0967297417158131", &
      "  1.5745272273791413  0.1243470825760870  3.9731040773988129", &
      "  5.8221065925130420  5.3013563342055878  1.7264876737078123", &
      "  3.4487807319551416  3.6355832152975864  0.7429568016758125", &
      "  4.8499393376520423  3.4713855169305874  6.4691872586348129", &
      "  0.2495364434351412  2.4795455690160870  2.1043557230378123", &
      "  5.6691068338331423  1.1234174220755870  2.1414388326468128", &
      "  3.7072009289431418  2.4357632918535872  3.0094700999208119", &
      "  4.1414520030430415  5.7877262477775879  1.7803680119358125", &
      "  5.0142851411171421  2.4165926460955873  4.1857610486448129", &
      "  3.0280930003030413  4.6201081184690871  6.2533190952188136", &
      "  0.5863628696651412  0.5757236365910867  4.1021714214668128", &
      "  2.3776130524831411  1.6969724987740866  5.2327688986668139", &
      "  1.9486148363011413  0.4390675147070869  2.9999022491838123", &
      "  3.5312997625581413  0.4467415528495868  4.8114121395028135", &
      "  6.5089895990100421  5.2100409408535882  6.0066553789008132", &
      "  0.9001165013630412  3.6420787128610868  5.4413106648508132", &
      "  1.6012116650460413  5.6845471271780879  0.7675566847298124"
   rewind(unit)

   call read_vasp(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, struc%nat, 32, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, 3, "Number of species does not match")
   if (allocated(error)) return

end subroutine test_valid5_poscar


subroutine test_invalid1_poscar(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      ""
   rewind(unit)

   call read_vasp(struc, unit, error)
   close(unit)
   if (allocated(error)) return

end subroutine test_invalid1_poscar


subroutine test_invalid2_poscar(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "Ti  O ", &
      " 1.0000000000000000", &
      "     4.59373    0.00000    0.00000", &
      "     0.00000    4.59373    0.00000", &
      "     0.00000    0.00000    2.95812", &
      "   2   4", &
      "Selective", &
      "  0.000000000  0.000000000  0.000000000", &
      "  2.296865000  2.296865000  1.479060000", &
      "  1.402465769  1.402465769  0.000000000", &
      "  3.191264231  3.191264231  0.000000000", &
      "  3.699330769  0.894399231  1.479060000", &
      "  0.894399231  3.699330769  1.479060000"
   rewind(unit)

   call read_vasp(struc, unit, error)
   close(unit)
   if (allocated(error)) return

end subroutine test_invalid2_poscar


subroutine test_invalid3_poscar(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "Ti  O ", &
      " 1.0000000000000000", &
      "     4.59373    0.00000    0.00000", &
      "     0.00000    4.59373    0.00000"
   rewind(unit)

   call read_vasp(struc, unit, error)
   close(unit)
   if (allocated(error)) return

end subroutine test_invalid3_poscar


subroutine test_invalid4_poscar(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "Ti  O ", &
      " 1.0000000000000000", &
      "     4.59373    0.00000    0.00000", &
      "     0.00000    4.59373    0.00000", &
      "     0.00000    0.00000    2.95812", &
      "   2   2   2", &
      "Cartesian", &
      "  0.000000000  0.000000000  0.000000000", &
      "  2.296865000  2.296865000  1.479060000", &
      "  1.402465769  1.402465769  0.000000000", &
      "  3.191264231  3.191264231  0.000000000", &
      "  3.699330769  0.894399231  1.479060000", &
      "  0.894399231  3.699330769  1.479060000"
   rewind(unit)

   call read_vasp(struc, unit, error)
   close(unit)
   if (allocated(error)) return

end subroutine test_invalid4_poscar


subroutine test_invalid5_poscar(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "Ti  O ", &
      " 1.0000000000000000", &
      "     *******    0.00000    0.00000", &
      "     0.00000    *******    0.00000", &
      "     0.00000    0.00000    *******", &
      "   2   4", &
      "Cartesian", &
      "  0.000000000  0.000000000  0.000000000", &
      "  2.296865000  2.296865000  1.479060000", &
      "  1.402465769  1.402465769  0.000000000", &
      "  3.191264231  3.191264231  0.000000000", &
      "  3.699330769  0.894399231  1.479060000", &
      "  0.894399231  3.699330769  1.479060000"
   rewind(unit)

   call read_vasp(struc, unit, error)
   close(unit)
   if (allocated(error)) return

end subroutine test_invalid5_poscar


subroutine test_invalid6_poscar(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "Titan  Oxygen", &
      " 1.0000000000000000", &
      "     4.59373    0.00000    0.00000", &
      "     0.00000    4.59373    0.00000", &
      "     0.00000    0.00000    2.95812", &
      "   2   4", &
      "Cartesian", &
      "  0.000000000  0.000000000  0.000000000", &
      "  2.296865000  2.296865000  1.479060000", &
      "  1.402465769  1.402465769  0.000000000", &
      "  3.191264231  3.191264231  0.000000000", &
      "  3.699330769  0.894399231  1.479060000", &
      "  0.894399231  3.699330769  1.479060000"
   rewind(unit)

   call read_vasp(struc, unit, error)
   close(unit)
   if (allocated(error)) return

end subroutine test_invalid6_poscar


subroutine test_invalid7_poscar(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "# Rutile", &
      "Ti  O ", &
      " 1.0000000000000000", &
      "     4.59373    0.00000    0.00000", &
      "     0.00000    4.59373    0.00000", &
      "     0.00000    0.00000    2.95812", &
      "   2   4", &
      "Cartesian", &
      "  0.000000000  0.000000000  0.000000000", &
      "  2.296865000  2.296865000  1.479060000", &
      "  1.402465769  1.402465769  0.000000000", &
      "  3.191264231  3.191264231  0.000000000", &
      "  3.699330769  0.894399231  1.479060000", &
      "  0.894399231  3.699330769  1.479060000"
   rewind(unit)

   call read_vasp(struc, unit, error)
   close(unit)
   if (allocated(error)) return

end subroutine test_invalid7_poscar


end module test_read_vasp
