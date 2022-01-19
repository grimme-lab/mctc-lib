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

module test_read_xyz
   use mctc_env_testing, only : new_unittest, unittest_type, error_type, check
   use mctc_io_read_xyz
   use mctc_io_structure
   implicit none
   private

   public :: collect_read_xyz


contains


!> Collect all exported unit tests
subroutine collect_read_xyz(testsuite)

   !> Collection of tests
   type(unittest_type), allocatable, intent(out) :: testsuite(:)

   testsuite = [ &
      & new_unittest("valid1-xyz", test_valid1_xyz), &
      & new_unittest("valid2-xyz", test_valid2_xyz), &
      & new_unittest("valid3-xyz", test_valid3_xyz), &
      & new_unittest("valid4-xyz", test_valid4_xyz), &
      & new_unittest("valid5-xyz", test_valid5_xyz), &
      & new_unittest("invalid1-xyz", test_invalid1_xyz, should_fail=.true.), &
      & new_unittest("invalid2-xyz", test_invalid2_xyz, should_fail=.true.), &
      & new_unittest("invalid3-xyz", test_invalid3_xyz, should_fail=.true.), &
      & new_unittest("invalid4-xyz", test_invalid4_xyz, should_fail=.true.), &
      & new_unittest("invalid5-xyz", test_invalid5_xyz, should_fail=.true.), &
      & new_unittest("invalid6-xyz", test_invalid6_xyz, should_fail=.true.), &
      & new_unittest("invalid7-xyz", test_invalid7_xyz, should_fail=.true.) &
      & ]

end subroutine collect_read_xyz


subroutine test_valid1_xyz(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "9", &
      "WATER27, (H2O)3", &
      "O     1.1847029    1.1150792   -0.0344641 ", &
      "H     0.4939088    0.9563767    0.6340089 ", &
      "H     2.0242676    1.0811246    0.4301417 ", &
      "O    -1.1469443    0.0697649    1.1470196 ", &
      "H    -1.2798308   -0.5232169    1.8902833 ", &
      "H    -1.0641398   -0.4956693    0.3569250 ", &
      "O    -0.1633508   -1.0289346   -1.2401808 ", &
      "H     0.4914771   -0.3248733   -1.0784838 ", &
      "H    -0.5400907   -0.8496512   -2.1052499 "
   rewind(unit)

   call read_xyz(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, allocated(struc%comment), "Comment line should be preserved")
   if (allocated(error)) return
   call check(error, struc%comment, "WATER27, (H2O)3")
   if (allocated(error)) return
   call check(error, struc%nat, 9, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, 2, "Number of species does not match")
   if (allocated(error)) return

end subroutine test_valid1_xyz


subroutine test_valid2_xyz(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "24", &
      "", &
      "C          1.07317        0.04885       -0.07573", &
      "N          2.51365        0.01256       -0.07580", &
      "C*         3.35199        1.09592       -0.07533", &
      "N          4.61898        0.73028       -0.07549", &
      "C*         4.57907       -0.63144       -0.07531", &
      "C          3.30131       -1.10256       -0.07524", &
      "C          2.98068       -2.48687       -0.07377", &
      "18O        1.82530       -2.90038       -0.07577", &
      "N          4.11440       -3.30433       -0.06936", &
      "C*         5.45174       -2.85618       -0.07235", &
      "O          6.38934       -3.65965       -0.07232", &
      "N          5.66240       -1.47682       -0.07487", &
      "C          7.00947       -0.93648       -0.07524", &
      "C          3.92063       -4.74093       -0.06158", &
      "D          0.73398        1.08786       -0.07503", &
      "D          0.71239       -0.45698        0.82335", &
      "D          0.71240       -0.45580       -0.97549", &
      "H          2.99301        2.11762       -0.07478", &
      "H          7.76531       -1.72634       -0.07591", &
      "H          7.14864       -0.32182        0.81969", &
      "H          7.14802       -0.32076       -0.96953", &
      "H          2.86501       -5.02316       -0.05833", &
      "H          4.40233       -5.15920        0.82837", &
      "H          4.40017       -5.16929       -0.94780"
   rewind(unit)

   call read_xyz(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, .not.allocated(struc%comment), "Empty comment line should not be saved")
   if (allocated(error)) return
   call check(error, struc%nat, 24, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, 7, "Number of species does not match")
   if (allocated(error)) return

end subroutine test_valid2_xyz


subroutine test_valid3_xyz(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "24", &
      "#", &
      "c  1.07317  0.04885 -0.07573 -0.05445590", &
      "n  2.51365  0.01256 -0.07580 -0.00457526", &
      "c  3.35199  1.09592 -0.07533  0.08391889", &
      "n  4.61898  0.73028 -0.07549 -0.27870751", &
      "c  4.57907 -0.63144 -0.07531  0.11914924", &
      "c  3.30131 -1.10256 -0.07524 -0.02621044", &
      "c  2.98068 -2.48687 -0.07377  0.26115960", &
      "o  1.82530 -2.90038 -0.07577 -0.44071824", &
      "n  4.11440 -3.30433 -0.06936 -0.10804747", &
      "c  5.45174 -2.85618 -0.07235  0.30411699", &
      "o  6.38934 -3.65965 -0.07232 -0.44083760", &
      "n  5.66240 -1.47682 -0.07487 -0.07457706", &
      "c  7.00947 -0.93648 -0.07524 -0.04790859", &
      "c  3.92063 -4.74093 -0.06158 -0.03738239", &
      "h  0.73398  1.08786 -0.07503  0.06457802", &
      "h  0.71239 -0.45698  0.82335  0.08293905", &
      "h  0.71240 -0.45580 -0.97549  0.08296802", &
      "h  2.99301  2.11762 -0.07478  0.05698136", &
      "h  7.76531 -1.72634 -0.07591  0.09025556", &
      "h  7.14864 -0.32182  0.81969  0.07152988", &
      "h  7.14802 -0.32076 -0.96953  0.07159003", &
      "h  2.86501 -5.02316 -0.05833  0.08590674", &
      "h  4.40233 -5.15920  0.82837  0.06906357", &
      "h  4.40017 -5.16929 -0.94780  0.06926350"
   rewind(unit)

   call read_xyz(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, struc%nat, 24, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, 4, "Number of species does not match")
   if (allocated(error)) return

end subroutine test_valid3_xyz


subroutine test_valid4_xyz(error)

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

   call read_xyz(struc, unit, error)
   if (.not.allocated(error)) then
      call read_xyz(struc, unit, error)
   end if
   close(unit)
   if (allocated(error)) return

   call check(error, struc%nat, 3, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, 2, "Number of species does not match")
   if (allocated(error)) return

end subroutine test_valid4_xyz


subroutine test_valid5_xyz(error)

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

   call read_xyz(struc, unit, error)
   if (.not.allocated(error)) then
      call read_xyz(struc, unit, error)
   end if
   close(unit)
   if (allocated(error)) return

   call check(error, struc%nat, 3, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, 2, "Number of species does not match")
   if (allocated(error)) return

end subroutine test_valid5_xyz


subroutine test_invalid1_xyz(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "9", &
      "WATER27, (H2O)3", &
      "O     1.1847029    1.1150792   -0.0344641 ", &
      "H     0.4939088    0.9563767    0.6340089 ", &
      "H     2.0242676    1.0811246    0.4301417 ", &
      "O    -1.1469443    0.0697649    1.1470196 ", &
      "H    -1.2798308   -0.5232169    1.8902833 ", &
      "H    -1.0641398   -0.4956693    0.3569250 "
   rewind(unit)

   call read_xyz(struc, unit, error)
   close(unit)
   if (allocated(error)) return

end subroutine test_invalid1_xyz


subroutine test_invalid2_xyz(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "", &
      "WATER27, (H2O)3", &
      "O     1.1847029    1.1150792   -0.0344641 ", &
      "H     0.4939088    0.9563767    0.6340089 ", &
      "H     2.0242676    1.0811246    0.4301417 ", &
      "O    -1.1469443    0.0697649    1.1470196 ", &
      "H    -1.2798308   -0.5232169    1.8902833 ", &
      "H    -1.0641398   -0.4956693    0.3569250 ", &
      "O    -0.1633508   -1.0289346   -1.2401808 ", &
      "H     0.4914771   -0.3248733   -1.0784838 ", &
      "H    -0.5400907   -0.8496512   -2.1052499 "
   rewind(unit)

   call read_xyz(struc, unit, error)
   close(unit)
   if (allocated(error)) return

end subroutine test_invalid2_xyz


subroutine test_invalid3_xyz(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      & "120"
   rewind(unit)

   call read_xyz(struc, unit, error)
   close(unit)
   if (allocated(error)) return

end subroutine test_invalid3_xyz


subroutine test_invalid4_xyz(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "-3", &
      "H2O", &
      "O     1.1847029    1.1150792   -0.0344641 ", &
      "H     0.4939088    0.9563767    0.6340089 ", &
      "H     2.0242676    1.0811246    0.4301417 "
   rewind(unit)

   call read_xyz(struc, unit, error)
   close(unit)
   if (allocated(error)) return

end subroutine test_invalid4_xyz


subroutine test_invalid5_xyz(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "3", &
      "H2O", &
      "****o   1.1847029    1.1150792   -0.0344641 ", &
      "****h   0.4939088    0.9563767    0.6340089 ", &
      "****h   2.0242676    1.0811246    0.4301417 "
   rewind(unit)

   call read_xyz(struc, unit, error)
   close(unit)
   if (allocated(error)) return

end subroutine test_invalid5_xyz


subroutine test_invalid6_xyz(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "3", &
      "H2O", &
      "1.1847029    1.1150792   -0.0344641      O", &
      "0.4939088    0.9563767    0.6340089      H", &
      "2.0242676    1.0811246    0.4301417      H"
   rewind(unit)

   call read_xyz(struc, unit, error)
   close(unit)
   if (allocated(error)) return

end subroutine test_invalid6_xyz


subroutine test_invalid7_xyz(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "nine", &
      "WATER27, (H2O)3", &
      "O     1.1847029    1.1150792   -0.0344641 ", &
      "H     0.4939088    0.9563767    0.6340089 ", &
      "H     2.0242676    1.0811246    0.4301417 ", &
      "O    -1.1469443    0.0697649    1.1470196 ", &
      "H    -1.2798308   -0.5232169    1.8902833 ", &
      "H    -1.0641398   -0.4956693    0.3569250 ", &
      "O    -0.1633508   -1.0289346   -1.2401808 ", &
      "H     0.4914771   -0.3248733   -1.0784838 ", &
      "H    -0.5400907   -0.8496512   -2.1052499 "
   rewind(unit)

   call read_xyz(struc, unit, error)
   close(unit)

end subroutine test_invalid7_xyz


end module test_read_xyz
