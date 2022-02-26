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

module test_read_qchem
   use mctc_env, only : wp
   use mctc_env_testing, only : new_unittest, unittest_type, error_type, check
   use mctc_io_read_qchem
   use mctc_io_structure
   implicit none
   private

   public :: collect_read_qchem


contains


!> Collect all exported unit tests
subroutine collect_read_qchem(testsuite)

   !> Collection of tests
   type(unittest_type), allocatable, intent(out) :: testsuite(:)

   testsuite = [ &
      & new_unittest("valid1-qchem", test_valid1_qchem), &
      & new_unittest("valid2-qchem", test_valid2_qchem), &
      & new_unittest("valid3-qchem", test_valid3_qchem), &
      & new_unittest("invalid1-qchem", test_invalid1_qchem, should_fail=.true.), &
      & new_unittest("invalid2-qchem", test_invalid2_qchem, should_fail=.true.), &
      & new_unittest("invalid3-qchem", test_invalid3_qchem, should_fail=.true.), &
      & new_unittest("invalid4-qchem", test_invalid4_qchem, should_fail=.true.), &
      & new_unittest("invalid5-qchem", test_invalid5_qchem, should_fail=.true.) &
      & ]

end subroutine collect_read_qchem


subroutine test_valid1_qchem(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "$molecule", &
      "   0 1", &
      "   8   0.000000   0.000000  -0.212195", &
      "   1   1.370265   0.000000   0.848778", &
      "   1  -1.370265   0.000000   0.848778", &
      "$end"
   rewind(unit)

   call read_qchem(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, struc%nat, 3, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, 2, "Number of species does not match")
   if (allocated(error)) return

end subroutine test_valid1_qchem


subroutine test_valid2_qchem(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "", &
      "$molecule", &
      "   0 1", &
      "   O   0.000000   0.000000  -0.212195", &
      "   H   1.370265   0.000000   0.848778", &
      "   H  -1.370265   0.000000   0.848778", &
      "$end"
   rewind(unit)

   call read_qchem(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, struc%nat, 3, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, 2, "Number of species does not match")
   if (allocated(error)) return

end subroutine test_valid2_qchem


subroutine test_valid3_qchem(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "$MOLECULE", &
      "0 1", &
      "c  1.07317  0.04885 -0.07573", &
      "n  2.51365  0.01256 -0.07580", &
      "c  3.35199  1.09592 -0.07533", &
      "n  4.61898  0.73028 -0.07549", &
      "c  4.57907 -0.63144 -0.07531", &
      "c  3.30131 -1.10256 -0.07524", &
      "c  2.98068 -2.48687 -0.07377", &
      "o  1.82530 -2.90038 -0.07577", &
      "n  4.11440 -3.30433 -0.06936", &
      "c  5.45174 -2.85618 -0.07235", &
      "o  6.38934 -3.65965 -0.07232", &
      "n  5.66240 -1.47682 -0.07487", &
      "c  7.00947 -0.93648 -0.07524", &
      "c  3.92063 -4.74093 -0.06158", &
      "h  0.73398  1.08786 -0.07503", &
      "h  0.71239 -0.45698  0.82335", &
      "h  0.71240 -0.45580 -0.97549", &
      "h  2.99301  2.11762 -0.07478", &
      "h  7.76531 -1.72634 -0.07591", &
      "h  7.14864 -0.32182  0.81969", &
      "h  7.14802 -0.32076 -0.96953", &
      "h  2.86501 -5.02316 -0.05833", &
      "h  4.40233 -5.15920  0.82837", &
      "h  4.40017 -5.16929 -0.94780", &
      "$END"
   rewind(unit)

   call read_qchem(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, struc%nat, 24, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, 4, "Number of species does not match")
   if (allocated(error)) return

end subroutine test_valid3_qchem



subroutine test_invalid1_qchem(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "$molecule", &
      "   0 1", &
      "C   -0.0090   -0.0157   -0.0000", &
      "C   -0.7131    1.2038   -0.0000", &
      "C    1.3990   -0.0157   -0.0000", &
      "C   -0.0090    2.4232   -0.0000", &
      "C    2.1031    1.2038   -0.0000", &
      "C    1.3990    2.4232    0.0000", &
      "hh  -0.5203   -0.9011   -0.0000", &
      "hh  -1.7355    1.2038    0.0000", &
      "hh   1.9103   -0.9011    0.0000", &
      "H   -0.5203    3.3087    0.0000", &
      "H    3.1255    1.2038    0.0000", &
      "H    1.9103    3.3087   -0.0000", &
      "$end"
   rewind(unit)

   call read_qchem(struc, unit, error)
   close(unit)

end subroutine test_invalid1_qchem


subroutine test_invalid2_qchem(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "$molecule", &
      "24", &
      "c  1.07317  0.04885 -0.07573", &
      "n  2.51365  0.01256 -0.07580", &
      "c  3.35199  1.09592 -0.07533", &
      "n  4.61898  0.73028 -0.07549", &
      "c  4.57907 -0.63144 -0.07531", &
      "c  3.30131 -1.10256 -0.07524", &
      "c  2.98068 -2.48687 -0.07377", &
      "o  1.82530 -2.90038 -0.07577", &
      "n  4.11440 -3.30433 -0.06936", &
      "c  5.45174 -2.85618 -0.07235", &
      "o  6.38934 -3.65965 -0.07232", &
      "n  5.66240 -1.47682 -0.07487", &
      "c  7.00947 -0.93648 -0.07524", &
      "c  3.92063 -4.74093 -0.06158", &
      "h  0.73398  1.08786 -0.07503", &
      "h  0.71239 -0.45698  0.82335", &
      "h  0.71240 -0.45580 -0.97549", &
      "h  2.99301  2.11762 -0.07478", &
      "h  7.76531 -1.72634 -0.07591", &
      "h  7.14864 -0.32182  0.81969", &
      "h  7.14802 -0.32076 -0.96953", &
      "h  2.86501 -5.02316 -0.05833", &
      "h  4.40233 -5.15920  0.82837", &
      "h  4.40017 -5.16929 -0.94780", &
      "$end"
   rewind(unit)

   call read_qchem(struc, unit, error)
   close(unit)

end subroutine test_invalid2_qchem


subroutine test_invalid3_qchem(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "$molecule", &
      "   0 1", &
      "C   -0.0090   -0.0157   -0.0000", &
      "C   -0.7131    1.2038   -0.0000", &
      "C    1.3990   -0.0157   -0.0000", &
      "C   -0.0090    2.4232   -0.0000", &
      "C    2.1031    1.2038   -0.0000", &
      "C    1.3990    2.4232    0.0000", &
      "H   -0.5203   -0.9011   -0.0000", &
      "H   -1.7355    1.2038    0.0000", &
      "H    1.9103   -0.9011    0.0000", &
      "H   -0.5203    3.3087    0.0000", &
      "H    3.1255    1.2038    0.0000", &
      "H    1.9103    3.3087   -0.0000"
   rewind(unit)

   call read_qchem(struc, unit, error)
   close(unit)

end subroutine test_invalid3_qchem


subroutine test_invalid4_qchem(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "$mol", &
      " 0  1", &
      " 1 0 0 0", &
      "$end"
   rewind(unit)

   call read_qchem(struc, unit, error)
   close(unit)

end subroutine test_invalid4_qchem


subroutine test_invalid5_qchem(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "$molecule", &
      "   0 1", &
      "C    ******    ******    0.0000", &
      "C    ******    1.2038    0.0000", &
      "C    1.3990    ******    0.0000", &
      "C    ******    2.4232    0.0000", &
      "C    2.1031    1.2038    0.0000", &
      "C    1.3990    2.4232    0.0000", &
      "H    ******    ******    0.0000", &
      "H    ******    1.2038    0.0000", &
      "H    1.9103    ******    0.0000", &
      "H    ******    3.3087    0.0000", &
      "H    3.1255    1.2038    0.0000", &
      "H    1.9103    3.3087    0.0000", &
      "$end"
   rewind(unit)

   call read_qchem(struc, unit, error)
   close(unit)

end subroutine test_invalid5_qchem


end module test_read_qchem
