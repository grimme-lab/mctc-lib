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

module test_read_aims
   use mctc_env, only : wp
   use mctc_env_testing, only : new_unittest, unittest_type, error_type, check
   use mctc_io_read_aims
   use mctc_io_structure
   implicit none
   private

   public :: collect_read_aims


contains


!> Collect all exported unit tests
subroutine collect_read_aims(testsuite)

   !> Collection of tests
   type(unittest_type), allocatable, intent(out) :: testsuite(:)

   testsuite = [ &
      & new_unittest("valid1-aims", test_valid1_aims), &
      & new_unittest("valid2-aims", test_valid2_aims), &
      & new_unittest("valid3-aims", test_valid3_aims), &
      & new_unittest("valid4-aims", test_valid4_aims), &
      & new_unittest("valid5-aims", test_valid5_aims), &
      & new_unittest("valid6-aims", test_valid6_aims), &
      & new_unittest("valid7-aims", test_valid7_aims), &
      & new_unittest("invalid1-aims", test_invalid1_aims, should_fail=.true.), &
      & new_unittest("invalid2-aims", test_invalid2_aims, should_fail=.true.), &
      & new_unittest("invalid3-aims", test_invalid3_aims, should_fail=.true.), &
      & new_unittest("invalid4-aims", test_invalid4_aims, should_fail=.true.) &
      & ]

end subroutine collect_read_aims


subroutine test_valid1_aims(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "atom   -0.0090   -0.0157   -0.0000 C", &
      "atom   -0.7131    1.2038   -0.0000 C", &
      "atom    1.3990   -0.0157   -0.0000 C", &
      "atom   -0.0090    2.4232   -0.0000 C", &
      "atom    2.1031    1.2038   -0.0000 C", &
      "atom    1.3990    2.4232    0.0000 C", &
      "atom   -0.5203   -0.9011   -0.0000 H", &
      "atom   -1.7355    1.2038    0.0000 H", &
      "atom    1.9103   -0.9011    0.0000 H", &
      "atom   -0.5203    3.3087    0.0000 H", &
      "atom    3.1255    1.2038    0.0000 H", &
      "atom    1.9103    3.3087   -0.0000 H"
   rewind(unit)

   call read_aims(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, .not.allocated(struc%comment), "Empty comment line should not be saved")
   if (allocated(error)) return
   call check(error, struc%nat, 12, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, 2, "Number of species does not match")
   if (allocated(error)) return

end subroutine test_valid1_aims


subroutine test_valid2_aims(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "atom        1.07317        0.04885       -0.07573  C  ", &
      "atom        2.51365        0.01256       -0.07580  N  ", &
      "atom        3.35199        1.09592       -0.07533  C* ", &
      "atom        4.61898        0.73028       -0.07549  N  ", &
      "atom        4.57907       -0.63144       -0.07531  C* ", &
      "atom        3.30131       -1.10256       -0.07524  C  ", &
      "atom        2.98068       -2.48687       -0.07377  C  ", &
      "# special marked atom", &
      "atom        1.82530       -2.90038       -0.07577  18O", &
      "atom        4.11440       -3.30433       -0.06936  N  ", &
      "atom        5.45174       -2.85618       -0.07235  C* ", &
      "atom        6.38934       -3.65965       -0.07232  O  ", &
      "atom        5.66240       -1.47682       -0.07487  N  ", &
      "atom        7.00947       -0.93648       -0.07524  C  ", &
      "atom        3.92063       -4.74093       -0.06158  C  ", &
      "# isotopes included here", &
      "atom        0.73398        1.08786       -0.07503  D  ", &
      "atom        0.71239       -0.45698        0.82335  D  ", &
      "atom        0.71240       -0.45580       -0.97549  D  ", &
      "atom        2.99301        2.11762       -0.07478  H  ", &
      "atom        7.76531       -1.72634       -0.07591  H  ", &
      "atom        7.14864       -0.32182        0.81969  H  ", &
      "atom        7.14802       -0.32076       -0.96953  H  ", &
      "atom        2.86501       -5.02316       -0.05833  H  ", &
      "atom        4.40233       -5.15920        0.82837  H  ", &
      "atom        4.40017       -5.16929       -0.94780  H  "
   rewind(unit)

   call read_aims(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, .not.allocated(struc%comment), "Empty comment line should not be saved")
   if (allocated(error)) return
   call check(error, struc%nat, 24, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, 7, "Number of species does not match")
   if (allocated(error)) return

end subroutine test_valid2_aims


subroutine test_valid3_aims(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "# 24", &
      "", &
      "atom  1.07317  0.04885 -0.07573  c", &
      "atom  2.51365  0.01256 -0.07580  n", &
      "atom  3.35199  1.09592 -0.07533  c", &
      "atom  4.61898  0.73028 -0.07549  n", &
      "atom  4.57907 -0.63144 -0.07531  c", &
      "atom  3.30131 -1.10256 -0.07524  c", &
      "atom  2.98068 -2.48687 -0.07377  c", &
      "atom  1.82530 -2.90038 -0.07577  o", &
      "atom  4.11440 -3.30433 -0.06936  n", &
      "atom  5.45174 -2.85618 -0.07235  c", &
      "atom  6.38934 -3.65965 -0.07232  o", &
      "atom  5.66240 -1.47682 -0.07487  n", &
      "atom  7.00947 -0.93648 -0.07524  c", &
      "atom  3.92063 -4.74093 -0.06158  c", &
      "atom  0.73398  1.08786 -0.07503  h", &
      "atom  0.71239 -0.45698  0.82335  h", &
      "atom  0.71240 -0.45580 -0.97549  h", &
      "atom  2.99301  2.11762 -0.07478  h", &
      "atom  7.76531 -1.72634 -0.07591  h", &
      "atom  7.14864 -0.32182  0.81969  h", &
      "atom  7.14802 -0.32076 -0.96953  h", &
      "atom  2.86501 -5.02316 -0.05833  h", &
      "atom  4.40233 -5.15920  0.82837  h", &
      "atom  4.40017 -5.16929 -0.94780  h"
   rewind(unit)

   call read_aims(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, struc%nat, 24, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, 4, "Number of species does not match")
   if (allocated(error)) return

end subroutine test_valid3_aims


subroutine test_valid4_aims(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "lattice_vector     4.59373    0.00000    0.00000", &
      "lattice_vector     0.00000    4.59373    0.00000", &
      "lattice_vector     0.00000    0.00000    2.95812", &
      "atom  0.000000000  0.000000000  0.000000000  Ti", &
      "atom  2.296865000  2.296865000  1.479060000  Ti", &
      "atom  1.402465769  1.402465769  0.000000000  O", &
      "atom  3.191264231  3.191264231  0.000000000  O", &
      "atom  3.699330769  0.894399231  1.479060000  O", &
      "atom  0.894399231  3.699330769  1.479060000  O"
   rewind(unit)

   call read_aims(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   if (allocated(error)) return
   call check(error, struc%nat, 6, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, 2, "Number of species does not match")
   if (allocated(error)) return

end subroutine test_valid4_aims


subroutine test_valid5_aims(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "# cubic diamond", &
      "atom  0.0 0.0 0.0 C", &
      "atom_frac  0.25 0.25 0.25 C", &
      "lattice_vector    1.85 1.85 0.0", &
      "lattice_vector    0.0  1.85 1.85", &
      "lattice_vector    1.85 0.0  1.85"
   rewind(unit)

   call read_aims(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, struc%nat, 2, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, 1, "Number of species does not match")
   if (allocated(error)) return

end subroutine test_valid5_aims


subroutine test_valid6_aims(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc1, struc2
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "atom            0.00000000000000   0.00000000000000   0.00000000000000 Mg", &
      "atom            1.48881365396205  -1.48881365396205   0.00000000000000 O", &
      "atom            1.48881365396205   1.48881365396205   0.00000000000000 O", &
      "atom            0.00000000000000   0.00000000000000   2.10550046127949 O", &
      "atom            2.97762730792410   0.00000000000000   0.00000000000000 Mg", &
      "atom            1.48881365396205  -1.48881365396205   2.10550046127949 Mg", &
      "atom            1.48881365396205   1.48881365396205   2.10550046127949 Mg", &
      "atom            2.97762730792410   0.00000000000000   2.10550046127949 O", &
      "lattice_vector  2.97762730792410  -2.97762730792410   0.00000000000000", &
      "lattice_vector  2.97762730792410   2.97762730792410   0.00000000000000"
   rewind(unit)

   call read_aims(struc1, unit, error)
   close(unit)
   if (allocated(error)) return

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "atom_frac       0.00000000000000   0.00000000000000   0.00000000000000 Mg", &
      "atom_frac       0.50000000000000   0.00000000000000   0.00000000000000 O", &
      "atom_frac       0.00000000000000   0.50000000000000   0.00000000000000 O", &
      "atom_frac       0.00000000000000   0.00000000000000   2.10550046127949 O", &
      "atom_frac       0.50000000000000   0.50000000000000   0.00000000000000 Mg", &
      "atom_frac       0.50000000000000   0.00000000000000   2.10550046127949 Mg", &
      "atom_frac       0.00000000000000   0.50000000000000   2.10550046127949 Mg", &
      "atom_frac       0.50000000000000   0.50000000000000   2.10550046127949 O", &
      "lattice_vector  2.97762730792410  -2.97762730792410   0.00000000000000", &
      "lattice_vector  2.97762730792410   2.97762730792410   0.00000000000000"
   rewind(unit)

   call read_aims(struc2, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, norm2(struc1%xyz - struc2%xyz), 0.0_wp, "Coordinates do not match")
   if (allocated(error)) return

end subroutine test_valid6_aims


subroutine test_valid7_aims(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "atom             -1.05835465887935   1.85522662363901   0.00000000000000 B", &
      "atom             -1.05835465887935   1.57910813351869   1.38575958673374 N", &
      "atom             -1.05835465887935   0.79318285794365  -0.93200541748473 N", &
      "atom             -1.05835465887935   2.94621239911295  -0.36993970607209 H", &
      "atom             -1.05835465887935   0.24094589146163   1.83951376127452 B", &
      "atom             -1.05835465887935  -0.54497939258025  -0.47825125458585 B", &
      "atom             -1.05835465887935  -0.82109787740880   0.90750834908157 N", &
      "atom             -1.05835465887935   0.01583015330186   2.96930500972370 H", &
      "atom             -1.05835465887935  -1.41084943428660  -1.23810284035547 H", &
      "atom             -1.05835465887935   2.39762594336943   2.10405675666812 H", &
      "atom             -1.05835465887935  -1.85242037510793   1.25721697940438 H", &
      "atom             -1.05835465887935   1.00598756166737  -2.00001121245014 H", &
      "atom              1.05835465887935   0.82109787740880  -0.90750833320625 B", &
      "atom              1.05835465887935   0.54497938728848   0.47825125193996 N", &
      "atom              1.05835465887935  -0.24094588775739  -1.83951375598275 N", &
      "atom              1.05835465887935   1.91208365288273  -1.27744804245341 H", &
      "atom              1.05835465887935  -0.79318285794365   0.93200542277650 B", &
      "atom              1.05835465887935  -1.57910813881047  -1.38575959202551 B", &
      "atom              1.05835465887935  -1.85522662893078   0.00000001308910 N", &
      "atom              1.05835465887935  -1.01829859700301   2.06179667651746 H", &
      "atom              1.05835465887935  -2.44497818051681  -2.14561117356172 H", &
      "atom              1.05835465887935   1.36349719184744   1.19654842346187 H", &
      "atom              1.05835465887935  -2.88654912133814   0.34970864461060 H", &
      "atom              1.05835465887935  -0.02814118763207  -2.90751955094817 H", &
      "lattice_vector    4.23341864610095   0.00000000000000   0.00000000000000"
   rewind(unit)

   call read_aims(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, struc%nat, 24, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, 3, "Number of species does not match")
   if (allocated(error)) return
   call check(error, count(struc%periodic), 1, "Periodicity does not match")
   if (allocated(error)) return

end subroutine test_valid7_aims



subroutine test_invalid1_aims(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "atom   -0.0090   -0.0157   -0.0000 C", &
      "atom   -0.7131    1.2038   -0.0000 C", &
      "atom    1.3990   -0.0157   -0.0000 C", &
      "atom   -0.0090    2.4232   -0.0000 C", &
      "atom    2.1031    1.2038   -0.0000 C", &
      "atom    1.3990    2.4232    0.0000 C", &
      "atom   -0.5203   -0.9011   -0.0000 hh", &
      "atom   -1.7355    1.2038    0.0000 hh", &
      "atom    1.9103   -0.9011    0.0000 hh", &
      "atom   -0.5203    3.3087    0.0000 H", &
      "atom    3.1255    1.2038    0.0000 H", &
      "atom    1.9103    3.3087   -0.0000 H"
   rewind(unit)

   call read_aims(struc, unit, error)
   close(unit)

end subroutine test_invalid1_aims


subroutine test_invalid2_aims(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "# cubic diamond", &
      "atom  0.0 0.0 0.0 C", &
      "atom_frac  0.25 0.25 0.25 C", &
      "lattice    1.85 1.85 0.0", &
      "lattice    0.0  1.85 1.85", &
      "lattice    1.85 0.0  1.85"
   rewind(unit)

   call read_aims(struc, unit, error)
   close(unit)

end subroutine test_invalid2_aims


subroutine test_invalid3_aims(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "lattice_vector     4.59373    0.00000    0.00000", &
      "lattice_vector     0.00000    4.59373    0.00000", &
      "lattice_vector     0.00000    abcdefg    2.95812", &
      "atom  0.000000000  0.000000000  0.000000000  Ti", &
      "atom  2.296865000  2.296865000  1.479060000  Ti", &
      "atom  1.402465769  1.402465769  0.000000000  O", &
      "atom  3.191264231  3.191264231  0.000000000  O", &
      "atom  3.699330769  0.894399231  1.479060000  O", &
      "atom  0.894399231  3.699330769  1.479060000  O"
   rewind(unit)

   call read_aims(struc, unit, error)
   close(unit)

end subroutine test_invalid3_aims


subroutine test_invalid4_aims(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "# nothing", &
      "# to", &
      "# see", &
      "# here"
   rewind(unit)

   call read_aims(struc, unit, error)
   close(unit)

end subroutine test_invalid4_aims


end module test_read_aims
