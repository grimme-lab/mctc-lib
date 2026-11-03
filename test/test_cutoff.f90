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

module test_cutoff
   use, intrinsic :: ieee_arithmetic, only : ieee_value, ieee_quiet_nan, ieee_positive_inf
   use mctc_cutoff, only : get_lattice_points, wrap_to_central_cell
   use mctc_env, only : wp
   use mctc_env_testing, only : new_unittest, unittest_type, error_type, &
   & check, test_failed
   use mctc_io_structure, only : structure_type
   use testsuite_structure, only : get_structure
   implicit none
   private

   public :: collect_cutoff

   real(wp), parameter :: thr = 100*epsilon(1.0_wp)
   real(wp), parameter :: thr2 = sqrt(epsilon(1.0_wp))

contains


   !> Collect all exported unit tests
   subroutine collect_cutoff(testsuite)

      !> Collection of tests
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
      & new_unittest("lattice-points-cutoff", test_lattice_points_cutoff), &
      & new_unittest("lattice-points-rep", test_lattice_points_rep), &
      & new_unittest("lattice-points-partial-pbc", test_lattice_points_partial_pbc), &
      & new_unittest("lattice-points-overflow-and-errors", test_lattice_points_overflow_and_errors), &
      & new_unittest("wrap", test_wrap), &
      & new_unittest("wrap-partial-pbc", test_wrap_partial_pbc) &
      & ]

   end subroutine collect_cutoff


   subroutine test_lattice_points_cutoff(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      real(wp), allocatable :: lattr(:, :)

      real(wp), parameter :: cutoff = 15.0_wp
      real(wp), parameter :: ref(3, 125) = reshape([&
      &  0.00000000000E+0_wp,  0.00000000000E+0_wp,  0.00000000000E+0_wp, &
      &  0.00000000000E+0_wp,  0.00000000000E+0_wp,  9.47387737000E+0_wp, &
      &  0.00000000000E+0_wp,  0.00000000000E+0_wp, -9.47387737000E+0_wp, &
      &  0.00000000000E+0_wp,  0.00000000000E+0_wp,  1.89477547400E+1_wp, &
      &  0.00000000000E+0_wp,  0.00000000000E+0_wp, -1.89477547400E+1_wp, &
      &  0.00000000000E+0_wp,  9.47387737000E+0_wp,  0.00000000000E+0_wp, &
      &  0.00000000000E+0_wp, -9.47387737000E+0_wp,  0.00000000000E+0_wp, &
      &  0.00000000000E+0_wp,  9.47387737000E+0_wp,  9.47387737000E+0_wp, &
      &  0.00000000000E+0_wp,  9.47387737000E+0_wp, -9.47387737000E+0_wp, &
      &  0.00000000000E+0_wp, -9.47387737000E+0_wp,  9.47387737000E+0_wp, &
      &  0.00000000000E+0_wp, -9.47387737000E+0_wp, -9.47387737000E+0_wp, &
      &  0.00000000000E+0_wp,  9.47387737000E+0_wp,  1.89477547400E+1_wp, &
      &  0.00000000000E+0_wp,  9.47387737000E+0_wp, -1.89477547400E+1_wp, &
      &  0.00000000000E+0_wp, -9.47387737000E+0_wp,  1.89477547400E+1_wp, &
      &  0.00000000000E+0_wp, -9.47387737000E+0_wp, -1.89477547400E+1_wp, &
      &  0.00000000000E+0_wp,  1.89477547400E+1_wp,  0.00000000000E+0_wp, &
      &  0.00000000000E+0_wp, -1.89477547400E+1_wp,  0.00000000000E+0_wp, &
      &  0.00000000000E+0_wp,  1.89477547400E+1_wp,  9.47387737000E+0_wp, &
      &  0.00000000000E+0_wp,  1.89477547400E+1_wp, -9.47387737000E+0_wp, &
      &  0.00000000000E+0_wp, -1.89477547400E+1_wp,  9.47387737000E+0_wp, &
      &  0.00000000000E+0_wp, -1.89477547400E+1_wp, -9.47387737000E+0_wp, &
      &  0.00000000000E+0_wp,  1.89477547400E+1_wp,  1.89477547400E+1_wp, &
      &  0.00000000000E+0_wp,  1.89477547400E+1_wp, -1.89477547400E+1_wp, &
      &  0.00000000000E+0_wp, -1.89477547400E+1_wp,  1.89477547400E+1_wp, &
      &  0.00000000000E+0_wp, -1.89477547400E+1_wp, -1.89477547400E+1_wp, &
      &  9.47387737000E+0_wp,  0.00000000000E+0_wp,  0.00000000000E+0_wp, &
      & -9.47387737000E+0_wp,  0.00000000000E+0_wp,  0.00000000000E+0_wp, &
      &  9.47387737000E+0_wp,  0.00000000000E+0_wp,  9.47387737000E+0_wp, &
      &  9.47387737000E+0_wp,  0.00000000000E+0_wp, -9.47387737000E+0_wp, &
      & -9.47387737000E+0_wp,  0.00000000000E+0_wp,  9.47387737000E+0_wp, &
      & -9.47387737000E+0_wp,  0.00000000000E+0_wp, -9.47387737000E+0_wp, &
      &  9.47387737000E+0_wp,  0.00000000000E+0_wp,  1.89477547400E+1_wp, &
      &  9.47387737000E+0_wp,  0.00000000000E+0_wp, -1.89477547400E+1_wp, &
      & -9.47387737000E+0_wp,  0.00000000000E+0_wp,  1.89477547400E+1_wp, &
      & -9.47387737000E+0_wp,  0.00000000000E+0_wp, -1.89477547400E+1_wp, &
      &  9.47387737000E+0_wp,  9.47387737000E+0_wp,  0.00000000000E+0_wp, &
      &  9.47387737000E+0_wp, -9.47387737000E+0_wp,  0.00000000000E+0_wp, &
      & -9.47387737000E+0_wp,  9.47387737000E+0_wp,  0.00000000000E+0_wp, &
      & -9.47387737000E+0_wp, -9.47387737000E+0_wp,  0.00000000000E+0_wp, &
      &  9.47387737000E+0_wp,  9.47387737000E+0_wp,  9.47387737000E+0_wp, &
      &  9.47387737000E+0_wp,  9.47387737000E+0_wp, -9.47387737000E+0_wp, &
      &  9.47387737000E+0_wp, -9.47387737000E+0_wp,  9.47387737000E+0_wp, &
      &  9.47387737000E+0_wp, -9.47387737000E+0_wp, -9.47387737000E+0_wp, &
      & -9.47387737000E+0_wp,  9.47387737000E+0_wp,  9.47387737000E+0_wp, &
      & -9.47387737000E+0_wp,  9.47387737000E+0_wp, -9.47387737000E+0_wp, &
      & -9.47387737000E+0_wp, -9.47387737000E+0_wp,  9.47387737000E+0_wp, &
      & -9.47387737000E+0_wp, -9.47387737000E+0_wp, -9.47387737000E+0_wp, &
      &  9.47387737000E+0_wp,  9.47387737000E+0_wp,  1.89477547400E+1_wp, &
      &  9.47387737000E+0_wp,  9.47387737000E+0_wp, -1.89477547400E+1_wp, &
      &  9.47387737000E+0_wp, -9.47387737000E+0_wp,  1.89477547400E+1_wp, &
      &  9.47387737000E+0_wp, -9.47387737000E+0_wp, -1.89477547400E+1_wp, &
      & -9.47387737000E+0_wp,  9.47387737000E+0_wp,  1.89477547400E+1_wp, &
      & -9.47387737000E+0_wp,  9.47387737000E+0_wp, -1.89477547400E+1_wp, &
      & -9.47387737000E+0_wp, -9.47387737000E+0_wp,  1.89477547400E+1_wp, &
      & -9.47387737000E+0_wp, -9.47387737000E+0_wp, -1.89477547400E+1_wp, &
      &  9.47387737000E+0_wp,  1.89477547400E+1_wp,  0.00000000000E+0_wp, &
      &  9.47387737000E+0_wp, -1.89477547400E+1_wp,  0.00000000000E+0_wp, &
      & -9.47387737000E+0_wp,  1.89477547400E+1_wp,  0.00000000000E+0_wp, &
      & -9.47387737000E+0_wp, -1.89477547400E+1_wp,  0.00000000000E+0_wp, &
      &  9.47387737000E+0_wp,  1.89477547400E+1_wp,  9.47387737000E+0_wp, &
      &  9.47387737000E+0_wp,  1.89477547400E+1_wp, -9.47387737000E+0_wp, &
      &  9.47387737000E+0_wp, -1.89477547400E+1_wp,  9.47387737000E+0_wp, &
      &  9.47387737000E+0_wp, -1.89477547400E+1_wp, -9.47387737000E+0_wp, &
      & -9.47387737000E+0_wp,  1.89477547400E+1_wp,  9.47387737000E+0_wp, &
      & -9.47387737000E+0_wp,  1.89477547400E+1_wp, -9.47387737000E+0_wp, &
      & -9.47387737000E+0_wp, -1.89477547400E+1_wp,  9.47387737000E+0_wp, &
      & -9.47387737000E+0_wp, -1.89477547400E+1_wp, -9.47387737000E+0_wp, &
      &  9.47387737000E+0_wp,  1.89477547400E+1_wp,  1.89477547400E+1_wp, &
      &  9.47387737000E+0_wp,  1.89477547400E+1_wp, -1.89477547400E+1_wp, &
      &  9.47387737000E+0_wp, -1.89477547400E+1_wp,  1.89477547400E+1_wp, &
      &  9.47387737000E+0_wp, -1.89477547400E+1_wp, -1.89477547400E+1_wp, &
      & -9.47387737000E+0_wp,  1.89477547400E+1_wp,  1.89477547400E+1_wp, &
      & -9.47387737000E+0_wp,  1.89477547400E+1_wp, -1.89477547400E+1_wp, &
      & -9.47387737000E+0_wp, -1.89477547400E+1_wp,  1.89477547400E+1_wp, &
      & -9.47387737000E+0_wp, -1.89477547400E+1_wp, -1.89477547400E+1_wp, &
      &  1.89477547400E+1_wp,  0.00000000000E+0_wp,  0.00000000000E+0_wp, &
      & -1.89477547400E+1_wp,  0.00000000000E+0_wp,  0.00000000000E+0_wp, &
      &  1.89477547400E+1_wp,  0.00000000000E+0_wp,  9.47387737000E+0_wp, &
      &  1.89477547400E+1_wp,  0.00000000000E+0_wp, -9.47387737000E+0_wp, &
      & -1.89477547400E+1_wp,  0.00000000000E+0_wp,  9.47387737000E+0_wp, &
      & -1.89477547400E+1_wp,  0.00000000000E+0_wp, -9.47387737000E+0_wp, &
      &  1.89477547400E+1_wp,  0.00000000000E+0_wp,  1.89477547400E+1_wp, &
      &  1.89477547400E+1_wp,  0.00000000000E+0_wp, -1.89477547400E+1_wp, &
      & -1.89477547400E+1_wp,  0.00000000000E+0_wp,  1.89477547400E+1_wp, &
      & -1.89477547400E+1_wp,  0.00000000000E+0_wp, -1.89477547400E+1_wp, &
      &  1.89477547400E+1_wp,  9.47387737000E+0_wp,  0.00000000000E+0_wp, &
      &  1.89477547400E+1_wp, -9.47387737000E+0_wp,  0.00000000000E+0_wp, &
      & -1.89477547400E+1_wp,  9.47387737000E+0_wp,  0.00000000000E+0_wp, &
      & -1.89477547400E+1_wp, -9.47387737000E+0_wp,  0.00000000000E+0_wp, &
      &  1.89477547400E+1_wp,  9.47387737000E+0_wp,  9.47387737000E+0_wp, &
      &  1.89477547400E+1_wp,  9.47387737000E+0_wp, -9.47387737000E+0_wp, &
      &  1.89477547400E+1_wp, -9.47387737000E+0_wp,  9.47387737000E+0_wp, &
      &  1.89477547400E+1_wp, -9.47387737000E+0_wp, -9.47387737000E+0_wp, &
      & -1.89477547400E+1_wp,  9.47387737000E+0_wp,  9.47387737000E+0_wp, &
      & -1.89477547400E+1_wp,  9.47387737000E+0_wp, -9.47387737000E+0_wp, &
      & -1.89477547400E+1_wp, -9.47387737000E+0_wp,  9.47387737000E+0_wp, &
      & -1.89477547400E+1_wp, -9.47387737000E+0_wp, -9.47387737000E+0_wp, &
      &  1.89477547400E+1_wp,  9.47387737000E+0_wp,  1.89477547400E+1_wp, &
      &  1.89477547400E+1_wp,  9.47387737000E+0_wp, -1.89477547400E+1_wp, &
      &  1.89477547400E+1_wp, -9.47387737000E+0_wp,  1.89477547400E+1_wp, &
      &  1.89477547400E+1_wp, -9.47387737000E+0_wp, -1.89477547400E+1_wp, &
      & -1.89477547400E+1_wp,  9.47387737000E+0_wp,  1.89477547400E+1_wp, &
      & -1.89477547400E+1_wp,  9.47387737000E+0_wp, -1.89477547400E+1_wp, &
      & -1.89477547400E+1_wp, -9.47387737000E+0_wp,  1.89477547400E+1_wp, &
      & -1.89477547400E+1_wp, -9.47387737000E+0_wp, -1.89477547400E+1_wp, &
      &  1.89477547400E+1_wp,  1.89477547400E+1_wp,  0.00000000000E+0_wp, &
      &  1.89477547400E+1_wp, -1.89477547400E+1_wp,  0.00000000000E+0_wp, &
      & -1.89477547400E+1_wp,  1.89477547400E+1_wp,  0.00000000000E+0_wp, &
      & -1.89477547400E+1_wp, -1.89477547400E+1_wp,  0.00000000000E+0_wp, &
      &  1.89477547400E+1_wp,  1.89477547400E+1_wp,  9.47387737000E+0_wp, &
      &  1.89477547400E+1_wp,  1.89477547400E+1_wp, -9.47387737000E+0_wp, &
      &  1.89477547400E+1_wp, -1.89477547400E+1_wp,  9.47387737000E+0_wp, &
      &  1.89477547400E+1_wp, -1.89477547400E+1_wp, -9.47387737000E+0_wp, &
      & -1.89477547400E+1_wp,  1.89477547400E+1_wp,  9.47387737000E+0_wp, &
      & -1.89477547400E+1_wp,  1.89477547400E+1_wp, -9.47387737000E+0_wp, &
      & -1.89477547400E+1_wp, -1.89477547400E+1_wp,  9.47387737000E+0_wp, &
      & -1.89477547400E+1_wp, -1.89477547400E+1_wp, -9.47387737000E+0_wp, &
      &  1.89477547400E+1_wp,  1.89477547400E+1_wp,  1.89477547400E+1_wp, &
      &  1.89477547400E+1_wp,  1.89477547400E+1_wp, -1.89477547400E+1_wp, &
      &  1.89477547400E+1_wp, -1.89477547400E+1_wp,  1.89477547400E+1_wp, &
      &  1.89477547400E+1_wp, -1.89477547400E+1_wp, -1.89477547400E+1_wp, &
      & -1.89477547400E+1_wp,  1.89477547400E+1_wp,  1.89477547400E+1_wp, &
      & -1.89477547400E+1_wp,  1.89477547400E+1_wp, -1.89477547400E+1_wp, &
      & -1.89477547400E+1_wp, -1.89477547400E+1_wp,  1.89477547400E+1_wp, &
      & -1.89477547400E+1_wp, -1.89477547400E+1_wp, -1.89477547400E+1_wp],&
      &  shape(ref))

      call get_structure(mol, "x04")

      call get_lattice_points(mol%periodic, mol%lattice, cutoff, lattr, error)
      if (allocated(error)) return

      if (any(abs(lattr - ref) > thr)) then
         call test_failed(error, "Structures do not match")
         print"(3es21.14)", lattr
      end if

   end subroutine test_lattice_points_cutoff

   subroutine test_lattice_points_rep(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      real(wp), allocatable :: lattr(:, :)

      integer, parameter :: rep(3) = [2, 1, 1]
      real(wp), parameter :: ref(3, 44) = reshape([&
      &  0.00000000000E+0_wp,  0.00000000000E+0_wp,  9.47387737000E+0_wp, &
      &  0.00000000000E+0_wp,  0.00000000000E+0_wp, -9.47387737000E+0_wp, &
      &  0.00000000000E+0_wp,  9.47387737000E+0_wp,  0.00000000000E+0_wp, &
      &  0.00000000000E+0_wp, -9.47387737000E+0_wp,  0.00000000000E+0_wp, &
      &  0.00000000000E+0_wp,  9.47387737000E+0_wp,  9.47387737000E+0_wp, &
      &  0.00000000000E+0_wp,  9.47387737000E+0_wp, -9.47387737000E+0_wp, &
      &  0.00000000000E+0_wp, -9.47387737000E+0_wp,  9.47387737000E+0_wp, &
      &  0.00000000000E+0_wp, -9.47387737000E+0_wp, -9.47387737000E+0_wp, &
      &  9.47387737000E+0_wp,  0.00000000000E+0_wp,  0.00000000000E+0_wp, &
      & -9.47387737000E+0_wp,  0.00000000000E+0_wp,  0.00000000000E+0_wp, &
      &  9.47387737000E+0_wp,  0.00000000000E+0_wp,  9.47387737000E+0_wp, &
      &  9.47387737000E+0_wp,  0.00000000000E+0_wp, -9.47387737000E+0_wp, &
      & -9.47387737000E+0_wp,  0.00000000000E+0_wp,  9.47387737000E+0_wp, &
      & -9.47387737000E+0_wp,  0.00000000000E+0_wp, -9.47387737000E+0_wp, &
      &  9.47387737000E+0_wp,  9.47387737000E+0_wp,  0.00000000000E+0_wp, &
      &  9.47387737000E+0_wp, -9.47387737000E+0_wp,  0.00000000000E+0_wp, &
      & -9.47387737000E+0_wp,  9.47387737000E+0_wp,  0.00000000000E+0_wp, &
      & -9.47387737000E+0_wp, -9.47387737000E+0_wp,  0.00000000000E+0_wp, &
      &  9.47387737000E+0_wp,  9.47387737000E+0_wp,  9.47387737000E+0_wp, &
      &  9.47387737000E+0_wp,  9.47387737000E+0_wp, -9.47387737000E+0_wp, &
      &  9.47387737000E+0_wp, -9.47387737000E+0_wp,  9.47387737000E+0_wp, &
      &  9.47387737000E+0_wp, -9.47387737000E+0_wp, -9.47387737000E+0_wp, &
      & -9.47387737000E+0_wp,  9.47387737000E+0_wp,  9.47387737000E+0_wp, &
      & -9.47387737000E+0_wp,  9.47387737000E+0_wp, -9.47387737000E+0_wp, &
      & -9.47387737000E+0_wp, -9.47387737000E+0_wp,  9.47387737000E+0_wp, &
      & -9.47387737000E+0_wp, -9.47387737000E+0_wp, -9.47387737000E+0_wp, &
      &  1.89477547400E+1_wp,  0.00000000000E+0_wp,  0.00000000000E+0_wp, &
      & -1.89477547400E+1_wp,  0.00000000000E+0_wp,  0.00000000000E+0_wp, &
      &  1.89477547400E+1_wp,  0.00000000000E+0_wp,  9.47387737000E+0_wp, &
      &  1.89477547400E+1_wp,  0.00000000000E+0_wp, -9.47387737000E+0_wp, &
      & -1.89477547400E+1_wp,  0.00000000000E+0_wp,  9.47387737000E+0_wp, &
      & -1.89477547400E+1_wp,  0.00000000000E+0_wp, -9.47387737000E+0_wp, &
      &  1.89477547400E+1_wp,  9.47387737000E+0_wp,  0.00000000000E+0_wp, &
      &  1.89477547400E+1_wp, -9.47387737000E+0_wp,  0.00000000000E+0_wp, &
      & -1.89477547400E+1_wp,  9.47387737000E+0_wp,  0.00000000000E+0_wp, &
      & -1.89477547400E+1_wp, -9.47387737000E+0_wp,  0.00000000000E+0_wp, &
      &  1.89477547400E+1_wp,  9.47387737000E+0_wp,  9.47387737000E+0_wp, &
      &  1.89477547400E+1_wp,  9.47387737000E+0_wp, -9.47387737000E+0_wp, &
      &  1.89477547400E+1_wp, -9.47387737000E+0_wp,  9.47387737000E+0_wp, &
      &  1.89477547400E+1_wp, -9.47387737000E+0_wp, -9.47387737000E+0_wp, &
      & -1.89477547400E+1_wp,  9.47387737000E+0_wp,  9.47387737000E+0_wp, &
      & -1.89477547400E+1_wp,  9.47387737000E+0_wp, -9.47387737000E+0_wp, &
      & -1.89477547400E+1_wp, -9.47387737000E+0_wp,  9.47387737000E+0_wp, &
      & -1.89477547400E+1_wp, -9.47387737000E+0_wp, -9.47387737000E+0_wp],&
      &  shape(ref))

      call get_structure(mol, "x04")

      call get_lattice_points(mol%lattice, rep, .false., lattr, error)
      if (allocated(error)) return

      if (any(abs(lattr - ref) > thr)) then
         call test_failed(error, "Structures do not match")
         print"(3es21.14)", lattr
      end if

   end subroutine test_lattice_points_rep


   subroutine test_wrap(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      real(wp), allocatable :: xyz_shift(:,:)

      real(wp), parameter :: ref(3, 32) = reshape([&
      &  8.66499304004865_wp,  9.33379706146984_wp,  7.91683397818847_wp, &
      & 11.12335640640559_wp,  2.15906103856442_wp, 12.28606540623589_wp, &
      &  2.69933763795316_wp,  8.69604933222823_wp,  5.94619038919326_wp, &
      &  4.40748472136020_wp,  2.73477407301820_wp,  1.34585013180550_wp, &
      &  8.94474331294941_wp,  5.24347823723191_wp,  6.03793064749213_wp, &
      & 10.69920497119952_wp,  4.18256695916720_wp,  4.01290993162606_wp, &
      & 10.03588165896165_wp,  5.08392793928533_wp,  1.39546189302066_wp, &
      &  8.49376544841568_wp,  7.47023679468653_wp,  1.28534952220463_wp, &
      &  9.07166275596235_wp,  9.29463739048189_wp,  3.43672461445391_wp, &
      &  8.87252167134021_wp,  8.08457959995134_wp,  6.01243046499065_wp, &
      &  2.61511156242260_wp,  9.98111860346994_wp,  7.83909475310702_wp, &
      &  1.94055488237084_wp,  8.92607461070971_wp, 10.39160309677660_wp, &
      &  3.94058158183755_wp,  9.62934843688878_wp, 12.34968145677051_wp, &
      &  4.41149849250265_wp, -0.13918998706016_wp, 12.14066417045890_wp, &
      &  4.70428751489632_wp,  1.17418327733924_wp,  9.63142334925047_wp, &
      &  2.97542523741995_wp,  0.23498193061563_wp,  7.50807857572976_wp, &
      & 11.00218693527931_wp, 10.01811156711939_wp,  3.26258886294108_wp, &
      &  6.51725105145864_wp,  6.87025658460207_wp,  1.40398487849503_wp, &
      &  9.16505707508531_wp,  6.55996790419566_wp, 12.22499217557358_wp, &
      &  0.47155553650782_wp,  4.68566204197289_wp,  3.97665598787901_wp, &
      & 10.71305929405516_wp,  2.12295125271661_wp,  4.04673290893442_wp, &
      &  7.00559444915079_wp,  4.60292552897028_wp,  5.68707427274543_wp, &
      &  7.82621004905011_wp, 10.93721749961744_wp,  3.36440794575946_wp, &
      &  9.47562563356497_wp,  4.56669825883223_wp,  7.90994201013468_wp, &
      &  5.72226645413659_wp,  8.73073901566233_wp, 11.81706046742993_wp, &
      &  1.10806523402693_wp,  1.08795999732802_wp,  7.75198050780745_wp, &
      &  4.49303750240828_wp,  3.20681326576017_wp,  9.88850009827761_wp, &
      &  3.68234836535172_wp,  0.82971735354827_wp,  5.66899365523961_wp, &
      &  6.67318941946786_wp,  0.84421918391682_wp,  9.09225122218742_wp, &
      & 12.30020769805011_wp,  9.84555048261104_wp, 11.35093359840778_wp, &
      &  1.70097366892267_wp,  6.88253129604813_wp, 10.28258692216369_wp, &
      &  3.02585151643235_wp, 10.74223721977969_wp,  1.45047192019470_wp],&
      &  shape(ref))

      call get_structure(mol, "x01")

      xyz_shift = mol%xyz - mol%lattice(1, 1)

      call wrap_to_central_cell(xyz_shift, mol%lattice, mol%periodic)

      if (any(abs(mol%xyz - ref) > thr)) then
         call test_failed(error, "Structures do not match")
         print"(3es21.14)", mol%xyz
      end if

   end subroutine test_wrap


   subroutine test_lattice_points_partial_pbc(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(error_type), allocatable :: call_err
      real(wp), allocatable :: trans1(:, :), trans2(:, :)
      real(wp) :: lat(3, 3), a1(3), a2(3)
      logical :: masks1d(3, 3), masks2d(3, 3)
      integer :: imask, j, p1, p2, np

      masks1d = reshape([ &
         & .true., .false., .false., &
         & .false., .true., .false., &
         & .false., .false., .true.], [3, 3])
      masks2d = reshape([ &
         & .true., .true., .false., &
         & .true., .false., .true., &
         & .false., .true., .true.], [3, 3])

      ! 1D rotated periodic vector (length 5.0) across TFF, FTF, FFT with zero nonperiodic columns
      a1 = [3.0_wp, 4.0_wp, 0.0_wp]
      do imask = 1, 3
         lat = 0.0_wp
         lat(:, imask) = a1
         call get_lattice_points(masks1d(:, imask), lat, thr2, trans1, error)
         if (allocated(error)) return

         call check(error, size(trans1, 1), 3)
         if (allocated(error)) return
         call check(error, size(trans1, 2), 3)
         if (allocated(error)) return

         ! ceiling(12.0 / 5.0) = 3 -> 2*3 + 1 = 7 translations
         call get_lattice_points(masks1d(:, imask), lat, 12.0_wp, trans1, error)
         if (allocated(error)) return

         call check(error, size(trans1, 2), 7)
         if (allocated(error)) return

         ! Nonperiodic lattice vectors must be completely ignored
         do j = 1, 3
            if (j /= imask) lat(:, j) = [100.0_wp * j, -50.0_wp, 250.0_wp]
         end do
         call get_lattice_points(masks1d(:, imask), lat, 12.0_wp, trans2, error)
         if (allocated(error)) return

         call check(error, size(trans2, 2), 7)
         if (allocated(error)) return
         if (maxval(abs(trans1 - trans2)) > thr) then
            call test_failed(error, "1D lattice points depend on nonperiodic lattice vectors")
            return
         end if
      end do

      ! 2D skewed periodic subspace across TTF, TFT, FTT with zero nonperiodic column
      ! a1 = [5, 0, 0] (len1 = 5), a2 = [4, 3, 0] (len2 = 5), area = 15 -> h1 = h2 = 3.0
      a1 = [5.0_wp, 0.0_wp, 0.0_wp]
      a2 = [4.0_wp, 3.0_wp, 0.0_wp]
      do imask = 1, 3
         select case (imask)
         case (1); p1 = 1; p2 = 2; np = 3
         case (2); p1 = 1; p2 = 3; np = 2
         case default; p1 = 2; p2 = 3; np = 1
         end select
         lat = 0.0_wp
         lat(:, p1) = a1
         lat(:, p2) = a2
         call get_lattice_points(masks2d(:, imask), lat, thr2, trans1, error)
         if (allocated(error)) return

         call check(error, size(trans1, 2), 9)
         if (allocated(error)) return

         ! rthr = 7.0 -> ceiling(7.0 / 3.0) = 3 along both active axes -> (2*3+1)^2 = 49
         call get_lattice_points(masks2d(:, imask), lat, 7.0_wp, trans1, error)
         if (allocated(error)) return

         call check(error, size(trans1, 2), 49)
         if (allocated(error)) return

         lat(:, np) = [42.0_wp, -99.0_wp, 500.0_wp]
         call get_lattice_points(masks2d(:, imask), lat, 7.0_wp, trans2, error)
         if (allocated(error)) return

         call check(error, size(trans2, 2), 49)
         if (allocated(error)) return
         if (maxval(abs(trans1 - trans2)) > thr) then
            call test_failed(error, "2D lattice points depend on nonperiodic lattice vector")
            return
         end if
      end do

      ! Degenerate active periodic subspaces and negative cutoffs must fail explicitly
      ! and leave trans unallocated (never silently returning an origin-only translation set)
      lat = 0.0_wp
      call get_lattice_points([.true., .false., .false.], lat, 10.0_wp, trans1, call_err)
      call check(error, allocated(call_err), .true.)
      if (allocated(error)) return
      call check(error, .not. allocated(trans1), .true.)
      if (allocated(error)) return

      lat(:, 1) = a1
      lat(:, 2) = 2.0_wp * a1
      call get_lattice_points([.true., .true., .false.], lat, 10.0_wp, trans1, call_err)
      call check(error, allocated(call_err), .true.)
      if (allocated(error)) return
      call check(error, .not. allocated(trans1), .true.)
      if (allocated(error)) return

      call get_lattice_points([.true., .false., .false.], lat, -1.0_wp, trans1, call_err)
      call check(error, allocated(call_err), .true.)
      if (allocated(error)) return
      call check(error, .not. allocated(trans1), .true.)
      if (allocated(error)) return

   end subroutine test_lattice_points_partial_pbc


   subroutine test_lattice_points_overflow_and_errors(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(error_type), allocatable :: call_err
      real(wp) :: lat(3, 3)
      real(wp), allocatable :: trans(:, :)

      ! 0D valid case returns origin only
      lat = 0.0_wp
      call get_lattice_points([.false., .false., .false.], lat, 10.0_wp, trans, error)
      if (allocated(error)) return

      call check(error, allocated(trans), .true.)
      if (allocated(error)) return
      call check(error, size(trans, 2), 1)
      if (allocated(error)) return

      ! Large valid 1D repetition count (rep = [20000, 0, 0] -> 40001 points)
      ! with zero inactive lattice columns
      lat = 0.0_wp
      lat(:, 1) = [5.0_wp, 0.0_wp, 0.0_wp]
      call get_lattice_points(lat, [20000, 0, 0], .true., trans, error)
      if (allocated(error)) return

      call check(error, size(trans, 2), 40001)
      if (allocated(error)) return

      ! Large valid 1D cutoff (rthr = 100000.0, len1 = 5.0 -> rep(1) = 20000 -> 40001 points)
      call get_lattice_points([.true., .false., .false.], lat, 100000.0_wp, trans, error)
      if (allocated(error)) return

      call check(error, size(trans, 2), 40001)
      if (allocated(error)) return

      ! 2D with zero inactive lattice column
      lat = 0.0_wp
      lat(:, 1) = [5.0_wp, 0.0_wp, 0.0_wp]
      lat(:, 2) = [0.0_wp, 5.0_wp, 0.0_wp]
      call get_lattice_points([.true., .true., .false.], lat, 10.0_wp, trans, error)
      if (allocated(error)) return

      call check(error, size(trans, 2), 25)
      if (allocated(error)) return

      ! Excessive 2D repetition product exceeding resource limit (10001^2 = 100,020,001 > 10^6)
      call get_lattice_points(lat, [5000, 5000, 0], .true., trans, call_err)
      call check(error, allocated(call_err), .true.)
      if (allocated(error)) return
      call check(error, .not. allocated(trans), .true.)
      if (allocated(error)) return

      ! Excessive 3D repetition product exceeding 32-bit signed integer:
      ! rep = [650, 650, 650] -> (2*650 + 1)^3 = 2,202,075,301 > huge(0)
      lat(:, 3) = [0.0_wp, 0.0_wp, 5.0_wp]
      call get_lattice_points(lat, [650, 650, 650], .true., trans, call_err)
      call check(error, allocated(call_err), .true.)
      if (allocated(error)) return
      call check(error, .not. allocated(trans), .true.)
      if (allocated(error)) return

      ! Extreme 3D repetition bounds near huge(0) must be caught by resource limit guard
      call get_lattice_points(lat, [huge(0), huge(0), huge(0)], .true., trans, call_err)
      call check(error, allocated(call_err), .true.)
      if (allocated(error)) return
      call check(error, .not. allocated(trans), .true.)
      if (allocated(error)) return

      ! Values near real-to-integer conversion limits (rthr / len1 > max_cutoff_trans)
      call get_lattice_points([.true., .false., .false.], lat, 1.0e12_wp, trans, call_err)
      call check(error, allocated(call_err), .true.)
      if (allocated(error)) return
      call check(error, .not. allocated(trans), .true.)
      if (allocated(error)) return

      ! Negative and nonfinite cutoffs
      call get_lattice_points([.true., .false., .false.], lat, -1.0_wp, trans, call_err)
      call check(error, allocated(call_err), .true.)
      if (allocated(error)) return
      call check(error, .not. allocated(trans), .true.)
      if (allocated(error)) return

      call get_lattice_points([.true., .false., .false.], lat, &
         & ieee_value(0.0_wp, ieee_quiet_nan), trans, call_err)
      call check(error, allocated(call_err), .true.)
      if (allocated(error)) return
      call check(error, .not. allocated(trans), .true.)
      if (allocated(error)) return

      call get_lattice_points([.true., .false., .false.], lat, &
         & ieee_value(0.0_wp, ieee_positive_inf), trans, call_err)
      call check(error, allocated(call_err), .true.)
      if (allocated(error)) return
      call check(error, .not. allocated(trans), .true.)
      if (allocated(error)) return

      ! Negative explicit repetition count
      call get_lattice_points(lat, [-1, 0, 0], .true., trans, call_err)
      call check(error, allocated(call_err), .true.)
      if (allocated(error)) return
      call check(error, .not. allocated(trans), .true.)
      if (allocated(error)) return

      ! Degenerate active lattice vectors (1D zero, 2D collinear, 3D coplanar)
      lat = 0.0_wp
      call get_lattice_points([.true., .false., .false.], lat, 10.0_wp, trans, call_err)
      call check(error, allocated(call_err), .true.)
      if (allocated(error)) return
      call check(error, .not. allocated(trans), .true.)
      if (allocated(error)) return

      lat(:, 1) = [5.0_wp, 0.0_wp, 0.0_wp]
      lat(:, 2) = [10.0_wp, 0.0_wp, 0.0_wp]
      call get_lattice_points([.true., .true., .false.], lat, 10.0_wp, trans, call_err)
      call check(error, allocated(call_err), .true.)
      if (allocated(error)) return
      call check(error, .not. allocated(trans), .true.)
      if (allocated(error)) return

      lat(:, 2) = [0.0_wp, 5.0_wp, 0.0_wp]
      lat(:, 3) = [5.0_wp, 5.0_wp, 0.0_wp]
      call get_lattice_points([.true., .true., .true.], lat, 10.0_wp, trans, call_err)
      call check(error, allocated(call_err), .true.)
      if (allocated(error)) return
      call check(error, .not. allocated(trans), .true.)
      if (allocated(error)) return

      ! Nonfinite active lattice vector
      lat(:, 1) = [ieee_value(0.0_wp, ieee_quiet_nan), 0.0_wp, 0.0_wp]
      call get_lattice_points([.true., .false., .false.], lat, 10.0_wp, trans, call_err)
      call check(error, allocated(call_err), .true.)
      if (allocated(error)) return
      call check(error, .not. allocated(trans), .true.)
      if (allocated(error)) return

   end subroutine test_lattice_points_overflow_and_errors


   subroutine test_wrap_partial_pbc(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      real(wp) :: lat(3, 3), a1(3), a2(3), r_perp1(3), r_perp2(3)
      real(wp) :: xyz_ref(3, 3), xyz_test(3, 3), xyz_dummy(3, 3)
      logical :: masks1d(3, 3), masks2d(3, 3)
      integer :: imask, j, p1, p2, np

      masks1d = reshape([ &
         & .true., .false., .false., &
         & .false., .true., .false., &
         & .false., .false., .true.], [3, 3])
      masks2d = reshape([ &
         & .true., .true., .false., &
         & .true., .false., .true., &
         & .false., .true., .true.], [3, 3])

      ! 0D with zero lattice: no-op
      lat = 0.0_wp
      xyz_ref = reshape([ &
         & 1.5_wp, -2.3_wp, 4.7_wp, &
         & -8.1_wp, 12.4_wp, -0.5_wp, &
         & 0.0_wp, 0.0_wp, 100.0_wp], [3, 3])
      xyz_test = xyz_ref
      call wrap_to_central_cell(xyz_test, lat, [.false., .false., .false.])
      if (maxval(abs(xyz_test - xyz_ref)) > thr) then
         call test_failed(error, "0D wrap_to_central_cell modified coordinates")
         return
      end if

      ! 1D rotated periodic vector (a1 = [3, 4, 12], ||a1|| = 13) with orthogonal displacements
      a1 = [3.0_wp, 4.0_wp, 12.0_wp]
      r_perp1 = [4.0_wp, -3.0_wp, 0.0_wp] ! orthogonal to a1
      r_perp2 = [12.0_wp, 0.0_wp, -3.0_wp] ! orthogonal to a1
      xyz_ref(:, 1) = 0.25_wp * a1 + 1.7_wp * r_perp1 - 0.8_wp * r_perp2
      xyz_ref(:, 2) = 0.80_wp * a1 - 2.3_wp * r_perp1 + 3.1_wp * r_perp2
      ! Near-boundary point: s1 = 1.0 - 0.5e-14 wraps to -0.5e-14
      xyz_ref(:, 3) = (-0.5e-14_wp) * a1 + 0.5_wp * r_perp1

      do imask = 1, 3
         lat = 0.0_wp
         lat(:, imask) = a1
         xyz_test(:, 1) = xyz_ref(:, 1) - 3.0_wp * a1
         xyz_test(:, 2) = xyz_ref(:, 2) + 5.0_wp * a1
         xyz_test(:, 3) = xyz_ref(:, 3) + 2.0_wp * a1
         call wrap_to_central_cell(xyz_test, lat, masks1d(:, imask))
         if (maxval(abs(xyz_test - xyz_ref)) > 1.0e-11_wp) then
            call test_failed(error, "1D wrap_to_central_cell failed to preserve nonperiodic components")
            return
         end if

         ! Arbitrary nonperiodic columns must have zero effect
         do j = 1, 3
            if (j /= imask) lat(:, j) = [42.0_wp * j, -19.0_wp, 73.0_wp]
         end do
         xyz_dummy(:, 1) = xyz_ref(:, 1) + 4.0_wp * a1
         xyz_dummy(:, 2) = xyz_ref(:, 2) - 2.0_wp * a1
         xyz_dummy(:, 3) = xyz_ref(:, 3) - 1.0_wp * a1
         call wrap_to_central_cell(xyz_dummy, lat, masks1d(:, imask))
         if (maxval(abs(xyz_dummy - xyz_ref)) > 1.0e-11_wp) then
            call test_failed(error, "1D wrap_to_central_cell depends on nonperiodic lattice vectors")
            return
         end if
      end do

      ! 2D skewed rotated periodic subspace: a1 = [4, 3, 0], a2 = [-3, 4, 5], normal = [15, -20, 25]
      a1 = [4.0_wp, 3.0_wp, 0.0_wp]
      a2 = [-3.0_wp, 4.0_wp, 5.0_wp]
      r_perp1 = [3.0_wp, -4.0_wp, 5.0_wp] ! orthogonal to both a1 and a2
      xyz_ref(:, 1) = 0.20_wp * a1 + 0.35_wp * a2 + 2.5_wp * r_perp1
      xyz_ref(:, 2) = 0.75_wp * a1 + 0.60_wp * a2 - 4.1_wp * r_perp1
      xyz_ref(:, 3) = 0.10_wp * a1 + 0.90_wp * a2 + 0.0_wp * r_perp1

      do imask = 1, 3
         select case (imask)
         case (1); p1 = 1; p2 = 2; np = 3
         case (2); p1 = 1; p2 = 3; np = 2
         case default; p1 = 2; p2 = 3; np = 1
         end select
         lat = 0.0_wp
         lat(:, p1) = a1
         lat(:, p2) = a2
         xyz_test(:, 1) = xyz_ref(:, 1) + 3.0_wp * a1 - 2.0_wp * a2
         xyz_test(:, 2) = xyz_ref(:, 2) - 4.0_wp * a1 + 5.0_wp * a2
         xyz_test(:, 3) = xyz_ref(:, 3) + 1.0_wp * a1 + 1.0_wp * a2
         call wrap_to_central_cell(xyz_test, lat, masks2d(:, imask))
         if (maxval(abs(xyz_test - xyz_ref)) > 1.0e-11_wp) then
            call test_failed(error, "2D wrap_to_central_cell failed on skewed rotated subspace")
            return
         end if

         lat(:, np) = [99.0_wp, -77.0_wp, 123.0_wp]
         xyz_dummy(:, 1) = xyz_ref(:, 1) - 2.0_wp * a1 + 4.0_wp * a2
         xyz_dummy(:, 2) = xyz_ref(:, 2) + 1.0_wp * a1 - 3.0_wp * a2
         xyz_dummy(:, 3) = xyz_ref(:, 3) - 5.0_wp * a1 - 2.0_wp * a2
         call wrap_to_central_cell(xyz_dummy, lat, masks2d(:, imask))
         if (maxval(abs(xyz_dummy - xyz_ref)) > 1.0e-11_wp) then
            call test_failed(error, "2D wrap_to_central_cell depends on nonperiodic lattice vector")
            return
         end if
      end do

   end subroutine test_wrap_partial_pbc

end module test_cutoff
