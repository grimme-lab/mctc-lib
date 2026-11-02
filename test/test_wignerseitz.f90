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

module test_wignerseitz
   use mctc_cutoff, only: get_lattice_points
   use mctc_env, only: wp
   use mctc_env_testing, only: new_unittest, unittest_type, error_type, check, &
      & test_failed
   use mctc_io_structure, only: structure_type, new
   use mctc_wignerseitz, only : wignerseitz_cell, new_wignerseitz_cell, get_pairs, &
      & get_wignerseitz_weights
   use testsuite_structure, only : get_structure
   implicit none
   private

   public :: collect_wignerseitz

   real(wp), parameter :: thr = 100 * epsilon(1.0_wp)
   real(wp), parameter :: thr1 = 1.0e5_wp*epsilon(1.0_wp)
   real(wp), parameter :: thr2 = sqrt(epsilon(1.0_wp))

contains

!> Collect all exported unit tests
subroutine collect_wignerseitz(testsuite)

   !> Collection of tests
   type(unittest_type), allocatable, intent(out) :: testsuite(:)

   testsuite = [ &
      & new_unittest("lattice-points-0d", test_latticepoints_0d), &
      & new_unittest("lattice-points-3d", test_latticepoints_3d), &
      & new_unittest("wignerseitz-cell-0d", test_wsc_0d), &
      & new_unittest("wignerseitz-cell-3d", test_wsc_3d), &
      & new_unittest("get-pairs-all-images", test_get_pairs_all_images), &
      & new_unittest("compact-images", test_compact_images) &
      & ]

end subroutine collect_wignerseitz

subroutine test_latticepoints_0d(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol
   real(wp), allocatable :: trans(:, :)
   real(wp), parameter :: cutoff = 25.0_wp

   call get_structure(mol, "mindless02")

   call get_lattice_points(mol%periodic, mol%lattice, thr2, trans)

   call check(error, size(trans, 1), 3)
   if (allocated(error)) return

   call check(error, size(trans, 2), 1)
   if (allocated(error)) return

   call get_lattice_points(mol%periodic, mol%lattice, cutoff, trans)

   call check(error, size(trans, 1), 3)
   if (allocated(error)) return

   call check(error, size(trans, 2), 1)
   if (allocated(error)) return

end subroutine test_latticepoints_0d

subroutine test_latticepoints_3d(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol
   real(wp), allocatable :: trans(:, :)
   real(wp), parameter :: cutoff = 25.0_wp

   call get_structure(mol, "ice-ii")

   call get_lattice_points(mol%periodic, mol%lattice, thr2, trans)

   call check(error, size(trans, 1), 3)
   if (allocated(error)) return

   call check(error, size(trans, 2), 27)
   if (allocated(error)) return

   call get_lattice_points(mol%periodic, mol%lattice, cutoff, trans)

   call check(error, size(trans, 1), 3)
   if (allocated(error)) return

   call check(error, size(trans, 2), 343)
   if (allocated(error)) return

end subroutine test_latticepoints_3d

subroutine test_wsc_0d(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol
   type(wignerseitz_cell) :: wsc

   call get_structure(mol, "mindless02")

   call new_wignerseitz_cell(wsc, mol)

   call check(error, size(wsc%trans, 1), 3)
   if (allocated(error)) return

   call check(error, size(wsc%trans, 2), 1)
   if (allocated(error)) return

end subroutine test_wsc_0d

subroutine test_wsc_3d(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol
   type(wignerseitz_cell) :: wsc

   call get_structure(mol, "pyrazole")

   call new_wignerseitz_cell(wsc, mol)

   call check(error, size(wsc%trans, 1), 3)
   if (allocated(error)) return

   call check(error, size(wsc%trans, 2), 27)
   if (allocated(error)) return

end subroutine test_wsc_3d

!> All candidate images are tied, the image search must stop after the last one
subroutine test_get_pairs_all_images(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   real(wp), parameter :: trans(3, 2) = reshape([ &
      & 0.0_wp, 0.0_wp, 0.0_wp, 2.0_wp, 0.0_wp, 0.0_wp], [3, 2])
   real(wp), parameter :: rij(3) = [1.0_wp, 0.0_wp, 0.0_wp]
   integer :: nimg, list(2)

   call get_pairs(nimg, trans, rij, list)

   call check(error, nimg, 2)
   if (allocated(error)) return

   call check(error, list(1), 1)
   if (allocated(error)) return

   call check(error, list(2), 2)
   if (allocated(error)) return

end subroutine test_get_pairs_all_images

!> Compact nearest-image storage, including no images and all candidates retained
subroutine test_compact_images(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol
   type(wignerseitz_cell) :: wsc
   real(wp), parameter :: lattice(3, 3) = reshape([ &
      & 10.0_wp, 0.0_wp, 0.0_wp, 0.0_wp, 10.0_wp, 0.0_wp, 0.0_wp, 0.0_wp, 10.0_wp], &
      & [3, 3])
   real(wp), allocatable :: weights(:)

   call new(mol, [1], reshape([0.0_wp, 0.0_wp, 0.0_wp], [3, 1]), &
      & lattice=lattice, periodic=[.true., .true., .true.])

   ! Simple cubic cell, six tied nearest images
   call new_wignerseitz_cell(wsc, mol)
   call check(error, wsc%nimg(1, 1), 6)
   if (allocated(error)) return
   call check(error, size(wsc%tridx, 1), 6)
   if (allocated(error)) return
   allocate(weights(6))
   call get_wignerseitz_weights(wsc, 1, 1, mol%xyz(:, 1), weights)
   call check(error, all(abs(weights - 1.0_wp/6.0_wp) < thr1), "Tied image weights")
   if (allocated(error)) return

   ! All 26 nonzero candidates fall in the smooth nearest-image interval
   mol%lattice = 0.01_wp*lattice
   call new_wignerseitz_cell(wsc, mol)
   call check(error, wsc%nimg(1, 1), 26)
   if (allocated(error)) return
   call check(error, size(wsc%tridx, 1), 26)
   if (allocated(error)) return
   deallocate(weights)
   allocate(weights(26))
   call get_wignerseitz_weights(wsc, 1, 1, mol%xyz(:, 1), weights)
   call check(error, abs(sum(weights) - 1.0_wp) < thr1 .and. all(weights > 0.0_wp), &
      & "Normalized image weights")
   if (allocated(error)) return

   ! Non-periodic structure, no images at all
   mol%periodic = .false.
   call new_wignerseitz_cell(wsc, mol)
   call check(error, wsc%nimg(1, 1), 0)
   if (allocated(error)) return
   call check(error, size(wsc%tridx, 1), 0)
   if (allocated(error)) return

end subroutine test_compact_images

end module test_wignerseitz
