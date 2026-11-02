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
      & new_unittest("get-pairs-all-images", test_get_pairs_all_images), &
      & new_unittest("compact-images", test_compact_images), &
      & new_unittest("wignerseitz-cell-1d", test_wsc_1d), &
      & new_unittest("wignerseitz-cell-2d", test_wsc_2d), &
      & new_unittest("wignerseitz-cell-3d", test_wsc_3d), &
      & new_unittest("wignerseitz-robustness", test_wsc_robustness) &
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

subroutine test_wsc_1d(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol
   type(wignerseitz_cell) :: wsc
   real(wp) :: lat(3, 3), vec(3), total(3)
   logical :: masks(3, 3)
   integer :: imask, i

   masks = reshape([ &
      & .true., .false., .false., &
      & .false., .true., .false., &
      & .false., .false., .true.], [3, 3])

   do imask = 1, 3
      lat = 0.0_wp
      lat(:, imask) = [3.0_wp, 4.0_wp, 0.0_wp]
      call new(mol, [8, 1], reshape([0.0_wp, 0.0_wp, 0.0_wp, &
         & 1.5_wp, 2.0_wp, 1.2_wp], [3, 2]), &
         & lattice=lat, periodic=masks(:, imask))
      call new_wignerseitz_cell(wsc, mol)
      call check(error, size(wsc%trans, 2), 3)
      if (allocated(error)) return
      ! Atom 1 has 2 closest self images (+a and -a), testing get_pairs exhaustion guard
      call check(error, wsc%nimg(1, 1), 2)
      if (allocated(error)) return
      ! Atom 2 is at half-cell along a, so it has 2 equidistant images to atom 1
      call check(error, wsc%nimg(2, 1), 2)
      if (allocated(error)) return
      total = 0.0_wp
      do i = 1, wsc%nimg(1, 1)
         vec = wsc%trans(:, wsc%tridx(i, 1, 1))
         if (abs(norm2(vec) - 5.0_wp) > thr) then
            call test_failed(error, "1D WSC self image has wrong length")
            return
         end if
         total = total + vec
      end do
      if (maxval(abs(total)) > thr) then
         call test_failed(error, "1D WSC self images do not cancel")
         return
      end if
   end do

end subroutine test_wsc_1d

subroutine test_wsc_2d(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol
   type(wignerseitz_cell) :: wsc
   real(wp) :: lat(3, 3), total(3), vec(3)
   integer :: i

   ! Square 2D lattice in YZ plane (FTT) with nonperiodic X vector shorter than Y/Z
   lat = reshape([ &
      & 1.5_wp, 0.0_wp, 0.0_wp, &
      & 0.0_wp, 5.2_wp, 0.0_wp, &
      & 0.0_wp, 0.0_wp, 5.2_wp], [3, 3])
   call new(mol, [8], reshape([0.0_wp, 0.0_wp, 0.0_wp], [3, 1]), &
      & lattice=lat, periodic=[.false., .true., .true.])
   call new_wignerseitz_cell(wsc, mol)
   call check(error, size(wsc%trans, 2), 9)
   if (allocated(error)) return
   call check(error, wsc%nimg(1, 1), 4)
   if (allocated(error)) return
   total = 0.0_wp
   do i = 1, wsc%nimg(1, 1)
      vec = wsc%trans(:, wsc%tridx(i, 1, 1))
      if (abs(vec(1)) > thr .or. abs(norm2(vec) - 5.2_wp) > thr) then
         call test_failed(error, "2D WSC selected nonperiodic or non-minimal self image")
         return
      end if
      total = total + vec
   end do
   if (maxval(abs(total)) > thr) then
      call test_failed(error, "2D WSC self images do not cancel")
      return
   end if

end subroutine test_wsc_2d

subroutine test_wsc_robustness(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol_ref, mol_test
   type(wignerseitz_cell) :: wsc_ref, wsc_test
   real(wp) :: a1(3), a2(3), lat_ref(3, 3), lat_test(3, 3), xyz_ref(3, 3), xyz_test(3, 3)
   real(wp) :: v_ref(3), v_test(3), weights(16)
   integer :: iat, jat

   ! 1. 1D wire rotated in 3D space with zero nonperiodic lattice columns
   !    and multi-cell atom translations (|n| > 1)
   a1 = [3.0_wp, 4.0_wp, 12.0_wp]
   lat_ref = 0.0_wp
   lat_ref(:, 2) = a1
   xyz_ref = reshape([ &
      & 0.1_wp, 0.2_wp, -0.3_wp, &
      & 1.1_wp, 1.4_wp,  3.5_wp, &
      & -0.8_wp, -0.9_wp, -2.5_wp], [3, 3])
   xyz_test = xyz_ref
   xyz_test(:, 2) = xyz_test(:, 2) + 5.0_wp * a1
   xyz_test(:, 3) = xyz_test(:, 3) - 4.0_wp * a1

   call new(mol_ref, [6, 8, 1], xyz_ref, lattice=lat_ref, periodic=[.false., .true., .false.])
   call new(mol_test, [6, 8, 1], xyz_test, lattice=lat_ref, periodic=[.false., .true., .false.])
   call new_wignerseitz_cell(wsc_ref, mol_ref)
   call new_wignerseitz_cell(wsc_test, mol_test)

   do iat = 1, 3
      do jat = 1, 3
         call check(error, wsc_test%nimg(jat, iat), wsc_ref%nimg(jat, iat))
         if (allocated(error)) return
         if (iat /= jat) then
            v_ref = mol_ref%xyz(:, jat) - mol_ref%xyz(:, iat) &
               & + wsc_ref%trans(:, wsc_ref%tridx(1, jat, iat))
            v_test = mol_test%xyz(:, jat) - mol_test%xyz(:, iat) &
               & + wsc_test%trans(:, wsc_test%tridx(1, jat, iat))
            if (maxval(abs(v_test - v_ref)) > 1.0e-12_wp) then
               call test_failed(error, "1D WSC failed for multi-cell atom displacement")
               return
            end if
         end if
      end do
   end do

   ! 2. 2D slab in rotated 3D subspace with zero nonperiodic column,
   !    skewed unimodular basis transformation (a2' = a2 + 6*a1), and multi-cell shifts
   a1 = [4.0_wp, 3.0_wp, 0.0_wp]
   a2 = [-3.0_wp, 4.0_wp, 5.0_wp]
   lat_ref = 0.0_wp
   lat_ref(:, 1) = a1
   lat_ref(:, 3) = a2

   lat_test = 0.0_wp
   lat_test(:, 1) = a1
   lat_test(:, 3) = a2 + 6.0_wp * a1

   xyz_ref = reshape([ &
      & 0.2_wp, -0.1_wp, 0.3_wp, &
      & 1.4_wp,  1.2_wp, 1.1_wp, &
      & -1.1_wp, 0.8_wp, 1.7_wp], [3, 3])
   xyz_test = xyz_ref
   xyz_test(:, 2) = xyz_test(:, 2) + 3.0_wp * a1 - 4.0_wp * a2
   xyz_test(:, 3) = xyz_test(:, 3) - 5.0_wp * a1 + 2.0_wp * a2

   call new(mol_ref, [6, 8, 1], xyz_ref, lattice=lat_ref, periodic=[.true., .false., .true.])
   call new(mol_test, [6, 8, 1], xyz_test, lattice=lat_test, periodic=[.true., .false., .true.])
   call new_wignerseitz_cell(wsc_ref, mol_ref)
   call new_wignerseitz_cell(wsc_test, mol_test)

   do iat = 1, 3
      do jat = 1, 3
         call check(error, wsc_test%nimg(jat, iat), wsc_ref%nimg(jat, iat))
         if (allocated(error)) return
         if (iat /= jat) then
            v_ref = mol_ref%xyz(:, jat) - mol_ref%xyz(:, iat) &
               & + wsc_ref%trans(:, wsc_ref%tridx(1, jat, iat))
            v_test = mol_test%xyz(:, jat) - mol_test%xyz(:, iat) &
               & + wsc_test%trans(:, wsc_test%tridx(1, jat, iat))
            if (maxval(abs(v_test - v_ref)) > 1.0e-12_wp) then
               call test_failed(error, &
                  & "2D WSC failed under skewed unimodular basis and multi-cell shift")
               return
            end if
         end if
      end do
   end do

   ! 3. Exact 2D Wigner-Seitz corner vs smooth weights and perturbation beyond tol
   lat_ref = 0.0_wp
   lat_ref(:, 1) = [6.0_wp, 0.0_wp, 0.0_wp]
   lat_ref(:, 2) = [0.0_wp, 6.0_wp, 0.0_wp]
   call new(mol_ref, [8, 1], reshape([ &
      & 0.0_wp, 0.0_wp, 0.0_wp, &
      & 3.0_wp, 3.0_wp, 0.4_wp], [3, 2]), &
      & lattice=lat_ref, periodic=[.true., .true., .false.])
   call new_wignerseitz_cell(wsc_ref, mol_ref)
   call check(error, wsc_ref%nimg(2, 1), 4)
   if (allocated(error)) return
   call get_wignerseitz_weights(wsc_ref, 2, 1, mol_ref%xyz(:, 1) - mol_ref%xyz(:, 2), weights)
   if (maxval(abs(weights(1:4) - 0.25_wp)) > thr) then
      call test_failed(error, "2D WSC corner weights are not equal")
      return
   end if

end subroutine test_wsc_robustness

end module test_wignerseitz
