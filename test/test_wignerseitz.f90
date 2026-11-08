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
   use mctc_cutoff, only : get_lattice_points
   use mctc_env, only : wp
   use mctc_env_testing, only : new_unittest, unittest_type, error_type, check, &
      & test_failed
   use mctc_io_structure, only : structure_type, new
   use mctc_wignerseitz, only : wignerseitz_cell, new_wignerseitz_cell, &
      & get_wignerseitz_weights, find_wsc_pairs
   use testsuite_structure, only : get_structure
   implicit none
   private

   public :: collect_wignerseitz

   real(wp), parameter :: thr = 100 * epsilon(1.0_wp)
   real(wp), parameter :: thr2 = sqrt(epsilon(1.0_wp))


contains


!> Collect all exported unit tests
subroutine collect_wignerseitz(testsuite)

   !> Collection of tests
   type(unittest_type), allocatable, intent(out) :: testsuite(:)

   testsuite = [ &
      & new_unittest("compact-images", test_compact_images), &
      & new_unittest("find-wsc-pairs", test_find_wsc_pairs), &
      & new_unittest("lattice-points-0d", test_latticepoints_0d), &
      & new_unittest("lattice-points-3d", test_latticepoints_3d), &
      & new_unittest("wignerseitz-cell-0d", test_wsc_cell_0d), &
      & new_unittest("wignerseitz-cell-1d", test_wsc_cell_1d), &
      & new_unittest("wignerseitz-cell-2d", test_wsc_cell_2d), &
      & new_unittest("wignerseitz-cell-3d", test_wsc_cell_3d), &
      & new_unittest("wignerseitz-skewed-3d", test_wsc_skewed_3d), &
      & new_unittest("wignerseitz-sheared-3d", test_wsc_sheared_3d), &
      & new_unittest("wignerseitz-exhaustive-reference", test_wsc_exhaustive_reference), &
      & new_unittest("wignerseitz-high-symmetry", test_wsc_high_symmetry), &
      & new_unittest("wignerseitz-degenerate-and-limits", test_wsc_degenerate_and_limits), &
      & new_unittest("wignerseitz-derivatives-2d", test_wsc_derivatives_2d), &
      & new_unittest("wignerseitz-derivatives-3d", test_wsc_derivatives_3d), &
      & new_unittest("wignerseitz-window-edge", test_wsc_window_edge) &
      & ]

end subroutine collect_wignerseitz


!> Compact nearest-image storage, including no images and more than 27 images
subroutine test_compact_images(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol
   type(wignerseitz_cell) :: wsc
   real(wp), allocatable :: weights(:)

   integer, parameter :: nat = 1
   integer, parameter :: num(nat) = [1]
   real(wp), parameter :: xyz(3, nat) = reshape([ &
      & 0.0_wp, 0.0_wp, 0.0_wp], &
      & [3, nat])
   real(wp), parameter :: lattice(3, 3) = reshape([ &
      & 10.0_wp,  0.0_wp,  0.0_wp, &
      &  0.0_wp, 10.0_wp,  0.0_wp, &
      &  0.0_wp,  0.0_wp, 10.0_wp], &
      & [3, 3])
   logical, parameter :: periodic(3) = [.true., .true., .true.]

   call new(mol, num, xyz, lattice=lattice, periodic=periodic)

   ! Simple cubic cell, six tied nearest images
   call new_wignerseitz_cell(wsc, mol, error)
   if (allocated(error)) return

   call check(error, wsc%nimg(1, 1), 6)
   if (allocated(error)) return

   call check(error, size(wsc%tridx, 1), 6)
   if (allocated(error)) return

   allocate(weights(6))
   call get_wignerseitz_weights(wsc, 1, 1, mol%xyz(:, 1), weights)
   call check(error, all(abs(weights - 1.0_wp / 6.0_wp) < thr), "Tied image weights")
   if (allocated(error)) return

   ! Small cubic cell, 56 images fall in the smooth nearest-image interval
   mol%lattice = 0.025_wp * lattice
   call new_wignerseitz_cell(wsc, mol, error)
   if (allocated(error)) return

   call check(error, wsc%nimg(1, 1), 56)
   if (allocated(error)) return

   deallocate(weights)
   allocate(weights(56))
   call get_wignerseitz_weights(wsc, 1, 1, mol%xyz(:, 1), weights)
   call check(error, abs(sum(weights) - 1.0_wp) < thr .and. all(weights > 0.0_wp), &
      & "Normalized image weights")
   if (allocated(error)) return

   ! Non-periodic structure, no images at all
   mol%periodic = .false.
   call new_wignerseitz_cell(wsc, mol, error)
   if (allocated(error)) return

   call check(error, wsc%nimg(1, 1), 0)
   if (allocated(error)) return

   call check(error, size(wsc%tridx, 1), 0)
   if (allocated(error)) return

   ! Periodic structure without atoms, no pairs at all
   call new(mol, num(:0), xyz(:, :0), lattice=lattice, periodic=periodic)
   call new_wignerseitz_cell(wsc, mol, error)
   if (allocated(error)) return

   call check(error, size(wsc%nimg), 0)
   if (allocated(error)) return

   call check(error, wsc%nimg_max, 0)
   if (allocated(error)) return

end subroutine test_compact_images


!> Image search for two tied images, only counting and storing them
subroutine test_find_wsc_pairs(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   integer :: nimg, list(2), empty(0)
   real(wp) :: min_r2

   real(wp), parameter :: trans(3, 2) = reshape([ &
      & 0.0_wp, 0.0_wp, 0.0_wp, &
      & 2.0_wp, 0.0_wp, 0.0_wp], &
      & [3, 2])
   real(wp), parameter :: rij(3) = [1.0_wp, 0.0_wp, 0.0_wp]

   call find_wsc_pairs(trans, rij, nimg, empty, min_r2)
   call check(error, nimg, 2)
   if (allocated(error)) return

   call check(error, min_r2, 1.0_wp, thr=thr)
   if (allocated(error)) return

   call find_wsc_pairs(trans, rij, nimg, list, min_r2)
   call check(error, nimg, 2)
   if (allocated(error)) return

   call check(error, all(list == [1, 2]), "Image indices not stored in order")
   if (allocated(error)) return

end subroutine test_find_wsc_pairs


subroutine test_latticepoints_0d(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol
   real(wp), allocatable :: trans(:, :)

   real(wp), parameter :: cutoff = 25.0_wp

   call get_structure(mol, "mindless02")

   call get_lattice_points(mol%periodic, mol%lattice, thr2, trans, error)
   if (allocated(error)) return

   call check(error, size(trans, 1), 3)
   if (allocated(error)) return

   call check(error, size(trans, 2), 1)
   if (allocated(error)) return

   call get_lattice_points(mol%periodic, mol%lattice, cutoff, trans, error)
   if (allocated(error)) return

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

   call get_lattice_points(mol%periodic, mol%lattice, thr2, trans, error)
   if (allocated(error)) return

   call check(error, size(trans, 1), 3)
   if (allocated(error)) return

   call check(error, size(trans, 2), 27)
   if (allocated(error)) return

   call get_lattice_points(mol%periodic, mol%lattice, cutoff, trans, error)
   if (allocated(error)) return

   call check(error, size(trans, 1), 3)
   if (allocated(error)) return

   call check(error, size(trans, 2), 343)
   if (allocated(error)) return

end subroutine test_latticepoints_3d


subroutine test_wsc_cell_0d(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol
   type(wignerseitz_cell) :: wsc

   call get_structure(mol, "mindless02")

   call new_wignerseitz_cell(wsc, mol, error)
   if (allocated(error)) return

   call check(error, size(wsc%trans, 1), 3)
   if (allocated(error)) return

   call check(error, size(wsc%trans, 2), 1)
   if (allocated(error)) return

end subroutine test_wsc_cell_0d


!> Wire along the second lattice vector, the second atom is unwrapped by three
!> periods and lies half way between two images of the first atom
subroutine test_wsc_cell_1d(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol
   type(wignerseitz_cell) :: wsc

   integer, parameter :: nat = 2
   integer, parameter :: num(nat) = [8, 1]
   real(wp), parameter :: xyz(3, nat) = reshape([ &
      &  0.0_wp,  0.0_wp, 0.0_wp, &
      & 10.5_wp, 14.0_wp, 1.2_wp], &
      & [3, nat])
   real(wp), parameter :: lattice(3, 3) = reshape([ &
      & 0.0_wp, 0.0_wp, 0.0_wp, &
      & 3.0_wp, 4.0_wp, 0.0_wp, &
      & 0.0_wp, 0.0_wp, 0.0_wp], &
      & [3, 3])

   call new(mol, num, xyz, lattice=lattice, periodic=[.false., .true., .false.])
   call new_wignerseitz_cell(wsc, mol, error)
   if (allocated(error)) return

   call check(error, wsc%nimg(1, 1), 2)
   if (allocated(error)) return

   call check(error, wsc%nimg(2, 1), 2)
   if (allocated(error)) return

   call verify_wsc_against_ref(mol, wsc, 6, error)
   if (allocated(error)) return

end subroutine test_wsc_cell_1d


!> Square lattice in the yz-plane, the nonperiodic lattice vector is shorter than
!> the periodic ones and must not contribute images
subroutine test_wsc_cell_2d(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol
   type(wignerseitz_cell) :: wsc

   integer, parameter :: nat = 1
   integer, parameter :: num(nat) = [8]
   real(wp), parameter :: xyz(3, nat) = reshape([ &
      & 0.0_wp, 0.0_wp, 0.0_wp], &
      & [3, nat])
   real(wp), parameter :: lattice(3, 3) = reshape([ &
      & 1.5_wp, 0.0_wp, 0.0_wp, &
      & 0.0_wp, 5.2_wp, 0.0_wp, &
      & 0.0_wp, 0.0_wp, 5.2_wp], &
      & [3, 3])

   call new(mol, num, xyz, lattice=lattice, periodic=[.false., .true., .true.])
   call new_wignerseitz_cell(wsc, mol, error)
   if (allocated(error)) return

   call check(error, wsc%nimg(1, 1), 4)
   if (allocated(error)) return

   call verify_wsc_against_ref(mol, wsc, 4, error)
   if (allocated(error)) return

end subroutine test_wsc_cell_2d


!> Molecular crystal with unwrapped coordinates spanning several cells
subroutine test_wsc_cell_3d(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol
   type(wignerseitz_cell) :: wsc

   call get_structure(mol, "pyrazole")

   call new_wignerseitz_cell(wsc, mol, error)
   if (allocated(error)) return

   call verify_wsc_against_ref(mol, wsc, 4, error)
   if (allocated(error)) return

end subroutine test_wsc_cell_3d


!> Skewed cell where the nearest images (2,0,0), (3,0,0), (-2,1,0), and (-3,1,0)
!> lie outside the box of next-neighbor cells
subroutine test_wsc_skewed_3d(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol
   type(wignerseitz_cell) :: wsc
   real(wp) :: weights(4)

   integer, parameter :: nat = 2
   integer, parameter :: num(nat) = [6, 8]
   real(wp), parameter :: xyz(3, nat) = reshape([ &
      &  0.0_wp, 0.0_wp, 0.0_wp, &
      & 12.5_wp, 0.5_wp, 0.0_wp], &
      & [3, nat])
   real(wp), parameter :: lattice(3, 3) = reshape([ &
      &  5.0_wp, 0.0_wp, 0.0_wp, &
      & 25.0_wp, 1.0_wp, 0.0_wp, &
      &  0.0_wp, 0.0_wp, 5.0_wp], &
      & [3, 3])

   call new(mol, num, xyz, lattice=lattice, periodic=[.true., .true., .true.])
   call new_wignerseitz_cell(wsc, mol, error)
   if (allocated(error)) return

   call check(error, wsc%nimg(1, 2), 4)
   if (allocated(error)) return

   call verify_wsc_against_ref(mol, wsc, 10, error)
   if (allocated(error)) return

   call get_wignerseitz_weights(wsc, 1, 2, mol%xyz(:, 2) - mol%xyz(:, 1), weights)
   call check(error, all(abs(weights - 0.25_wp) < thr), "Tied image weights")

end subroutine test_wsc_skewed_3d


!> Unwrapped atoms in a strongly sheared basis must give the same image vectors
!> as wrapped atoms in the reduced basis of the same lattice
subroutine test_wsc_sheared_3d(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol_ref, mol_test
   type(wignerseitz_cell) :: wsc_ref, wsc_test
   real(wp) :: v_ref(3), v_test(3)
   integer :: iat, jat

   integer, parameter :: nat = 3
   integer, parameter :: num(nat) = [6, 8, 1]
   real(wp), parameter :: a1(3) = [5.0_wp, 0.3_wp, 0.1_wp]
   real(wp), parameter :: a2(3) = [0.4_wp, 5.5_wp, -0.2_wp]
   real(wp), parameter :: a3(3) = [0.2_wp, -0.3_wp, 6.0_wp]
   real(wp), parameter :: lat_ref(3, 3) = reshape([a1, a2, a3], [3, 3])
   real(wp), parameter :: lat_test(3, 3) = reshape([ &
      & a1, &
      & a2 + 6.0_wp * a1, &
      & a3 - 6.0_wp * a2 + 6.0_wp * a1], &
      & [3, 3])
   real(wp), parameter :: xyz_ref(3, nat) = reshape([ &
      &  0.1_wp, 0.2_wp, -0.3_wp, &
      &  2.6_wp, 1.9_wp,  3.1_wp, &
      & -1.7_wp, 2.4_wp,  1.2_wp], &
      & [3, nat])
   real(wp), parameter :: xyz_test(3, nat) = reshape([ &
      & xyz_ref(:, 1), &
      & xyz_ref(:, 2) + 3.0_wp * lat_test(:, 1) - 2.0_wp * lat_test(:, 2), &
      & xyz_ref(:, 3) - lat_test(:, 2) + 2.0_wp * lat_test(:, 3)], &
      & [3, nat])
   logical, parameter :: periodic(3) = [.true., .true., .true.]

   call new(mol_ref, num, xyz_ref, lattice=lat_ref, periodic=periodic)
   call new(mol_test, num, xyz_test, lattice=lat_test, periodic=periodic)
   call new_wignerseitz_cell(wsc_ref, mol_ref, error)
   if (allocated(error)) return

   call verify_wsc_against_ref(mol_ref, wsc_ref, 4, error)
   if (allocated(error)) return

   call new_wignerseitz_cell(wsc_test, mol_test, error)
   if (allocated(error)) return

   do iat = 1, nat
      do jat = 1, nat
         call check(error, wsc_test%nimg(jat, iat), wsc_ref%nimg(jat, iat))
         if (allocated(error)) return

         if (iat /= jat) then
            v_ref = mol_ref%xyz(:, jat) - mol_ref%xyz(:, iat) &
               & + wsc_ref%trans(:, wsc_ref%tridx(1, jat, iat))
            v_test = mol_test%xyz(:, jat) - mol_test%xyz(:, iat) &
               & + wsc_test%trans(:, wsc_test%tridx(1, jat, iat))
            if (maxval(abs(v_test - v_ref)) > thr2) then
               call test_failed(error, "3D WSC failed for strongly sheared basis")
               return
            end if
         end if
      end do
   end do

end subroutine test_wsc_sheared_3d


!> Oblique slab and triclinic cell, both in a sheared basis with unwrapped atoms
subroutine test_wsc_exhaustive_reference(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol
   type(wignerseitz_cell) :: wsc
   real(wp) :: lat(3, 3), xyz(3, 3), a1(3), a2(3), a3(3)

   integer, parameter :: nat = 3
   integer, parameter :: num(nat) = [6, 8, 1]
   real(wp), parameter :: pi = 4.0_wp * atan(1.0_wp)
   real(wp), parameter :: ang = 65.0_wp * pi / 180.0_wp

   ! Oblique slab in the xz-plane of the lattice with a 65 degree angle
   a1 = [4.8_wp, 0.3_wp, -0.2_wp]
   a2 = 3.5_wp * [4.8_wp * cos(ang), 4.8_wp * sin(ang), 0.4_wp]
   lat = 0.0_wp
   lat(:, 1) = a1
   lat(:, 3) = a2 + 2.0_wp * a1
   xyz(:, 1) = [0.1_wp, -0.2_wp, 0.3_wp]
   xyz(:, 2) = xyz(:, 1) + 1.39_wp * a1 + 1.39_wp * a2 + [0.0_wp, 0.0_wp, 0.5_wp]
   xyz(:, 3) = xyz(:, 1) - 2.45_wp * a1 + 3.48_wp * a2 + [0.0_wp, 0.0_wp, -0.4_wp]

   call new(mol, num, xyz, lattice=lat, periodic=[.true., .false., .true.])
   call new_wignerseitz_cell(wsc, mol, error)
   if (allocated(error)) return

   call verify_wsc_against_ref(mol, wsc, 16, error)
   if (allocated(error)) return

   ! Triclinic cell
   a1 = [4.5_wp, 0.4_wp, -0.2_wp]
   a2 = [1.8_wp, 4.9_wp, 0.5_wp] + 2.0_wp * a1
   a3 = [-1.1_wp, 1.5_wp, 5.2_wp] + 2.0_wp * (a1 - a2)
   lat = reshape([a1, a2, a3], [3, 3])
   xyz(:, 1) = [0.15_wp, -0.25_wp, 0.10_wp]
   xyz(:, 2) = xyz(:, 1) + 1.38_wp * a1 + 1.41_wp * a2 - 0.49_wp * a3
   xyz(:, 3) = xyz(:, 1) - 2.47_wp * a1 + 0.51_wp * a2 + 2.35_wp * a3

   call new(mol, num, xyz, lattice=lat, periodic=[.true., .true., .true.])
   call new_wignerseitz_cell(wsc, mol, error)
   if (allocated(error)) return

   call verify_wsc_against_ref(mol, wsc, 7, error)
   if (allocated(error)) return

end subroutine test_wsc_exhaustive_reference


!> High-symmetry cells with many tied nearest images
subroutine test_wsc_high_symmetry(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol
   type(wignerseitz_cell) :: wsc
   real(wp) :: weights(12)

   integer, parameter :: nat = 2
   integer, parameter :: num(nat) = [6, 8]
   real(wp), parameter :: xyz_hex(3, nat) = reshape([ &
      & 0.0_wp, 0.0_wp, 0.0_wp, &
      & 2.0_wp, 1.0_wp, 0.0_wp], &
      & [3, nat])
   real(wp), parameter :: lat_hex(3, 3) = reshape([ &
      & 6.0_wp, 0.0_wp, 0.0_wp, &
      & 3.0_wp, 3.0_wp * sqrt(3.0_wp), 0.0_wp, &
      & 0.0_wp, 0.0_wp, 0.0_wp], &
      & [3, 3])
   real(wp), parameter :: lat_tric(3, 3) = reshape([ &
      & 5.0_wp, 0.0_wp, 0.0_wp, &
      & 5.0_wp * 0.49_wp, 5.0_wp * sqrt(0.90_wp), 0.0_wp, &
      & 0.0_wp, 5.0_wp * 0.45_wp * sqrt(0.90_wp), 5.0_wp * sqrt(0.86775_wp)], &
      & [3, 3])
   real(wp), parameter :: xyz_tric(3, nat) = reshape([ &
      & 0.0_wp, 0.0_wp, 0.0_wp, &
      & 0.48_wp * lat_tric(:, 1) - 0.51_wp * lat_tric(:, 2) + 0.49_wp * lat_tric(:, 3)], &
      & [3, nat])
   integer, parameter :: nat_fcc = 1
   integer, parameter :: num_fcc(nat_fcc) = [6]
   real(wp), parameter :: xyz_fcc(3, nat_fcc) = reshape([ &
      & 0.0_wp, 0.0_wp, 0.0_wp], &
      & [3, nat_fcc])
   real(wp), parameter :: lat_fcc(3, 3) = reshape([ &
      & 0.0_wp, 4.0_wp, 4.0_wp, &
      & 4.0_wp, 0.0_wp, 4.0_wp, &
      & 4.0_wp, 4.0_wp, 0.0_wp], &
      & [3, 3])
   logical, parameter :: pbc_2d(3) = [.true., .true., .false.]
   logical, parameter :: pbc_3d(3) = [.true., .true., .true.]

   ! 1. 2D 60-degree hexagonal lattice with six tied self images
   call new(mol, num, xyz_hex, lattice=lat_hex, periodic=pbc_2d)
   call new_wignerseitz_cell(wsc, mol, error)
   if (allocated(error)) return

   call check(error, wsc%nimg(1, 1), 6)
   if (allocated(error)) return

   call verify_wsc_against_ref(mol, wsc, 4, error)
   if (allocated(error)) return

   ! 2. Triclinic 3D cell with nearly half-integer fractional pair coordinates
   call new(mol, num, xyz_tric, lattice=lat_tric, periodic=pbc_3d)
   call new_wignerseitz_cell(wsc, mol, error)
   if (allocated(error)) return

   call verify_wsc_against_ref(mol, wsc, 4, error)
   if (allocated(error)) return

   ! 3. 3D FCC primitive cell with 12 equidistant nearest self images
   call new(mol, num_fcc, xyz_fcc, lattice=lat_fcc, periodic=pbc_3d)
   call new_wignerseitz_cell(wsc, mol, error)
   if (allocated(error)) return

   call check(error, wsc%nimg(1, 1), 12)
   if (allocated(error)) return

   call get_wignerseitz_weights(wsc, 1, 1, xyz_fcc(:, 1), weights)
   call check(error, all(abs(weights - 1.0_wp / 12.0_wp) < thr), "Tied image weights")

end subroutine test_wsc_high_symmetry


!> A degenerate periodic basis and too many candidate translations fail
subroutine test_wsc_degenerate_and_limits(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol
   type(wignerseitz_cell) :: wsc
   type(error_type), allocatable :: wsc_err
   real(wp) :: lat(3, 3), xyz(3, 2)

   integer, parameter :: nat = 2
   integer, parameter :: num(nat) = [6, 8]

   xyz(:, 1) = [0.0_wp, 0.0_wp, 0.0_wp]
   xyz(:, 2) = [1.2_wp, 0.0_wp, 0.0_wp]

   ! Collinear periodic lattice vectors
   lat = 0.0_wp
   lat(:, 1) = [5.0_wp, 0.0_wp, 0.0_wp]
   lat(:, 2) = [10.0_wp, 0.0_wp, 0.0_wp]
   call new(mol, num, xyz, lattice=lat, periodic=[.true., .true., .false.])
   call new_wignerseitz_cell(wsc, mol, wsc_err)
   call check(error, allocated(wsc_err), "Degenerate lattice accepted")
   if (allocated(error)) return

   ! Atoms 120,000 periods apart, more than 100,000 candidate translations
   lat(:, 2) = 0.0_wp
   xyz(:, 2) = [600000.0_wp, 0.0_wp, 0.0_wp]
   call new(mol, num, xyz, lattice=lat, periodic=[.true., .false., .false.])
   call new_wignerseitz_cell(wsc, mol, wsc_err)
   call check(error, allocated(wsc_err), "Candidate translation limit not enforced")

end subroutine test_wsc_degenerate_and_limits


!> Competing images of a slab pair near the cell corner with unequal weights
subroutine test_wsc_derivatives_2d(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol

   integer, parameter :: nat = 2
   integer, parameter :: num(nat) = [8, 1]
   real(wp), parameter :: xyz(3, nat) = reshape([ &
      & 0.0_wp, 0.0_wp, 0.0_wp, &
      & 3.01_wp, 2.995_wp, 0.4_wp], &
      & [3, nat])
   real(wp), parameter :: lattice(3, 3) = reshape([ &
      & 6.0_wp, 0.0_wp, 0.0_wp, &
      & 0.0_wp, 6.0_wp, 0.0_wp, &
      & 0.0_wp, 0.0_wp, 0.0_wp], &
      & [3, 3])

   call new(mol, num, xyz, lattice=lattice, periodic=[.true., .true., .false.])
   call test_derivatives_gen(error, mol, 2, 1)

end subroutine test_wsc_derivatives_2d


!> Competing images near the body center of a cube given in a sheared basis
subroutine test_wsc_derivatives_3d(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol

   integer, parameter :: nat = 2
   integer, parameter :: num(nat) = [8, 1]
   real(wp), parameter :: xyz(3, nat) = reshape([ &
      & 0.0_wp, 0.0_wp, 0.0_wp, &
      & 3.01_wp, 2.995_wp, 3.0035_wp], &
      & [3, nat])
   real(wp), parameter :: lattice(3, 3) = reshape([ &
      & 6.0_wp, 0.0_wp, 0.0_wp, &
      & 6.0_wp, 6.0_wp, 0.0_wp, &
      & -6.0_wp, 6.0_wp, 6.0_wp], &
      & [3, 3])

   call new(mol, num, xyz, lattice=lattice, periodic=[.true., .true., .true.])
   call test_derivatives_gen(error, mol, 2, 1)

end subroutine test_wsc_derivatives_3d


!> Check normalization, sum rules, and finite difference derivatives of the
!> image weights. Images are identified by their physical translation, the
!> displacements keep all images within the smooth switching interval.
subroutine test_derivatives_gen(error, mol, iat, jat)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   !> Molecular structure data
   type(structure_type), intent(in) :: mol

   !> Atom indices of the examined pair
   integer, intent(in) :: iat, jat

   !> Central differences with a relative tolerance, the truncation error of
   !> the steep switching function is about 1e-9 relative for this step
   real(wp), parameter :: step = 1.0e-6_wp, thr_fd = 1.0e-7_wp
   type(structure_type) :: displ
   type(wignerseitz_cell) :: wsc
   integer :: nimg, ic, jc
   real(wp) :: rij(3), strain(3, 3)
   real(wp), allocatable :: weight(:), dwdr(:, :), dwdL(:, :, :), trans(:, :)
   real(wp), allocatable :: wr(:), wl(:)

   call new_wignerseitz_cell(wsc, mol, error)
   if (allocated(error)) return

   nimg = wsc%nimg(jat, iat)
   allocate(weight(nimg), dwdr(3, nimg), dwdL(3, 3, nimg), trans(3, nimg), &
      & wr(nimg), wl(nimg))
   rij = mol%xyz(:, iat) - mol%xyz(:, jat)
   call get_wignerseitz_weights(wsc, jat, iat, rij, weight, dwdr, dwdL)
   trans(:, :) = wsc%trans(:, wsc%tridx(:nimg, jat, iat))

   call check(error, nimg >= 4 .and. maxval(weight) - minval(weight) > 0.1_wp, &
      & "Expected several competing images with unequal weights")
   if (allocated(error)) return

   call check(error, abs(sum(weight) - 1.0_wp) < thr, "Weights are not normalized")
   if (allocated(error)) return

   call check(error, maxval(abs(sum(dwdr, dim=2))) < thr2 &
      & .and. maxval(abs(sum(dwdL, dim=3))) < thr2, &
      & "Weight derivatives do not sum to zero")
   if (allocated(error)) return

   do ic = 1, 3
      displ = mol
      displ%xyz(ic, iat) = mol%xyz(ic, iat) + step
      call get_matched_weights(error, displ, iat, jat, trans, wr)
      if (allocated(error)) return

      displ%xyz(ic, iat) = mol%xyz(ic, iat) - step
      call get_matched_weights(error, displ, iat, jat, trans, wl)
      if (allocated(error)) return

      if (maxval(abs((wr - wl) / (2 * step) - dwdr(ic, :))) &
         & > thr_fd * max(1.0_wp, maxval(abs(dwdr(ic, :))))) then
         call test_failed(error, "Weight derivative w.r.t. the pair vector is wrong")
         return
      end if
   end do

   do ic = 1, 3
      do jc = 1, 3
         strain(:, :) = reshape([1.0_wp, 0.0_wp, 0.0_wp, 0.0_wp, 1.0_wp, 0.0_wp, &
            & 0.0_wp, 0.0_wp, 1.0_wp], [3, 3])
         strain(ic, jc) = strain(ic, jc) + step

         displ = mol
         displ%xyz(:, :) = matmul(strain, mol%xyz)
         displ%lattice(:, :) = matmul(strain, mol%lattice)
         call get_matched_weights(error, displ, iat, jat, matmul(strain, trans), wr)
         if (allocated(error)) return

         strain(ic, jc) = strain(ic, jc) - 2 * step
         displ%xyz(:, :) = matmul(strain, mol%xyz)
         displ%lattice(:, :) = matmul(strain, mol%lattice)
         call get_matched_weights(error, displ, iat, jat, matmul(strain, trans), wl)
         if (allocated(error)) return

         if (maxval(abs((wr - wl) / (2 * step) - dwdL(ic, jc, :))) &
            & > thr_fd * max(1.0_wp, maxval(abs(dwdL(ic, jc, :))))) then
            call test_failed(error, "Weight derivative w.r.t. strain is wrong")
            return
         end if
      end do
   end do

end subroutine test_derivatives_gen


!> Weights of a pair for the given physical translations, the image set must
!> not change
subroutine get_matched_weights(error, mol, iat, jat, trans, weight)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   !> Molecular structure data
   type(structure_type), intent(in) :: mol

   !> Atom indices of the examined pair
   integer, intent(in) :: iat, jat

   !> Physical translations of the images
   real(wp), intent(in) :: trans(:, :)

   !> Weights of the images in the order of the translations
   real(wp), intent(out) :: weight(:)

   type(wignerseitz_cell) :: wsc
   integer :: img, jmg
   logical :: found
   real(wp) :: rij(3), raw(size(weight))

   call new_wignerseitz_cell(wsc, mol, error)
   if (allocated(error)) return

   call check(error, wsc%nimg(jat, iat), size(weight))
   if (allocated(error)) return

   rij = mol%xyz(:, iat) - mol%xyz(:, jat)
   call get_wignerseitz_weights(wsc, jat, iat, rij, raw)
   do img = 1, size(weight)
      found = .false.
      do jmg = 1, size(weight)
         if (maxval(abs(wsc%trans(:, wsc%tridx(jmg, jat, iat)) - trans(:, img))) < thr2) then
            weight(img) = raw(jmg)
            found = .true.
         end if
      end do
      if (.not. found) then
         call test_failed(error, "Image set changed by the displacement")
         return
      end if
   end do

end subroutine get_matched_weights


!> Images on the edge of the switching interval for distant unwrapped atoms,
!> the second image lies at tol * (1 + eta) from the nearest one
subroutine test_wsc_window_edge(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol
   type(wignerseitz_cell) :: wsc
   integer :: icase, nimg, empty(0)
   real(wp) :: lat(3, 3), xyz(3, 2), min_r2
   real(wp), allocatable :: trans(:, :)

   integer, parameter :: ncell(4) = [11904, 15702, 14557, 657]
   real(wp), parameter :: eta(4) = [-1.9993706177917171E-14_wp, &
      & -1.7747065442496475E-14_wp, 1.1962650749815001E-14_wp, &
      & -1.4853366378160838E-15_wp]

   do icase = 1, size(ncell)
      lat(:, :) = 0.0_wp
      lat(1, 1) = sqrt(0.3_wp / (1.8_wp * (1.0_wp + eta(icase))))
      xyz(:, 1) = [0.0_wp, 0.0_wp, 0.0_wp]
      xyz(:, 2) = [(ncell(icase) + 0.6_wp) * lat(1, 1), 0.3_wp, 0.0_wp]
      call new(mol, [6, 8], xyz, lattice=lat, periodic=[.true., .false., .false.])
      call new_wignerseitz_cell(wsc, mol, error)
      if (allocated(error)) return

      ! Same image search with plenty of translations
      call get_lattice_points(lat, [ncell(icase) + 6, 0, 0], .true., trans, error)
      if (allocated(error)) return
      call find_wsc_pairs(trans, xyz(:, 2) - xyz(:, 1), nimg, empty, min_r2)

      call check(error, wsc%nimg(1, 2), nimg)
      if (allocated(error)) return
   end do

end subroutine test_wsc_window_edge


!> Compare the nearest images with an exhaustive search over `n_ref` periods
!> along each periodic direction
subroutine verify_wsc_against_ref(mol, wsc, n_ref, error)

   !> Molecular structure data
   type(structure_type), intent(in) :: mol

   !> Wigner-Seitz cell to check
   type(wignerseitz_cell), intent(in) :: wsc

   !> Number of periods searched along each periodic direction
   integer, intent(in) :: n_ref

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   integer :: iat, jat, r1, r2, r3, n1, n2, n3, nimg_ref, img
   real(wp) :: rij(3), tvec(3), d2, dmin2_ref, dmin2_wsc

   real(wp), parameter :: tol_wsc = 0.3_wp

   r1 = merge(n_ref, 0, mol%periodic(1))
   r2 = merge(n_ref, 0, mol%periodic(2))
   r3 = merge(n_ref, 0, mol%periodic(3))

   do iat = 1, mol%nat
      do jat = 1, mol%nat
         rij = mol%xyz(:, iat) - mol%xyz(:, jat)
         dmin2_ref = huge(1.0_wp)
         do n1 = -r1, r1
            do n2 = -r2, r2
               do n3 = -r3, r3
                  tvec = real(n1, wp) * mol%lattice(:, 1) &
                     & + real(n2, wp) * mol%lattice(:, 2) &
                     & + real(n3, wp) * mol%lattice(:, 3)
                  d2 = sum((rij - tvec)**2)
                  if (d2 < thr2) cycle
                  dmin2_ref = min(dmin2_ref, d2)
               end do
            end do
         end do

         nimg_ref = 0
         do n1 = -r1, r1
            do n2 = -r2, r2
               do n3 = -r3, r3
                  tvec = real(n1, wp) * mol%lattice(:, 1) &
                     & + real(n2, wp) * mol%lattice(:, 2) &
                     & + real(n3, wp) * mol%lattice(:, 3)
                  d2 = sum((rij - tvec)**2)
                  if (d2 < thr2) cycle
                  if (d2 - dmin2_ref <= tol_wsc) nimg_ref = nimg_ref + 1
               end do
            end do
         end do

         call check(error, wsc%nimg(jat, iat), nimg_ref)
         if (allocated(error)) return

         dmin2_wsc = sum((rij - wsc%trans(:, wsc%tridx(1, jat, iat)))**2)
         if (abs(dmin2_wsc - dmin2_ref) > thr2) then
            call test_failed(error, "Nearest image disagrees with exhaustive reference")
            return
         end if

         do img = 1, wsc%nimg(jat, iat)
            d2 = sum((rij - wsc%trans(:, wsc%tridx(img, jat, iat)))**2)
            if (d2 - dmin2_ref > tol_wsc + thr) then
               call test_failed(error, "Image outside of the switching interval")
               return
            end if
         end do
      end do
   end do

end subroutine verify_wsc_against_ref


end module test_wignerseitz
