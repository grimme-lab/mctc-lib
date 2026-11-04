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
      & get_wignerseitz_weights, get_pairs
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
      & new_unittest("get-pairs-all-images", test_get_pairs_all_images), &
      & new_unittest("lattice-points-0d", test_latticepoints_0d), &
      & new_unittest("lattice-points-3d", test_latticepoints_3d), &
      & new_unittest("wignerseitz-3d-skewed-counterexample", test_wsc_3d_skewed_counterexample), &
      & new_unittest("wignerseitz-65deg-counterexample", test_wsc_65deg_counterexample), &
      & new_unittest("wignerseitz-cell-0d", test_wsc_cell_0d), &
      & new_unittest("wignerseitz-cell-1d", test_wsc_cell_1d), &
      & new_unittest("wignerseitz-cell-2d", test_wsc_cell_2d), &
      & new_unittest("wignerseitz-cell-3d", test_wsc_cell_3d), &
      & new_unittest("wignerseitz-degenerate-and-limits", test_wsc_degenerate_and_limits), &
      & new_unittest("wignerseitz-exhaustive-reference", test_wsc_exhaustive_reference), &
      & new_unittest("wignerseitz-high-symmetry", test_wsc_high_symmetry), &
      & new_unittest("wignerseitz-robustness-1d", test_wsc_robustness_1d), &
      & new_unittest("wignerseitz-robustness-2d", test_wsc_robustness_2d), &
      & new_unittest("wignerseitz-robustness-2d-corner", test_wsc_robustness_2d_corner), &
      & new_unittest("wignerseitz-sheared-3d", test_wsc_sheared_3d) &
      & ]

end subroutine collect_wignerseitz


!> Compact nearest-image storage, including no images and all candidates retained
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

   ! All 738 competing images fall in the smooth nearest-image interval
   mol%lattice = 0.01_wp * lattice
   call new_wignerseitz_cell(wsc, mol, error)
   if (allocated(error)) return

   call check(error, wsc%nimg(1, 1), 738)
   if (allocated(error)) return

   call check(error, size(wsc%tridx, 1), 738)
   if (allocated(error)) return

   deallocate(weights)
   allocate(weights(738))
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


!> All candidate images are tied, the image search must stop after the last one
subroutine test_get_pairs_all_images(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   integer :: nimg, list(2)

   real(wp), parameter :: trans(3, 2) = reshape([ &
      & 0.0_wp, 0.0_wp, 0.0_wp, &
      & 2.0_wp, 0.0_wp, 0.0_wp], &
      & [3, 2])
   real(wp), parameter :: rij(3) = [1.0_wp, 0.0_wp, 0.0_wp]

   call get_pairs(nimg, trans, rij, list)

   call check(error, nimg, 2)
   if (allocated(error)) return

   call check(error, list(1), 1)
   if (allocated(error)) return

   call check(error, list(2), 2)
   if (allocated(error)) return

end subroutine test_get_pairs_all_images


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


!> Deterministic 3D counterexample where rep = [1, 1, 1] gives d^2 = 56.5 bohr^2
!> while the four true minimum-distance images (2,0,0), (3,0,0), (-2,1,0), (-3,1,0)
!> have d^2 = 6.5 bohr^2.
subroutine test_wsc_3d_skewed_counterexample(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol
   type(wignerseitz_cell) :: wsc
   real(wp) :: rij(3), vec(3), weights(16), d2
   integer :: img
   logical :: found(4)

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
   logical, parameter :: periodic(3) = [.true., .true., .true.]

   call new(mol, num, xyz, lattice=lattice, periodic=periodic)
   call new_wignerseitz_cell(wsc, mol, error)
   if (allocated(error)) return

   ! Atom 1 and Atom 2 must have 4 equidistant minimum images at d^2 = 6.5 bohr^2
   call check(error, wsc%nimg(1, 2), 4)
   if (allocated(error)) return

   call check(error, wsc%nimg(2, 1), 4)
   if (allocated(error)) return

   rij = mol%xyz(:, 2) - mol%xyz(:, 1)
   found(:) = .false.
   do img = 1, wsc%nimg(1, 2)
      vec = wsc%trans(:, wsc%tridx(img, 1, 2))
      d2 = sum((rij - vec)**2)
      if (abs(d2 - 6.5_wp) > thr) then
         call test_failed(error, "3D skewed counterexample selected image with wrong squared distance")
         return
      end if

      if (maxval(abs(vec - [10.0_wp, 0.0_wp, 0.0_wp])) < thr) then
         found(1) = .true.
      end if
      if (maxval(abs(vec - [15.0_wp, 0.0_wp, 0.0_wp])) < thr) then
         found(2) = .true.
      end if
      if (maxval(abs(vec - [15.0_wp, 1.0_wp, 0.0_wp])) < thr) then
         found(3) = .true.
      end if
      if (maxval(abs(vec - [10.0_wp, 1.0_wp, 0.0_wp])) < thr) then
         found(4) = .true.
      end if
   end do

   if (.not. all(found)) then
      call test_failed(error, &
         & "3D skewed counterexample did not recover all four images (2,0,0), (3,0,0), (-2,1,0), (-3,1,0)")
      return
   end if

   call get_wignerseitz_weights(wsc, 1, 2, rij, weights)
   if (maxval(abs(weights(1:4) - 0.25_wp)) > thr) then
      call test_failed(error, "3D skewed counterexample weights are not [0.25, 0.25, 0.25, 0.25]")
      return
   end if

end subroutine test_wsc_3d_skewed_counterexample


subroutine test_wsc_65deg_counterexample(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol
   type(wignerseitz_cell) :: wsc
   real(wp) :: rij(3), vec1(3), vec2(3)
   real(wp) :: d2_found1, d2_found2, d2_expect, weights(16)

   real(wp), parameter :: pi = 4.0_wp * atan(1.0_wp)
   real(wp), parameter :: angle = 65.0_wp * pi / 180.0_wp
   integer, parameter :: nat = 2
   integer, parameter :: num(nat) = [6, 8]
   real(wp), parameter :: lattice(3, 3) = reshape([ &
      & 5.0_wp, 0.0_wp, 0.0_wp, &
      & 5.0_wp * cos(angle), 5.0_wp * sin(angle), 0.0_wp, &
      & 0.0_wp, 0.0_wp, 0.0_wp], &
      & [3, 3])
   real(wp), parameter :: xyz(3, nat) = reshape([ &
      & 0.0_wp, 0.0_wp, 0.0_wp, &
      & 1.39_wp * (lattice(:, 1) + lattice(:, 2))], &
      & [3, nat])
   logical, parameter :: periodic(3) = [.true., .true., .false.]

   call new(mol, num, xyz, lattice=lattice, periodic=periodic)
   call new_wignerseitz_cell(wsc, mol, error)
   if (allocated(error)) return

   ! True minimum-distance images for rij = xyz(:, 2) - xyz(:, 1) are (2, 1) and (1, 2)
   ! with squared distance 8.0781427585 bohr^2 (whereas (1, 1) has 10.8185950039 bohr^2)
   call check(error, wsc%nimg(1, 2), 2)
   if (allocated(error)) return

   call check(error, wsc%nimg(2, 1), 2)
   if (allocated(error)) return

   rij = mol%xyz(:, 2) - mol%xyz(:, 1)
   d2_expect = sum((rij - 2.0_wp * lattice(:, 1) - lattice(:, 2))**2)
   vec1 = rij - wsc%trans(:, wsc%tridx(1, 1, 2))
   vec2 = rij - wsc%trans(:, wsc%tridx(2, 1, 2))
   d2_found1 = sum(vec1**2)
   d2_found2 = sum(vec2**2)

   if (abs(d2_found1 - d2_expect) > thr .or. abs(d2_found2 - d2_expect) > thr) then
      call test_failed(error, "65-degree 2D counterexample failed to find (2,1) and (1,2) images")
      return
   end if

   call get_wignerseitz_weights(wsc, 1, 2, rij, weights)
   if (maxval(abs(weights(1:2) - 0.5_wp)) > thr) then
      call test_failed(error, "65-degree 2D counterexample weights are not [0.5, 0.5]")
      return
   end if

end subroutine test_wsc_65deg_counterexample


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


subroutine test_wsc_cell_1d(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol
   type(wignerseitz_cell) :: wsc
   real(wp) :: lat(3, 3), vec(3), total(3)
   integer :: imask, i

   integer, parameter :: nat = 2
   integer, parameter :: num(nat) = [8, 1]
   real(wp), parameter :: a1(3) = [3.0_wp, 4.0_wp, 0.0_wp]
   real(wp), parameter :: xyz(3, nat) = reshape([ &
      & 0.0_wp, 0.0_wp, 0.0_wp, &
      & 1.5_wp, 2.0_wp, 1.2_wp], &
      & [3, nat])
   logical, parameter :: masks(3, 3) = reshape([ &
      & .true., .false., .false., &
      & .false., .true., .false., &
      & .false., .false., .true.], &
      & [3, 3])

   do imask = 1, 3
      lat = 0.0_wp
      lat(:, imask) = a1
      call new(mol, num, xyz, lattice=lat, periodic=masks(:, imask))
      call new_wignerseitz_cell(wsc, mol, error)
      if (allocated(error)) return

      call verify_wsc_against_ref(mol, wsc, 4, error)
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

end subroutine test_wsc_cell_1d


subroutine test_wsc_cell_2d(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol
   type(wignerseitz_cell) :: wsc
   real(wp) :: total(3), vec(3)
   integer :: i

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
   logical, parameter :: periodic(3) = [.false., .true., .true.]

   ! Square 2D lattice in YZ plane (FTT) with nonperiodic X vector shorter than Y/Z
   call new(mol, num, xyz, lattice=lattice, periodic=periodic)
   call new_wignerseitz_cell(wsc, mol, error)
   if (allocated(error)) return

   call verify_wsc_against_ref(mol, wsc, 4, error)
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

end subroutine test_wsc_cell_2d


subroutine test_wsc_cell_3d(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol
   type(wignerseitz_cell) :: wsc

   call get_structure(mol, "pyrazole")

   call new_wignerseitz_cell(wsc, mol, error)
   if (allocated(error)) return

   call check(error, size(wsc%trans, 1), 3)
   if (allocated(error)) return

   ! pyrazole has unwrapped coordinates spanning several cells
   call check(error, size(wsc%tridx, 1), wsc%nimg_max)
   if (allocated(error)) return

   call verify_wsc_against_ref(mol, wsc, 4, error)
   if (allocated(error)) return

end subroutine test_wsc_cell_3d


!> Degenerate periodic lattices and the limit on candidate translations
subroutine test_wsc_degenerate_and_limits(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol
   type(wignerseitz_cell) :: wsc
   type(error_type), allocatable :: wsc_err
   real(wp) :: lat(3, 3), xyz(3, 2)

   integer, parameter :: nat = 2
   integer, parameter :: num(nat) = [6, 8]
   integer, parameter :: nat1 = 1
   integer, parameter :: num1(nat1) = [6]
   real(wp), parameter :: xyz1(3, nat1) = reshape([ &
      & 0.0_wp, 0.0_wp, 0.0_wp], &
      & [3, nat1])
   real(wp), parameter :: lat_small(3, 3) = reshape([ &
      & 1.0e-6_wp, 0.0_wp,    0.0_wp, &
      & 0.0_wp,    1.0e-6_wp, 0.0_wp, &
      & 0.0_wp,    0.0_wp,    1.0e-6_wp], &
      & [3, 3])
   logical, parameter :: pbc_1d(3) = [.true., .false., .false.]
   logical, parameter :: pbc_2d(3) = [.true., .true., .false.]
   logical, parameter :: pbc_3d(3) = [.true., .true., .true.]

   ! 1. Valid 0D geometry
   call get_structure(mol, "mindless02")
   call new_wignerseitz_cell(wsc, mol, error)
   if (allocated(error)) return

   call check(error, allocated(wsc%trans))
   if (allocated(error)) return

   call check(error, size(wsc%trans, 2), 1)
   if (allocated(error)) return

   ! 2. Valid 1D periodic geometry
   lat = 0.0_wp
   lat(:, 1) = [5.0_wp, 0.0_wp, 0.0_wp]
   xyz(:, 1) = [0.0_wp, 0.0_wp, 0.0_wp]
   xyz(:, 2) = [2.0_wp, 0.0_wp, 0.0_wp]
   call new(mol, num, xyz, lattice=lat, periodic=pbc_1d)
   call new_wignerseitz_cell(wsc, mol, error)
   if (allocated(error)) return

   call check(error, allocated(wsc%trans))
   if (allocated(error)) return

   call check(error, size(wsc%trans, 2), 3)
   if (allocated(error)) return

   call check(error, size(wsc%tridx, 1), wsc%nimg_max)
   if (allocated(error)) return

   ! 2b. Large valid 1D unwrapped shift (19,999 periods -> 39999 translations, below the
   !     limit): image indices are only stored for the 2 competing images
   xyz(:, 1) = 0.0_wp
   xyz(:, 2) = 19999.0_wp * lat(:, 1) + [1.2_wp, 0.0_wp, 0.0_wp]
   call new(mol, num, xyz, lattice=lat, periodic=pbc_1d)
   call new_wignerseitz_cell(wsc, mol, error)
   if (allocated(error)) return

   call check(error, size(wsc%trans, 2) > 2 * 19999)
   if (allocated(error)) return

   call check(error, size(wsc%tridx, 1), wsc%nimg_max)
   if (allocated(error)) return

   call check(error, size(wsc%tridx, 1), 2)
   if (allocated(error)) return

   xyz(:, 2) = [1.2_wp, 0.0_wp, 0.0_wp]

   ! 3. Degenerate active 1D lattice (zero active lattice vector)
   lat = 0.0_wp
   call new(mol, num, xyz, lattice=lat, periodic=pbc_1d)
   call new_wignerseitz_cell(wsc, mol, wsc_err)
   call check(error, allocated(wsc_err))
   if (allocated(error)) return

   call check(error, .not. allocated(wsc%trans))
   if (allocated(error)) return

   ! 4. Degenerate active 2D lattice (collinear active lattice vectors)
   lat = 0.0_wp
   lat(:, 1) = [5.0_wp, 0.0_wp, 0.0_wp]
   lat(:, 2) = [10.0_wp, 0.0_wp, 0.0_wp]
   call new(mol, num, xyz, lattice=lat, periodic=pbc_2d)
   call new_wignerseitz_cell(wsc, mol, wsc_err)
   call check(error, allocated(wsc_err))
   if (allocated(error)) return

   call check(error, .not. allocated(wsc%trans))
   if (allocated(error)) return

   ! 5. Degenerate active 3D lattice (coplanar lattice vectors)
   lat(:, 3) = [3.0_wp, 0.0_wp, 0.0_wp]
   call new(mol, num, xyz, lattice=lat, periodic=pbc_3d)
   call new_wignerseitz_cell(wsc, mol, wsc_err)
   call check(error, allocated(wsc_err))
   if (allocated(error)) return

   call check(error, .not. allocated(wsc%trans))
   if (allocated(error)) return

   ! 6. Candidate translations far beyond the limit (and the integer range)
   lat = 0.0_wp
   lat(:, 1) = [5.0_wp, 0.0_wp, 0.0_wp]
   xyz(:, 1) = [0.0_wp, 0.0_wp, 0.0_wp]
   xyz(:, 2) = [1.0e12_wp, 0.0_wp, 0.0_wp]
   call new(mol, num, xyz, lattice=lat, periodic=pbc_1d)
   call new_wignerseitz_cell(wsc, mol, wsc_err)
   call check(error, allocated(wsc_err))
   if (allocated(error)) return

   call check(error, .not. allocated(wsc%trans))
   if (allocated(error)) return

   ! 7. 1D candidate-translation limit exhaustion: a1 = [5, 0, 0], two atoms separated
   !    by > 100,000 lattice periods along x (120,000 periods -> 600,000 bohr)
   lat = 0.0_wp
   lat(:, 1) = [5.0_wp, 0.0_wp, 0.0_wp]
   xyz(:, 1) = [0.0_wp, 0.0_wp, 0.0_wp]
   xyz(:, 2) = [600000.0_wp, 0.0_wp, 0.0_wp]
   call new(mol, num, xyz, lattice=lat, periodic=pbc_1d)
   call new_wignerseitz_cell(wsc, mol, wsc_err)
   call check(error, allocated(wsc_err))
   if (allocated(error)) return

   call check(error, .not. allocated(wsc%trans))
   if (allocated(error)) return

   ! 8. 2D candidate-translation limit: two atoms separated by 200 periods
   !    along both a1 and a2 so (2*201 + 1)^2 = 162,409 > 100,000 candidate translations
   lat = 0.0_wp
   lat(:, 1) = [5.0_wp, 0.0_wp, 0.0_wp]
   lat(:, 2) = [0.0_wp, 5.0_wp, 0.0_wp]
   xyz(:, 1) = [0.0_wp, 0.0_wp, 0.0_wp]
   xyz(:, 2) = [1001.0_wp, 1001.0_wp, 0.0_wp]
   call new(mol, num, xyz, lattice=lat, periodic=pbc_2d)
   call new_wignerseitz_cell(wsc, mol, wsc_err)
   call check(error, allocated(wsc_err))
   if (allocated(error)) return

   call check(error, .not. allocated(wsc%trans))
   if (allocated(error)) return

   ! 9. Tiny non-degenerate lattice vectors (1.0e-6 bohr), the switching interval
   !    covers far more than the maximum number of candidate translations
   call new(mol, num1, xyz1, lattice=lat_small, periodic=pbc_1d)
   call new_wignerseitz_cell(wsc, mol, wsc_err)
   call check(error, allocated(wsc_err))
   if (allocated(error)) return

   call new(mol, num1, xyz1, lattice=lat_small, periodic=pbc_2d)
   call new_wignerseitz_cell(wsc, mol, wsc_err)
   call check(error, allocated(wsc_err))
   if (allocated(error)) return

   call new(mol, num1, xyz1, lattice=lat_small, periodic=pbc_3d)
   call new_wignerseitz_cell(wsc, mol, wsc_err)
   call check(error, allocated(wsc_err))
   if (allocated(error)) return

end subroutine test_wsc_degenerate_and_limits


subroutine test_wsc_exhaustive_reference(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol, mol_unimod
   type(wignerseitz_cell) :: wsc, wsc_unimod
   type(error_type), allocatable :: wsc_err
   real(wp) :: lat(3, 3), lat_unimod(3, 3), xyz(3, 3), xyz_unimod(3, 3)
   real(wp) :: a1(3), a2(3), a3(3), v_ref(3), v_test(3), weights(16)
   real(wp) :: ang, ratio
   integer :: iang, irat, imask, ishift, iat, jat

   integer, parameter :: nat = 3
   integer, parameter :: num(nat) = [6, 8, 1]
   integer, parameter :: nat2 = 2
   integer, parameter :: num2(nat2) = [6, 8]
   real(wp), parameter :: pi = 4.0_wp * atan(1.0_wp)
   real(wp), parameter :: angles_deg(4) = [60.0_wp, 65.0_wp, 90.0_wp, 120.0_wp]
   real(wp), parameter :: ratios(2) = [1.0_wp, 3.5_wp]
   real(wp), parameter :: xyz1_1d(3) = [0.2_wp, -0.1_wp, 0.3_wp]
   real(wp), parameter :: off2_1d(3) = [0.3_wp, 0.4_wp, -0.2_wp]
   real(wp), parameter :: off3_1d(3) = [-0.2_wp, 0.5_wp, 0.1_wp]
   real(wp), parameter :: a1_2d(3) = [4.8_wp, 0.3_wp, -0.2_wp]
   real(wp), parameter :: xyz1_2d(3) = [0.1_wp, -0.2_wp, 0.3_wp]
   real(wp), parameter :: off2_2d(3) = [0.0_wp, 0.0_wp, 0.5_wp]
   real(wp), parameter :: off3_2d(3) = [0.0_wp, 0.0_wp, -0.4_wp]
   real(wp), parameter :: a1_3d(3) = [4.5_wp, 0.4_wp, -0.2_wp]
   real(wp), parameter :: xyz1_3d(3) = [0.15_wp, -0.25_wp, 0.10_wp]
   real(wp), parameter :: xyz_cube(3, nat) = reshape([ &
      & 0.0_wp, 0.0_wp,   0.0_wp, &
      & 3.0_wp, 3.0_wp,   3.0_wp, &
      & 3.0_wp, 3.005_wp, 0.0_wp], &
      & [3, nat])
   real(wp), parameter :: lat_cube(3, 3) = reshape([ &
      & 6.0_wp, 0.0_wp, 0.0_wp, &
      & 0.0_wp, 6.0_wp, 0.0_wp, &
      & 0.0_wp, 0.0_wp, 6.0_wp], &
      & [3, 3])
   real(wp), parameter :: lat_collinear(3, 3) = reshape([ &
      & 4.0_wp, 0.0_wp, 0.0_wp, &
      & 8.0_wp, 0.0_wp, 0.0_wp, &
      & 0.0_wp, 0.0_wp, 0.0_wp], &
      & [3, 3])
   logical, parameter :: masks_1d(3, 3) = reshape([ &
      & .true., .false., .false., &
      & .false., .true., .false., &
      & .false., .false., .true.], &
      & [3, 3])
   logical, parameter :: masks_2d(3, 3) = reshape([ &
      & .true., .true., .false., &
      & .true., .false., .true., &
      & .false., .true., .true.], &
      & [3, 3])
   logical, parameter :: pbc_2d(3) = [.true., .true., .false.]
   logical, parameter :: pbc_3d(3) = [.true., .true., .true.]

   ! 1. Exhaustive 1D comparison across all periodic masks, rotated axes, and multi-cell shifts
   xyz(:, 1) = xyz1_1d
   do ishift = 1, 3
      a1 = [3.2_wp + 0.5_wp * real(ishift, wp), -2.4_wp, 4.1_wp]
      xyz(:, 2) = xyz(:, 1) + (0.49_wp + real(2 * ishift, wp)) * a1 + off2_1d
      xyz(:, 3) = xyz(:, 1) - (1.51_wp + real(ishift, wp)) * a1 + off3_1d
      do imask = 1, 3
         lat = 0.0_wp
         lat(:, imask) = a1
         call new(mol, num, xyz, lattice=lat, periodic=masks_1d(:, imask))
         call new_wignerseitz_cell(wsc, mol, error)
         if (allocated(error)) return

         call verify_wsc_against_ref(mol, wsc, 16, error)
         if (allocated(error)) return
      end do
   end do

   ! 2. Exhaustive 2D comparison across angles (60, 65, 90, 120 deg), aspect ratios,
   !    periodic masks, unimodular shear, and unwrapped coordinates up to several cells
   a1 = a1_2d
   xyz(:, 1) = xyz1_2d
   do iang = 1, size(angles_deg)
      ang = angles_deg(iang) * pi / 180.0_wp
      do irat = 1, size(ratios)
         ratio = ratios(irat)
         a2 = ratio * [4.8_wp * cos(ang), 4.8_wp * sin(ang), 0.4_wp]
         xyz(:, 2) = xyz(:, 1) + 1.39_wp * a1 + 1.39_wp * a2 + off2_2d
         xyz(:, 3) = xyz(:, 1) - 2.45_wp * a1 + 3.48_wp * a2 + off3_2d
         do imask = 1, 3
            lat = 0.0_wp
            select case (imask)
            case (1)
               lat(:, 1) = a1
               lat(:, 2) = a2
            case (2)
               lat(:, 1) = a1
               lat(:, 3) = a2 + 2.0_wp * a1
            case (3)
               lat(:, 2) = a1 - a2
               lat(:, 3) = a2
            case default
               continue
            end select

            call new(mol, num, xyz, lattice=lat, periodic=masks_2d(:, imask))
            call new_wignerseitz_cell(wsc, mol, error)
            if (allocated(error)) return

            call verify_wsc_against_ref(mol, wsc, 16, error)
            if (allocated(error)) return
         end do
      end do
   end do

   ! 3. Exhaustive 3D validation: triclinic lattices, strongly skewed cells,
   !    unwrapped coordinates, unimodular basis transformations, and multi-period shifts
   a1 = a1_3d
   xyz(:, 1) = xyz1_3d
   do ishift = 1, 3
      a2 = [1.8_wp, 4.9_wp, 0.5_wp] + real(ishift - 1, wp) * a1
      a3 = [-1.1_wp, 1.5_wp, 5.2_wp] + real(ishift - 1, wp) * (a1 - a2)
      lat(:, 1) = a1
      lat(:, 2) = a2
      lat(:, 3) = a3

      xyz(:, 2) = xyz(:, 1) + 1.38_wp * a1 + 1.41_wp * a2 - 0.49_wp * a3
      xyz(:, 3) = xyz(:, 1) - 2.47_wp * a1 + 0.51_wp * a2 + 2.35_wp * a3

      call new(mol, num, xyz, lattice=lat, periodic=pbc_3d)
      call new_wignerseitz_cell(wsc, mol, error)
      if (allocated(error)) return

      call verify_wsc_against_ref(mol, wsc, 7, error)
      if (allocated(error)) return

      ! Unimodular 3D basis transformation: a2' = a2 + 2*a1, a3' = a3 - a1 + a2
      ! combined with multi-period integer lattice shifts of atoms 2 and 3
      lat_unimod(:, 1) = a1
      lat_unimod(:, 2) = a2 + 2.0_wp * a1
      lat_unimod(:, 3) = a3 - a1 + a2
      xyz_unimod = xyz
      xyz_unimod(:, 2) = xyz_unimod(:, 2) + 2.0_wp * a1 - 3.0_wp * a2 + a3
      xyz_unimod(:, 3) = xyz_unimod(:, 3) - 3.0_wp * a1 + 2.0_wp * a2 - 2.0_wp * a3

      call new(mol_unimod, num, xyz_unimod, lattice=lat_unimod, periodic=pbc_3d)
      call new_wignerseitz_cell(wsc_unimod, mol_unimod, error)
      if (allocated(error)) return

      do iat = 1, 3
         do jat = 1, 3
            call check(error, wsc_unimod%nimg(jat, iat), wsc%nimg(jat, iat))
            if (allocated(error)) return

            if (iat /= jat) then
               v_ref = mol%xyz(:, jat) - mol%xyz(:, iat) &
                  & + wsc%trans(:, wsc%tridx(1, jat, iat))
               v_test = mol_unimod%xyz(:, jat) - mol_unimod%xyz(:, iat) &
                  & + wsc_unimod%trans(:, wsc_unimod%tridx(1, jat, iat))
               if (maxval(abs(v_test - v_ref)) > thr2) then
                  call test_failed(error, &
                     & "3D WSC minimum-image vector changed under unimodular transformation and shift")
                  return
               end if
            end if
         end do
      end do
   end do

   ! 4. 3D Wigner-Seitz boundary degeneracies: exact body-center (8 images) and
   !    near-boundary perturbation within tol = 0.3_wp
   call new(mol, num, xyz_cube, lattice=lat_cube, periodic=pbc_3d)
   call new_wignerseitz_cell(wsc, mol, error)
   if (allocated(error)) return

   call verify_wsc_against_ref(mol, wsc, 4, error)
   if (allocated(error)) return

   ! Atom 2 at exact body center of 6x6x6 cube has 8 equidistant nearest images
   call check(error, wsc%nimg(1, 2), 8)
   if (allocated(error)) return

   call get_wignerseitz_weights(wsc, 1, 2, mol%xyz(:, 2) - mol%xyz(:, 1), weights)
   if (maxval(abs(weights(1:8) - 0.125_wp)) > thr) then
      call test_failed(error, "3D body-center 8-fold degenerate weights are not 1/8")
      return
   end if

   ! Atom 3 near face/edge boundary (3.0, 3.005, 0.0) has 4 competing images within tol = 0.3
   call check(error, wsc%nimg(1, 3), 4)
   if (allocated(error)) return

   call get_wignerseitz_weights(wsc, 1, 3, mol%xyz(:, 3) - mol%xyz(:, 1), weights)
   if (abs(sum(weights(1:4)) - 1.0_wp) > thr) then
      call test_failed(error, "3D near-boundary smooth weights do not sum to 1")
      return
   end if

   ! 5. Degenerate (collinear 2D) lattice fails closed without allocating fake origin-only trans
   call new(mol, num2, xyz_cube(:, 1:2), lattice=lat_collinear, periodic=pbc_2d)
   call new_wignerseitz_cell(wsc, mol, wsc_err)
   call check(error, allocated(wsc_err))
   if (allocated(error)) return

   call check(error, .not. allocated(wsc%trans))
   if (allocated(error)) return

end subroutine test_wsc_exhaustive_reference


!> High-symmetry cells with many tied nearest images
subroutine test_wsc_high_symmetry(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol
   type(wignerseitz_cell) :: wsc
   real(wp) :: weights(16)
   integer :: iws_small, iws_full, list_small(8), list_full(27)

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

   ! 3. 3D FCC primitive cell with 12 equidistant nearest self images,
   !    get_pairs returns the leading images if the output buffer is shorter
   call new(mol, num_fcc, xyz_fcc, lattice=lat_fcc, periodic=pbc_3d)
   call new_wignerseitz_cell(wsc, mol, error)
   if (allocated(error)) return

   call check(error, wsc%nimg(1, 1), 12)
   if (allocated(error)) return

   call check(error, size(wsc%tridx, 1), 12)
   if (allocated(error)) return

   call get_wignerseitz_weights(wsc, 1, 1, xyz_fcc(:, 1), weights)
   if (maxval(abs(weights(1:12) - 1.0_wp / 12.0_wp)) > thr) then
      call test_failed(error, "FCC 12-fold self-image weights are not 1/12")
      return
   end if

   call get_pairs(iws_small, wsc%trans, xyz_fcc(:, 1), list_small)
   call check(error, iws_small, 8)
   if (allocated(error)) return

   call get_pairs(iws_full, wsc%trans, xyz_fcc(:, 1), list_full)
   call check(error, iws_full, 12)
   if (allocated(error)) return

   if (any(list_small(1:8) /= list_full(1:8))) then
      call test_failed(error, "get_pairs truncated list does not match leading sorted images")
      return
   end if

end subroutine test_wsc_high_symmetry


subroutine test_wsc_robustness_1d(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol_ref, mol_test
   type(wignerseitz_cell) :: wsc_ref, wsc_test
   real(wp) :: v_ref(3), v_test(3)
   integer :: iat, jat

   integer, parameter :: nat = 3
   integer, parameter :: num(nat) = [6, 8, 1]
   real(wp), parameter :: a1(3) = [3.0_wp, 4.0_wp, 12.0_wp]
   real(wp), parameter :: xyz_ref(3, nat) = reshape([ &
      &  0.1_wp,  0.2_wp, -0.3_wp, &
      &  1.1_wp,  1.4_wp,  3.5_wp, &
      & -0.8_wp, -0.9_wp, -2.5_wp], &
      & [3, nat])
   real(wp), parameter :: xyz_test(3, nat) = reshape([ &
      & xyz_ref(:, 1), &
      & xyz_ref(:, 2) + 5.0_wp * a1, &
      & xyz_ref(:, 3) - 4.0_wp * a1], &
      & [3, nat])
   real(wp), parameter :: lat_ref(3, 3) = reshape([ &
      & 0.0_wp, 0.0_wp,  0.0_wp, &
      & 3.0_wp, 4.0_wp, 12.0_wp, &
      & 0.0_wp, 0.0_wp,  0.0_wp], &
      & [3, 3])
   logical, parameter :: periodic(3) = [.false., .true., .false.]

   ! 1D wire rotated in 3D space with zero nonperiodic lattice columns
   ! and multi-cell atom translations (|n| > 1)
   call new(mol_ref, num, xyz_ref, lattice=lat_ref, periodic=periodic)
   call new(mol_test, num, xyz_test, lattice=lat_ref, periodic=periodic)
   call new_wignerseitz_cell(wsc_ref, mol_ref, error)
   if (allocated(error)) return

   call new_wignerseitz_cell(wsc_test, mol_test, error)
   if (allocated(error)) return

   do iat = 1, 3
      do jat = 1, 3
         call check(error, wsc_test%nimg(jat, iat), wsc_ref%nimg(jat, iat))
         if (allocated(error)) return

         if (iat /= jat) then
            v_ref = mol_ref%xyz(:, jat) - mol_ref%xyz(:, iat) &
               & + wsc_ref%trans(:, wsc_ref%tridx(1, jat, iat))
            v_test = mol_test%xyz(:, jat) - mol_test%xyz(:, iat) &
               & + wsc_test%trans(:, wsc_test%tridx(1, jat, iat))
            if (maxval(abs(v_test - v_ref)) > thr2) then
               call test_failed(error, "1D WSC failed for multi-cell atom displacement")
               return
            end if
         end if
      end do
   end do

end subroutine test_wsc_robustness_1d


subroutine test_wsc_robustness_2d(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol_ref, mol_test
   type(wignerseitz_cell) :: wsc_ref, wsc_test
   real(wp) :: v_ref(3), v_test(3)
   integer :: iat, jat

   integer, parameter :: nat = 3
   integer, parameter :: num(nat) = [6, 8, 1]
   real(wp), parameter :: a1(3) = [4.0_wp, 3.0_wp, 0.0_wp]
   real(wp), parameter :: a2(3) = [-3.0_wp, 4.0_wp, 5.0_wp]
   real(wp), parameter :: xyz_ref(3, nat) = reshape([ &
      &  0.2_wp, -0.1_wp, 0.3_wp, &
      &  1.4_wp,  1.2_wp, 1.1_wp, &
      & -1.1_wp,  0.8_wp, 1.7_wp], &
      & [3, nat])
   real(wp), parameter :: xyz_test(3, nat) = reshape([ &
      & xyz_ref(:, 1), &
      & xyz_ref(:, 2) + 3.0_wp * a1 - 4.0_wp * a2, &
      & xyz_ref(:, 3) - 5.0_wp * a1 + 2.0_wp * a2], &
      & [3, nat])
   real(wp), parameter :: lat_ref(3, 3) = reshape([ &
      &  4.0_wp, 3.0_wp, 0.0_wp, &
      &  0.0_wp, 0.0_wp, 0.0_wp, &
      & -3.0_wp, 4.0_wp, 5.0_wp], &
      & [3, 3])
   real(wp), parameter :: lat_test(3, 3) = reshape([ &
      & a1, &
      & 0.0_wp, 0.0_wp, 0.0_wp, &
      & a2 + 6.0_wp * a1], &
      & [3, 3])
   logical, parameter :: periodic(3) = [.true., .false., .true.]

   ! 2D slab in rotated 3D subspace with zero nonperiodic column,
   ! skewed unimodular basis transformation (a2' = a2 + 6*a1), and multi-cell shifts
   call new(mol_ref, num, xyz_ref, lattice=lat_ref, periodic=periodic)
   call new(mol_test, num, xyz_test, lattice=lat_test, periodic=periodic)
   call new_wignerseitz_cell(wsc_ref, mol_ref, error)
   if (allocated(error)) return

   call new_wignerseitz_cell(wsc_test, mol_test, error)
   if (allocated(error)) return

   do iat = 1, 3
      do jat = 1, 3
         call check(error, wsc_test%nimg(jat, iat), wsc_ref%nimg(jat, iat))
         if (allocated(error)) return

         if (iat /= jat) then
            v_ref = mol_ref%xyz(:, jat) - mol_ref%xyz(:, iat) &
               & + wsc_ref%trans(:, wsc_ref%tridx(1, jat, iat))
            v_test = mol_test%xyz(:, jat) - mol_test%xyz(:, iat) &
               & + wsc_test%trans(:, wsc_test%tridx(1, jat, iat))
            if (maxval(abs(v_test - v_ref)) > thr2) then
               call test_failed(error, &
                  & "2D WSC failed under skewed unimodular basis and multi-cell shift")
               return
            end if
         end if
      end do
   end do

end subroutine test_wsc_robustness_2d


subroutine test_wsc_robustness_2d_corner(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol_ref
   type(wignerseitz_cell) :: wsc_ref
   real(wp) :: weights(16)

   integer, parameter :: nat = 2
   integer, parameter :: num(nat) = [8, 1]
   real(wp), parameter :: xyz(3, nat) = reshape([ &
      & 0.0_wp, 0.0_wp, 0.0_wp, &
      & 3.0_wp, 3.0_wp, 0.4_wp], &
      & [3, nat])
   real(wp), parameter :: lattice(3, 3) = reshape([ &
      & 6.0_wp, 0.0_wp, 0.0_wp, &
      & 0.0_wp, 6.0_wp, 0.0_wp, &
      & 0.0_wp, 0.0_wp, 0.0_wp], &
      & [3, 3])
   logical, parameter :: periodic(3) = [.true., .true., .false.]

   ! Exact 2D Wigner-Seitz corner vs smooth weights
   call new(mol_ref, num, xyz, lattice=lattice, periodic=periodic)
   call new_wignerseitz_cell(wsc_ref, mol_ref, error)
   if (allocated(error)) return

   call check(error, wsc_ref%nimg(2, 1), 4)
   if (allocated(error)) return

   call get_wignerseitz_weights(wsc_ref, 2, 1, mol_ref%xyz(:, 1) - mol_ref%xyz(:, 2), weights)
   if (maxval(abs(weights(1:4) - 0.25_wp)) > thr) then
      call test_failed(error, "2D WSC corner weights are not equal")
      return
   end if

end subroutine test_wsc_robustness_2d_corner


!> Strongly sheared basis of a 3D lattice with unwrapped atoms, the conservative
!> box in the given basis would exceed the candidate limit
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


subroutine verify_wsc_against_ref(mol, wsc, n_ref, error)
   type(structure_type), intent(in) :: mol
   type(wignerseitz_cell), intent(in) :: wsc
   integer, intent(in) :: n_ref
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
                  if (d2 < thr2) then
                     cycle
                  end if
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
                  if (d2 < thr2) then
                     cycle
                  end if
                  if (d2 - dmin2_ref <= tol_wsc) then
                     nimg_ref = nimg_ref + 1
                  end if
               end do
            end do
         end do

         call check(error, wsc%nimg(jat, iat), nimg_ref)
         if (allocated(error)) return

         dmin2_wsc = sum((rij - wsc%trans(:, wsc%tridx(1, jat, iat)))**2)
         if (abs(dmin2_wsc - dmin2_ref) > thr2) then
            call test_failed(error, "WSC minimum squared distance disagrees with exhaustive reference")
            return
         end if

         do img = 1, wsc%nimg(jat, iat)
            d2 = sum((rij - wsc%trans(:, wsc%tridx(img, jat, iat)))**2)
            if (d2 - dmin2_ref > tol_wsc + thr) then
               call test_failed(error, "WSC included image outside tol window")
               return
            end if
         end do
      end do
   end do

end subroutine verify_wsc_against_ref


end module test_wignerseitz
