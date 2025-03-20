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

module test_data
   use mctc_env, only : wp
   use mctc_env_testing, only : new_unittest, unittest_type, error_type, &
   & test_failed, check
   use mctc_io_structure, only : structure_type
   use testsuite_structure, only : get_structure
   use mctc_data, only : get_atomic_rad, get_covalent_rad, get_pauling_en, get_vdw_rad
   implicit none
   private

   public :: collect_data

   real(wp), parameter :: thr = 100*epsilon(1.0_wp)
   real(wp), parameter :: thr2 = sqrt(epsilon(1.0_wp))

contains


   !> Collect all exported unit tests
   subroutine collect_data(testsuite)

      !> Collection of tests
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
      & new_unittest("atomic_rad", test_atomic_rad), &
      & new_unittest("atomic_rad_mb01", test_atomic_rad_mb01), &
      & new_unittest("covalent_rad", test_covalent_rad), &
      & new_unittest("covalent_rad_mb02", test_covalent_rad_mb02), &
      & new_unittest("pauling_en", test_pauling_en), &
      & new_unittest("pauling_en_mb03", test_pauling_en_mb03), &
      & new_unittest("vdw_rad", test_vdw_rad), &
      & new_unittest("vdw_rad_mb04", test_vdw_rad_mb04) &
      & ]

   end subroutine collect_data


   subroutine test_atomic_rad(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error
   
      call check(error, get_atomic_rad("C"), get_atomic_rad(6))
      if (allocated(error)) return
      call check(error, get_atomic_rad("Am"), get_atomic_rad(95))
      if (allocated(error)) return
      call check(error, get_atomic_rad("Og"), get_atomic_rad(118))
      if (allocated(error)) return
      call check(error, get_atomic_rad("X"), get_atomic_rad(-1))
   
   end subroutine test_atomic_rad   


   subroutine test_atomic_rad_mb01(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      real(wp), allocatable :: lattr(:, :)
      real(wp), allocatable :: ratm_sym(:), ratm_num(:)

      real(wp), parameter :: ref(8) = [&
      & 3.02356179939270E+0_wp, 6.04712359878541E-1_wp, 1.20942471975708E+0_wp, &
      & 1.13383567477226E+0_wp, 1.34170554848051E+0_wp, 1.88972612462044E+0_wp, &
      & 1.58736994468117E+0_wp, 2.34326039452935E+0_wp]

      call get_structure(mol, "mindless01")

      ratm_sym = get_atomic_rad(mol%sym)
      ratm_num = get_atomic_rad(mol%num)
      
      if (any(abs(ratm_sym - ratm_num) > thr) .or. any(abs(ratm_sym - ref) > thr)) then
         call test_failed(error, "Atomic radii do not match")
         print'(3es21.14)', ratm_sym
         print'("---")'
         print'(3es21.14)', ratm_num
         print'("---")'
         print'(3es21.14)', ref
      end if

   end subroutine test_atomic_rad_mb01


   subroutine test_covalent_rad(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error
   
      call check(error, get_covalent_rad("C"), get_covalent_rad(6))
      if (allocated(error)) return
      call check(error, get_covalent_rad("Am"), get_covalent_rad(95))
      if (allocated(error)) return
      call check(error, get_covalent_rad("Og"), get_covalent_rad(118))
      if (allocated(error)) return
      call check(error, get_covalent_rad("X"), get_covalent_rad(-1))
   
   end subroutine test_covalent_rad   


   subroutine test_covalent_rad_mb02(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      real(wp), allocatable :: lattr(:, :)
      real(wp), allocatable :: rcov_sym(:), rcov_num(:)

      real(wp), parameter :: ref(8) = [&
      & 8.06283146504721E-1_wp, 2.57002752948380E+0_wp, 1.94011882127699E+0_wp, &
      & 1.58736994468117E+0_wp, 3.14954354103407E+0_wp, 2.62042022614034E+0_wp, &
      & 3.02356179939270E+0_wp, 1.61256629300944E+0_wp]

      call get_structure(mol, "mindless02")

      rcov_sym = get_covalent_rad(mol%sym)
      rcov_num = get_covalent_rad(mol%num)
      
      if (any(abs(rcov_sym - rcov_num) > thr) .or. any(abs(rcov_sym - ref) > thr)) then
         call test_failed(error, "Covalent radii do not match")
         print'(3es21.14)', rcov_sym
         print'("---")'
         print'(3es21.14)', rcov_num
         print'("---")'
         print'(3es21.14)', ref
      end if

   end subroutine test_covalent_rad_mb02


   subroutine test_pauling_en(error)
      
      !> Error handling
      type(error_type), allocatable, intent(out) :: error
   
      call check(error, get_pauling_en("C"), get_pauling_en(6))
      if (allocated(error)) return
      call check(error, get_pauling_en("Am"), get_pauling_en(95))
      if (allocated(error)) return
      call check(error, get_pauling_en("Og"), get_pauling_en(118))
      if (allocated(error)) return
      call check(error, get_pauling_en("X"), get_pauling_en(-1))
   
   end subroutine test_pauling_en   


   subroutine test_pauling_en_mb03(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      real(wp), allocatable :: lattr(:, :)
      real(wp), allocatable :: en_sym(:), en_num(:)

      real(wp), parameter :: ref(9) = [&
      & 2.55000000000000E+0_wp, 3.44000000000000E+0_wp, 2.20000000000000E+0_wp, &
      & 9.80000000000000E-1_wp, 1.31000000000000E+0_wp, 1.61000000000000E+0_wp, &
      & 3.98000000000000E+0_wp, 2.58000000000000E+0_wp, 9.30000000000000E-1_wp]

      call get_structure(mol, "mindless03")

      en_sym = get_pauling_en(mol%sym)
      en_num = get_pauling_en(mol%num)
      
      if (any(abs(en_sym - en_num) > thr) .or. any(abs(en_sym - ref) > thr)) then
         call test_failed(error, "Pauling electronegativities do not match")
         print'(3es21.14)', en_sym
         print'("---")'
         print'(3es21.14)', en_num
         print'("---")'
         print'(3es21.14)', ref
      end if

   end subroutine test_pauling_en_mb03


   subroutine test_vdw_rad(error)
      
      !> Error handling
      type(error_type), allocatable, intent(out) :: error
   
      call check(error, get_vdw_rad("C"), get_vdw_rad(6))
      if (allocated(error)) return
      call check(error, get_vdw_rad("Am"), get_vdw_rad(95))
      if (allocated(error)) return
      call check(error, get_vdw_rad("Og"), get_vdw_rad(118))
      if (allocated(error)) return
      call check(error, get_vdw_rad("X"), get_vdw_rad(-1))
   
   end subroutine test_vdw_rad 


   subroutine test_vdw_rad_mb04(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      real(wp), allocatable :: lattr(:, :)
      real(wp), allocatable :: rvdw_sym(:), rvdw_num(:), rvdw_pair_sym(:), rvdw_pair_num(:)

      real(wp), parameter :: ref(7) = [&
      & 2.06197466087959E+0_wp, 3.03867960838967E+0_wp, 2.17280709808858E+0_wp, &
      & 3.58263727336166E+0_wp, 2.74983497024143E+0_wp, 3.78946779770137E+0_wp, &
      & 2.34486666173527E+0_wp]

      real(wp), parameter :: ref_pair(7) = [&
      & 4.12394932175919E+0_wp, 4.75096044990825E+0_wp, 3.90152855689136E+0_wp, &
      & 5.45828493835368E+0_wp, 4.62831722442038E+0_wp, 5.43598617008316E+0_wp, &
      & 4.11355582807377E+0_wp]

      call get_structure(mol, "mindless04")

      rvdw_sym = get_vdw_rad(mol%sym)
      rvdw_num = get_vdw_rad(mol%num)
      
      if (any(abs(rvdw_sym - rvdw_num) > thr) .or. any(abs(rvdw_sym - ref) > thr)) then
         call test_failed(error, "Van der Waals radii do not match")
         print'(3es21.14)', rvdw_sym
         print'("---")'
         print'(3es21.14)', rvdw_num
         print'("---")'
         print'(3es21.14)', ref
      end if

      rvdw_pair_sym = get_vdw_rad(mol%num, 1)
      rvdw_pair_num = get_vdw_rad('H', mol%sym)

      if (any(abs(rvdw_pair_sym - rvdw_pair_num) > thr) &
         & .or. any(abs(rvdw_pair_num - ref_pair) > thr)) then
         call test_failed(error, "Pairwise van der Waals radii do not match")
         print'(3es21.14)', rvdw_pair_sym
         print'("---")'
         print'(3es21.14)', rvdw_pair_num
         print'("---")'
         print'(3es21.14)', ref_pair
      end if

   end subroutine test_vdw_rad_mb04


end module test_data
