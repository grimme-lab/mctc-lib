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

!> Driver for unit testing
program tester
   use, intrinsic :: iso_fortran_env, only : error_unit
   use mctc_env_system, only : get_argument
   use mctc_env_testing, only : run_testsuite, new_testsuite, testsuite_type, &
      & select_suite, run_selected
   use test_math, only : collect_math
   use test_read, only : collect_read
   use test_read_aims, only : collect_read_aims
   use test_read_cjson, only : collect_read_cjson
   use test_read_ctfile, only : collect_read_ctfile
   use test_read_gaussian, only : collect_read_gaussian
   use test_read_genformat, only : collect_read_genformat
   use test_read_pdb, only : collect_read_pdb
   use test_read_qchem, only : collect_read_qchem
   use test_read_qcschema, only : collect_read_qcschema
   use test_read_turbomole, only : collect_read_turbomole
   use test_read_vasp, only : collect_read_vasp
   use test_read_xyz, only : collect_read_xyz
   use test_symbols, only : collect_symbols
   use test_write, only : collect_write
   use test_write_aims, only : collect_write_aims
   use test_write_cjson, only : collect_write_cjson
   use test_write_ctfile, only : collect_write_ctfile
   use test_write_gaussian, only : collect_write_gaussian
   use test_write_genformat, only : collect_write_genformat
   use test_write_pdb, only : collect_write_pdb
   use test_write_qchem, only : collect_write_qchem
   use test_write_turbomole, only : collect_write_turbomole
   use test_write_vasp, only : collect_write_vasp
   use test_write_xyz, only : collect_write_xyz
   implicit none
   integer :: stat, is
   character(len=:), allocatable :: suite_name, test_name
   type(testsuite_type), allocatable :: testsuites(:)
   character(len=*), parameter :: fmt = '("#", *(1x, a))'

   stat = 0

   testsuites = [ &
      & new_testsuite("math", collect_math), &
      & new_testsuite("symbols", collect_symbols), &
      & new_testsuite("read", collect_read), &
      & new_testsuite("read-aims", collect_read_aims), &
      & new_testsuite("read-cjson", collect_read_cjson), &
      & new_testsuite("read-ctfile", collect_read_ctfile), &
      & new_testsuite("read-gaussian", collect_read_gaussian), &
      & new_testsuite("read-genformat", collect_read_genformat), &
      & new_testsuite("read-pdb", collect_read_pdb), &
      & new_testsuite("read-qchem", collect_read_qchem), &
      & new_testsuite("read-qcschema", collect_read_qcschema), &
      & new_testsuite("read-turbomole", collect_read_turbomole), &
      & new_testsuite("read-vasp", collect_read_vasp), &
      & new_testsuite("read-xyz", collect_read_xyz), &
      & new_testsuite("write", collect_write), &
      & new_testsuite("write-aims", collect_write_aims), &
      & new_testsuite("write-cjson", collect_write_cjson), &
      & new_testsuite("write-ctfile", collect_write_ctfile), &
      & new_testsuite("write-gaussian", collect_write_gaussian), &
      & new_testsuite("write-genformat", collect_write_genformat), &
      & new_testsuite("write-pdb", collect_write_pdb), &
      & new_testsuite("write-qchem", collect_write_qchem), &
      & new_testsuite("write-turbomole", collect_write_turbomole), &
      & new_testsuite("write-vasp", collect_write_vasp), &
      & new_testsuite("write-xyz", collect_write_xyz) &
      & ]

   call get_argument(1, suite_name)
   call get_argument(2, test_name)

   if (allocated(suite_name)) then
      is = select_suite(testsuites, suite_name)
      if (is > 0 .and. is <= size(testsuites)) then
         if (allocated(test_name)) then
            write(error_unit, fmt) "Suite:", testsuites(is)%name
            call run_selected(testsuites(is)%collect, test_name, error_unit, stat)
            if (stat < 0) then
               error stop 1
            end if
         else
            write(error_unit, fmt) "Testing:", testsuites(is)%name
            call run_testsuite(testsuites(is)%collect, error_unit, stat)
         end if
      else
         write(error_unit, fmt) "Available testsuites"
         do is = 1, size(testsuites)
            write(error_unit, fmt) "-", testsuites(is)%name
         end do
         error stop 1
      end if
   else
      do is = 1, size(testsuites)
         write(error_unit, fmt) "Testing:", testsuites(is)%name
         call run_testsuite(testsuites(is)%collect, error_unit, stat)
      end do
   end if

   if (stat > 0) then
      write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
      error stop 1
   end if


end program tester
