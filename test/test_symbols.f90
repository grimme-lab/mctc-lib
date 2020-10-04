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

module test_symbols
   use mctc_env_testing, only : new_unittest, unittest_type, error_type, check
   use mctc_io_symbols
   implicit none
   private

   public :: collect_symbols


contains


!> Collect all exported unit tests
subroutine collect_symbols(testsuite)

   !> Collection of tests
   type(unittest_type), allocatable, intent(out) :: testsuite(:)

   testsuite = [ &
      & new_unittest("valid-num-ids", test_num_ids), &
      & new_unittest("valid-sym-ids", test_sym_ids), &
      & new_unittest("valid-num", test_valid_num), &
      & new_unittest("valid-pse", test_valid_pse), &
      & new_unittest("valid-sym", test_valid_sym) &
      & ]

end subroutine collect_symbols


subroutine test_valid_sym(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check(error, to_number("K"), 19, "Wrong atomic number for alium")
   if (allocated(error)) return

   call check(error, to_number("ca "), 20, "Could not identify calcium")
   if (allocated(error)) return

   call check(error, to_number(" aU "), 79, "Could not identify gold")
   if (allocated(error)) return

   call check(error, to_number("CN*"), 112, "Could not identify coperneticum")
   if (allocated(error)) return

   call check(error, to_number("*sn"), 50, "Could not identify tin")
   if (allocated(error)) return

   call check(error, to_number("d"), 1, "Could not identify deuterium")
   if (allocated(error)) return

end subroutine test_valid_sym


subroutine test_valid_num(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call check(error, to_symbol(19), "K", "Wrong atomic number for alium")
   if (allocated(error)) return

   call check(error, to_symbol(20), "Ca", "Could not identify calcium")
   if (allocated(error)) return

   call check(error, to_symbol(79), "Au", "Could not identify gold")
   if (allocated(error)) return

   call check(error, to_symbol(112), "Cn", "Could not identify coperneticum")
   if (allocated(error)) return

   call check(error, to_symbol(50), "Sn", "Could not identify tin")
   if (allocated(error)) return

   call check(error, to_symbol(1), "H", "Could not identify hydrogen")
   if (allocated(error)) return

end subroutine test_valid_num


subroutine test_valid_pse(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   integer :: num
   character(len=symbol_length) :: sym

   do num = 1, 118
      sym = to_symbol(num)
      call check(error, to_number(sym), num, "Could not match all elements of the PSE")
      if (allocated(error)) exit
   end do

end subroutine test_valid_pse


subroutine test_sym_ids(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   integer :: nid
   integer, allocatable :: ids(:)
   character(symbol_length), allocatable :: sym(:)

   sym = [character(symbol_length) :: &
      & 'Al', 'Ca', 'Ti', 'O ', 'F ', 'Ga', 'Ca', 'Ti', 'S ', 'Cl', 'O ', 'O ']

   allocate(ids(size(sym)))
   call get_identity(nid, ids, sym)

   call check(error, nid, 8, "Expected eight unique element symbols")
   if (allocated(error)) return

   call check(error, minval(ids), 1, "Lower bound of IDs is not one")
   if (allocated(error)) return

   call check(error, maxval(ids), nid, "Upper bound of IDs is not number of IDs")

end subroutine test_sym_ids


subroutine test_num_ids(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   integer :: nid
   integer, allocatable :: ids(:)
   integer, allocatable :: num(:)

   num = [integer :: &
      & 1, 2, 8, 112, 112, 7, 7, 19, 19, 1, 2, 112, 7, 12, 19, 7, 3, 2, 2]

   allocate(ids(size(num)))
   call get_identity(nid, ids, num)

   call check(error, nid, 8, "Expected eight unique atomic numbers")
   if (allocated(error)) return

   call check(error, minval(ids), 1, "Lower bound of IDs is not one")
   if (allocated(error)) return

   call check(error, maxval(ids), nid, "Upper bound of IDs is not number of IDs")

end subroutine test_num_ids


end module test_symbols
