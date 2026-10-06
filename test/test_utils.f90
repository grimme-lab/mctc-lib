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

module test_utils
   use mctc_env_accuracy, only : wp
   use mctc_env_testing, only : new_unittest, unittest_type, error_type, check, &
      & test_failed
   use mctc_io_utils, only : token_type, read_token
   implicit none
   private

   public :: collect_utils


contains


!> Collect all exported unit tests
subroutine collect_utils(testsuite)

   !> Collection of tests
   type(unittest_type), allocatable, intent(out) :: testsuite(:)

   testsuite = [ &
      & new_unittest("real-plain", test_real_plain), &
      & new_unittest("real-signed", test_real_signed), &
      & new_unittest("real-exponent", test_real_exponent), &
      & new_unittest("real-fallback", test_real_fallback), &
      & new_unittest("real-out-of-range", test_real_out_of_range), &
      & new_unittest("real-invalid", test_real_invalid), &
      & new_unittest("real-invalid-iomsg", test_real_invalid_iomsg), &
      & new_unittest("real-bad-token", test_real_bad_token) &
      & ]

end subroutine collect_utils


!> Check that tokens are read bit-identical to the list-directed internal read
subroutine test_real_gen(error, str)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   !> Strings holding a single number each
   character(len=*), intent(in) :: str(:)

   integer :: i, stat, stat_ref
   real(wp) :: val, ref

   do i = 1, size(str)
      call read_token(trim(str(i)), token_type(1, len_trim(str(i))), val, stat)
      read(str(i), *, iostat=stat_ref) ref
      if (stat_ref /= 0) ref = 0.0_wp

      call check(error, stat, stat_ref, "Status differs for '"//trim(str(i))//"'")
      if (allocated(error)) return

      call check(error, val, ref, thr=0.0_wp, &
         & message="Value differs for '"//trim(str(i))//"'")
      if (allocated(error)) return
   end do

end subroutine test_real_gen


subroutine test_real_plain(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: str(*) = [character(len=24) :: &
      & "0", "1", "42", "0.0", "1.0", "0.1", "0.3", "3.14159265358979", &
      & "654.8660", "189.82800", "5.", ".5", "00012.50", "123456789012.345", &
      & "9007199254740992", "0.000001", "1234567.890123456"]

   call test_real_gen(error, str)

end subroutine test_real_plain


subroutine test_real_signed(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: str(*) = [character(len=24) :: &
      & "+7", "-7", "-0.0", "+0.25", "-12.345", "-.5", "+.5", "-654.866"]

   call test_real_gen(error, str)

end subroutine test_real_signed


subroutine test_real_exponent(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: str(*) = [character(len=24) :: &
      & "1e5", "1E5", "1.5e-3", "-2.5E+2", "1e+0", "1e-0", "1.7976931348623157e20", &
      & "123.456e-10", "1e22", "1e-22", "9.999999999999999e-5", "0.5e01"]

   call test_real_gen(error, str)

end subroutine test_real_exponent


subroutine test_real_fallback(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   ! Forms not accepted by the fast path but valid for list-directed input
   character(len=*), parameter :: str(*) = [character(len=24) :: &
      & "1d3", "1.5D-2", "1e", "1.5+3", "1,5", "1.0/"]

   call test_real_gen(error, str)

end subroutine test_real_fallback


subroutine test_real_out_of_range(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   ! Mantissa above 2^53 or decimal exponent beyond 22 needs correct rounding
   character(len=*), parameter :: str(*) = [character(len=40) :: &
      & "9007199254740993", "12345678901234567890", "0.12345678901234567890123", &
      & "1e23", "1e-23", "1e30", "1e-30", "1e300", "1e-300", "1e999", "4.9e-324", &
      & "1.0000000000000000000000001", "123456789012345678901234567890.5"]

   call test_real_gen(error, str)

end subroutine test_real_out_of_range


subroutine test_real_invalid(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: str(*) = [character(len=24) :: &
      & "abc", "1.2.3", "-", "+", ".", "--1", "1e5e5", "12x", "e5"]
   integer :: i, stat
   real(wp) :: val

   do i = 1, size(str)
      call read_token(trim(str(i)), token_type(1, len_trim(str(i))), val, stat)
      if (stat == 0) then
         call test_failed(error, "Accepted invalid number '"//trim(str(i))//"'")
         return
      end if
      call check(error, val, 0.0_wp, thr=0.0_wp, &
         & message="Value not reset for '"//trim(str(i))//"'")
      if (allocated(error)) return
   end do

end subroutine test_real_invalid


subroutine test_real_invalid_iomsg(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: str = "12.5"
   character(len=:), allocatable :: msg
   integer :: stat
   real(wp) :: val

   call read_token(str, token_type(1, len(str)), val, stat, msg)
   call check(error, stat, 0)
   if (allocated(error)) return

   call check(error, len(msg), 0, "Message set on successful read")
   if (allocated(error)) return

   call read_token("abc", token_type(1, 3), val, stat, msg)
   if (stat == 0) then
      call test_failed(error, "Accepted invalid number")
      return
   end if

   call check(error, len(msg) > 0, "Missing message for failed read")

end subroutine test_real_invalid_iomsg


subroutine test_real_bad_token(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: str = "1.0"
   integer :: stat
   real(wp) :: val

   call read_token(str, token_type(0, 0), val, stat)
   call check(error, stat /= 0, "Empty token accepted")
   if (allocated(error)) return

   call read_token(str, token_type(1, len(str) + 1), val, stat)
   call check(error, stat /= 0, "Token past end of line accepted")

end subroutine test_real_bad_token


end module test_utils
