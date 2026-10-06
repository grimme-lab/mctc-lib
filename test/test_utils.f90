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
   use mctc_env_accuracy, only : wp, i8
   use mctc_env_testing, only : new_unittest, unittest_type, error_type, check, &
      & test_failed
   use mctc_io_utils, only : token_type, read_token, read_next_token
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
      & new_unittest("real-boundaries", test_real_boundaries), &
      & new_unittest("real-rounding", test_real_rounding), &
      & new_unittest("real-roundtrip", test_real_roundtrip), &
      & new_unittest("real-long", test_real_long), &
      & new_unittest("real-special", test_real_special), &
      & new_unittest("real-next-token", test_real_next_token), &
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

      call check(error, transfer(val, 0_i8) == transfer(ref, 0_i8), &
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


subroutine test_real_boundaries(error)

   type(error_type), allocatable, intent(out) :: error

   integer(i8), parameter :: mant(*) = [0_i8, 1_i8, 3_i8, 7_i8, &
      & 999999999999999_i8, 1000000000000001_i8, &
      & 2_i8**52-1, 2_i8**52, 2_i8**52+1, &
      & 2_i8**53-1, 2_i8**53, 2_i8**53+1, huge(0_i8)]
   character(len=64) :: str(2)
   integer :: i, ex

   do ex = -23, 23
      do i = 1, size(mant)
         write(str(1), "(i0,a,i0)") mant(i), "e", ex
         str(2) = "-"//trim(str(1))
         call test_real_gen(error, str)
         if (allocated(error)) return
      end do
   end do

   call test_real_gen(error, [character(len=64) :: &
      & "0.1e23", "0.1e24", "0.1e-21", "0.1e-22", &
      & "1.e+00022", ".1E+00023", "0009007199254740992", &
      & "-0", "-0e22", "-0e-22", "-0e23", "-0e-23", &
      & "1e100", "1e101", "1e-100", "1e-101", &
      & "1e2147483647", "1e2147483648", "1e-2147483648", &
      & "1e999999999999999999999999999999999999"])

end subroutine test_real_boundaries


!> Check against known results, independently of the runtime decimal reader
subroutine check_real_value(error, str, ref)

   type(error_type), allocatable, intent(out) :: error
   character(len=*), intent(in) :: str
   real(wp), intent(in) :: ref

   real(wp) :: val
   integer :: stat

   call read_token(str, token_type(1, len(str)), val, stat)
   call check(error, stat, 0, "Failed to read '"//str//"'")
   if (allocated(error)) return

   call check(error, transfer(val, 0_i8) == transfer(ref, 0_i8), &
      & message="Incorrect rounding for '"//str//"'")

end subroutine check_real_value


subroutine test_real_rounding(error)

   type(error_type), allocatable, intent(out) :: error

   ! Exact ties beyond 19 digits are resolved by the runtime reader (ifort rounds
   ! them incorrectly), so only the fast-path-independent cases are tested here
   character(len=*), parameter :: str(*) = [character(len=80) :: &
      & "1.00000000000000011102230246251565404236316680908203124", &
      & "1.00000000000000011102230246251565404236316680908203125", &
      & "4503599627370498e1", "4503599627370502e1", &
      & "9007199254740993", "9007199254740995", &
      & "1.0000000000000002", "0.9999999999999999", &
      & "1.7976931348623157e308", "2.2250738585072014e-308", &
      & "2.225073858507201e-308", "4.9406564584124654e-324", &
      & "2.4703282292062327e-324", "2.4703282292062328e-324"]
   real(wp) :: ref(size(str))
   integer :: i

   ref = [1.0_wp, 1.0_wp, &
      & 45035996273704976.0_wp, 45035996273705024.0_wp, &
      & 9007199254740992.0_wp, 9007199254740996.0_wp, &
      & nearest(1.0_wp, 1.0_wp), nearest(1.0_wp, -1.0_wp), &
      & huge(1.0_wp), tiny(1.0_wp), transfer(int(z'000FFFFFFFFFFFFF', i8), 0.0_wp), &
      & transfer(1_i8, 0.0_wp), 0.0_wp, transfer(1_i8, 0.0_wp)]

   do i = 1, size(str)
      call check_real_value(error, trim(str(i)), ref(i))
      if (allocated(error)) return
      call check_real_value(error, "-"//trim(str(i)), -ref(i))
      if (allocated(error)) return
   end do

end subroutine test_real_rounding


!> Seventeen significant decimal digits must recover every sampled binary64 value
subroutine test_real_roundtrip(error)

   type(error_type), allocatable, intent(out) :: error

   integer(i8), parameter :: fraction(*) = [0_i8, 1_i8, &
      & int(z'5555555555555', i8), int(z'AAAAAAAAAAAAA', i8), 2_i8**52-1]
   integer(i8) :: bits
   integer :: ex, i, sgn
   real(wp) :: ref
   character(len=32) :: str

   call check(error, radix(1.0_wp) == 2 .and. digits(1.0_wp) == 53 &
      & .and. storage_size(1.0_wp) == 64, "Test requires binary64 working precision")
   if (allocated(error)) return

   ! Include subnormals and signed zero, but exclude the nonfinite exponent.
   do ex = 0, 2046
      do i = 1, size(fraction)
         do sgn = 0, 1
            bits = ior(shiftl(int(ex, i8), 52), fraction(i))
            if (sgn == 1) bits = ibset(bits, 63)
            ref = transfer(bits, ref)
            write(str, "(es24.16e3)") ref
            ! Some runtimes (ifort) drop the sign of negative zero
            if (sgn == 1 .and. index(str, "-") == 0) str = "-"//adjustl(str)
            call check_real_value(error, trim(adjustl(str)), ref)
            if (allocated(error)) return
         end do
      end do
   end do

end subroutine test_real_roundtrip


subroutine test_real_long(error)

   type(error_type), allocatable, intent(out) :: error

   call test_real_gen(error, [ &
      & repeat("0", 400)//"1", &
      & "1e"//repeat("0", 396)//"+22", &
      & "1e+"//repeat("0", 396)//"22"])
   if (allocated(error)) return

   call check_real_value(error, "0."//repeat("0", 99)//"1e100", 1.0_wp)
   if (allocated(error)) return
   call check_real_value(error, "0."//repeat("0", 100)//"1e101", 1.0_wp)
   if (allocated(error)) return
   call check_real_value(error, "0."//repeat("0", 399)//"1e400", 1.0_wp)
   if (allocated(error)) return
   call check_real_value(error, "1."//repeat("0", 400), 1.0_wp)

end subroutine test_real_long


subroutine test_real_special(error)

   type(error_type), allocatable, intent(out) :: error

   call test_real_gen(error, [character(len=12) :: &
      & "Inf", "+Infinity", "-Inf", "NaN", "-NaN"])

end subroutine test_real_special


!> Tokens embedded in a line must only be parsed within their bounds
subroutine test_real_next_token(error)

   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: line = &
      & "C"//achar(9)//"-1.5e3  .25"//achar(9)//"7  1d0 0.1"//achar(13)
   real(wp), parameter :: ref(*) = [-1.5e3_wp, 0.25_wp, 7.0_wp, 1.0_wp, 0.1_wp]
   type(token_type) :: token
   integer :: i, pos, stat
   real(wp) :: val

   pos = 1
   do i = 1, size(ref)
      call read_next_token(line, pos, token, val, stat)
      call check(error, stat, 0, "Failed to read token in line")
      if (allocated(error)) return
      call check(error, transfer(val, 0_i8) == transfer(ref(i), 0_i8), &
         & message="Value differs for '"//line(token%first:token%last)//"'")
      if (allocated(error)) return
   end do

   call read_next_token(line, pos, token, val, stat)
   call check(error, stat /= 0, "Read past the last token")

end subroutine test_real_next_token


subroutine test_real_invalid(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: str(*) = [character(len=24) :: &
      & "abc", "1.2.3", "-", "+", ".", "--1", "1e5e5", "12x", "e5", &
      & "", "+.", "-.", ".e1", "1e+", "1e-", "1e++2", "1e--2", &
      & "1e+-2", "1e2x", "1e2.0", "1..", "1.0e1.0"]
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

   character(len=*), parameter :: str(*) = [character(len=24) :: &
      & "12.5", "1D0", "9007199254740993", "1e23", "-0e-23"]
   character(len=:), allocatable :: msg
   integer :: stat, i
   real(wp) :: val

   do i = 1, size(str)
      call read_token("abc", token_type(1, 3), val, stat, msg)
      if (stat == 0) then
         call test_failed(error, "Accepted invalid number")
         return
      end if

      call check(error, len(msg) > 0, "Missing message for failed read")
      if (allocated(error)) return

      call read_token(trim(str(i)), token_type(1, len_trim(str(i))), val, stat, msg)
      call check(error, stat, 0)
      if (allocated(error)) return

      call check(error, len(msg), 0, "Message set on successful read of '"//trim(str(i))//"'")
      if (allocated(error)) return
   end do

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
