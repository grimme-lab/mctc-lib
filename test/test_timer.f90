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

module test_timer
   use mctc_env_accuracy, only : wp
   use mctc_env_testing, only : new_unittest, unittest_type, error_type, check
   use mctc_env_timer, only : timer_type, format_time
   implicit none
   private

   public :: collect_timer

contains

subroutine collect_timer(testsuite)

   type(unittest_type), allocatable, intent(out) :: testsuite(:)

   testsuite = [ &
      & new_unittest("format-time", test_format_time), &
      & new_unittest("push-pop-get", test_push_pop) &
      & ]

end subroutine collect_timer


subroutine test_format_time(error)

   type(error_type), allocatable, intent(out) :: error
   character(len=:), allocatable :: s, expected
   real(wp) :: t

   t = 1.234_wp
   s = format_time(t)
   expected = repeat(" ", 19) // "1.234 sec"

   call check(error, s, expected)

end subroutine test_format_time


subroutine test_push_pop(error)

   type(error_type), allocatable, intent(out) :: error
   type(timer_type) :: tmr
   real(wp) :: elapsed
   integer :: t0, tnow, rate

   ! start the timer record
   call tmr%push("test")

   ! busy-wait for a small positive time interval
   call sleep(1)

   elapsed = tmr%get("test")

   call check(error, elapsed > 1.0_wp, .true., "timer elapsed time must be positive")

end subroutine test_push_pop

end module test_timer
