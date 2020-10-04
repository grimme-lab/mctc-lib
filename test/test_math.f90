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

module test_math
   use mctc_env_accuracy, only : wp
   use mctc_env_testing, only : new_unittest, unittest_type, error_type, check
   use mctc_io_math
   implicit none
   private

   public :: collect_math


   real(wp), parameter :: thr = sqrt(epsilon(1.0_wp))

contains


!> Collect all exported unit tests
subroutine collect_math(testsuite)

   !> Collection of tests
   type(unittest_type), allocatable, intent(out) :: testsuite(:)

   testsuite = [ &
      & new_unittest("valid-eigval", test_eigval), &
      & new_unittest("valid-eigvec", test_eigvec), &
      & new_unittest("valid-matdet", test_matdet), &
      & new_unittest("valid-matinv", test_matinv) &
      & ]

end subroutine collect_math


subroutine test_matdet(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   real(wp) :: mat(3, 3), det

   mat = reshape(&
      & [2.0_wp, 1.0_wp, 3.0_wp, 1.0_wp, 3.0_wp, 0.0_wp, 3.0_wp, 0.0_wp, 4.0_wp], &
      & shape(mat))

   det = matdet_3x3(mat)

   call check(error, det, -7.0_wp, thr=thr)

end subroutine test_matdet


subroutine test_matinv(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   real(wp) :: mat(3, 3), inv(3, 3)

   mat = reshape(&
      & [2.0_wp, 1.0_wp, 3.0_wp, 1.0_wp, 3.0_wp, 0.0_wp, 3.0_wp, 0.0_wp, 4.0_wp], &
      & shape(mat))

   inv = matinv_3x3(mat)

   mat = matmul(mat, inv)

   call check(error, mat(1, 1), 1.0_wp, thr=thr)
   if (allocated(error)) return
   call check(error, mat(2, 2), 1.0_wp, thr=thr)
   if (allocated(error)) return
   call check(error, mat(3, 3), 1.0_wp, thr=thr)
   if (allocated(error)) return
   call check(error, mat(1, 2), 0.0_wp, thr=thr)
   if (allocated(error)) return
   call check(error, mat(1, 3), 0.0_wp, thr=thr)
   if (allocated(error)) return
   call check(error, mat(2, 3), 0.0_wp, thr=thr)
   if (allocated(error)) return

end subroutine test_matinv


subroutine test_eigval(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   real(wp) :: mat(3, 3), eval(3)

   mat = reshape(&
      & [2.0_wp, 1.0_wp, 3.0_wp, 1.0_wp, 3.0_wp, 0.0_wp, 3.0_wp, 0.0_wp, 4.0_wp], &
      & shape(mat))

   call eigval_3x3(mat, eval)

   call check(error, eval(1),-0.3611775878183057_wp, thr=thr)
   if (allocated(error)) return
   call check(error, eval(2), 3.0909775466663136_wp, thr=thr)
   if (allocated(error)) return
   call check(error, eval(3), 6.2702000411519920_wp, thr=thr)
   if (allocated(error)) return

end subroutine test_eigval


subroutine test_eigvec(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   real(wp) :: mat(3, 3), eval(3), evec(3, 3)

   mat = reshape(&
      & [2.0_wp, 1.0_wp, 3.0_wp, 1.0_wp, 3.0_wp, 0.0_wp, 3.0_wp, 0.0_wp, 4.0_wp], &
      & shape(mat))

   call eigvec_3x3(mat, eval, evec)

   call check(error, eval(1),-0.3611775878183057_wp, thr=thr)
   if (allocated(error)) return
   call check(error, eval(2), 3.0909775466663136_wp, thr=thr)
   if (allocated(error)) return
   call check(error, eval(3), 6.2702000411519920_wp, thr=thr)
   if (allocated(error)) return

   call check(error, evec(1, 1), 0.80020375200069072_wp, thr=thr)
   if (allocated(error)) return
   call check(error, evec(2, 1),-0.23807244071268843_wp, thr=thr)
   if (allocated(error)) return
   call check(error, evec(3, 1),-0.55045024139982024_wp, thr=thr)
   if (allocated(error)) return
   call check(error, evec(1, 2), 0.08680581175113650_wp, thr=thr)
   if (allocated(error)) return
   call check(error, evec(2, 2), 0.95414544502416110_wp, thr=thr)
   if (allocated(error)) return
   call check(error, evec(3, 2),-0.28648075116117611_wp, thr=thr)
   if (allocated(error)) return
   call check(error, evec(1, 3), 0.59341276219023387_wp, thr=thr)
   if (allocated(error)) return
   call check(error, evec(2, 3), 0.18146069192182893_wp, thr=thr)
   if (allocated(error)) return
   call check(error, evec(3, 3), 0.78417683653434167_wp, thr=thr)
   if (allocated(error)) return

end subroutine test_eigvec


end module test_math
