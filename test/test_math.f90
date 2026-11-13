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
   use mctc_io_math, only : matdet_3x3, matinv_3x3, eigval_3x3, eigvec_3x3
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
      & new_unittest("eigvec-norm-selection", test_eigvec_norm_selection), &
      & new_unittest("eigvec-degenerate", test_eigvec_degenerate), &
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


!> Regression test for eigvec_3x3 cross-product norm selection when n2 > n3 > n1
subroutine test_eigvec_norm_selection(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   real(wp), parameter :: alpha = 1.0e-14_wp
   real(wp), parameter :: tol = 100.0_wp * epsilon(1.0_wp)
   real(wp) :: a0(3, 3), mat(3, 3), eval(3), evec(3, 3)

   ! Symmetric matrix with exact smallest eigenvalue 1 and well-separated spectrum
   ! {1, (5 - sqrt(5))/2, (5 + sqrt(5))/2}, constructed so that for A - w(1)*I:
   ! n1 = 0, n2 = O(1), and n3 = 10^-28 > 0 (n2 > n3 > n1).
   a0 = reshape([ &
      & 2.0_wp, alpha, 1.0_wp, &
      & alpha, 1.0_wp + alpha**2, alpha, &
      & 1.0_wp, alpha, 3.0_wp], &
      & shape(a0))

   mat = a0
   call eigvec_3x3(mat, eval, evec)

   call check_eigensystem(error, a0, eval, evec, tol, tol)

end subroutine test_eigvec_norm_selection


!> Regression test for eigval_3x3 and eigvec_3x3 with 2-fold and 3-fold degenerate eigenvalues
subroutine test_eigvec_degenerate(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   real(wp), parameter :: tol = 100.0_wp * epsilon(1.0_wp)
   real(wp), parameter :: deg_res_tol = 10.0_wp * thr
   real(wp) :: a0(3, 3), mat(3, 3), eval(3), evec(3, 3), rot(3, 3), diag(3, 3)

   ! 1. Scalar multiple of identity (3-fold degeneracy: p = 0)
   a0 = reshape([ &
      & 2.5_wp, 0.0_wp, 0.0_wp, &
      & 0.0_wp, 2.5_wp, 0.0_wp, &
      & 0.0_wp, 0.0_wp, 2.5_wp], &
      & shape(a0))
   call eigval_3x3(a0, eval)
   ! NaN compares as within tolerance in check, test for it explicitly
   call check(error, all(eval == eval), "Eigenvalues of a scaled identity are NaN")
   if (allocated(error)) return
   call check(error, eval(1), 2.5_wp, thr=tol)
   if (allocated(error)) return
   call check(error, eval(2), 2.5_wp, thr=tol)
   if (allocated(error)) return
   call check(error, eval(3), 2.5_wp, thr=tol)
   if (allocated(error)) return

   mat = a0
   call eigvec_3x3(mat, eval, evec)
   call check_eigensystem(error, a0, eval, evec, tol, tol)
   if (allocated(error)) return

   ! 2. Rotated 2-fold degenerate minimum eigenvalue (w1 = w2 < w3)
   rot = reshape([ &
      &  3.0_wp/5.0_wp,  0.0_wp,  4.0_wp/5.0_wp, &
      &  0.0_wp,         1.0_wp,  0.0_wp, &
      & -4.0_wp/5.0_wp,  0.0_wp,  3.0_wp/5.0_wp], &
      & shape(rot))
   diag = reshape([ &
      & 1.0_wp, 0.0_wp, 0.0_wp, &
      & 0.0_wp, 1.0_wp, 0.0_wp, &
      & 0.0_wp, 0.0_wp, 5.0_wp], &
      & shape(diag))
   a0 = matmul(rot, matmul(diag, transpose(rot)))
   mat = a0
   call eigvec_3x3(mat, eval, evec)
   call check_eigensystem(error, a0, eval, evec, deg_res_tol, tol)
   if (allocated(error)) return

   ! 3. Rotated 2-fold degenerate maximum eigenvalue (w1 < w2 = w3)
   diag = reshape([ &
      & 1.0_wp, 0.0_wp, 0.0_wp, &
      & 0.0_wp, 5.0_wp, 0.0_wp, &
      & 0.0_wp, 0.0_wp, 5.0_wp], &
      & shape(diag))
   a0 = matmul(rot, matmul(diag, transpose(rot)))
   mat = a0
   call eigvec_3x3(mat, eval, evec)
   call check_eigensystem(error, a0, eval, evec, deg_res_tol, tol)
   if (allocated(error)) return

   ! 4. Shifted nearly spherical 2-fold bottom degeneracy (bmax << 1 after precon)
   diag = reshape([ &
      & 100.0_wp, 0.0_wp, 0.0_wp, &
      & 0.0_wp, 100.0_wp, 0.0_wp, &
      & 0.0_wp, 0.0_wp, 100.0_wp + 1.0e-5_wp], &
      & shape(diag))
   a0 = matmul(rot, matmul(diag, transpose(rot)))
   mat = a0
   call eigvec_3x3(mat, eval, evec)
   call check_eigensystem(error, a0, eval, evec, 100.0_wp * deg_res_tol, tol)

end subroutine test_eigvec_degenerate


!> Verify A*Q = Q*diag(w) and Q^T*Q = I
subroutine check_eigensystem(error, a0, eval, evec, res_tol, orth_tol)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   !> Original symmetric matrix
   real(wp), intent(in) :: a0(3, 3)

   !> Computed eigenvalues
   real(wp), intent(in) :: eval(3)

   !> Computed eigenvectors
   real(wp), intent(in) :: evec(3, 3)

   !> Residual tolerance for A*Q - Q*diag(w)
   real(wp), intent(in) :: res_tol

   !> Orthonormality tolerance for Q^T*Q - I
   real(wp), intent(in) :: orth_tol

   real(wp) :: res(3, 3), orth(3, 3)
   integer :: i, j

   call check(error, all(eval == eval) .and. all(evec == evec), &
      & "Eigenvalues or eigenvectors are NaN")
   if (allocated(error)) return

   orth = matmul(transpose(evec), evec)
   do j = 1, 3
      res(:, j) = matmul(a0, evec(:, j)) - eval(j) * evec(:, j)
      do i = 1, 3
         if (i == j) then
            call check(error, orth(i, j), 1.0_wp, thr=orth_tol)
         else
            call check(error, orth(i, j), 0.0_wp, thr=orth_tol)
         end if
         if (allocated(error)) return
         call check(error, res(i, j), 0.0_wp, thr=res_tol)
         if (allocated(error)) return
      end do
   end do

end subroutine check_eigensystem


end module test_math
