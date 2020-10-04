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

!> Simple algebraic functions
module mctc_io_math
   use mctc_env_accuracy, only : wp
   use mctc_io_constants, only : pi
   implicit none
   private

   public :: matdet_3x3, matinv_3x3, crossprod, eigval_3x3, eigvec_3x3


   real(wp), parameter :: twothirdpi = 2.0_wp * pi / 3.0_wp


contains


!> Performs a direct calculation of the inverse of a 3×3 matrix.
!
!  reference: http://fortranwiki.org/fortran/show/Matrix+inversion
pure function matinv_3x3(a) result(b)

   !> Matrix
   real(wp), intent(in) :: a(3, 3)

   !> Inverse matrix
   real(wp) :: b(3, 3)

   real(wp) :: detinv

   ! Calculate the inverse determinant of the matrix
   detinv = 1.0_wp/matdet_3x3(a)

   ! Calculate the inverse of the matrix
   b(1, 1) = +detinv * (a(2, 2) * a(3, 3) - a(2, 3) * a(3, 2))
   b(2, 1) = -detinv * (a(2, 1) * a(3, 3) - a(2, 3) * a(3, 1))
   b(3, 1) = +detinv * (a(2, 1) * a(3, 2) - a(2, 2) * a(3, 1))
   b(1, 2) = -detinv * (a(1, 2) * a(3, 3) - a(1, 3) * a(3, 2))
   b(2, 2) = +detinv * (a(1, 1) * a(3, 3) - a(1, 3) * a(3, 1))
   b(3, 2) = -detinv * (a(1, 1) * a(3, 2) - a(1, 2) * a(3, 1))
   b(1, 3) = +detinv * (a(1, 2) * a(2, 3) - a(1, 3) * a(2, 2))
   b(2, 3) = -detinv * (a(1, 1) * a(2, 3) - a(1, 3) * a(2, 1))
   b(3, 3) = +detinv * (a(1, 1) * a(2, 2) - a(1, 2) * a(2, 1))

end function matinv_3x3


!> Determinat of 3×3 matrix
pure function matdet_3x3(a) result (det)

   !> Matrix
   real(wp), intent(in) :: a(3, 3)

   !> Determinant
   real(wp) :: det

   det =  a(1, 1) * a(2, 2) * a(3, 3)  &
      & - a(1, 1) * a(2, 3) * a(3, 2)  &
      & - a(1, 2) * a(2, 1) * a(3, 3)  &
      & + a(1, 2) * a(2, 3) * a(3, 1)  &
      & + a(1, 3) * a(2, 1) * a(3, 2)  &
      & - a(1, 3) * a(2, 2) * a(3, 1)

end function matdet_3x3


!> Implements the cross/vector product between two 3D vectors
pure function crossprod(a,b) result(c)

   !> First vector
   real(wp), intent(in) :: a(3)

   !> Second vector
   real(wp), intent(in) :: b(3)

   !> Orthogonal vector
   real(wp) :: c(3)

   c(1) = a(2) * b(3) - b(2) * a(3)
   c(2) = a(3) * b(1) - b(3) * a(1)
   c(3) = a(1) * b(2) - b(1) * a(2)

end function crossprod


!> Calculates eigenvalues based on the trigonometric solution of A = pB + qI
pure subroutine eigval_3x3(a, w)

   !> The symmetric input matrix
   real(wp), intent(in) :: a(3, 3)

   !> Contains eigenvalues on exit
   real(wp), intent(out) :: w(3)

   real(wp) :: q, p, r

   r = a(1, 2) * a(1, 2) + a(1, 3) * a(1, 3) + a(2, 3) * a(2, 3)
   q = (a(1, 1) + a(2, 2) + a(3, 3)) / 3.0_wp
   w(1) = a(1, 1) - q
   w(2) = a(2, 2) - q
   w(3) = a(3, 3) - q
   p = sqrt((w(1) * w(1) + w(2) * w(2) + w(3) * w(3) + 2*r) / 6.0_wp)
   r = (w(1) * (w(2) * w(3) - a(2, 3) * a(2, 3)) &
      & - a(1, 2) * (a(1, 2) * w(3) - a(2, 3) * a(1, 3)) &
      & + a(1, 3) * (a(1, 2) * a(2, 3) - w(2) * a(1, 3))) / (p*p*p) * 0.5_wp

   if (r <= -1.0_wp) then
      r = 0.5_wp * twothirdpi
   else if (r >= 1.0_wp) then
      r = 0.0_wp
   else
      r = acos(r) / 3.0_wp
   end if

   w(3) = q + 2 * p * cos(r)
   w(1) = q + 2 * p * cos(r + twothirdpi)
   w(2) = 3 * q - w(1) - w(3)

end subroutine eigval_3x3


!> Calculates eigenvector using an analytical method based on vector cross
!  products.
pure subroutine eigvec_3x3(a, w, q)
   real(wp), intent(inout) :: a(3,3)
   real(wp), intent(out) :: w(3)
   real(wp), intent(out) :: q(3,3)

   real(wp), parameter :: eps = epsilon(1.0_wp)
   real(wp) norm, n1, n2, n3, precon
   integer :: i

   w(1) = max(abs(a(1, 1)), abs(a(1, 2)))
   w(2) = max(abs(a(1, 3)), abs(a(2, 2)))
   w(3) = max(abs(a(2, 3)), abs(a(3, 3)))
   precon = max(w(1), max(w(2), w(3)))

   ! null matrix
   if (precon < eps) then
      w(1) = 0.0_wp
      w(2) = 0.0_wp
      w(3) = 0.0_wp
      q(1, 1) = 1.0_wp
      q(2, 2) = 1.0_wp
      q(3, 3) = 1.0_wp
      q(1, 2) = 0.0_wp
      q(1, 3) = 0.0_wp
      q(2, 3) = 0.0_wp
      q(2, 1) = 0.0_wp
      q(3, 1) = 0.0_wp
      q(3, 2) = 0.0_wp
      return
   end if

   norm = 1.0_wp / precon

   a(1, 1) = a(1, 1) * norm
   a(1, 2) = a(1, 2) * norm
   a(2, 2) = a(2, 2) * norm
   a(1, 3) = a(1, 3) * norm
   a(2, 3) = a(2, 3) * norm
   a(3, 3) = a(3, 3) * norm

   ! Calculate eigenvalues
   call eigval_3x3(a, w)

   ! Compute first eigenvector
   a(1, 1) = a(1, 1) - w(1)
   a(2, 2) = a(2, 2) - w(1)
   a(3, 3) = a(3, 3) - w(1)

   q(1, 1) = a(1, 2) * a(2, 3) - a(1, 3) * a(2, 2)
   q(2, 1) = a(1, 3) * a(1, 2) - a(1, 1) * a(2, 3)
   q(3, 1) = a(1, 1) * a(2, 2) - a(1, 2) * a(1, 2)
   q(1, 2) = a(1, 2) * a(3, 3) - a(1, 3) * a(2, 3)
   q(2, 2) = a(1, 3) * a(1, 3) - a(1, 1) * a(3, 3)
   q(3, 2) = a(1, 1) * a(2, 3) - a(1, 2) * a(1, 3)
   q(1, 3) = a(2, 2) * a(3, 3) - a(2, 3) * a(2, 3)
   q(2, 3) = a(2, 3) * a(1, 3) - a(1, 2) * a(3, 3)
   q(3, 3) = a(1, 2) * a(2, 3) - a(2, 2) * a(1, 3)
   n1 = q(1, 1) * q(1, 1) + q(2, 1) * q(2, 1) + q(3, 1) * q(3, 1)
   n2 = q(1, 2) * q(1, 2) + q(2, 2) * q(2, 2) + q(3, 2) * q(3, 2)
   n3 = q(1, 3) * q(1, 3) + q(2, 3) * q(2, 3) + q(3, 3) * q(3, 3)

   norm = n1
   i = 1
   if (n2 > norm) then
      i = 2
      norm = n1
   end if
   if (n3 > norm) then
      i = 3
   end if

   if (i == 1) then
      norm = sqrt(1.0_wp / n1)
      q(1, 1) = q(1, 1) * norm
      q(2, 1) = q(2, 1) * norm
      q(3, 1) = q(3, 1) * norm
   else if (i == 2) then
      norm = sqrt(1.0_wp / n2)
      q(1, 1) = q(1, 2) * norm
      q(2, 1) = q(2, 2) * norm
      q(3, 1) = q(3, 2) * norm
   else
      norm = sqrt(1.0_wp / n3)
      q(1, 1) = q(1, 3) * norm
      q(2, 1) = q(2, 3) * norm
      q(3, 1) = q(3, 3) * norm
   end if

   ! Robustly compute a right-hand orthonormal set (ev1, u, v)
   if (abs(q(1, 1)) > abs(q(2, 1))) then
      norm = sqrt(1.0_wp / (q(1, 1) * q(1, 1) + q(3, 1) * q(3, 1)))
      q(1, 2) = -q(3, 1) * norm
      q(2, 2) = 0.0_wp
      q(3, 2) = +q(1, 1) * norm
   else
      norm = sqrt(1.0_wp / (q(2, 1) * q(2, 1) + q(3, 1) * q(3, 1)))
      q(1, 2) = 0.0_wp
      q(2, 2) = +q(3, 1) * norm
      q(3, 2) = -q(2, 1) * norm
   end if
   q(1, 3) = q(2, 1) * q(3, 2) - q(3, 1) * q(2, 2)
   q(2, 3) = q(3, 1) * q(1, 2) - q(1, 1) * q(3, 2)
   q(3, 3) = q(1, 1) * q(2, 2) - q(2, 1) * q(1, 2)

   ! Reset A
   a(1, 1) = a(1, 1) + w(1)
   a(2, 2) = a(2, 2) + w(1)
   a(3, 3) = a(3, 3) + w(1)

   ! A*U
   n1 = a(1, 1) * q(1, 2) + a(1, 2) * q(2, 2) + a(1, 3) * q(3, 2)
   n2 = a(1, 2) * q(1, 2) + a(2, 2) * q(2, 2) + a(2, 3) * q(3, 2)
   n3 = a(1, 3) * q(1, 2) + a(2, 3) * q(2, 2) + a(3, 3) * q(3, 2)

   ! A*V, note out of order computation
   a(3, 3) = a(1, 3) * q(1, 3) + a(2, 3) * q(2, 3) + a(3, 3) * q(3, 3)
   a(1, 3) = a(1, 1) * q(1, 3) + a(1, 2) * q(2, 3) + a(1, 3) * q(3, 3)
   a(2, 3) = a(1, 2) * q(1, 3) + a(2, 2) * q(2, 3) + a(2, 3) * q(3, 3)

   ! UT*(A*U) - l2*E
   n1 = q(1, 2) * n1 + q(2, 2) * n2 + q(3, 2) * n3 - w(2)
   ! UT*(A*V)
   n2 = q(1, 2) * a(1, 3) + q(2, 2) * a(2, 3) + q(3, 2) * a(3, 3)
   ! VT*(A*V) - l2*E
   n3 = q(1, 3) * a(1, 3) + q(2, 3) * a(2, 3) + q(3, 3) * a(3, 3) - w(2)

   if (abs(n1) >= abs(n3)) then
      norm = max(abs(n1), abs(n2))
      if (norm > eps) then
         if (abs(n1) >= abs(n2)) then
            n2 = n2 / n1
            n1 = sqrt(1.0_wp / (1.0_wp + n2 * n2))
            n2 = n2 * n1
         else
            n1 = n1 / n2
            n2 = sqrt(1.0_wp / (1.0_wp + n1 * n1))
            n1 = n1 * n2
         end if
         q(1, 2) = n2 * q(1, 2) - n1 * q(1, 3)
         q(2, 2) = n2 * q(2, 2) - n1 * q(2, 3)
         q(3, 2) = n2 * q(3, 2) - n1 * q(3, 3)
      end if
   else
      norm = max(abs(n3), abs(n2))
      if (norm > eps) then
         if (abs(n3) >= abs(n2)) then
            n2 = n2 / n3
            n3 = sqrt(1.0_wp / (1.0_wp + n2 * n2))
            n2 = n2 * n3
         else
            n3 = n3 / n2
            n2 = sqrt(1.0_wp / (1.0_wp + n3 * n3))
            n3 = n3 * n2
         end if
         q(1, 2) = n3 * q(1, 2) - n2 * q(1, 3)
         q(2, 2) = n3 * q(2, 2) - n2 * q(2, 3)
         q(3, 2) = n3 * q(3, 2) - n2 * q(3, 3)
      end if
   end if

   ! Calculate third eigenvector from cross product
   q(1, 3) = q(2, 1) * q(3, 2) - q(3, 1) * q(2, 2)
   q(2, 3) = q(3, 1) * q(1, 2) - q(1, 1) * q(3, 2)
   q(3, 3) = q(1, 1) * q(2, 2) - q(2, 1) * q(1, 2)

   w(1) = w(1) * precon
   w(2) = w(2) * precon
   w(3) = w(3) * precon

end subroutine eigvec_3x3


end module mctc_io_math
