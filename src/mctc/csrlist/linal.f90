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

!> @file mctc/csrlist/linal.f90
!> Sparse matrix-vector and matrix-matrix routines for CSR compressed matrices.
!> The routines follow the argument conventions of the MKL sparse BLAS.

module mctc_csrlist_linal
   use mctc_csrlist_type, only : csr_list
   use mctc_env, only : wp, i8
   implicit none
   private

   public :: spgemv_csr, spsymv_csr, spmm_csr, spmspv_csr

   !> Performs the general Compressed Sparse Row (CSR) matrix-vector operation
   !>
   !>    y := A*x  or  y := A^T*x
   !>
   !> where A is a square m-by-m matrix. The operation selector transa
   !> is an optional trailing argument and defaults to 'N'.
   interface spgemv_csr
      !> Standard CSR matrix-vector multiplication.
      module procedure spgemv_csr
      !> CSR matrix-vector multiplication with separate diagonal elements.
      module procedure spgemv_csr_sepdiag
      !> CSR matrix-vector multiplication with a CSR-indexed full matrix.
      module procedure gemv_csr
   end interface spgemv_csr

   !> Performs the symmetric Compressed Sparse Row (CSR) matrix-vector operation
   !>
   !>    y := A*x
   !>
   !> where A is a symmetric m-by-m matrix of which only the upper or lower
   !> triangle is referenced. The triangle selector uplo is
   !> an optional trailing argument and defaults to 'U'.
   interface spsymv_csr
      !> Standard CSR matrix-vector multiplication.
      module procedure spsymv_csr
      !> CSR matrix-vector multiplication with separate diagonal elements.
      module procedure spsymv_csr_sepdiag
      !> CSR matrix-vector multiplication with a CSR-indexed full matrix.
      module procedure dspsymv_csr
   end interface spsymv_csr

   !> Performs the sparse matrix - sparse vector operation
   !>
   !>    y := alpha*A*x
   !>
   !> where alpha is a scalar, A is a matrix given by a complete CSR list and
   !> x is a sparse vector given as a pair of index/value arrays, holding the
   !> positions and values of its non-zero elements. The resulting vector y is
   !> returned in the same sparse representation, its index/value arrays are
   !> allocated by the routine.
   interface spmspv_csr
      !> Sparse matrix - sparse vector multiplication, complete CSR storage.
      module procedure spmspv_csr
   end interface spmspv_csr

   !> Performs the Compressed Sparse Row (CSR) matrix-matrix operations
   !>
   !>    C := op(A)*B              (sparse B and C)
   !>    C := alpha*op(A)*B + beta*C  (dense B and C)
   !>
   !> with op(A) = A or A^T.
   interface spmm_csr
      !> Sparse-sparse product with a sparse result.
      module procedure spspmm_csr
      !> Sparse-dense product for a dense right-hand side block.
      module procedure spgemm_csr
   end interface spmm_csr

contains

!> Multiply a general CSR matrix by a vector
subroutine spgemv_csr(m, a, ia, ja, x, y, transa)

   !> Number of rows of the matrix A
   integer, intent(in) :: m

   !> Non-zero elements of the matrix A
   real(wp), intent(in) :: a(:)

   !> Row offsets of A into a and ja, size m + 1
   integer(i8), intent(in) :: ia(:)

   !> Column indices of the non-zero elements of A
   integer, intent(in) :: ja(:)

   !> Input vector
   real(wp), intent(in) :: x(:)

   !> Output vector, its first m elements are overwritten
   real(wp), intent(inout) :: y(:)

   !> Operation selector, 'N' for A*x (default), 'T' or 'C' for A^T*x
   character(len=1), intent(in), optional :: transa

   integer :: i, j
   integer(i8) :: k
   logical :: trans
   real(wp) :: y_tmp_i
   real(wp), allocatable :: y_priv(:)

   trans = .false.
   if (present(transa)) trans = scan(transa, "TtCc") > 0

   if (.not. trans) then
      !$omp parallel do default(none) schedule(guided) &
      !$omp& private(i, k, y_tmp_i) shared(m, a, ia, ja, x, y)
      do i = 1, m
         y_tmp_i = 0.0_wp
         do k = ia(i), ia(i+1) - 1
            y_tmp_i = y_tmp_i + a(k) * x(ja(k))
         end do
         y(i) = y_tmp_i
      end do
      !$omp end parallel do
   else
      y(:m) = 0.0_wp

      !$omp parallel default(none) &
      !$omp& private(y_priv, i, k, j) shared(m, a, ia, ja, x, y)
      allocate(y_priv(m), source=0.0_wp)

      !$omp do schedule(guided)
      do i = 1, m
         do k = ia(i), ia(i+1) - 1
            j = ja(k)
            y_priv(j) = y_priv(j) + a(k) * x(i)
         end do
      end do
      !$omp end do

      !$omp critical
      y(:m) = y(:m) + y_priv
      !$omp end critical

      deallocate(y_priv)
      !$omp end parallel
   end if

end subroutine spgemv_csr

!> Multiply a general CSR matrix with separate diagonal elements by a vector
subroutine spgemv_csr_sepdiag(m, a, diag, ia, ja, x, y, transa)

   !> Number of rows of the matrix A
   integer, intent(in) :: m

   !> Non-zero elements of the matrix A
   real(wp), intent(in) :: a(:)

   !> Diagonal elements, added to the diagonal stored in a
   real(wp), intent(in) :: diag(:)

   !> Row offsets of A into a and ja, size m + 1
   integer(i8), intent(in) :: ia(:)

   !> Column indices of the non-zero elements of A
   integer, intent(in) :: ja(:)

   !> Input vector
   real(wp), intent(in) :: x(:)

   !> Output vector, its first m elements are overwritten
   real(wp), intent(inout) :: y(:)

   !> Operation selector, 'N' for A*x (default), 'T' or 'C' for A^T*x
   character(len=1), intent(in), optional :: transa

   call spgemv_csr(m, a, ia, ja, x, y, transa)
   y(:m) = y(:m) + diag(:m) * x(:m)

end subroutine spgemv_csr_sepdiag

!> Multiply a CSR-indexed full matrix by a vector
subroutine gemv_csr(m, a, ia, ja, x, y, transa)

   !> Number of rows of the matrix A
   integer, intent(in) :: m

   !> Full matrix indexed by neighbouring and central atoms
   real(wp), intent(in) :: a(:, :)

   !> Row offsets of A into ja, size m + 1
   integer(i8), intent(in) :: ia(:)

   !> Column indices of the non-zero elements of A
   integer, intent(in) :: ja(:)

   !> Input vector
   real(wp), intent(in) :: x(:)

   !> Output vector, its first m elements are overwritten
   real(wp), intent(inout) :: y(:)

   !> Operation selector, 'N' for A*x (default), 'T' or 'C' for A^T*x
   character(len=1), intent(in), optional :: transa

   integer :: i, j
   integer(i8) :: k
   logical :: trans
   real(wp) :: y_tmp_i
   real(wp), allocatable :: y_priv(:)

   trans = .false.
   if (present(transa)) trans = scan(transa, "TtCc") > 0

   if (.not. trans) then
      !$omp parallel do default(none) schedule(guided) &
      !$omp& private(i, j, k, y_tmp_i) shared(m, a, ia, ja, x, y)
      do i = 1, m
         y_tmp_i = 0.0_wp
         do k = ia(i), ia(i+1) - 1
            j = ja(k)
            y_tmp_i = y_tmp_i + a(j, i) * x(j)
         end do
         y(i) = y_tmp_i
      end do
      !$omp end parallel do
   else
      y(:m) = 0.0_wp

      !$omp parallel default(none) &
      !$omp& private(y_priv, i, k, j) shared(m, a, ia, ja, x, y)
      allocate(y_priv(m), source=0.0_wp)

      !$omp do schedule(guided)
      do i = 1, m
         do k = ia(i), ia(i+1) - 1
            j = ja(k)
            y_priv(j) = y_priv(j) + a(j, i) * x(i)
         end do
      end do
      !$omp end do

      !$omp critical
      y(:m) = y(:m) + y_priv
      !$omp end critical

      deallocate(y_priv)
      !$omp end parallel
   end if

end subroutine gemv_csr

!> Multiply a symmetric CSR matrix by a vector
subroutine spsymv_csr(m, a, ia, ja, x, y, uplo)

   !> Number of rows of the matrix A
   integer, intent(in) :: m

   !> Non-zero elements of the matrix A
   real(wp), intent(in) :: a(:)

   !> Row offsets of A into a and ja, size m + 1
   integer(i8), intent(in) :: ia(:)

   !> Column indices of the non-zero elements of A
   integer, intent(in) :: ja(:)

   !> Input vector
   real(wp), intent(in) :: x(:)

   !> Output vector, its first m elements are overwritten
   real(wp), intent(inout) :: y(:)

   !> Triangle selector, 'U' for the upper (default), 'L' for the lower one
   character(len=1), intent(in), optional :: uplo

   integer :: i, j
   integer(i8) :: k
   logical :: upper
   real(wp) :: y_tmp_i
   real(wp), allocatable :: y_priv(:)

   upper = .true.
   if (present(uplo)) upper = scan(uplo, "Uu") > 0

   y(:m) = 0.0_wp

   !$omp parallel default(none) &
   !$omp& private(y_priv, i, k, j, y_tmp_i) shared(m, a, ia, ja, x, y, upper)
   allocate(y_priv(m), source=0.0_wp)

   !$omp do schedule(guided)
   do i = 1, m
      y_tmp_i = 0.0_wp
      do k = ia(i), ia(i+1) - 1
         j = ja(k)
         if (j == i) then
            y_tmp_i = y_tmp_i + a(k) * x(i)
         else if ((j > i) .eqv. upper) then
            ! Contributions to row i and to the mirrored row j
            y_tmp_i = y_tmp_i + a(k) * x(j)
            y_priv(j) = y_priv(j) + a(k) * x(i)
         end if
      end do
      y_priv(i) = y_priv(i) + y_tmp_i
   end do
   !$omp end do

   !$omp critical
   y(:m) = y(:m) + y_priv
   !$omp end critical

   deallocate(y_priv)
   !$omp end parallel

end subroutine spsymv_csr

!> Multiply a symmetric CSR matrix with separate diagonal elements by a vector
subroutine spsymv_csr_sepdiag(m, a, diag, ia, ja, x, y, uplo)

   !> Number of rows of the matrix A
   integer, intent(in) :: m

   !> Non-zero elements of the matrix A
   real(wp), intent(in) :: a(:)

   !> Diagonal elements, added to the diagonal stored in a
   real(wp), intent(in) :: diag(:)

   !> Row offsets of A into a and ja, size m + 1
   integer(i8), intent(in) :: ia(:)

   !> Column indices of the non-zero elements of A
   integer, intent(in) :: ja(:)

   !> Input vector
   real(wp), intent(in) :: x(:)

   !> Output vector, its first m elements are overwritten
   real(wp), intent(inout) :: y(:)

   !> Triangle selector, 'U' for the upper (default), 'L' for the lower one
   character(len=1), intent(in), optional :: uplo

   call spsymv_csr(m, a, ia, ja, x, y, uplo)
   y(:m) = y(:m) + diag(:m) * x(:m)

end subroutine spsymv_csr_sepdiag

!> Multiply a symmetric CSR-indexed full matrix by a vector
subroutine dspsymv_csr(m, a, ia, ja, x, y, uplo)

   !> Number of rows of the matrix A
   integer, intent(in) :: m

   !> Full matrix indexed by neighbouring and central atoms
   real(wp), intent(in) :: a(:, :)

   !> Row offsets of A into ja, size m + 1
   integer(i8), intent(in) :: ia(:)

   !> Column indices of the non-zero elements of A
   integer, intent(in) :: ja(:)

   !> Input vector
   real(wp), intent(in) :: x(:)

   !> Output vector, its first m elements are overwritten
   real(wp), intent(inout) :: y(:)

   !> Triangle selector, 'U' for the upper (default), 'L' for the lower one
   character(len=1), intent(in), optional :: uplo

   integer :: i, j
   integer(i8) :: k
   logical :: upper
   real(wp) :: y_tmp_i
   real(wp), allocatable :: y_priv(:)

   upper = .true.
   if (present(uplo)) upper = scan(uplo, "Uu") > 0

   y(:m) = 0.0_wp

   !$omp parallel default(none) &
   !$omp& private(y_priv, i, k, j, y_tmp_i) &
   !$omp& shared(m, a, ia, ja, x, y, upper)
   allocate(y_priv(m), source=0.0_wp)

   !$omp do schedule(guided)
   do i = 1, m
      y_tmp_i = 0.0_wp
      do k = ia(i), ia(i+1) - 1
         j = ja(k)
         if (j == i) then
            y_tmp_i = y_tmp_i + a(i, i) * x(i)
         else if ((j > i) .eqv. upper) then
            ! Contributions to row i and to the mirrored row j
            y_tmp_i = y_tmp_i + a(j, i) * x(j)
            y_priv(j) = y_priv(j) + a(j, i) * x(i)
         end if
      end do
      y_priv(i) = y_priv(i) + y_tmp_i
   end do
   !$omp end do

   !$omp critical
   y(:m) = y(:m) + y_priv
   !$omp end critical

   deallocate(y_priv)
   !$omp end parallel

end subroutine dspsymv_csr

!> Multiply a CSR matrix given in complete storage by a sparse vector
subroutine spmspv_csr(list, mlist, xptr, xval, yptr, yval, alpha)

   !> CSR neighbour-list structure
   type(csr_list), intent(in) :: list

   !> Matrix elements in CSR order
   real(wp), intent(in) :: mlist(:)

   !> Row indices of the non-zero elements of the input vector
   integer, intent(in) :: xptr(:)

   !> Values of the non-zero elements of the input vector
   real(wp), intent(in) :: xval(:)

   !> Row indices of the non-zero elements of the product vector, allocated here
   integer, allocatable, intent(out) :: yptr(:)

   !> Values of the non-zero elements of the product vector, allocated here
   real(wp), allocatable, intent(out) :: yval(:)

   !> Matrix scaling factor, defaults to one
   real(wp), intent(in), optional :: alpha

   integer :: i, j, n, m, nnz
   integer(i8) :: k
   real(wp) :: a, y_tmp_i
   real(wp), allocatable :: xdense(:), ydense(:)
   logical, allocatable :: yflag(:)

   a = 1.0_wp
   if (present(alpha)) a = alpha

   n = size(list%inl) - 1

   if (size(mlist) /= size(list%nlat) .or. n < 0) then
      allocate(yptr(0), yval(0))
      return
   end if

   allocate(xdense(n), source=0.0_wp)
   m = min(size(xptr), size(xval))
   do i = 1, m
      j = xptr(i)
      if (j >= 1 .and. j <= n) xdense(j) = xdense(j) + xval(i)
   end do

   allocate(ydense(n))
   allocate(yflag(n), source=.false.)

   !$omp parallel do default(none) schedule(guided) &
   !$omp& private(i, k, j, y_tmp_i) &
   !$omp& shared(list, mlist, xdense, ydense, yflag, a, n)
   do i = 1, n
      y_tmp_i = 0.0_wp
      do k = list%inl(i), list%inl(i+1) - 1
         j = list%nlat(k)
         y_tmp_i = y_tmp_i + mlist(k) * xdense(j)
      end do
      ydense(i) = a * y_tmp_i
      yflag(i) = y_tmp_i /= 0.0_wp
   end do
   !$omp end parallel do

   nnz = count(yflag)
   allocate(yptr(nnz))
   allocate(yval(nnz))

   nnz = 0
   do i = 1, n
      if (yflag(i)) then
         nnz = nnz + 1
         yptr(nnz) = i
         yval(nnz) = ydense(i)
      end if
   end do

end subroutine spmspv_csr

!> Multiply two CSR matrices, C := op(A)*B, with a CSR result. The columns
!> of each row of C are returned in ascending order.
recursive subroutine spspmm_csr(trans, request, sort, m, n, k, a, ja, ia, &
      & b, jb, ib, c, jc, ic, nzmax, info)

   !> Operation selector, 'N' for A*B, 'T' or 'C' for A^T*B
   character(len=1), intent(in) :: trans

   !> Computation stage, 0 computes ic, jc and c within nzmax elements,
   !> 1 computes only ic, 2 computes jc and c from an ic of a previous call
   integer, intent(in) :: request

   !> Reordering selector, accepted for compatibility only since the inputs
   !> need no sorting and the result is always sorted
   integer, intent(in) :: sort

   !> Number of rows of the matrix A
   integer, intent(in) :: m

   !> Number of columns of the matrix A
   integer, intent(in) :: n

   !> Number of columns of the matrix B
   integer, intent(in) :: k

   !> Non-zero elements of the matrix A
   real(wp), intent(in) :: a(:)

   !> Column indices of the non-zero elements of A
   integer, intent(in) :: ja(:)

   !> Row offsets of A into a and ja, size m + 1
   integer(i8), intent(in) :: ia(:)

   !> Non-zero elements of the matrix B
   real(wp), intent(in) :: b(:)

   !> Column indices of the non-zero elements of B
   integer, intent(in) :: jb(:)

   !> Row offsets of B into b and jb, size n + 1 for 'N' and m + 1 for 'T'
   integer(i8), intent(in) :: ib(:)

   !> Non-zero elements of the matrix C, not referenced for request 1
   real(wp), intent(inout) :: c(:)

   !> Column indices of the non-zero elements of C, not referenced for request 1
   integer, intent(inout) :: jc(:)

   !> Row offsets of C into c and jc, size m + 1 for 'N' and n + 1 for 'T'
   integer(i8), intent(inout) :: ic(:)

   !> Maximum number of non-zero elements of C, only referenced for request 0
   integer(i8), intent(in) :: nzmax

   !> Exit status, zero on success, or the row of C exceeding nzmax for request 0
   integer, intent(out) :: info

   integer :: i, j, jcol, ntouch, it, jt
   integer(i8) :: ka, kb, kc
   real(wp) :: aij
   integer(i8), allocatable :: iat(:), pos(:)
   integer, allocatable :: jat(:)
   real(wp), allocatable :: at(:)

   ! Thread-private sparse accumulator for a single row of the product
   real(wp), allocatable :: acc(:)
   logical, allocatable :: flag(:)
   integer, allocatable :: touch(:)

   info = 0

   ! Transposed product, form A^T explicitly and evaluate A^T*B
   if (scan(trans, "TtCc") > 0) then
      allocate(iat(n + 1), source=0_i8)
      do ka = 1, ia(m+1) - 1
         iat(ja(ka) + 1) = iat(ja(ka) + 1) + 1
      end do
      iat(1) = 1
      do j = 1, n
         iat(j+1) = iat(j+1) + iat(j)
      end do

      allocate(at(ia(m+1) - 1), jat(ia(m+1) - 1))
      allocate(pos(n), source=iat(:n))
      do i = 1, m
         do ka = ia(i), ia(i+1) - 1
            j = ja(ka)
            at(pos(j)) = a(ka)
            jat(pos(j)) = i
            pos(j) = pos(j) + 1
         end do
      end do

      call spspmm_csr("N", request, sort, n, m, k, at, jat, iat, &
         & b, jb, ib, c, jc, ic, nzmax, info)
      return
   end if

   if (request /= 2) then
      !$omp parallel default(none) &
      !$omp& private(i, j, jcol, ka, kb, ntouch, it, flag, touch) &
      !$omp& shared(m, k, ia, ja, ib, jb, ic)
      allocate(flag(k), source=.false.)
      allocate(touch(k))

      !$omp do schedule(guided)
      do i = 1, m
         ntouch = 0
         do ka = ia(i), ia(i+1) - 1
            j = ja(ka)
            do kb = ib(j), ib(j+1) - 1
               jcol = jb(kb)
               if (.not. flag(jcol)) then
                  flag(jcol) = .true.
                  ntouch = ntouch + 1
                  touch(ntouch) = jcol
               end if
            end do
         end do
         ic(i+1) = ntouch
         do it = 1, ntouch
            flag(touch(it)) = .false.
         end do
      end do
      !$omp end do

      deallocate(flag, touch)
      !$omp end parallel

      ic(1) = 1
      do i = 1, m
         ic(i+1) = ic(i+1) + ic(i)
      end do

      if (request == 1) return

      if (ic(m+1) - 1 > nzmax) then
         do i = 1, m
            if (ic(i+1) - 1 > nzmax) exit
         end do
         info = i
         return
      end if
   end if

   !$omp parallel default(none) &
   !$omp& private(i, j, jcol, ka, kb, kc, aij, ntouch, it, jt, acc, flag, touch) &
   !$omp& shared(m, k, a, ia, ja, b, ib, jb, c, ic, jc)
   allocate(acc(k), source=0.0_wp)
   allocate(flag(k), source=.false.)
   allocate(touch(k))

   !$omp do schedule(guided)
   do i = 1, m
      ntouch = 0
      do ka = ia(i), ia(i+1) - 1
         j = ja(ka)
         aij = a(ka)
         do kb = ib(j), ib(j+1) - 1
            jcol = jb(kb)
            if (.not. flag(jcol)) then
               flag(jcol) = .true.
               ntouch = ntouch + 1
               touch(ntouch) = jcol
               acc(jcol) = 0.0_wp
            end if
            acc(jcol) = acc(jcol) + aij * b(kb)
         end do
      end do
      do it = 2, ntouch
         jcol = touch(it)
         jt = it - 1
         do while (jt >= 1)
            if (touch(jt) <= jcol) exit
            touch(jt+1) = touch(jt)
            jt = jt - 1
         end do
         touch(jt+1) = jcol
      end do
      kc = ic(i)
      do it = 1, ntouch
         jcol = touch(it)
         jc(kc) = jcol
         c(kc) = acc(jcol)
         flag(jcol) = .false.
         kc = kc + 1
      end do
   end do
   !$omp end do

   deallocate(acc, flag, touch)
   !$omp end parallel

end subroutine spspmm_csr

!> Multiply a CSR matrix by a dense matrix, C := alpha*op(A)*B + beta*C.
subroutine spgemm_csr(transa, m, n, k, alpha, matdescra, val, indx, pntrb, &
      & pntre, b, ldb, beta, c, ldc)

   !> Operation selector, 'N' for A*B, 'T' or 'C' for A^T*B
   character(len=1), intent(in) :: transa

   !> Number of rows of the matrix A
   integer, intent(in) :: m

   !> Number of columns of the matrices B and C
   integer, intent(in) :: n

   !> Number of columns of the matrix A
   integer, intent(in) :: k

   !> Matrix scaling factor
   real(wp), intent(in) :: alpha

   !> Matrix descriptor, structure in the first and triangle in the second
   !> character, the triangle is only referenced for symmetric matrices
   character(len=*), intent(in) :: matdescra

   !> Non-zero elements of the matrix A
   real(wp), intent(in) :: val(:)

   !> Column indices of the non-zero elements of A
   integer, intent(in) :: indx(:)

   !> Offsets of the first element of each row of A, size m
   integer(i8), intent(in) :: pntrb(:)

   !> Offsets past the last element of each row of A, size m
   integer(i8), intent(in) :: pntre(:)

   !> Leading dimension of B
   integer, intent(in) :: ldb

   !> Dense right-hand side block, k-by-n for 'N' and m-by-n for 'T'
   real(wp), intent(in) :: b(ldb, n)

   !> Existing-matrix scaling factor
   real(wp), intent(in) :: beta

   !> Leading dimension of C
   integer, intent(in) :: ldc

   !> Dense product block, m-by-n for 'N' and k-by-n for 'T'
   real(wp), intent(inout) :: c(ldc, n)

   integer :: i, j, nrow
   integer(i8) :: kk
   logical :: trans, sym, upper
   real(wp) :: v
   real(wp), allocatable :: c_tmp(:), c_priv(:, :)

   trans = scan(transa, "TtCc") > 0
   sym = scan(matdescra(1:1), "SsHh") > 0
   upper = .false.
   if (sym .and. len(matdescra) > 1) upper = scan(matdescra(2:2), "Uu") > 0

   nrow = m
   if (trans .and. .not. sym) nrow = k

   if (beta == 0.0_wp) then
      c(:nrow, :n) = 0.0_wp
   else if (beta /= 1.0_wp) then
      c(:nrow, :n) = beta * c(:nrow, :n)
   end if

   if (.not. (trans .or. sym)) then
      ! General non-transposed product, rows of C are independent
      !$omp parallel default(none) private(i, j, kk, c_tmp) &
      !$omp& shared(m, n, alpha, val, indx, pntrb, pntre, b, c)
      allocate(c_tmp(n))

      !$omp do schedule(guided)
      do i = 1, m
         c_tmp(:) = 0.0_wp
         do kk = pntrb(i), pntre(i) - 1
            j = indx(kk)
            c_tmp(:) = c_tmp(:) + val(kk) * b(j, :n)
         end do
         c(i, :n) = c(i, :n) + alpha * c_tmp(:)
      end do
      !$omp end do

      deallocate(c_tmp)
      !$omp end parallel
   else
      ! Transposed or symmetric product, scatter into private blocks
      !$omp parallel default(none) private(i, j, kk, v, c_priv) &
      !$omp& shared(m, n, nrow, alpha, val, indx, pntrb, pntre, b, c, sym, upper)
      allocate(c_priv(nrow, n), source=0.0_wp)

      !$omp do schedule(guided)
      do i = 1, m
         do kk = pntrb(i), pntre(i) - 1
            j = indx(kk)
            v = alpha * val(kk)
            if (.not. sym) then
               c_priv(j, :) = c_priv(j, :) + v * b(i, :n)
            else if (j == i) then
               c_priv(i, :) = c_priv(i, :) + v * b(i, :n)
            else if ((j > i) .eqv. upper) then
               c_priv(i, :) = c_priv(i, :) + v * b(j, :n)
               c_priv(j, :) = c_priv(j, :) + v * b(i, :n)
            end if
         end do
      end do
      !$omp end do

      !$omp critical
      c(:nrow, :n) = c(:nrow, :n) + c_priv
      !$omp end critical

      deallocate(c_priv)
      !$omp end parallel
   end if

end subroutine spgemm_csr

end module mctc_csrlist_linal
