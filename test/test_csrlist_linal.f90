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

!> Unit tests for compressed sparse row neighbor lists
module test_csrlist_linal
   use mctc_csrlist, only : csr_list, spgemv_csr, spsymv_csr, spmm_csr, spmspv_csr
   use mctc_env, only : wp, i8, timer_type, format_time
   use mctc_env_testing, only : new_unittest, unittest_type, error_type, &
      & test_failed, check
   implicit none
   private

   public :: collect_csrlist_linal

   !> Tolerance for floating-point comparisons
   real(wp), parameter :: thr = 100*epsilon(1.0_wp)


contains


!> Collect all exported unit tests
subroutine collect_csrlist_linal(testsuite)

   !> Collection of tests
   type(unittest_type), allocatable, intent(out) :: testsuite(:)

   testsuite = [ &
      & new_unittest("spgemv-mchrg", test_spgemv_mcharge), &
      & new_unittest("spgemv-standard-csr", test_spgemv_csr), &
      & new_unittest("spgemv-standard-csr-complete-list", test_spgemv_csr_complete), &
      & new_unittest("spgemv-full-matrix", test_spgemv_fullmat), &
      & new_unittest("spmm-sparse-sparse", test_spmm_csr_sparse), &
      & new_unittest("spmm-sparse-dense", test_spmm_csr_dense), &
      & new_unittest("spmspv-sparse-vector", test_spmspv_csr) &
      & ]

end subroutine collect_csrlist_linal


subroutine test_spgemv_mcharge(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   real(wp), parameter :: mdiag(5) = [&
      1.95301539743829E+01_wp, 1.60040861485901E+01_wp, 1.60040861378386E+01_wp, &
      1.60040861396139E+01_wp, 1.60040861464410E+01_wp &
      ]
   real(wp), parameter :: mlist(15) = [&
      0.00000000000000E+00_wp, -1.40182420507753E+00_wp, -1.40182420319457E+00_wp, &
      -1.40182420372164E+00_wp, -1.40182420463048E+00_wp, 0.00000000000000E+00_wp, &
      -3.28540587512732E-01_wp, -3.28540587378075E-01_wp, -3.28540587649869E-01_wp, &
      0.00000000000000E+00_wp, -3.28540587252914E-01_wp, -3.28540587468433E-01_wp, &
      0.00000000000000E+00_wp, -3.28540587433195E-01_wp, 0.00000000000000E+00_wp &
      ]
   real(wp), parameter :: vec(5) = [&
      -1.32959314386409E-01_wp, +3.39873408224628E-02_wp, +3.39873400653214E-02_wp, &
      +3.39873402792960E-02_wp, +3.39873406420377E-02_wp &
      ]
   real(wp), parameter :: vrhs(5) = [&
      -2.78729298821854E+00_wp, 6.96823253402542E-01_wp, 6.96823240431084E-01_wp, &
      6.96823244062044E-01_wp, 6.96823250322879E-01_wp &
      ]
   integer(i8), parameter :: ptr(6) = [&
      1, 6, 10, &
      13, 15, 16 &
      ]
   integer, parameter :: cindx(15) = [&
      1, 2, 3, &
      4, 5, 2, &
      3, 4, 5, &
      3, 4, 5, &
      4, 5, 5 &
      ]

   type(csr_list), allocatable :: list

   real(wp), allocatable :: y(:)

   allocate(list)
   allocate(list%inl, source = ptr)
   allocate(list%nlat, source = cindx)

   allocate(y(size(vec)), source = 0.0_wp)
   call spsymv_csr(5, mlist, mdiag, list%inl, list%nlat, vec, y)

   if (any(abs(y - vrhs) > thr)) then
      call test_failed(error, "Multicharge version of the spgemv crashed.")
      print"(20a)", "Expected product:"
      print"(5es21.14)", vrhs
      print"(20a)", "Diff:"
      print"(5es21.14)", y - vrhs
   end if

end subroutine test_spgemv_mcharge

subroutine test_spgemv_csr(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   real(wp), parameter :: mlist(15) = [&
      1.95301539743829E+01_wp, -1.40182420507753E+00_wp, -1.40182420319457E+00_wp, &
      -1.40182420372164E+00_wp, -1.40182420463048E+00_wp, 1.60040861485901E+01_wp, &
      -3.28540587512732E-01_wp, -3.28540587378075E-01_wp, -3.28540587649869E-01_wp, &
      1.60040861378386E+01_wp, -3.28540587252914E-01_wp, -3.28540587468433E-01_wp, &
      1.60040861396139E+01_wp, -3.28540587433195E-01_wp, 1.60040861464410E+01_wp &
      ]
   real(wp), parameter :: vec(5) = [&
      -1.32959314386409E-01_wp, +3.39873408224628E-02_wp, +3.39873400653214E-02_wp, &
      +3.39873402792960E-02_wp, +3.39873406420377E-02_wp &
      ]
   real(wp), parameter :: vrhs(5) = [&
      -2.78729298821854E+00_wp, 6.96823253402542E-01_wp, 6.96823240431084E-01_wp, &
      6.96823244062044E-01_wp, 6.96823250322879E-01_wp &
      ]
   integer(i8), parameter :: ptr(6) = [&
      1, 6, 10, &
      13, 15, 16 &
      ]
   integer, parameter :: cindx(15) = [&
      1, 2, 3, &
      4, 5, 2, &
      3, 4, 5, &
      3, 4, 5, &
      4, 5, 5 &
      ]

   type(csr_list), allocatable :: list

   real(wp), allocatable :: y(:)

   allocate(list)
   allocate(list%inl, source = ptr)
   allocate(list%nlat, source = cindx)

   allocate(y(size(vec)), source = 0.0_wp)
   call spsymv_csr(5, mlist, list%inl, list%nlat, vec, y)

   if (any(abs(y - vrhs) > thr)) then
      call test_failed(error, "Standard CSR version of the spgemv crashed.")
      print"(20a)", "Expected product:"
      print"(5es21.14)", vrhs
      print"(20a)", "Diff:"
      print"(5es21.14)", y - vrhs
   end if

end subroutine test_spgemv_csr

subroutine test_spgemv_csr_complete(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   real(wp), parameter :: mlist(25) = [ &
      1.95301539743829E+01_wp, -1.40182420507753E+00_wp, -1.40182420319457E+00_wp, &
      -1.40182420372164E+00_wp, -1.40182420463048E+00_wp, 1.60040861485901E+01_wp, &
      -1.40182420507753E+00_wp, -3.28540587512732E-01_wp, -3.28540587378075E-01_wp, &
      -3.28540587649869E-01_wp, 1.60040861378386E+01_wp, -1.40182420319457E+00_wp, &
      -3.28540587512732E-01_wp, -3.28540587252914E-01_wp, -3.28540587468433E-01_wp, &
      1.60040861396139E+01_wp, -1.40182420372164E+00_wp, -3.28540587378075E-01_wp, &
      -3.28540587252914E-01_wp, -3.28540587433195E-01_wp, 1.60040861464410E+01_wp, &
      -1.40182420463048E+00_wp, -3.28540587649869E-01_wp, -3.28540587468433E-01_wp, &
      -3.28540587433195E-01_wp &
      ]
   real(wp), parameter :: vec(5) = [&
      -1.32959314386409E-01_wp, +3.39873408224628E-02_wp, +3.39873400653214E-02_wp, &
      +3.39873402792960E-02_wp, +3.39873406420377E-02_wp &
      ]
   real(wp), parameter :: vrhs(5) = [&
      -2.78729298821854E+00_wp, 6.96823253402542E-01_wp, 6.96823240431084E-01_wp, &
      6.96823244062044E-01_wp, 6.96823250322879E-01_wp &
      ]
   integer(i8), parameter :: ptr(6) = [&
      1, 6, 11, &
      16, 21, 26 &
      ]
   integer, parameter :: cindx(25) = [&
      1, 2, 3, &
      4, 5, 2, &
      1, 3, 4, &
      5, 3, 1, &
      2, 4, 5, &
      4, 1, 2, &
      3, 5, 5, &
      1, 2, 3, &
      4 &
      ]

   type(csr_list), allocatable :: list

   real(wp), allocatable :: y(:)

   allocate(list)
   allocate(list%inl, source = ptr)
   allocate(list%nlat, source = cindx)

   allocate(y(size(vec)), source = 0.0_wp)
   call spgemv_csr(5, mlist, list%inl, list%nlat, vec, y)

   if (any(abs(y - vrhs) > thr)) then
      call test_failed(error, "Full matrix version of the spgemv crashed.")
      print"(20a)", "Expected product:"
      print"(5es21.14)", vrhs
      print"(20a)", "Diff:"
      print"(5es21.14)", y - vrhs
      return
   end if

   ! The matrix is symmetric, the transposed product must agree
   call spgemv_csr(5, mlist, list%inl, list%nlat, vec, y, transa="T")

   if (any(abs(y - vrhs) > thr)) then
      call test_failed(error, "Transposed version of the spgemv crashed.")
      print"(20a)", "Diff:"
      print"(5es21.14)", y - vrhs
      return
   end if

   ! Symmetric products only reference one triangle of the complete list
   call spsymv_csr(5, mlist, list%inl, list%nlat, vec, y, uplo="L")

   if (any(abs(y - vrhs) > thr)) then
      call test_failed(error, "Lower triangle version of the SymMV crashed.")
      print"(20a)", "Diff:"
      print"(5es21.14)", y - vrhs
   end if

end subroutine test_spgemv_csr_complete

subroutine test_spgemv_fullmat(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   real(wp), parameter :: matrix(5,5) = reshape([ &
      1.95301539743829E+01_wp, -1.40182420507753E+00_wp, -1.40182420319457E+00_wp, &
      -1.40182420372164E+00_wp, -1.40182420463048E+00_wp, -1.40182420507753E+00_wp, &
      1.60040861485901E+01_wp, -3.28540587512732E-01_wp, -3.28540587378075E-01_wp, &
      -3.28540587649869E-01_wp, -1.40182420319457E+00_wp, -3.28540587512732E-01_wp, &
      1.60040861378386E+01_wp, -3.28540587252914E-01_wp, -3.28540587468433E-01_wp, &
      -1.40182420372164E+00_wp, -3.28540587378075E-01_wp, -3.28540587252914E-01_wp, &
      1.60040861396139E+01_wp, -3.28540587433195E-01_wp, -1.40182420463048E+00_wp, &
      -3.28540587649869E-01_wp, -3.28540587468433E-01_wp, -3.28540587433195E-01_wp, &
      1.60040861464410E+01_wp &
      ], [5,5])
   real(wp), parameter :: vec(5) = [&
      -1.32959314386409E-01_wp, +3.39873408224628E-02_wp, +3.39873400653214E-02_wp, &
      +3.39873402792960E-02_wp, +3.39873406420377E-02_wp &
      ]
   real(wp), parameter :: vrhs(5) = [&
      -2.78729298821854E+00_wp, 6.96823253402542E-01_wp, 6.96823240431084E-01_wp, &
      6.96823244062044E-01_wp, 6.96823250322879E-01_wp &
      ]
   integer(i8), parameter :: ptr(6) = [&
      1, 6, 11, &
      16, 21, 26 &
      ]
   integer, parameter :: cindx(25) = [&
      1, 2, 3, &
      4, 5, 2, &
      1, 3, 4, &
      5, 3, 1, &
      2, 4, 5, &
      4, 1, 2, &
      3, 5, 5, &
      1, 2, 3, &
      4 &
      ]

   type(csr_list), allocatable :: list

   real(wp), allocatable :: y(:)

   allocate(list)
   allocate(list%inl, source = ptr)
   allocate(list%nlat, source = cindx)

   allocate(y(size(vec)), source = 0.0_wp)
   call spgemv_csr(5, matrix, list%inl, list%nlat, vec, y)

   if (any(abs(y - vrhs) > thr)) then
      call test_failed(error, "Full matrix version of the spgemv crashed.")
      print"(20a)", "Expected product:"
      print"(5es21.14)", vrhs
      print"(20a)", "Diff:"
      print"(5es21.14)", y - vrhs
   end if

end subroutine test_spgemv_fullmat

!> Expand a complete CSR matrix into its dense representation
subroutine csr_to_dense(inl, nlat, mlist, dense)

   !> Offset index of the CSR rows
   integer(i8), intent(in) :: inl(:)

   !> Column indices in CSR order
   integer, intent(in) :: nlat(:)

   !> Matrix elements in CSR order
   real(wp), intent(in) :: mlist(:)

   !> Dense representation of the matrix
   real(wp), intent(out) :: dense(:, :)

   integer :: i, j
   integer(i8) :: k

   dense(:, :) = 0.0_wp
   do i = 1, size(inl) - 1
      do k = inl(i), inl(i+1) - 1
         j = nlat(k)
         dense(i, j) = dense(i, j) + mlist(k)
      end do
   end do

end subroutine csr_to_dense

!> Sparse-sparse product of two differently patterned complete CSR matrices
subroutine test_spmm_csr_sparse(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   integer, parameter :: n = 4
   integer(i8), parameter :: inla(5) = [1, 3, 6, 9, 11]
   integer, parameter :: nlata(10) = [1, 2, 2, 1, 3, 3, 2, 4, 4, 3]
   integer(i8), parameter :: inlb(5) = [1, 3, 4, 7, 9]
   integer, parameter :: nlatb(8) = [1, 3, 2, 3, 1, 4, 4, 2]

   integer :: i, info
   integer(i8) :: inlc(n+1)
   integer, allocatable :: nlatc(:)
   real(wp) :: alist(size(nlata)), blist(size(nlatb))
   real(wp), allocatable :: clist(:)
   real(wp) :: adns(n, n), bdns(n, n), cdns(n, n)

   do i = 1, size(alist)
      alist(i) = 0.5_wp*real(i, wp) - 1.25_wp
   end do
   do i = 1, size(blist)
      blist(i) = 1.0_wp/real(i + 1, wp)
   end do

   call csr_to_dense(inla, nlata, alist, adns)
   call csr_to_dense(inlb, nlatb, blist, bdns)

   ! Two-stage product, first the row offsets, then the elements of C
   allocate(nlatc(0), clist(0))
   call spmm_csr("N", 1, 0, n, n, n, alist, nlata, inla, blist, nlatb, inlb, &
      & clist, nlatc, inlc, 0_i8, info)
   deallocate(nlatc, clist)
   allocate(nlatc(inlc(n+1) - 1), clist(inlc(n+1) - 1))
   call spmm_csr("N", 2, 0, n, n, n, alist, nlata, inla, blist, nlatb, inlb, &
      & clist, nlatc, inlc, 0_i8, info)

   call check(error, info, 0)
   if (allocated(error)) return

   call csr_to_dense(inlc, nlatc, clist, cdns)
   if (any(abs(cdns - matmul(adns, bdns)) > thr)) then
      call test_failed(error, "Sparse-sparse SpMM does not match the dense product.")
      print"(20a)", "Diff:"
      print"(4es21.14)", cdns - matmul(adns, bdns)
      return
   end if

   ! Single-stage transposed product within the maximal number of elements
   deallocate(nlatc, clist)
   allocate(nlatc(n*n), clist(n*n))
   call spmm_csr("T", 0, 0, n, n, n, alist, nlata, inla, blist, nlatb, inlb, &
      & clist, nlatc, inlc, int(n*n, i8), info)

   call check(error, info, 0)
   if (allocated(error)) return

   call csr_to_dense(inlc, nlatc, clist, cdns)
   if (any(abs(cdns - matmul(transpose(adns), bdns)) > thr)) then
      call test_failed(error, "Transposed sparse-sparse SpMM does not match the dense product.")
      print"(20a)", "Diff:"
      print"(4es21.14)", cdns - matmul(transpose(adns), bdns)
      return
   end if

   ! Insufficient space is reported by the row exceeding nzmax
   call spmm_csr("N", 0, 0, n, n, n, alist, nlata, inla, blist, nlatb, inlb, &
      & clist, nlatc, inlc, 1_i8, info)
   call check(error, info, 1)

end subroutine test_spmm_csr_sparse

!> Sparse-dense product of a complete CSR matrix with a dense block
subroutine test_spmm_csr_dense(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   integer, parameter :: n = 4, m = 3, nrow = 5
   integer(i8), parameter :: inla(5) = [1, 3, 6, 9, 11]
   integer, parameter :: nlata(10) = [1, 2, 2, 1, 3, 3, 2, 4, 4, 3]
   real(wp), parameter :: alpha = -0.75_wp, beta = 3.0_wp

   integer :: i, j
   real(wp) :: alist(size(nlata))
   real(wp) :: adns(n, n), bmat(n, m), cmat(nrow, m), cref(nrow, m)

   do i = 1, size(alist)
      alist(i) = 0.5_wp*real(i, wp) - 1.25_wp
   end do

   do j = 1, m
      do i = 1, n
         bmat(i, j) = 1.0_wp/real(i + 2*j, wp)
      end do
      do i = 1, nrow
         cmat(i, j) = 0.25_wp*real(i - j, wp)
      end do
   end do

   call csr_to_dense(inla, nlata, alist, adns)
   cref(:, :) = cmat(:, :)
   cref(:n, :) = beta*cmat(:n, :) + alpha*matmul(adns, bmat)

   call spmm_csr("N", n, m, n, alpha, "G", alist, nlata, inla(:n), inla(2:), &
      & bmat, n, beta, cmat, nrow)

   if (any(abs(cmat - cref) > thr)) then
      call test_failed(error, "Sparse-dense SpMM does not match the dense product.")
      print"(20a)", "Expected product:"
      print"(3es21.14)", cref
      print"(20a)", "Diff:"
      print"(3es21.14)", cmat - cref
   end if

end subroutine test_spmm_csr_dense

!> Sparse matrix - sparse vector product against a complete CSR matrix
subroutine test_spmspv_csr(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   integer, parameter :: n = 4
   integer(i8), parameter :: inla(5) = [1, 3, 6, 9, 11]
   integer, parameter :: nlata(10) = [1, 2, 2, 1, 3, 3, 2, 4, 4, 3]
   integer, parameter :: xptr(2) = [2, 4]
   real(wp), parameter :: xval(2) = [0.6_wp, -1.3_wp]
   real(wp), parameter :: alpha = -0.75_wp

   type(csr_list) :: lista

   integer :: i, j
   real(wp) :: alist(size(nlata))
   real(wp) :: adns(n, n), xdense(n), yref(n), ydense(n)
   integer, allocatable :: yptr(:)
   real(wp), allocatable :: yval(:)

   do i = 1, size(alist)
      alist(i) = 0.5_wp*real(i, wp) - 1.25_wp
   end do

   call csr_to_dense(inla, nlata, alist, adns)

   xdense(:) = 0.0_wp
   do i = 1, size(xptr)
      xdense(xptr(i)) = xdense(xptr(i)) + xval(i)
   end do
   yref(:) = alpha*matmul(adns, xdense)

   allocate(lista%inl, source=inla)
   allocate(lista%nlat, source=nlata)

   call spmspv_csr(lista, alist, xptr, xval, yptr, yval, alpha=alpha)

   ydense(:) = 0.0_wp
   do i = 1, size(yptr)
      ydense(yptr(i)) = ydense(yptr(i)) + yval(i)
   end do

   if (any(abs(ydense - yref) > thr)) then
      call test_failed(error, &
         & "Sparse matrix - sparse vector product does not match the dense reference.")
      print"(20a)", "Expected product:"
      print"(4es21.14)", yref
      print"(20a)", "Diff:"
      print"(4es21.14)", ydense - yref
      return
   end if

   if (size(yptr) /= size(yval)) then
      call test_failed(error, &
         & "Sparse matrix - sparse vector product returned inconsistent index/value arrays.")
      return
   end if

   do i = 1, size(yptr)
      if (yval(i) == 0.0_wp) then
         call test_failed(error, &
            & "Sparse matrix - sparse vector product reported an explicit zero entry.")
         return
      end if
      do j = i + 1, size(yptr)
         if (yptr(i) == yptr(j)) then
            call test_failed(error, &
               & "Sparse matrix - sparse vector product reported a duplicate row index.")
            return
         end if
      end do
   end do

end subroutine test_spmspv_csr

end module test_csrlist_linal
