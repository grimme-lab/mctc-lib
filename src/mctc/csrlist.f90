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

!> @dir mctc/csrlist
!> Contains type, generation routines, and basic sparse linear algebra procedures.

!> @file mctc/csrlist.f90
!> Reexports access to the CSR neighbour list components.

!> Environment module providing CSR-based neighbour list
!> and Linear Algebra procedures based on this saprsity structure.

module mctc_csrlist
   use mctc_csrlist_linal, only : spgemv_csr, spsymv_csr, spmm_csr, spmspv_csr
   use mctc_csrlist_type, only : csr_list, new_csr_list, compute_grid, get_linked_cell, &
      & grid_type
   implicit none
   public

end module mctc_csrlist
