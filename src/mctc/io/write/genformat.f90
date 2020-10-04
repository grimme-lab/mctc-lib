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

module mctc_io_write_genformat
   use mctc_env_accuracy, only : wp
   use mctc_io_convert, only : autoaa
   use mctc_io_math, only : matinv_3x3
   use mctc_io_symbols, only : to_symbol
   use mctc_io_structure, only : structure_type
   implicit none
   private

   public :: write_genformat


contains


subroutine write_genformat(mol, unit)
   class(structure_type), intent(in) :: mol
   integer, intent(in) :: unit
   integer :: iat, izp
   real(wp), parameter :: zero3(3) = 0.0_wp
   real(wp), allocatable :: inv_lat(:, :)
   real(wp), allocatable :: abc(:, :)

   write(unit, '(i0, 1x)', advance='no') mol%nat
   if (.not.any(mol%periodic)) then
      write(unit, '("C")') ! cluster
   else
      if (mol%info%cartesian) then
         write(unit, '("S")') ! supercell
      else
         write(unit, '("F")') ! fractional
      endif
   endif

   do izp = 1, mol%nid
      write(unit, '(1x, a)', advance='no') trim(mol%sym(izp))
   enddo
   write(unit, '(a)')

   if (.not.any(mol%periodic) .or. mol%info%cartesian) then
      ! now write the cartesian coordinates
      do iat = 1, mol%nat
         write(unit, '(2i5, 3es24.14)') iat, mol%id(iat), mol%xyz(:, iat)*autoaa
      enddo
   else
      inv_lat = matinv_3x3(mol%lattice)
      abc = matmul(inv_lat, mol%xyz)
      ! now write the fractional coordinates
      do iat = 1, mol%nat
         write(unit, '(2i5, 3es24.15)') iat, mol%id(iat), abc(:, iat)
      enddo
   endif

   if (any(mol%periodic)) then
      ! scaling factor for lattice parameters is always one
      write(unit, '(3f20.14)') zero3
      ! write the lattice parameters
      write(unit, '(3f20.14)') mol%lattice(:, :)*autoaa
   endif

end subroutine write_genformat


end module mctc_io_write_genformat
