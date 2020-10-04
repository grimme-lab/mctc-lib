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

module mctc_io_write_vasp
   use mctc_env_accuracy, only : wp
   use mctc_io_convert, only : autoaa
   use mctc_io_math, only : matinv_3x3
   use mctc_io_structure, only : structure_type
   implicit none
   private

   public :: write_vasp


contains


subroutine write_vasp(mol, unit, comment_line)
   class(structure_type), intent(in) :: mol
   integer, intent(in) :: unit
   character(len=*), intent(in), optional :: comment_line
   integer :: i, j, izp
   integer, allocatable :: kinds(:), species(:)
   real(wp), allocatable :: inv_lat(:, :)
   real(wp), allocatable :: abc(:, :)

   allocate(species(mol%nat))
   allocate(kinds(mol%nat), source=1)

   j = 0
   izp = 0
   do i = 1, mol%nat
      if (izp.eq.mol%id(i)) then
         kinds(j) = kinds(j)+1
      else
         j = j+1
         izp = mol%id(i)
         species(j) = mol%id(i)
      endif
   enddo

   ! use vasp 5.x format
   if (present(comment_line)) then
      write(unit, '(a)') comment_line
   else
      do i = 1, j
         write(unit, '(1x, a)', advance='no') mol%sym(species(i))
      enddo
      write(unit, '(a)')
   end if

   ! scaling factor for lattice parameters is always one
   write(unit, '(f20.14)') mol%info%scale
   ! write the lattice parameters
   if (allocated(mol%lattice)) then
      do i = 1, 3
         write(unit, '(3f20.14)') mol%lattice(:, i)*autoaa/mol%info%scale
      enddo
   else
      write(unit, '(3f20.14)') spread(0.0_wp, 1, 9)
   end if

   if (present(comment_line)) then
      do i = 1, j
         write(unit, '(1x, a)', advance='no') mol%sym(species(i))
      enddo
      write(unit, '(a)')
   end if

   ! write the count of the consequtive atom types
   do i = 1, j
      write(unit, '(1x, i0)', advance='no') kinds(i)
   enddo
   write(unit, '(a)')
   deallocate(kinds, species)

   if (mol%info%selective) write(unit, '("Selective")')

   ! we write cartesian coordinates
   if (.not.allocated(mol%lattice) .or. mol%info%cartesian) then
      write(unit, '("Cartesian")')

      ! now write the cartesian coordinates
      do i = 1, mol%nat
         write(unit, '(3f20.14)') mol%xyz(:, i)*autoaa/mol%info%scale
      enddo
   else
      write(unit, '("Direct")')
      inv_lat = matinv_3x3(mol%lattice)
      abc = matmul(inv_lat, mol%xyz)

      ! now write the fractional coordinates
      do i = 1, mol%nat
         write(unit, '(3f20.14)') abc(:, i)
      enddo
   endif

end subroutine write_vasp


end module mctc_io_write_vasp
