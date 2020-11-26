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


subroutine write_vasp(self, unit, comment_line)
   class(structure_type), intent(in) :: self
   integer, intent(in) :: unit
   character(len=*), intent(in), optional :: comment_line
   integer :: i, j, izp
   integer, allocatable :: kinds(:), species(:)
   real(wp), allocatable :: inv_lat(:, :)
   real(wp), allocatable :: abc(:, :)

   allocate(species(self%nat))
   allocate(kinds(self%nat), source=1)

   j = 0
   izp = 0
   do i = 1, self%nat
      if (izp.eq.self%id(i)) then
         kinds(j) = kinds(j)+1
      else
         j = j+1
         izp = self%id(i)
         species(j) = self%id(i)
      endif
   enddo

   ! use vasp 5.x format
   if (present(comment_line)) then
      write(unit, '(a)') comment_line
   else
      if (allocated(self%comment)) then
         write(unit, '(a)') self%comment
      else
         write(unit, '(a)')
      end if
   end if

   ! scaling factor for lattice parameters is always one
   write(unit, '(f20.14)') self%info%scale
   ! write the lattice parameters
   if (allocated(self%lattice)) then
      do i = 1, 3
         write(unit, '(3f20.14)') self%lattice(:, i)*autoaa/self%info%scale
      enddo
   else
      write(unit, '(3f20.14)') spread(0.0_wp, 1, 9)
   end if

   do i = 1, j
      write(unit, '(1x, a)', advance='no') self%sym(species(i))
   enddo
   write(unit, '(a)')

   ! write the count of the consequtive atom types
   do i = 1, j
      write(unit, '(1x, i0)', advance='no') kinds(i)
   enddo
   write(unit, '(a)')
   deallocate(kinds, species)

   if (self%info%selective) write(unit, '("Selective")')

   ! we write cartesian coordinates
   if (.not.allocated(self%lattice) .or. self%info%cartesian) then
      write(unit, '("Cartesian")')

      ! now write the cartesian coordinates
      do i = 1, self%nat
         write(unit, '(3f20.14)') self%xyz(:, i)*autoaa/self%info%scale
      enddo
   else
      write(unit, '("Direct")')
      inv_lat = matinv_3x3(self%lattice)
      abc = matmul(inv_lat, self%xyz)

      ! now write the fractional coordinates
      do i = 1, self%nat
         write(unit, '(3f20.14)') abc(:, i)
      enddo
   endif

end subroutine write_vasp


end module mctc_io_write_vasp
