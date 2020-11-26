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

module mctc_io_write_xyz
   use mctc_io_convert, only : autoaa
   use mctc_io_structure, only : structure_type
   implicit none
   private

   public :: write_xyz


contains


subroutine write_xyz(self, unit, comment_line)
   class(structure_type), intent(in) :: self
   integer, intent(in) :: unit
   character(len=*), intent(in), optional :: comment_line
   integer :: iat
   logical :: expo

   write(unit, '(i0)') self%nat
   if (present(comment_line)) then
      write(unit, '(a)') comment_line
   else
      if (allocated(self%comment)) then
         write(unit, '(a)') self%comment
      else
         write(unit, '(a)')
      end if
   end if
   expo = maxval(self%xyz) > 1.0e+5 .or. minval(self%xyz) < -1.0e+5
   if (expo) then
      do iat = 1, self%nat
         write(unit, '(a4, 1x, 3es24.14)') &
            & self%sym(self%id(iat)), self%xyz(:, iat)*autoaa
      enddo
   else
      do iat = 1, self%nat
         write(unit, '(a4, 1x, 3f24.14)') &
            & self%sym(self%id(iat)), self%xyz(:, iat)*autoaa
      enddo
   end if

end subroutine write_xyz


end module mctc_io_write_xyz
