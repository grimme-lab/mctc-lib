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
   use mctc_env_accuracy, only : wp
   use mctc_io_convert, only : autoaa
   use mctc_io_structure, only : structure_type
   implicit none
   private

   public :: write_xyz, write_extxyz


contains


subroutine write_xyz(self, unit, comment_line)
   class(structure_type), intent(in) :: self
   integer, intent(in) :: unit
   character(len=*), intent(in), optional :: comment_line
   integer :: iat
   logical :: expo

   if (requires_extxyz(self)) then
      if (present(comment_line)) then
         call write_extxyz(self, unit, comment_line)
      else
         call write_extxyz(self, unit)
      end if
      return
   end if

   write(unit, "(i0)") self%nat
   if (present(comment_line)) then
      write(unit, "(a)") comment_line
   else
      if (allocated(self%comment)) then
         write(unit, "(a)") self%comment
      else
         write(unit, "(a)")
      end if
   end if
   expo = maxval(self%xyz) > 1.0e+5 .or. minval(self%xyz) < -1.0e+5
   if (expo) then
      do iat = 1, self%nat
         write(unit, "(a4, 1x, 3es24.14)") &
            & self%sym(self%id(iat)), self%xyz(:, iat)*autoaa
      end do
   else
      do iat = 1, self%nat
         write(unit, "(a4, 1x, 3f24.14)") &
            & self%sym(self%id(iat)), self%xyz(:, iat)*autoaa
      end do
   end if

end subroutine write_xyz


subroutine write_extxyz(self, unit, comment_line)
   class(structure_type), intent(in) :: self
   integer, intent(in) :: unit
   character(len=*), intent(in), optional :: comment_line

   integer :: i, j, iat
   real(wp) :: lattice(3, 3)
   logical :: periodic(3), expo, have_lattice
   character(len=:), allocatable :: comment

   lattice = 0.0_wp
   periodic = .false.
   have_lattice = .false.
   if (allocated(self%periodic)) then
      periodic(:min(3, size(self%periodic))) = self%periodic(:min(3, size(self%periodic)))
   end if
   if (allocated(self%lattice)) then
      have_lattice = size(self%lattice) > 0
      if (have_lattice) then
         lattice(:min(3, size(self%lattice, 1)), :min(3, size(self%lattice, 2))) = &
            & self%lattice(:min(3, size(self%lattice, 1)), :min(3, size(self%lattice, 2)))
      end if
   end if

   write(unit, "(i0)") self%nat
   if (have_lattice) then
      write(unit, "(a)", advance="no") 'Lattice="'
      do j = 1, 3
         do i = 1, 3
            if (i /= 1 .or. j /= 1) write(unit, "(1x)", advance="no")
            write(unit, "(es24.16)", advance="no") lattice(i, j) * autoaa
         end do
      end do
      write(unit, "(a)", advance="no") '" '
   end if

   write(unit, "(a)", advance="no") 'Properties=species:S:1:pos:R:3 pbc="'
   do i = 1, 3
      if (i > 1) write(unit, "(1x)", advance="no")
      write(unit, "(a1)", advance="no") merge("T", "F", periodic(i))
   end do
   write(unit, "(a)", advance="no") '"'

   if (present(comment_line)) then
      comment = comment_line
   else if (allocated(self%comment)) then
      comment = self%comment
   end if
   if (allocated(comment)) then
      write(unit, "(a)", advance="no") ' comment="'//escape_string(comment)//'"'
   end if
   write(unit, "(a)")

   expo = maxval(self%xyz) > 1.0e+5 .or. minval(self%xyz) < -1.0e+5
   if (expo) then
      do iat = 1, self%nat
         write(unit, "(a4, 1x, 3es24.14)") &
            & self%sym(self%id(iat)), self%xyz(:, iat)*autoaa
      end do
   else
      do iat = 1, self%nat
         write(unit, "(a4, 1x, 3f24.14)") &
            & self%sym(self%id(iat)), self%xyz(:, iat)*autoaa
      end do
   end if

end subroutine write_extxyz


logical function requires_extxyz(self)
   class(structure_type), intent(in) :: self

   requires_extxyz = allocated(self%lattice) .and. size(self%lattice) > 0
   if (.not.requires_extxyz .and. allocated(self%periodic)) then
      requires_extxyz = any(self%periodic)
   end if

end function requires_extxyz


function escape_string(string) result(output)
   character(len=*), intent(in) :: string
   character(len=:), allocatable :: output

   integer :: i

   output = ""
   do i = 1, len(string)
      select case(string(i:i))
      case(achar(92))
         output = output // achar(92) // achar(92)
      case('"')
         output = output // achar(92) // '"'
      case(new_line("a"))
         output = output // achar(92) // "n"
      case default
         output = output // string(i:i)
      end select
   end do

end function escape_string


end module mctc_io_write_xyz
