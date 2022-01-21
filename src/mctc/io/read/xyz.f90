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

module mctc_io_read_xyz
   use mctc_env_accuracy, only : wp
   use mctc_env_error, only : error_type, fatal_error
   use mctc_io_convert, only : aatoau
   use mctc_io_structure, only : structure_type, new
   use mctc_io_symbols, only : to_number, to_symbol, symbol_length
   use mctc_io_utils, only : next_line, token_type, next_token, io_error, filename, &
      read_next_token, to_string
   implicit none
   private

   public :: read_xyz


contains


subroutine read_xyz(self, unit, error)

   !> Instance of the molecular structure data
   type(structure_type), intent(out) :: self

   !> File handle
   integer, intent(in) :: unit

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   integer :: ii, n, iat, stat, pos, lnum
   real(wp) :: x, y, z, conv
   real(wp), allocatable :: xyz(:, :)
   type(token_type) :: token, tsym, tnat
   character(len=symbol_length) :: chdum
   character(len=symbol_length), allocatable :: sym(:)
   character(len=:), allocatable :: line, comment, fline

   conv = aatoau
   lnum = 0

   call next_line(unit, fline, pos, lnum, stat)
   call read_next_token(fline, pos, tnat, n, stat)
   if (stat /= 0) then
      call io_error(error, "Could not read number of atoms", &
         & fline, tnat, filename(unit), lnum, "expected integer value")
      return
   end if

   if (n.lt.1) then
      call io_error(error, "Impossible number of atoms provided", &
         & fline, tnat, filename(unit), lnum, "expected positive integer value")
      return
   end if

   allocate(sym(n))
   allocate(xyz(3, n))

   ! next record is a comment
   call next_line(unit, comment, pos, lnum, stat)
   if (stat /= 0) then
      call io_error(error, "Unexpected end of file", &
         & "", token_type(0, 0), filename(unit), lnum+1, "expected value")
      return
   end if

   ii = 0
   do while (ii < n)
      call next_line(unit, line, pos, lnum, stat)
      if (is_iostat_end(stat)) exit
      if (stat /= 0) then
         call io_error(error, "Could not read geometry from xyz file", &
            & "", token_type(0, 0), filename(unit), lnum+1, "expected value")
         return
      end if
      call next_token(line, pos, tsym)
      if (stat == 0) &
         call read_next_token(line, pos, token, x, stat)
      if (stat == 0) &
         call read_next_token(line, pos, token, y, stat)
      if (stat == 0) &
         call read_next_token(line, pos, token, z, stat)
      if (stat /= 0) then
         call io_error(error, "Could not parse coordinates from xyz file", &
            & line, token, filename(unit), lnum, "expected real value")
         return
      end if

      ! Adjust the token length to faithfully report the used chars in case of an error
      tsym%last = min(tsym%last, tsym%first + symbol_length - 1)
      chdum = line(tsym%first:tsym%last)
      iat = to_number(chdum)
      if (iat <= 0) then
         read(chdum, *, iostat=stat) iat
         if (stat == 0) then
            chdum = to_symbol(iat)
         else
            iat = 0
         end if
      end if
      if (iat > 0) then
         ii = ii+1
         sym(ii) = trim(chdum)
         xyz(:, ii) = [x, y, z]*conv
      else
         call io_error(error, "Cannot map symbol to atomic number", &
            & line, tsym, filename(unit), lnum, "unknown element")
         return
      end if
   end do

   if (ii /= n) then
      call io_error(error, "Atom number missmatch in xyz file", &
         & fline, tnat, filename(unit), 1, "found "//to_string(ii)//" atoms in input")
      return
   end if

   call new(self, sym, xyz)
   if (len(comment) > 0) self%comment = comment

end subroutine read_xyz


end module mctc_io_read_xyz
