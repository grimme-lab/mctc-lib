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

module mctc_io_read_qchem
   use mctc_env_accuracy, only : wp
   use mctc_env_error, only : error_type
   use mctc_io_convert, only : aatoau
   use mctc_io_resize, only : resize
   use mctc_io_symbols, only : symbol_length, to_number, to_symbol
   use mctc_io_structure, only : structure_type, new
   use mctc_io_utils, only : next_line, token_type, next_token, io_error, filename, &
      read_next_token, read_token
   implicit none
   private

   public :: read_qchem

   integer, parameter :: initial_size = 64

contains

subroutine read_qchem(mol, unit, error)

   !> Instance of the molecular structure data
   type(structure_type), intent(out) :: mol

   !> File handle
   integer, intent(in) :: unit

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   integer :: stat, pos, lnum, izp, iat
   integer :: charge, multiplicity
   type(token_type) :: token
   character(len=:), allocatable :: line
   real(wp) :: x, y, z
   character(len=symbol_length), allocatable :: sym(:)
   real(wp), allocatable :: xyz(:, :), abc(:, :), lattice(:, :)
   logical :: is_frac, periodic(3)

   iat = 0
   lnum = 0
   stat = 0

   do while(stat == 0)
      call next_line(unit, line, pos, lnum, stat)
      if (stat /= 0) exit

      call next_token(line, pos, token)
      if (token%first > len(line)) cycle
      if (to_lower(line(token%first:token%last)) == '$molecule') exit
   end do

   if (stat /= 0) then
      call io_error(error, "No atoms found", &
         & line, token_type(0, 0), filename(unit), lnum+1, "expected molecule block")
      return
   end if

   call next_line(unit, line, pos, lnum, stat)
   if (stat == 0) &
      call read_next_token(line, pos, token, charge, stat)
   if (stat == 0) &
      call read_next_token(line, pos, token, multiplicity, stat)
   if (stat /= 0) then
      call io_error(error, "Failed to read charge and multiplicity", &
         & line, token, filename(unit), lnum, "expected integer value")
      return
   end if

   allocate(sym(initial_size), source=repeat(' ', symbol_length))
   allocate(xyz(3, initial_size), source=0.0_wp)

   do while(stat == 0)
      call next_line(unit, line, pos, lnum, stat)
      if (stat /= 0) exit

      call next_token(line, pos, token)
      if (to_lower(line(token%first:token%last)) == '$end') exit

      if (iat >= size(sym)) call resize(sym)
      if (iat >= size(xyz, 2)) call resize(xyz)
      iat = iat + 1

      token%last = min(token%last, token%first + symbol_length - 1)
      sym(iat) = line(token%first:token%last)
      if (to_number(sym(iat)) == 0) then
         call read_token(line, token, izp, stat)
         sym(iat) = to_symbol(izp)
      end if
      if (stat /= 0) then
         call io_error(error, "Cannot map symbol to atomic number", &
            & line, token, filename(unit), lnum, "unknown element")
         return
      end if

      call read_next_token(line, pos, token, x, stat)
      if (stat == 0) &
         call read_next_token(line, pos, token, y, stat)
      if (stat == 0) &
         call read_next_token(line, pos, token, z, stat)
      if (stat /= 0) then
         call io_error(error, "Cannot read coordinates", &
            & line, token, filename(unit), lnum, "expected real value")
         return
      end if

      xyz(:, iat) = [x, y, z] * aatoau
   end do

   if (stat /= 0) then
      call io_error(error, "Failed to read molecule block", &
         & line, token_type(0, 0), filename(unit), lnum, "unexpected end of input")
      return
   end if

   call new(mol, sym(:iat), xyz, charge=real(charge, wp), uhf=multiplicity-1)
end subroutine read_qchem

!> Convert input string to lowercase
elemental function to_lower(str) result(lcstr)

   !> Input string
   character(len=*), intent(in) :: str

   !> Lowercase version of string
   character(len=len(str)):: lcstr

   integer :: ilen, iquote, i, iav, iqc
   integer, parameter :: offset = iachar('A') - iachar('a')

   ilen = len(str)
   iquote = 0
   lcstr = str

   do i = 1, ilen
      iav = iachar(str(i:i))
      if (iquote == 0 .and. (iav == 34 .or.iav == 39)) then
         iquote = 1
         iqc = iav
         cycle
      end if
      if (iquote == 1 .and. iav==iqc) then
         iquote=0
         cycle
      end if
      if (iquote == 1) cycle
      if (iav >= iachar('A') .and. iav <= iachar('Z')) then
         lcstr(i:i) = achar(iav - offset)
      else
         lcstr(i:i) = str(i:i)
      end if
   end do

end function to_lower

end module mctc_io_read_qchem
