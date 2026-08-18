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
      read_next_token, read_token, to_string
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
   integer :: species_col, z_col, pos_col, ncols
   real(wp) :: x, y, z, conv, lattice(3, 3)
   real(wp), allocatable :: xyz(:, :)
   logical :: extended, have_lattice, have_pbc, periodic(3)
   type(token_type) :: token, tsym, tnat
   character(len=symbol_length) :: chdum
   character(len=symbol_length), allocatable :: sym(:)
   character(len=:), allocatable :: line, comment, fline, properties, ext_comment

   conv = aatoau
   lnum = 0

   call next_line(unit, fline, pos, lnum, stat)
   call read_next_token(fline, pos, tnat, n, stat)
   if (stat /= 0) then
      call io_error(error, "Could not read number of atoms", &
         & fline, tnat, filename(unit), lnum, "expected integer value")
      return
   end if

   if (n<1) then
      call io_error(error, "Impossible number of atoms provided", &
         & fline, tnat, filename(unit), lnum, "expected positive integer value")
      return
   end if

   allocate(sym(n))
   allocate(xyz(3, n))

   ! next record is either a plain XYZ comment or an Extended XYZ header
   call next_line(unit, comment, pos, lnum, stat)
   if (stat /= 0) then
      call io_error(error, "Unexpected end of file", &
         & "", token_type(0, 0), filename(unit), lnum+1, "expected value")
      return
   end if

   call parse_extxyz_header(comment, extended, properties, lattice, have_lattice, &
      & periodic, have_pbc, ext_comment, stat)
   if (stat /= 0) then
      call fatal_error(error, "Could not parse Extended XYZ header in '"//filename(unit)//"'")
      return
   end if

   species_col = 0
   z_col = 0
   pos_col = 0
   ncols = 4
   if (extended) then
      call parse_properties(properties, species_col, z_col, pos_col, ncols, stat)
      if (stat /= 0) then
         call fatal_error(error, "Invalid Properties specification in Extended XYZ file '"// &
            & filename(unit)//"'")
         return
      end if
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

      if (extended) then
         call read_extxyz_atom(line, species_col, z_col, pos_col, ncols, &
            & chdum, x, y, z, token, stat)
         if (stat /= 0) then
            call io_error(error, "Could not parse atom data from Extended XYZ file", &
               & line, token, filename(unit), lnum, "unexpected value")
            return
         end if
         iat = to_number(chdum)
      else
         call next_token(line, pos, tsym)
         if (stat == 0) then
            call read_next_token(line, pos, token, x, stat)
         end if
         if (stat == 0) then
            call read_next_token(line, pos, token, y, stat)
         end if
         if (stat == 0) then
            call read_next_token(line, pos, token, z, stat)
         end if
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
      end if

      if (iat > 0) then
         ii = ii+1
         sym(ii) = trim(chdum)
         xyz(:, ii) = [x, y, z]*conv
      else
         if (extended) then
            call io_error(error, "Cannot map symbol to atomic number", &
               & line, token, filename(unit), lnum, "unknown element")
         else
            call io_error(error, "Cannot map symbol to atomic number", &
               & line, tsym, filename(unit), lnum, "unknown element")
         end if
         return
      end if
   end do

   if (ii /= n) then
      call io_error(error, "Atom number missmatch in xyz file", &
         & fline, tnat, filename(unit), 1, "found "//to_string(ii)//" atoms in input")
      return
   end if

   if (extended) then
      if (have_lattice) then
         call new(self, sym, xyz, lattice=lattice, periodic=periodic)
      else
         call new(self, sym, xyz, periodic=periodic)
      end if
      if (allocated(ext_comment)) self%comment = ext_comment
   else
      call new(self, sym, xyz)
      if (len(comment) > 0) self%comment = comment
   end if

end subroutine read_xyz


subroutine parse_extxyz_header(line, extended, properties, lattice, have_lattice, &
      & periodic, have_pbc, comment, stat)
   character(len=*), intent(in) :: line
   logical, intent(out) :: extended, have_lattice, have_pbc
   character(len=:), allocatable, intent(out) :: properties, comment
   real(wp), intent(out) :: lattice(3, 3)
   logical, intent(out) :: periodic(3)
   integer, intent(out) :: stat

   logical :: found
   character(len=:), allocatable :: value

   stat = 0
   extended = .false.
   have_lattice = .false.
   have_pbc = .false.
   lattice = 0.0_wp
   periodic = .false.

   call get_header_value(line, "Properties", properties, found, stat)
   if (stat /= 0 .or. .not.found) then
      stat = 0
      return
   end if
   extended = .true.

   call get_header_value(line, "Lattice", value, found, stat)
   if (stat /= 0) return
   if (found) then
      call parse_lattice(value, lattice, stat)
      if (stat /= 0) return
      have_lattice = .true.
      periodic = .true.
   end if

   call get_header_value(line, "pbc", value, found, stat)
   if (stat /= 0) return
   if (found) then
      call parse_pbc(value, periodic, stat)
      if (stat /= 0) return
      have_pbc = .true.
   end if

   call get_header_value(line, "comment", value, found, stat)
   if (stat /= 0) return
   if (found .and. len(value) > 0) comment = unescape_string(value)

end subroutine parse_extxyz_header


subroutine get_header_value(line, wanted, value, found, stat)
   character(len=*), intent(in) :: line, wanted
   character(len=:), allocatable, intent(out) :: value
   logical, intent(out) :: found
   integer, intent(out) :: stat

   integer :: i, j, first, last, depth, n
   character(len=:), allocatable :: key
   character(len=1) :: quote, open_char, close_char

   found = .false.
   stat = 0
   value = ""
   n = len_trim(line)
   i = 1

   do while(i <= n)
      do while(i <= n)
         if (.not.is_space(line(i:i))) exit
         i = i + 1
      end do
      if (i > n) exit

      if (line(i:i) == '"' .or. line(i:i) == "'") then
         quote = line(i:i)
         first = i + 1
         i = i + 1
         do while(i <= n)
            if (line(i:i) == quote) exit
            if (line(i:i) == achar(92) .and. i < n) i = i + 1
            i = i + 1
         end do
         if (i > n) then
            stat = 1
            return
         end if
         last = i - 1
         key = line(first:last)
         i = i + 1
      else
         first = i
         do while(i <= n)
            if (is_space(line(i:i)) .or. line(i:i) == "=") exit
            i = i + 1
         end do
         last = i - 1
         if (last < first) then
            i = i + 1
            cycle
         end if
         key = line(first:last)
      end if

      do while(i <= n)
         if (.not.is_space(line(i:i))) exit
         i = i + 1
      end do
      if (i > n) cycle
      if (line(i:i) /= "=") then
         do while(i <= n)
            if (is_space(line(i:i))) exit
            i = i + 1
         end do
         cycle
      end if
      i = i + 1
      do while(i <= n)
         if (.not.is_space(line(i:i))) exit
         i = i + 1
      end do
      if (i > n) then
         stat = 1
         return
      end if

      select case(line(i:i))
      case('"', "'")
         quote = line(i:i)
         first = i + 1
         i = i + 1
         do while(i <= n)
            if (line(i:i) == achar(92) .and. i < n) then
               i = i + 2
               cycle
            end if
            if (line(i:i) == quote) exit
            i = i + 1
         end do
         if (i > n) then
            stat = 1
            return
         end if
         last = i - 1
         i = i + 1

      case("[", "{")
         open_char = line(i:i)
         close_char = merge("]", "}", open_char == "[")
         first = i
         depth = 0
         quote = " "
         do while(i <= n)
            if (quote /= " ") then
               if (line(i:i) == achar(92) .and. i < n) then
                  i = i + 2
                  cycle
               else if (line(i:i) == quote) then
                  quote = " "
               end if
            else
               if (line(i:i) == '"' .or. line(i:i) == "'") then
                  quote = line(i:i)
               else if (line(i:i) == open_char) then
                  depth = depth + 1
               else if (line(i:i) == close_char) then
                  depth = depth - 1
                  if (depth == 0) exit
               end if
            end if
            i = i + 1
         end do
         if (i > n .or. depth /= 0) then
            stat = 1
            return
         end if
         last = i
         i = i + 1

      case default
         first = i
         do while(i <= n)
            if (is_space(line(i:i))) exit
            i = i + 1
         end do
         last = i - 1
      end select

      if (key == wanted) then
         if (last >= first) value = line(first:last)
         found = .true.
         return
      end if
   end do

end subroutine get_header_value


subroutine parse_lattice(value, lattice, stat)
   character(len=*), intent(in) :: value
   real(wp), intent(out) :: lattice(3, 3)
   integer, intent(out) :: stat

   character(len=:), allocatable :: buffer
   real(wp) :: vec(3), vals(9)
   integer :: i

   buffer = value
   do i = 1, len(buffer)
      select case(buffer(i:i))
      case("[", "]", "{", "}", ",")
         buffer(i:i) = " "
      case default
         continue
      end select
   end do

   read(buffer, *, iostat=stat) vals
   if (stat == 0) then
      lattice = reshape(vals, [3, 3]) * aatoau
      return
   end if

   read(buffer, *, iostat=stat) vec
   if (stat == 0) then
      lattice = 0.0_wp
      do i = 1, 3
         lattice(i, i) = vec(i) * aatoau
      end do
   end if

end subroutine parse_lattice


subroutine parse_pbc(value, periodic, stat)
   character(len=*), intent(in) :: value
   logical, intent(out) :: periodic(3)
   integer, intent(out) :: stat

   character(len=:), allocatable :: buffer
   integer :: i

   buffer = value
   do i = 1, len(buffer)
      select case(buffer(i:i))
      case("[", "]", "{", "}", ",")
         buffer(i:i) = " "
      case default
         continue
      end select
   end do
   read(buffer, *, iostat=stat) periodic
   if (stat /= 0) return

end subroutine parse_pbc


subroutine parse_properties(properties, species_col, z_col, pos_col, ncols, stat)
   character(len=*), intent(in) :: properties
   integer, intent(out) :: species_col, z_col, pos_col, ncols, stat

   integer :: cursor, col, count
   character(len=:), allocatable :: name, kind, count_string
   logical :: ok

   species_col = 0
   z_col = 0
   pos_col = 0
   ncols = 0
   stat = 0
   cursor = 1
   col = 1

   do while(cursor <= len(properties))
      call next_property_token(properties, cursor, name, ok)
      if (.not.ok) then
         stat = 1
         return
      end if
      call next_property_token(properties, cursor, kind, ok)
      if (.not.ok) then
         stat = 1
         return
      end if
      call next_property_token(properties, cursor, count_string, ok)
      if (.not.ok) then
         stat = 1
         return
      end if
      read(count_string, *, iostat=stat) count
      if (stat /= 0 .or. count < 1 .or. len(kind) /= 1) then
         stat = 1
         return
      end if

      select case(name)
      case("species")
         if (kind /= "S" .or. count /= 1) then
            stat = 1
            return
         end if
         species_col = col
      case("Z")
         if (kind /= "I" .or. count /= 1) then
            stat = 1
            return
         end if
         z_col = col
      case("pos")
         if (kind /= "R" .or. count /= 3) then
            stat = 1
            return
         end if
         pos_col = col
      case default
         continue
      end select

      col = col + count
   end do

   ncols = col - 1
   if (pos_col == 0 .or. (species_col == 0 .and. z_col == 0)) stat = 1

end subroutine parse_properties


subroutine next_property_token(string, cursor, token, ok)
   character(len=*), intent(in) :: string
   integer, intent(inout) :: cursor
   character(len=:), allocatable, intent(out) :: token
   logical, intent(out) :: ok

   integer :: first, last, n

   n = len(string)
   if (cursor > n) then
      token = ""
      ok = .false.
      return
   end if

   first = cursor
   last = index(string(first:), ":")
   if (last == 0) then
      token = string(first:)
      cursor = n + 1
   else
      last = first + last - 2
      token = string(first:last)
      cursor = last + 2
   end if
   ok = len(token) > 0

end subroutine next_property_token


subroutine read_extxyz_atom(line, species_col, z_col, pos_col, ncols, symbol, &
      & x, y, z, token, stat)
   character(len=*), intent(in) :: line
   integer, intent(in) :: species_col, z_col, pos_col, ncols
   character(len=symbol_length), intent(out) :: symbol
   real(wp), intent(out) :: x, y, z
   type(token_type), intent(out) :: token
   integer, intent(out) :: stat

   integer :: pos, col, atomic_number
   real(wp) :: coord(3)
   type(token_type) :: current

   pos = 0
   stat = 0
   atomic_number = 0
   symbol = ""
   coord = 0.0_wp
   token = token_type(1, 1)

   do col = 1, ncols
      call next_token(line, pos, current)
      token = current
      if (current%first > len_trim(line)) then
         stat = 1
         return
      end if

      if (col == species_col) then
         current%last = min(current%last, current%first + symbol_length - 1)
         symbol = line(current%first:current%last)
      else if (col == z_col) then
         call read_token(line, current, atomic_number, stat)
         if (stat /= 0) return
      else if (col >= pos_col .and. col < pos_col + 3) then
         call read_token(line, current, coord(col-pos_col+1), stat)
         if (stat /= 0) return
      end if
   end do

   call next_token(line, pos, current)
   if (current%first <= len_trim(line)) then
      token = current
      stat = 1
      return
   end if

   if (species_col == 0) then
      if (atomic_number <= 0) then
         stat = 1
         return
      end if
      symbol = to_symbol(atomic_number)
   end if

   x = coord(1)
   y = coord(2)
   z = coord(3)

end subroutine read_extxyz_atom


pure function is_space(char) result(space)
   character(len=1), intent(in) :: char
   logical :: space

   space = char == " " .or. char == achar(9)

end function is_space


function unescape_string(string) result(output)
   character(len=*), intent(in) :: string
   character(len=:), allocatable :: output

   integer :: i

   output = ""
   i = 1
   do while(i <= len(string))
      if (string(i:i) == achar(92) .and. i < len(string)) then
         select case(string(i+1:i+1))
         case("n")
            output = output // new_line("a")
         case default
            output = output // string(i+1:i+1)
         end select
         i = i + 2
      else
         output = output // string(i:i)
         i = i + 1
      end if
   end do

end function unescape_string


end module mctc_io_read_xyz
