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

module mctc_io_write_cjson
   use mctc_env_accuracy, only : wp
   use mctc_io_constants, only : pi
   use mctc_io_convert, only : autoaa
   use mctc_io_math, only : matinv_3x3
   use mctc_io_structure, only : structure_type
   implicit none
   private

   public :: write_cjson


   interface json_value
      module procedure :: json_value_char
      module procedure :: json_value_int
      module procedure :: json_value_real
   end interface json_value

   interface json_array
      module procedure :: json_array_char_1
      module procedure :: json_array_int_1
      module procedure :: json_array_real_1
   end interface json_array

   character(len=*), parameter :: nl = new_line('a')

contains


subroutine write_cjson(mol, unit)
   type(structure_type), intent(in) :: mol
   integer, intent(in) :: unit

   write(unit, '(a)') json_string(mol, "  ")
end subroutine write_cjson

pure function json_string(mol, indent) result(string)
   type(structure_type), intent(in) :: mol
   character(len=*), intent(in), optional :: indent
   character(len=:), allocatable :: string

   real(wp), allocatable :: inv_lat(:, :)
   real(wp), allocatable :: abc(:, :)
   real(wp) :: cellpar(6)

   string = "{"
   if (present(indent)) string = string // nl // indent
   string = string // json_key("chemicalJson", indent) // json_value(1)

   if (allocated(mol%comment)) then
      string = string // ","
      if (present(indent)) string = string // nl // indent
      string = string // json_key("name", indent) // json_value(mol%comment)
   end if

   string = string // ","
   if (present(indent)) string = string // nl // indent
   string = string // json_key("atoms", indent) // "{"

   if (present(indent)) string = string // nl // repeat(indent, 2)
   string = string // json_key("elements", indent) // "{"
   if (present(indent)) string = string // nl // repeat(indent, 3)
   string = string // json_key("number", indent) // json_array(mol%num(mol%id), 3, indent)
   if (present(indent)) string = string // nl // repeat(indent, 2)
   string = string // "}"

   string = string // ","
   if (present(indent)) string = string // nl // repeat(indent, 2)
   string = string // json_key("coords", indent) // "{"
   if (present(indent)) string = string // nl // repeat(indent, 3)
   if (mol%info%cartesian) then
      string = string // json_key("3d", indent) // json_array([mol%xyz * autoaa], 3, indent)
   else
      inv_lat = matinv_3x3(mol%lattice)
      abc = matmul(inv_lat, mol%xyz)
      string = string // json_key("3dFractional", indent) // json_array([abc], 3, indent)
   end if
   if (present(indent)) string = string // nl // repeat(indent, 2)
   string = string // "}"

   if (present(indent)) string = string // nl // indent
   string = string // "}"

   string = string // ","
   if (present(indent)) string = string // nl // indent
   string = string // json_key("properties", indent) // "{"
   if (present(indent)) string = string // nl // repeat(indent, 2)
   string = string // json_key("totalCharge", indent) // json_value(nint(mol%charge))
   if (mol%uhf > 0) then
      string = string // ","
      if (present(indent)) string = string // nl // repeat(indent, 2)
      string = string // json_key("totalSpinMultiplicity", indent) // json_value(mol%uhf + 1)
   end if
   if (present(indent)) string = string // nl // indent
   string = string // "}"

   if (allocated(mol%bond)) then
      string = string // ","
      if (present(indent)) string = string // nl // indent
      string = string // json_key("bonds", indent) // "{"
      if (present(indent)) string = string // nl // repeat(indent, 2)
      string = string // json_key("connections", indent) // "{"
      if (present(indent)) string = string // nl // repeat(indent, 3)
      string = string // json_key("index", indent) // json_array([mol%bond(1:2, :) - 1], 3, indent)
      if (present(indent)) string = string // nl // repeat(indent, 2)
      string = string // "}"

      if (size(mol%bond, 1) > 2) then
         string = string // ","
         if (present(indent)) string = string // nl // repeat(indent, 2)
         string = string // json_key("order", indent) // json_array(mol%bond(3, :), 2, indent)
         if (present(indent)) string = string // nl // indent
      end if
      string = string // "}"
   end if

   if (any(mol%periodic)) then
      call dlat_to_cell(mol%lattice, cellpar)
      cellpar(1:3) = cellpar(1:3) * autoaa
      cellpar(4:6) = cellpar(4:6) * 180 / pi

      string = string // ","
      if (present(indent)) string = string // nl // indent
      string = string // json_key("unitCell", indent) // "{"
      if (present(indent)) string = string // nl // repeat(indent, 2)
      string = string // json_key("a", indent) // json_value(cellpar(1), "(es23.16)")
      string = string // ","
      if (present(indent)) string = string // nl // repeat(indent, 2)
      string = string // json_key("b", indent) // json_value(cellpar(2), "(es23.16)")
      string = string // ","
      if (present(indent)) string = string // nl // repeat(indent, 2)
      string = string // json_key("c", indent) // json_value(cellpar(3), "(es23.16)")
      string = string // ","
      if (present(indent)) string = string // nl // repeat(indent, 2)
      string = string // json_key("alpha", indent) // json_value(cellpar(4), "(es23.16)")
      string = string // ","
      if (present(indent)) string = string // nl // repeat(indent, 2)
      string = string // json_key("beta", indent) // json_value(cellpar(5), "(es23.16)")
      string = string // ","
      if (present(indent)) string = string // nl // repeat(indent, 2)
      string = string // json_key("gamma", indent) // json_value(cellpar(6), "(es23.16)")
      if (present(indent)) string = string // nl // indent
      string = string // "}"
   end if

   if (present(indent)) string = string // nl
   string = string // "}"
end function json_string

pure function json_array_int_1(array, depth, indent) result(string)
   integer, intent(in) :: array(:)
   integer, intent(in) :: depth
   character(len=*), intent(in), optional :: indent
   character(len=:), allocatable :: string

   integer :: i, j

   string = "["
   do i = 1, size(array)
      if (present(indent)) string = string // nl // repeat(indent, depth+1)
      string = string // json_value(array(i))
      if (i /= size(array)) string = string // ","
   end do
   if (present(indent)) string = string // nl // repeat(indent, depth)
   string = string // "]"
end function json_array_int_1

pure function json_array_real_1(array, depth, indent) result(string)
   real(wp), intent(in) :: array(:)
   integer, intent(in) :: depth
   character(len=*), intent(in), optional :: indent
   character(len=:), allocatable :: string

   integer :: i, j

   string = "["
   do i = 1, size(array)
      if (present(indent)) string = string // nl // repeat(indent, depth+1)
      string = string // json_value(array(i), '(es23.16)')
      if (i /= size(array)) string = string // ","
   end do
   if (present(indent)) string = string // nl // repeat(indent, depth)
   string = string // "]"
end function json_array_real_1

pure function json_array_char_1(array, depth, indent) result(string)
   character(len=*), intent(in) :: array(:)
   integer, intent(in) :: depth
   character(len=*), intent(in), optional :: indent
   character(len=:), allocatable :: string

   integer :: i, j

   string = "["
   do i = 1, size(array)
      if (present(indent)) string = string // nl // repeat(indent, depth+1)
      string = string // json_value(trim(array(i)))
      if (i /= size(array)) string = string // ","
   end do
   if (present(indent)) string = string // nl // repeat(indent, depth)
   string = string // "]"
end function json_array_char_1

pure function json_key(key, indent) result(string)
   character(len=*), intent(in) :: key
   character(len=*), intent(in), optional :: indent
   character(len=:), allocatable :: string

   if (present(indent)) then
      string = json_value(key) // ": "
   else
      string = json_value(key) // ":"
   end if
end function json_key

pure function json_value_char(val) result(string)
   character(len=*), intent(in) :: val
   character(len=:), allocatable :: string

   string = """" // val // """"
end function json_value_char

pure function json_value_real(val, format) result(str)
   real(wp), intent(in) :: val
   character(len=*), intent(in) :: format
   character(len=:), allocatable :: str

   character(len=128) :: buffer
   integer :: stat

   write(buffer, format, iostat=stat) val
   if (stat == 0) then
      str = trim(buffer)
   else
      str = """*"""
   end if
end function json_value_real

pure function json_value_int(val) result(string)
   integer, intent(in) :: val
   character(len=:), allocatable :: string
   integer, parameter :: buffer_len = range(val)+2
   character(len=buffer_len) :: buffer
   integer :: pos
   integer :: n
   character(len=1), parameter :: numbers(0:9) = &
      ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]

   if (val == 0) then
      string = numbers(0)
      return
   end if

   n = abs(val)
   buffer = ""

   pos = buffer_len + 1
   do while (n > 0)
      pos = pos - 1
      buffer(pos:pos) = numbers(mod(n, 10))
      n = n/10
   end do
   if (val < 0) then
      pos = pos - 1
      buffer(pos:pos) = '-'
   end if

   string = buffer(pos:)
end function json_value_int

!> Convert direct lattice to cell parameters
pure subroutine dlat_to_cell(lattice,cellpar)
   implicit none
   real(wp),intent(in)  :: lattice(3,3) !< direct lattice
   real(wp),intent(out) :: cellpar(6)   !< cell parameters

   associate( alen => cellpar(1), blen => cellpar(2), clen => cellpar(3), &
      &       alp  => cellpar(4), bet  => cellpar(5), gam  => cellpar(6) )

   alen = norm2(lattice(:,1))
   blen = norm2(lattice(:,2))
   clen = norm2(lattice(:,3))

   alp = acos(dot_product(lattice(:,2),lattice(:,3))/(blen*clen))
   bet = acos(dot_product(lattice(:,1),lattice(:,3))/(alen*clen))
   gam = acos(dot_product(lattice(:,1),lattice(:,2))/(alen*blen))

   end associate

end subroutine dlat_to_cell

end module mctc_io_write_cjson
