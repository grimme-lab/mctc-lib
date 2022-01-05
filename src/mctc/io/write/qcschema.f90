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

module mctc_io_write_qcschema
   use mctc_env_accuracy, only : wp
   use mctc_io_structure, only : structure_type
   use mctc_version, only : get_mctc_version
   implicit none
   private

   public :: write_qcschema


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


subroutine write_qcschema(mol, unit)
   type(structure_type), intent(in) :: mol
   integer, intent(in) :: unit

   write(unit, '(a)') json_string(mol, "  ")
end subroutine write_qcschema

pure function json_string(mol, indent) result(string)
   type(structure_type), intent(in) :: mol
   character(len=*), intent(in), optional :: indent
   character(len=:), allocatable :: string

   string = "{"
   if (present(indent)) string = string // nl // indent
   string = string // json_key("schema_version", indent) // json_value(2)

   string = string // ","
   if (present(indent)) string = string // nl // indent
   string = string // json_key("schema_name", indent) // json_value("qcschema_molecule")

   string = string // ","
   block
      character(len=:), allocatable :: version
      call get_mctc_version(string=version)
      if (present(indent)) string = string // nl // indent
      string = string // json_key("provenance", indent) // "{"
      if (present(indent)) string = string // nl // indent // indent
      string = string // json_key("creator", indent) // json_value("mctc-lib")
      string = string // ","
      if (present(indent)) string = string // nl // indent // indent
      string = string // json_key("version", indent) // json_value(version)
      string = string // ","
      if (present(indent)) string = string // nl // indent // indent
      string = string // json_key("routine", indent) // &
         & json_value("mctc_io_write_qcschema::write_qcschema")
      if (present(indent)) string = string // nl // indent
      string = string // "}"
   end block

   if (allocated(mol%comment)) then
      string = string // ","
      if (present(indent)) string = string // nl // indent
      string = string // json_key("comment", indent) // json_value(mol%comment)
   end if

   string = string // ","
   if (present(indent)) string = string // nl // indent
   string = string // json_key("symbols", indent) // json_array(mol%sym(mol%id), indent)

   string = string // ","
   if (present(indent)) string = string // nl // indent
   string = string // json_key("atomic_numbers", indent) // &
      & json_array(mol%num(mol%id), indent)

   string = string // ","
   if (present(indent)) string = string // nl // indent
   string = string // json_key("geometry", indent) // json_array([mol%xyz], indent)

   string = string // ","
   if (present(indent)) string = string // nl // indent
   string = string // json_key("molecular_charge", indent) // json_value(nint(mol%charge))

   if (mol%uhf > 0) then
      string = string // ","
      if (present(indent)) string = string // nl // indent
      string = string // json_key("molecular_multiplicity", indent) // json_value(mol%uhf+1)
   end if

   if (allocated(mol%bond)) then
      string = string // ","
      if (present(indent)) string = string // nl // indent
      string = string // json_key("connectivity", indent) // "["
      block
         integer :: i
         do i = 1, mol%nbd
            if (present(indent)) string = string // nl // indent // indent
            string = string // "[" // json_value(mol%bond(1, i)-1) // "," // &
               & json_value(mol%bond(2, i)-1) // ","
            if (size(mol%bond, 1) > 2) then
               string = string // json_value(mol%bond(3, i)) // "]"
            else
               string = string // json_value(1) // "]"
            end if
            if (i /= mol%nbd) string = string // ","
         end do
      end block
      if (present(indent)) string = string // nl // indent
      string = string // "]"
   end if

   if (present(indent)) string = string // nl
   string = string // "}"
end function json_string

pure function json_array_int_1(array, indent) result(string)
   integer, intent(in) :: array(:)
   character(len=*), intent(in), optional :: indent
   character(len=:), allocatable :: string

   integer :: i, j

   string = "["
   do i = 1, size(array)
      if (present(indent)) string = string // nl // indent // indent
      string = string // json_value(array(i))
      if (i /= size(array)) string = string // ","
   end do
   if (present(indent)) string = string // nl // indent
   string = string // "]"
end function json_array_int_1

pure function json_array_real_1(array, indent) result(string)
   real(wp), intent(in) :: array(:)
   character(len=*), intent(in), optional :: indent
   character(len=:), allocatable :: string

   integer :: i, j

   string = "["
   do i = 1, size(array)
      if (present(indent)) string = string // nl // indent // indent
      string = string // json_value(array(i), '(es23.16)')
      if (i /= size(array)) string = string // ","
   end do
   if (present(indent)) string = string // nl // indent
   string = string // "]"
end function json_array_real_1

pure function json_array_char_1(array, indent) result(string)
   character(len=*), intent(in) :: array(:)
   character(len=*), intent(in), optional :: indent
   character(len=:), allocatable :: string

   integer :: i, j

   string = "["
   do i = 1, size(array)
      if (present(indent)) string = string // nl // indent // indent
      string = string // json_value(trim(array(i)))
      if (i /= size(array)) string = string // ","
   end do
   if (present(indent)) string = string // nl // indent
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


end module mctc_io_write_qcschema
