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

module mctc_io_write_pymatgen
   use mctc_env_accuracy, only : wp
   use mctc_io_convert, only : autoaa
   use mctc_io_math, only : matinv_3x3, matdet_3x3
   use mctc_io_structure, only : structure_type
   use mctc_version, only : get_mctc_version
   implicit none
   private

   public :: write_pymatgen


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


subroutine write_pymatgen(mol, unit)
   type(structure_type), intent(in) :: mol
   integer, intent(in) :: unit

   write(unit, '(a)') json_string(mol, "  ")
end subroutine write_pymatgen

pure function json_string(mol, indent) result(string)
   type(structure_type), intent(in) :: mol
   character(len=*), intent(in), optional :: indent
   character(len=:), allocatable :: string
   integer :: iat, isp, ilt
   real(wp) :: cellpar(6), volume, vec(3)
   real(wp), allocatable :: invlat(:, :)

   string = "{"
   if (present(indent)) string = string // nl // indent
   string = string // json_key("@module", indent) // json_value("pymatgen.core.structure")

   string = string // ","
   if (present(indent)) string = string // nl // indent
   if (any(mol%periodic)) then
      string = string // json_key("@class", indent) // json_value("Structure")
      invlat = matinv_3x3(mol%lattice)
   else
      string = string // json_key("@class", indent) // json_value("Molecule")
      string = string // ","

      if (present(indent)) string = string // nl // indent
      string = string // json_key("charge", indent) // json_value(nint(mol%charge))
      string = string // ","

      if (present(indent)) string = string // nl // indent
      string = string // json_key("spin_multiplicity", indent) // json_value(mol%uhf + 1)
   end if

   string = string // ","

   if (present(indent)) string = string // nl // indent
   string = string // json_key("sites", indent) // "["

   do iat = 1, mol%nat
      isp = mol%id(iat)
      if (present(indent)) string = string // nl // indent // indent
      string = string // "{"

      if (present(indent)) string = string // nl // indent // indent // indent
      string = string // json_key("name", indent) // json_value(trim(mol%sym(isp)))

      string = string // ","
      if (present(indent)) string = string // " "
      string = string // json_key("label", indent) // json_value(trim(mol%sym(isp)))

      string = string // ","
      if (present(indent)) string = string // " "
      string = string // json_key("species", indent) // "[{"
      string = string // json_key("element", indent) // json_value(trim(mol%sym(isp))) // ","
      if (present(indent)) string = string // " "
      string = string // json_key("occu", indent) // json_value(1)
      string = string // "}]"

      string = string // ","
      if (present(indent)) string = string // nl // indent // indent // indent
      string = string // json_key("xyz", indent) // &
         & "[" // json_value(mol%xyz(1, iat) * autoaa, '(es23.16)') // "," // &
         & json_value(mol%xyz(2, iat) * autoaa, '(es23.16)') // "," // &
         & json_value(mol%xyz(3, iat) * autoaa, '(es23.16)') // "]"

      ! if (allocated(invlat)) then
      !    vec = matmul(invlat, mol%xyz(:, iat))
      !    if (present(indent)) string = string // nl // indent // indent // indent
      !    string = string // json_key("abc", indent) // &
      !       & "[" // json_value(vec(1), '(es23.16)') // "," // &
      !       & json_value(vec(2), '(es23.16)') // "," // &
      !       & json_value(vec(3), '(es23.16)') // "]"
      ! end if

      string = string // ","
      if (present(indent)) string = string // nl // indent // indent // indent
      string = string // json_key("properties", indent) // "{}"

      if (present(indent)) string = string // nl // indent // indent
      string = string // "}"
      if (iat /= mol%nat) then
         string = string // ","
      end if
   end do
   if (present(indent)) string = string // nl // indent
   string = string // "]"

   if (any(mol%periodic)) then
      call dlat_to_cell(mol%lattice, cellpar)
      volume = matdet_3x3(mol%lattice)
      string = string // ","
      if (present(indent)) string = string // nl // indent
      string = string // json_key("lattice", indent) // "{"
      if (present(indent)) string = string // nl // indent // indent
      string = string // json_key("matrix", indent) // "["
      if (present(indent)) string = string // nl // indent // indent // indent
      do ilt = 1, 3
         string = string // "[" // &
            & json_value(mol%lattice(1, ilt) * autoaa, '(es23.16)') // "," // &
            & json_value(mol%lattice(2, ilt) * autoaa, '(es23.16)') // "," // &
            & json_value(mol%lattice(3, ilt) * autoaa, '(es23.16)') // "]"
         if (ilt < 3) then
            string = string // ","
            if (present(indent)) string = string // nl // indent // indent // indent
         end if
      end do
      if (present(indent)) string = string // nl // indent // indent
      string = string // "],"
      if (present(indent)) string = string // nl // indent // indent
      string = string // json_key("a", indent) // json_value(cellpar(1) * autoaa, '(es23.16)') // ","
      if (present(indent)) string = string // nl // indent // indent
      string = string // json_key("b", indent) // json_value(cellpar(2) * autoaa, '(es23.16)') // ","
      if (present(indent)) string = string // nl // indent // indent
      string = string // json_key("c", indent) // json_value(cellpar(3) * autoaa, '(es23.16)') // ","
      if (present(indent)) string = string // nl // indent // indent
      string = string // json_key("alpha", indent) // json_value(cellpar(4), '(es23.16)') // ","
      if (present(indent)) string = string // nl // indent // indent
      string = string // json_key("beta", indent) // json_value(cellpar(5), '(es23.16)') // ","
      if (present(indent)) string = string // nl // indent // indent
      string = string // json_key("gamma", indent) // json_value(cellpar(6), '(es23.16)') // ","
      if (present(indent)) string = string // nl // indent // indent
      string = string // json_key("volume", indent) // json_value(volume * autoaa**3, '(es23.16)')
      if (present(indent)) string = string // nl // indent
      string = string // "}"
   end if

   string = string // ","
   if (present(indent)) string = string // nl // indent
   string = string // json_key("properties", indent) // "{}"

   if (present(indent)) string = string // nl
   string = string // "}"
end function json_string

pure function json_array_int_1(array, indent) result(string)
   integer, intent(in) :: array(:)
   character(len=*), intent(in), optional :: indent
   character(len=:), allocatable :: string

   integer :: i

   string = "["
   do i = 1, size(array)
      if (present(indent)) string = string // nl // indent // indent
      string = string // json_value(array(i))
      if (i /= size(array)) string = string // ","
   end do
   if (present(indent)) string = string // nl // indent
   string = string // "]"
end function json_array_int_1

pure function json_array_real_1(array, indent, indent2, group) result(string)
   real(wp), intent(in) :: array(:)
   character(len=*), intent(in), optional :: indent
   character(len=*), intent(in), optional :: indent2
   integer, intent(in), optional :: group
   character(len=:), allocatable :: string

   integer :: i, j, step

   step = 1
   if (present(group)) step = group

   string = "["
   do i = 1, size(array), step
      if (present(indent2)) then
         string = string // nl // indent2
      else if (present(indent)) then
         string = string // nl // indent // indent
      end if
      do j = 1, step, 1
         string = string // json_value(array(i + j - 1), '(es23.16)')
         if (i + j - 1 /= size(array)) string = string // ","
      end do
   end do
   if (present(indent)) string = string // nl // indent
   string = string // "]"
end function json_array_real_1

pure function json_array_char_1(array, indent) result(string)
   character(len=*), intent(in) :: array(:)
   character(len=*), intent(in), optional :: indent
   character(len=:), allocatable :: string

   integer :: i

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

end module mctc_io_write_pymatgen

