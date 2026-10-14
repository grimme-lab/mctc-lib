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

!> Reader for crystallographic information files (CIF)
module mctc_io_read_cif
   use mctc_env_accuracy, only : wp
   use mctc_env_error, only : error_type, fatal_error
   use mctc_io_constants, only : pi
   use mctc_io_convert, only : aatoau
   use mctc_io_periodic, only : cell_to_dlat
   use mctc_io_resize, only : resize
   use mctc_io_structure, only : structure_type, new
   use mctc_io_symbols, only : to_number, to_symbol, symbol_length
   use mctc_io_utils, only : next_line, token_type, read_token, io_error, filename, &
      & to_lower
   implicit none
   private

   public :: read_cif


   !> Kinds of CIF tokens
   integer, parameter :: cif_eof = 0, cif_data = 1, cif_loop = 2, cif_tag = 3, &
      & cif_value = 4, cif_text = 5

   !> Kinds of CIF loops
   integer, parameter :: loop_ignore = 0, loop_atom = 1, loop_symop = 2

   !> Tolerance for identifying symmetry equivalent images in fractional coordinates
   real(wp), parameter :: symprec = 1.0e-3_wp

   !> Whitespace characters separating CIF tokens
   character(len=*), parameter :: blank = " "//achar(9)//achar(13)

   character(len=*), parameter :: letters = &
      & "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

   character(len=*), parameter :: cell_tags(6) = [character(len=17) :: &
      & "_cell_length_a", "_cell_length_b", "_cell_length_c", &
      & "_cell_angle_alpha", "_cell_angle_beta", "_cell_angle_gamma"]

   character(len=*), parameter :: symop_tags(2) = [character(len=32) :: &
      & "_space_group_symop_operation_xyz", "_symmetry_equiv_pos_as_xyz"]

   character(len=*), parameter :: hm_tags(2) = [character(len=30) :: &
      & "_symmetry_space_group_name_h-m", "_space_group_name_h-m_alt"]

   character(len=*), parameter :: itn_tags(2) = [character(len=27) :: &
      & "_space_group_it_number", "_symmetry_int_tables_number"]


contains


!> Read the first data block containing atom sites from a CIF file
subroutine read_cif(self, unit, error)

   !> Instance of the molecular structure data
   type(structure_type), intent(out) :: self

   !> File handle
   integer, intent(in) :: unit

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=:), allocatable :: line, block, hm, itn, tag, str, hmn, elem_line
   character(len=64), allocatable :: header(:)
   character(len=symbol_length), allocatable :: sym(:)
   type(token_type) :: token, elem_token
   integer :: kind, pos, lnum, i, isite, iop, nat, first
   integer :: ncol, icol, loop_kind, ilab, ityp, ix, iy, iz, iocc, idis, isym
   integer :: nop, nsite, elem_num, elem_lnum
   logical :: in_header, in_body, has_atoms, has_cell(6), elem_set, elem_from_type
   real(wp) :: cellpar(6), row(3), lattice(3, 3), g(3), d(3)
   real(wp), allocatable :: rot(:, :), trans(:, :), frac(:, :), fimg(:, :), xyz(:, :)
   integer, allocatable :: num(:)

   in_body = .false.
   allocate(character(len=0) :: line)
   allocate(header(16))
   pos = 0
   lnum = 0
   tag = ""
   call reset_block("")
   call end_loop

   do
      call next_cif_token(unit, line, pos, lnum, token, kind, error)
      if (allocated(error)) return
      select case(kind)
      case(cif_eof)
         call end_loop
         if (allocated(error)) return
         exit
      case(cif_data)
         call end_loop
         if (allocated(error)) return
         if (has_atoms) exit
         call reset_block(line(token%first+5:token%last))
      case(cif_loop)
         call end_loop
         if (allocated(error)) return
         tag = ""
         in_header = .true.
      case(cif_tag)
         if (in_header) then
            ncol = ncol + 1
            if (ncol > size(header)) call resize(header)
            header(ncol) = to_lower(line(token%first:token%last))
         else
            call end_loop
            if (allocated(error)) return
            tag = to_lower(line(token%first:token%last))
         end if
      case(cif_value, cif_text)
         if (kind == cif_text) then
            str = ""
         else
            str = line(token%first:token%last)
         end if
         if (in_header) then
            call start_loop
            if (allocated(error)) return
         end if
         if (in_body) then
            call loop_value
            if (allocated(error)) return
         else if (len(tag) > 0) then
            call item_value
            if (allocated(error)) return
            tag = ""
         end if
      case default
         call fatal_error(error, "Unknown CIF token kind")
         return
      end select
   end do

   if (.not.has_atoms) then
      call fatal_error(error, "No atom sites found in CIF input")
      return
   end if

   do i = 1, 3
      if (.not.has_cell(i)) then
         call fatal_error(error, "Missing '"//trim(cell_tags(i))//"' in CIF data block '"//&
            & block//"'")
         return
      end if
   end do

   if (nop == 0) then
      hmn = ""
      do i = 1, len(hm)
         if (hm(i:i) /= " ") hmn = hmn // to_lower(hm(i:i))
      end do
      if ((len(hmn) > 0 .and. hmn /= "p1") .or. (len(itn) > 0 .and. itn /= "1")) then
         if (len(hm) == 0) hm = itn
         call fatal_error(error, "Symmetry operations missing for space group '"//hm//"'")
         return
      end if
      nop = 1
      rot(:, 1) = [1, 0, 0, 0, 1, 0, 0, 0, 1]
      trans(:, 1) = 0.0_wp
   end if

   cellpar(1:3) = cellpar(1:3) * aatoau
   cellpar(4:6) = cellpar(4:6) * pi / 180.0_wp
   call cell_to_dlat(cellpar, lattice)

   allocate(fimg(3, nsite*nop), xyz(3, nsite*nop), sym(nsite*nop))
   nat = 0
   do isite = 1, nsite
      first = nat + 1
      do iop = 1, nop
         g = matmul(reshape(rot(:, iop), [3, 3]), frac(:, isite)) + trans(:, iop)
         do i = first, nat
            d = g - fimg(:, i)
            if (all(abs(d - nint(d)) < symprec)) exit
         end do
         if (i <= nat) cycle
         nat = nat + 1
         fimg(:, nat) = g
         xyz(:, nat) = matmul(lattice, g)
         sym(nat) = to_symbol(num(isite))
      end do
   end do

   call new(self, sym(:nat), xyz(:, :nat), lattice=lattice)
   if (len(block) > 0) self%comment = block

contains

   !> Start a new data block with default state
   subroutine reset_block(name)
      character(len=*), intent(in) :: name
      block = name
      cellpar(:) = [0.0_wp, 0.0_wp, 0.0_wp, 90.0_wp, 90.0_wp, 90.0_wp]
      has_cell(:) = .false.
      hm = ""
      itn = ""
      nop = 0
      nsite = 0
      has_atoms = .false.
      if (.not.allocated(rot)) allocate(rot(9, 8), trans(3, 8), frac(3, 64), num(64))
   end subroutine reset_block

   !> Close the current loop and check for incomplete atom site rows
   subroutine end_loop
      if (in_body .and. loop_kind == loop_atom .and. icol /= 0) then
         call fatal_error(error, "Incomplete row in CIF atom site loop")
         return
      end if
      in_header = .false.
      in_body = .false.
      loop_kind = loop_ignore
      ncol = 0
      icol = 0
      ilab = 0
      ityp = 0
      ix = 0
      iy = 0
      iz = 0
      iocc = 0
      idis = 0
      isym = 0
      elem_set = .false.
      elem_from_type = .false.
   end subroutine end_loop

   !> Finish the loop header and classify the loop by its data names
   subroutine start_loop
      in_header = .false.
      in_body = .true.
      icol = 0
      if (ncol == 0) then
         call fatal_error(error, "Loop without data names in CIF input")
         return
      end if
      ix = findloc(header(:ncol), "_atom_site_fract_x", 1)
      if (ix > 0) then
         iy = findloc(header(:ncol), "_atom_site_fract_y", 1)
         iz = findloc(header(:ncol), "_atom_site_fract_z", 1)
         ilab = findloc(header(:ncol), "_atom_site_label", 1)
         ityp = findloc(header(:ncol), "_atom_site_type_symbol", 1)
         iocc = findloc(header(:ncol), "_atom_site_occupancy", 1)
         idis = findloc(header(:ncol), "_atom_site_disorder_group", 1)
         if (iy == 0 .or. iz == 0 .or. (ilab == 0 .and. ityp == 0)) then
            call fatal_error(error, "Atom site loop requires '_atom_site_fract_x', "//&
               & "'_atom_site_fract_y', '_atom_site_fract_z' and a label or type symbol")
            return
         end if
         loop_kind = loop_atom
         has_atoms = .true.
      else if (findloc(header(:ncol), "_atom_site_cartn_x", 1) > 0) then
         call fatal_error(error, "Cartesian atom site coordinates are not supported in CIF input")
         return
      else
         isym = max(findloc(header(:ncol), symop_tags(1), 1), &
            & findloc(header(:ncol), symop_tags(2), 1))
         if (isym > 0) loop_kind = loop_symop
      end if
   end subroutine start_loop

   !> Process a value inside a loop body
   subroutine loop_value
      logical :: unknown
      integer :: stat
      real(wp) :: val
      unknown = kind == cif_value .and. (str == "?" .or. str == ".")
      icol = icol + 1
      select case(loop_kind)
      case(loop_atom)
         if (icol == ix .or. icol == iy .or. icol == iz) then
            call read_cif_real(line, token, val, stat)
            if (stat /= 0) then
               call io_error(error, "Cannot read fractional coordinate", line, token, &
                  & filename(unit), lnum, "expected real value")
               return
            end if
            if (icol == ix) row(1) = val
            if (icol == iy) row(2) = val
            if (icol == iz) row(3) = val
         end if
         if (icol == iocc .and. .not.unknown) then
            call read_cif_real(line, token, val, stat)
            if (stat /= 0) then
               call io_error(error, "Cannot read occupancy", line, token, &
                  & filename(unit), lnum, "expected real value")
               return
            end if
            if (abs(val - 1.0_wp) > epsilon(1.0_wp)) then
               call io_error(error, "Partial occupancy is not supported in CIF input", &
                  & line, token, filename(unit), lnum, "occupancy must be 1")
               return
            end if
         end if
         if (icol == idis .and. .not.unknown) then
            call io_error(error, "Disordered atom sites are not supported in CIF input", &
               & line, token, filename(unit), lnum, "disorder group must be '.' or '?'")
            return
         end if
         if (icol == ityp .and. .not.unknown) then
            call set_element
            elem_from_type = .true.
         end if
         if (icol == ilab .and. .not.elem_from_type) call set_element
         if (icol == ncol) then
            if (.not.elem_set) then
               call fatal_error(error, "Cannot determine element of atom site")
               return
            end if
            if (elem_num == 0) then
               call io_error(error, "Cannot determine element of atom site", elem_line, &
                  & elem_token, filename(unit), elem_lnum, "unknown element")
               return
            end if
            nsite = nsite + 1
            if (nsite > size(num)) then
               call resize(frac)
               call resize(num)
            end if
            frac(:, nsite) = row
            num(nsite) = elem_num
            icol = 0
            elem_set = .false.
            elem_from_type = .false.
         end if
      case(loop_symop)
         if (icol == isym) then
            call add_symop
            if (allocated(error)) return
         end if
         if (icol == ncol) icol = 0
      case default
         if (icol == ncol) icol = 0
      end select
   end subroutine loop_value

   !> Record the element of the current atom site from the current value
   subroutine set_element
      elem_num = cif_element(str)
      elem_line = line
      elem_token = token
      elem_lnum = lnum
      elem_set = .true.
   end subroutine set_element

   !> Process the value of a single data item
   subroutine item_value
      integer :: i, stat
      real(wp) :: val
      i = findloc(cell_tags, tag, 1)
      if (i > 0) then
         call read_cif_real(line, token, val, stat)
         if (stat /= 0) then
            call io_error(error, "Cannot read cell parameter", line, token, &
               & filename(unit), lnum, "expected real value")
            return
         end if
         cellpar(i) = val
         has_cell(i) = .true.
      else if (any(symop_tags == tag)) then
         call add_symop
      else if (any(hm_tags == tag)) then
         if (str /= "?" .and. str /= ".") hm = str
      else if (any(itn_tags == tag)) then
         if (str /= "?" .and. str /= ".") itn = str
      end if
   end subroutine item_value

   !> Parse the current value as symmetry operation and append it
   subroutine add_symop
      real(wp) :: r(3, 3), t(3)
      integer :: stat
      if (kind == cif_text) then
         stat = 1
      else
         call parse_symop(str, r, t, stat)
      end if
      if (stat /= 0) then
         call io_error(error, "Cannot read symmetry operation", line, token, &
            & filename(unit), lnum, "expected symmetry operation like 'x, y, z'")
         return
      end if
      nop = nop + 1
      if (nop > size(rot, 2)) then
         call resize(rot)
         call resize(trans)
      end if
      rot(:, nop) = reshape(r, [9])
      trans(:, nop) = t
   end subroutine add_symop

end subroutine read_cif


!> Advance to the next CIF token, skipping whitespace, comments and line breaks
subroutine next_cif_token(unit, line, pos, lnum, token, kind, error)

   !> File handle
   integer, intent(in) :: unit

   !> Current line
   character(len=:), allocatable, intent(inout) :: line

   !> Last consumed position in the current line
   integer, intent(inout) :: pos

   !> Current line number
   integer, intent(inout) :: lnum

   !> Token found, empty for text fields
   type(token_type), intent(out) :: token

   !> Kind of the token found
   integer, intent(out) :: kind

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   integer :: stat, first, last, k
   character(len=:), allocatable :: word

   token = token_type(0, -1)
   do
      if (pos >= len(line)) then
         call next_line(unit, line, pos, lnum, stat)
         if (stat /= 0) then
            kind = cif_eof
            return
         end if
         if (len(line) > 0) then
            if (line(1:1) == ";") then
               do
                  call next_line(unit, line, pos, lnum, stat)
                  if (stat /= 0) then
                     call fatal_error(error, "Unterminated text field in CIF input")
                     return
                  end if
                  if (len(line) > 0) then
                     if (line(1:1) == ";") exit
                  end if
               end do
               pos = 1
               kind = cif_text
               return
            end if
         end if
      end if

      first = verify(line(pos+1:), blank)
      if (first == 0) then
         pos = len(line)
         cycle
      end if
      first = pos + first
      if (line(first:first) == "#") then
         pos = len(line)
         cycle
      end if

      if (line(first:first) == "'" .or. line(first:first) == '"') then
         last = first
         do
            k = index(line(last+1:), line(first:first))
            if (k == 0) then
               call io_error(error, "Unterminated quoted string", line, &
                  & token_type(first, len(line)), filename(unit), lnum, "missing closing quote")
               return
            end if
            last = last + k
            if (last == len(line)) exit
            if (scan(line(last+1:last+1), blank) > 0) exit
         end do
         token = token_type(first+1, last-1)
         pos = last
         kind = cif_value
         return
      end if

      last = scan(line(first:), blank)
      if (last == 0) then
         last = len(line)
      else
         last = first + last - 2
      end if
      token = token_type(first, last)
      pos = last
      word = to_lower(line(first:last))
      if (word == "loop_") then
         kind = cif_loop
      else if (index(word, "data_") == 1) then
         kind = cif_data
      else if (word(1:1) == "_") then
         kind = cif_tag
      else
         kind = cif_value
      end if
      return
   end do

end subroutine next_cif_token


!> Read a real value, ignoring a trailing standard uncertainty like 1.234(5)
subroutine read_cif_real(line, token, val, stat)

   !> Current line
   character(len=*), intent(in) :: line

   !> Token containing the value
   type(token_type), intent(in) :: token

   !> Value read
   real(wp), intent(out) :: val

   !> Status of operation
   integer, intent(out) :: stat

   type(token_type) :: tok
   integer :: k

   val = 0.0_wp
   if (token%last < token%first) then
      stat = 1
      return
   end if
   tok = token
   k = index(line(tok%first:tok%last), "(")
   if (k > 1) tok%last = tok%first + k - 2
   call read_token(line, tok, val, stat)

end subroutine read_cif_real


!> Determine the element from a CIF type symbol or atom site label
!> like H1O, O-h1, Hg2+, CL1 or C1AA
pure function cif_element(str) result(num)

   !> Type symbol or label
   character(len=*), intent(in) :: str

   !> Atomic number, zero if unknown
   integer :: num

   num = 0
   if (len(str) < 1) return
   if (index(letters, str(1:1)) == 0) return
   if (len(str) >= 2) then
      if (index(letters, str(2:2)) > 0) num = to_number(str(1:2))
   end if
   if (num == 0) num = to_number(str(1:1))

end function cif_element


!> Parse a symmetry operation like '-x, 1/2+y, z-x' mapping fractional
!> coordinates f to matmul(rot, f) + trans
pure subroutine parse_symop(str, rot, trans, stat)

   !> Symmetry operation in xyz notation
   character(len=*), intent(in) :: str

   !> Rotational part
   real(wp), intent(out) :: rot(3, 3)

   !> Translational part
   real(wp), intent(out) :: trans(3)

   !> Status of operation
   integer, intent(out) :: stat

   character(len=*), parameter :: digits = "0123456789."
   integer :: i, j, k, ir
   real(wp) :: sgn, num, den

   rot(:, :) = 0.0_wp
   trans(:) = 0.0_wp
   ir = 1
   sgn = 1.0_wp
   stat = 0
   i = 0
   do while (i < len(str))
      i = i + 1
      select case(str(i:i))
      case(" ", achar(9))
         continue
      case(",")
         ir = ir + 1
         if (ir > 3) then
            stat = 1
            return
         end if
         sgn = 1.0_wp
      case("+")
         sgn = 1.0_wp
      case("-")
         sgn = -1.0_wp
      case("x", "y", "z", "X", "Y", "Z")
         j = mod(index("xyzXYZ", str(i:i)) - 1, 3) + 1
         rot(ir, j) = rot(ir, j) + sgn
         sgn = 1.0_wp
      case("0":"9", ".")
         j = verify(str(i:), digits)
         j = merge(len(str), i + j - 2, j == 0)
         read(str(i:j), *, iostat=stat) num
         if (stat /= 0) return
         i = j
         if (i < len(str)) then
            if (str(i+1:i+1) == "/") then
               j = verify(str(i+2:), digits)
               j = merge(len(str), i + j, j == 0)
               if (j < i + 2) then
                  stat = 1
                  return
               end if
               read(str(i+2:j), *, iostat=stat) den
               if (stat /= 0) return
               num = num / den
               i = j
            end if
         end if
         k = verify(str(i+1:), blank)
         j = 0
         if (k > 0) j = index("xyzXYZ", str(i+k:i+k))
         if (j > 0) then
            j = mod(j - 1, 3) + 1
            rot(ir, j) = rot(ir, j) + sgn * num
            i = i + k
         else
            trans(ir) = trans(ir) + sgn * num
         end if
         sgn = 1.0_wp
      case default
         stat = 1
         return
      end select
   end do
   stat = merge(0, 1, ir == 3)

end subroutine parse_symop


end module mctc_io_read_cif
