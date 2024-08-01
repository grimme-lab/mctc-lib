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

module mctc_io_read_ctfile
   use mctc_env_accuracy, only : wp
   use mctc_env_error, only : error_type, fatal_error
   use mctc_io_convert, only : aatoau
   use mctc_io_structure, only : structure_type, new
   use mctc_io_structure_info, only : sdf_data, structure_info
   use mctc_io_symbols, only : to_number, symbol_length
   use mctc_io_utils, only : next_line, token_type, next_token, io_error, filename, &
      read_token, read_next_token, to_string
   implicit none
   private

   public :: read_sdf, read_molfile


contains


subroutine read_sdf(self, unit, error)

   !> Instance of the molecular structure data
   type(structure_type), intent(out) :: self

   !> File handle
   integer, intent(in) :: unit

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=:), allocatable :: line
   integer :: stat, lnum, pos

   call read_molfile(self, unit, error)
   if (allocated(error)) return

   lnum = 0
   stat = 0
   do while(stat == 0)
      call next_line(unit, line, pos, lnum, stat)
      if (index(line, '$$$$') == 1) exit
   end do
   if (stat /= 0) then
      call fatal_error(error, "Failed while reading SDF key-value pairs")
      return
   end if

end subroutine read_sdf


subroutine read_molfile(self, unit, error)

   !> Instance of the molecular structure data
   type(structure_type), intent(out) :: self

   !> File handle
   integer, intent(in) :: unit

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=:), allocatable :: line
   character(len=:), allocatable :: comment
   integer :: stat, lnum, pos
   integer :: number_of_atoms, number_of_bonds
   integer :: list7(7), list12(12)
   real(wp) :: x, y, z
   character(len=2) :: sdf_dim
   logical :: two_dim, v3k
   type(token_type) :: token

   lnum = 0
   two_dim = .false.

   call next_line(unit, comment, pos, lnum, stat)
   call next_line(unit, line, pos, lnum, stat)
   read(line, '(20x, a2)', iostat=stat) sdf_dim
   if (stat == 0) then
      two_dim = sdf_dim == '2D' .or. sdf_dim == '2d'
   end if
   call next_line(unit, line, pos, lnum, stat)
   call next_line(unit, line, pos, lnum, stat)
   if (stat == 0) then
      token = token_type(1, 3)
      call read_token(line, token, number_of_atoms, stat)
   end if
   if (stat == 0) then
      token = token_type(4, 6)
      call read_token(line, token, number_of_bonds, stat)
   end if
   if (stat /= 0) then
      call io_error(error, "Cannot read header of molfile", &
         & line, token, filename(unit), lnum, "expected integer value")
      return
   end if
   token = token_type(35, 39)
   stat = 1
   if (len(line) >= 39) then
      v3k = line(35:39) == 'V3000'
      if (line(35:39) == 'V2000' .or. v3k) stat = 0
   end if

   if (stat /= 0) then
      call io_error(error, "Format version not supported", &
         & line, token, filename(unit), lnum, "invalid format version")
      return
   end if
   if (.not.v3k .and. number_of_atoms < 1) then
      call io_error(error, "Invalid number of atoms", &
         & line, token_type(1, 3), filename(unit), lnum, "expected positive integer")
      return
   end if

   if (v3k) then
      call read_molfile_v3k(self, unit, error)
   else
      call read_molfile_v2k(self, unit, number_of_atoms, number_of_bonds, error)
   end if
   if (allocated(error)) return

   ! Attach additional meta data
   self%info%two_dimensional = two_dim
   if (len(comment) > 0) self%comment = comment

end subroutine read_molfile


subroutine read_molfile_v2k(self, unit, number_of_atoms, number_of_bonds, error)

   !> Instance of the molecular structure data
   type(structure_type), intent(out) :: self

   !> File handle
   integer, intent(in) :: unit

   !> Number of atoms from header
   integer, intent(in) :: number_of_atoms

   !> Number of bonds from header
   integer, intent(in) :: number_of_bonds

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=:), allocatable :: line
   integer :: i, iatom, jatom, ibond, btype, atomtype
   integer :: stat, length, charge(2, 15), lnum, pos
   integer :: list7(7), list12(12)
   real(wp) :: x, y, z
   character(len=3) :: symbol
   integer, parameter :: ccc_to_charge(0:7) = [0, +3, +2, +1, 0, -1, -2, -3]
   type(token_type) :: token
   character(len=symbol_length), allocatable :: sym(:)
   type(sdf_data), allocatable :: sdf(:)
   type(structure_info) :: info
   real(wp), allocatable :: xyz(:, :)
   integer, allocatable :: bond(:, :)

   lnum = 4

   allocate(sdf(number_of_atoms))
   allocate(xyz(3, number_of_atoms))
   allocate(sym(number_of_atoms))

   do iatom = 1, number_of_atoms
      call next_line(unit, line, pos, lnum, stat)
      if (stat == 0) then
         token = token_type(1, 10)
         call read_token(line, token, x, stat)
      end if
      if (stat == 0) then
         token = token_type(11, 20)
         call read_token(line, token, y, stat)
      end if
      if (stat == 0) then
         token = token_type(21, 30)
         call read_token(line, token, z, stat)
      end if
      if (len(line) >= 34) then
         symbol = line(32:34)
      end if
      if (stat == 0) then
         token = token_type(35, 36)
         call read_token(line, token, list12(1), stat)
      end if
      list12(:) = 0
      do i = 1, 11
         if (stat == 0) then
            if ((36+i*3) > len(line)) exit
            token = token_type(34 + i*3, 36 + i*3)
            call read_token(line, token, list12(i+1), stat)
         end if
      end do
      if (stat /= 0) then
         call io_error(error, "Cannot read coordinates from connection table", &
            & line, token, filename(unit), lnum, "unexpected value")
         return
      end if
      atomtype = to_number(symbol)
      if (atomtype == 0) then
         call io_error(error, "Cannot map symbol to atomic number", &
            & line, token_type(32, 34), filename(unit), lnum, "unknown element")
         return
      end if
      xyz(:, iatom) = [x, y, z] * aatoau
      sym(iatom) = symbol
      sdf(iatom)%isotope = list12(1)
      sdf(iatom)%charge = ccc_to_charge(list12(2)) ! drop doublet radical
      sdf(iatom)%hydrogens = list12(4)
      sdf(iatom)%valence = list12(6)
   end do

   allocate(bond(3, number_of_bonds))
   do ibond = 1, number_of_bonds
      call next_line(unit, line, pos, lnum, stat)
      list7(:) = 0
      do i = 1, 7
         if (stat == 0) then
            if ((i*3) > len(line)) exit
            token = token_type(i*3 - 2, i*3)
            call read_token(line, token, list7(i), stat)
         end if
      end do
      if (stat /= 0) then
         call io_error(error, "Cannot read topology from connection table", &
            & line, token, filename(unit), lnum, "unexpected value")
         return
      end if
      iatom = list7(1)
      jatom = list7(2)
      btype = list7(3)
      bond(:, ibond) = [iatom, jatom, btype]
   end do

   do while(stat == 0)
      call next_line(unit, line, pos, lnum, stat)
      if (index(line, 'M  END') == 1) exit
      if (index(line, 'M  CHG') == 1) then
         token = token_type(7, 9)
         read(line(7:9), *) length
         call read_token(line, token, length, stat)
         if (stat == 0) then
            do i = 1, length
               if (stat /= 0) exit
               token = token_type(3 + i*8, 5 + i*8)
               call read_token(line, token, charge(1, i), stat)
               if (charge(1, i) > number_of_atoms .or. charge(1, i) < 1) stat = 1
               if (stat /= 0) exit
               token = token_type(7 + i*8, 9 + i*8)
               call read_token(line, token, charge(2, i), stat)
            end do
         end if
         if (stat /= 0) then
            call io_error(error, "Cannot read charges", &
               & line, token, filename(unit), lnum, "expected integer value")
            return
         end if
         do i = 1, length
            sdf(charge(1, i))%charge = charge(2, i)
         end do
      end if
   end do
   if (stat /= 0) then
      call fatal_error(error, "Cannot read connection table")
      return
   end if

   info = structure_info(missing_hydrogen=any(sdf%hydrogens > 1))
   call new(self, sym, xyz, charge=real(sum(sdf%charge), wp), info=info, bond=bond)
   call move_alloc(sdf, self%sdf)

end subroutine read_molfile_v2k


subroutine read_molfile_v3k(self, unit, error)

   !> Instance of the molecular structure data
   type(structure_type), intent(out) :: self

   !> File handle
   integer, intent(in) :: unit

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=:), allocatable :: line, group
   integer :: i, iatom, jatom, ibond, btype, atomtype, aamap, equal
   integer :: stat, charge(2, 15), lnum, pos, number_of_atoms, number_of_bonds, dummy
   real(wp) :: x, y, z
   character(len=3) :: symbol
   integer, parameter :: ccc_to_charge(0:7) = [0, +3, +2, +1, 0, -1, -2, -3]
   type(token_type) :: token, tsym
   character(len=symbol_length), allocatable :: sym(:)
   type(sdf_data), allocatable :: sdf(:)
   type(structure_info) :: info
   real(wp), allocatable :: xyz(:, :)
   integer, allocatable :: bond(:, :)

   lnum = 4

   call next_v30(unit, line, pos, lnum, stat)
   do while(stat == 0)
      call next_token(line, pos, token)
      if (slice(line, token%first, token%last) == 'BEGIN') then
         call next_token(line, pos, token)
         if (slice(line, token%first, token%last) == 'CTAB') exit
      end if
      call next_v30(unit, line, pos, lnum, stat)
   end do

   if (stat /= 0) then
      call io_error(error, "Cannot read connection table", &
         & line, token_type(0, 0), filename(unit), lnum, "CTAB header not found")
      return
   end if

   call next_v30(unit, line, pos, lnum, stat)
   if (stat == 0) then
      call next_token(line, pos, token)
      if (slice(line, token%first, token%last) /= 'COUNTS') then
         call io_error(error, "Cannot read connection table", &
            & line, token, filename(unit), lnum, "COUNTS header not found")
         return
      end if
   end if
   if (stat == 0) then
      call read_next_token(line, pos, token, number_of_atoms, stat)
      tsym = token
   end if
   if (stat == 0) &
      call read_next_token(line, pos, token, number_of_bonds, stat)
   if (stat == 0) &
      call read_next_token(line, pos, token, dummy, stat)
   if (stat == 0) &
      call read_next_token(line, pos, token, dummy, stat)
   if (stat == 0) &
      call read_next_token(line, pos, token, dummy, stat)
   if (stat /= 0) then
      call io_error(error, "Cannot read connection table counts", &
         & line, token, filename(unit), lnum, "expected integer value")
      return
   end if

   if (number_of_atoms < 1) then
      call io_error(error, "Invalid number of atoms", &
         & line, tsym, filename(unit), lnum, "expected positive integer")
      return
   end if

   allocate(sdf(number_of_atoms))
   allocate(xyz(3, number_of_atoms))
   allocate(sym(number_of_atoms))
   allocate(bond(3, number_of_bonds))

   call next_v30(unit, line, pos, lnum, stat)
   do while(stat == 0)
      call next_token(line, pos, token)
      if (slice(line, token%first, token%last) == 'END') exit
      if (slice(line, token%first, token%last) == 'BEGIN') then
         call next_token(line, pos, token)
         group = slice(line, token%first, token%last)
         select case(group)
         case("ATOM")
            do iatom = 1, number_of_atoms
               call next_v30(unit, line, pos, lnum, stat)
               if (stat == 0) &
                  call read_next_token(line, pos, token, dummy, stat)
               if (stat == 0) &
                  call next_token(line, pos, tsym)
               if (stat == 0) &
                  call read_next_token(line, pos, token, x, stat)
               if (stat == 0) &
                  call read_next_token(line, pos, token, y, stat)
               if (stat == 0) &
                  call read_next_token(line, pos, token, z, stat)
               if (stat == 0) &
                  call read_next_token(line, pos, token, aamap, stat)
               if (stat /= 0) then
                  call io_error(error, "Cannot read coordinates", &
                     & line, token, filename(unit), lnum, "unexpected value")
                  return
               end if

               if (aamap > 0) then
                  call io_error(error, "Mapping atoms is not supported", &
                     & line, token, filename(unit), lnum, "unsupported value")
                  return
               end if

               tsym%last = min(tsym%last, tsym%first + symbol_length - 1)
               sym(iatom) = slice(line, tsym%first, tsym%last)
               if (to_number(sym(iatom)) == 0) then
                  call io_error(error, "Cannot map symbol to atomic number", &
                     & line, tsym, filename(unit), lnum, "unknown element")
                  return
               end if
               xyz(:, iatom) = [x, y, z] * aatoau

               sdf(iatom) = sdf_data()
               do while(pos < len(line))
                  call next_token(line, pos, token)
                  equal = index(slice(line, token%first, token%last), '=') + token%first - 1
                  if (equal > token%first) then
                     select case(slice(line, token%first, equal - 1))
                     case("CHG")
                        token%first = equal + 1
                        call read_token(line, token, sdf(iatom)%charge, stat)
                     case("VAL")
                        token%first = equal + 1
                        call read_token(line, token, sdf(iatom)%valence, stat)
                     case("HCOUNT")
                        token%first = equal + 1
                        call read_token(line, token, sdf(iatom)%hydrogens, stat)
                     end select
                  end if
                  if (stat /= 0) then
                     call io_error(error, "Cannot read atom properties", &
                        & line, token, filename(unit), lnum, "unexpected value")
                     return
                  end if
               end do
            end do
            call next_v30(unit, line, pos, lnum, stat)
            call next_token(line, pos, token)

         case("BOND")
            do ibond = 1, number_of_bonds
               call next_v30(unit, line, pos, lnum, stat)
               if (stat == 0) &
                  call read_next_token(line, pos, token, dummy, stat)
               if (stat == 0) &
                  call read_next_token(line, pos, token, btype, stat)
               if (stat == 0) &
                  call read_next_token(line, pos, token, iatom, stat)
               if (stat == 0) &
                  call read_next_token(line, pos, token, jatom, stat)
               if (stat /= 0) then
                  call io_error(error, "Cannot read bond information", &
                     & line, token, filename(unit), lnum, "expected integer value")
                  return
               end if

               bond(:, ibond) = [iatom, jatom, btype]
            end do
            call next_v30(unit, line, pos, lnum, stat)
            call next_token(line, pos, token)

         case("COLLECTION", "SGROUP", "OBJ3D")
            do while(stat == 0)
               call next_v30(unit, line, pos, lnum, stat)
               call next_token(line, pos, token)
               if (slice(line, token%first, token%last) == 'END') exit
            end do

         case default
            call io_error(error, "Cannot read connection table", &
               & line, token, filename(unit), lnum, "Unknown entry found")
            return
         end select

         if (slice(line, token%first, token%last) /= 'END') then
            call io_error(error, group//" block is not terminated", &
               & line, token, filename(unit), lnum, "expected END label")
            return
         end if
         call next_token(line, pos, token)
         if (slice(line, token%first, token%last) /= group) then
            call io_error(error, group//" block is not terminated", &
               & line, token, filename(unit), lnum, "expected "//group//" label")
            return
         end if
      end if
      call next_v30(unit, line, pos, lnum, stat)
   end do

   if (slice(line, token%first, token%last) /= 'END') then
      call io_error(error, "Connection table is not terminated", &
         & line, token, filename(unit), lnum, "expected END label")
      return
   end if
   call next_token(line, pos, token)
   if (slice(line, token%first, token%last) /= 'CTAB') then
      call io_error(error, "Connection table is not terminated", &
         & line, token, filename(unit), lnum, "expected ATOM label")
      return
   end if

   call next_v30(unit, line, pos, lnum, stat)
   do while(stat == 0)
      call next_token(line, pos, token)
      if (slice(line, token%first, token%last) == 'END') exit
   end do

   if (stat /= 0) then
      call io_error(error, "Connection table is not terminated", &
         & line, token, filename(unit), lnum, "expected END label")
      return
   end if

   info = structure_info(missing_hydrogen=any(sdf%hydrogens > 1))
   call new(self, sym, xyz, charge=real(sum(sdf%charge), wp), info=info, bond=bond)
   call move_alloc(sdf, self%sdf)
end subroutine read_molfile_v3k


function slice(string, first, last)
   character(len=*), intent(in), target :: string
   integer, intent(in) :: first, last
   character(len=:), pointer :: slice

   slice => string(max(first, 1):min(last, len(string)))
end function slice


subroutine next_v30(unit, line, pos, lnum, iostat, iomsg)

   !> Formatted IO unit
   integer, intent(in) :: unit

   !> Line to read
   character(len=:), allocatable, intent(out) :: line

   !> Current position in line
   integer, intent(out) :: pos

   !> Current line number
   integer, intent(inout) :: lnum

   !> Status of operation
   integer, intent(out) :: iostat

   !> Error message
   character(len=:), allocatable, optional :: iomsg

   call next_line(unit, line, pos, lnum, iostat, iomsg)
   if (iostat /= 0) return

   if (index(line, 'M  END') == 1) pos = 3
   if (index(line, 'M  V30') == 1) pos = 6
end subroutine next_v30

end module mctc_io_read_ctfile
