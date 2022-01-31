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
      read_token, to_string
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
   integer :: i, iatom, jatom, ibond, btype, atomtype
   integer :: stat, length, charge(2, 15), lnum, pos
   integer :: number_of_atoms, number_of_bonds
   integer :: list7(7), list12(12)
   real(wp) :: x, y, z
   character(len=2) :: sdf_dim
   character(len=3) :: symbol
   character(len=5) :: v2000
   integer, parameter :: ccc_to_charge(0:7) = [0, +3, +2, +1, 0, -1, -2, -3]
   logical :: two_dim
   type(token_type) :: token
   character(len=symbol_length), allocatable :: sym(:)
   type(sdf_data), allocatable :: sdf(:)
   type(structure_info) :: info
   real(wp), allocatable :: xyz(:, :)
   integer, allocatable :: bond(:, :)

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
      if (line(35:39) == 'V2000') stat = 0
   end if

   if (stat /= 0) then
      call io_error(error, "Format version not supported", &
         & line, token, filename(unit), lnum, "invalid format version")
      return
   end if
   if (number_of_atoms < 1) then
      call io_error(error, "Invalid number of atoms", &
         & line, token_type(1, 3), filename(unit), lnum, "expected positive integer")
      return
   end if

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
      do i = 1, 11
         if (stat == 0) then
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
      do i = 1, 7
         if (stat == 0) then
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

   info = structure_info(two_dimensional=two_dim, &
      & missing_hydrogen=any(sdf%hydrogens > 1))
   call new(self, sym, xyz, charge=real(sum(sdf%charge), wp), info=info, bond=bond)
   call move_alloc(sdf, self%sdf)
   if (len(comment) > 0) self%comment = comment

end subroutine read_molfile


end module mctc_io_read_ctfile
