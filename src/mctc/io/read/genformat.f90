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

module mctc_io_read_genformat
   use mctc_env_accuracy, only : wp
   use mctc_env_error, only : error_type
   use mctc_io_constants, only : pi
   use mctc_io_convert, only : aatoau
   use mctc_io_structure, only : structure_type, new
   use mctc_io_structure_info, only : structure_info
   use mctc_io_symbols, only : to_number, symbol_length
   use mctc_io_utils, only : next_line, token_type, next_token, io_error, filename, &
      read_next_token, to_string
   implicit none
   private

   public :: read_genformat


contains


subroutine read_genformat(mol, unit, error)

   !> Instance of the molecular structure data
   type(structure_type),intent(out) :: mol

   !> File handle
   integer,intent(in) :: unit

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=:), allocatable :: line
   integer :: natoms, nspecies, iatom, dummy, isp, ilat, stat, istart, iend
   logical :: cartesian, periodic(3)
   real(wp) :: coord(3), origin(3)
   character(len=1) :: variant
   type(token_type) :: token
   character(len=symbol_length), allocatable :: species(:), sym(:)
   real(wp), allocatable :: xyz(:, :), abc(:, :), lattice(:, :)
   type(structure_info) :: info
   integer :: pos, lnum

   lnum = 0
   call advance_line(unit, line, pos, lnum, stat)
   call read_next_token(line, pos, token, natoms, stat)
   if (stat /= 0 .or. natoms < 1) then
      call io_error(error, "Could not read number of atoms", &
         & line, token, filename(unit), lnum, "expected integer value")
      return
   end if

   allocate(species(natoms))
   allocate(sym(natoms))
   allocate(xyz(3, natoms))
   allocate(abc(3, natoms))

   call next_token(line, pos, token)
   select case(line(token%first:token%last))
   case('c', 'C')
      cartesian = .true.
      periodic = .false.
   case('s', 'S')
      cartesian = .true.
      periodic = .true.
      allocate(lattice(3, 3), source=0.0_wp)
   case('f', 'F')
      cartesian = .false.
      periodic = .true.
      allocate(lattice(3, 3), source=0.0_wp)
   case('h', 'H')
      cartesian = .true.
      periodic = [.false., .false., .true.]
      allocate(lattice(3, 1), source=0.0_wp)
   case default
      call io_error(error, "Invalid input version found", &
         & line, token, filename(unit), lnum, "unknown identifier")
      return
   end select

   call advance_line(unit, line, pos, lnum, stat)
   isp = 0
   do while(pos < len(line))
      call next_token(line, pos, token)
      isp = isp + 1
      token%last = min(token%last, token%first + symbol_length - 1)
      species(isp) = line(token%first:token%last)
      if (to_number(species(isp)) == 0) then
         call io_error(error, "Cannot map symbol to atomic number", &
            & line, token, filename(unit), lnum, "unknown element")
         return
      end if
   end do
   nspecies = isp

   do iatom = 1, natoms
      token = token_type(0, 0)
      call advance_line(unit, line, pos, lnum, stat)
      if (stat == 0) &
         call read_next_token(line, pos, token, dummy, stat)
      if (stat == 0) &
         call read_next_token(line, pos, token, isp, stat)
      if (stat == 0) &
         call read_next_token(line, pos, token, coord(1), stat)
      if (stat == 0) &
         call read_next_token(line, pos, token, coord(2), stat)
      if (stat == 0) &
         call read_next_token(line, pos, token, coord(3), stat)
      if (stat /= 0) then
         call io_error(error, "Cannot read coordinates", &
            & line, token, filename(unit), lnum, "unexpected value")
         return
      end if
      sym(iatom) = species(isp)
      if (cartesian) then
         xyz(:, iatom) = coord * aatoau
      else
         abc(:, iatom) = coord
      end if
   end do

   if (any(periodic)) then
      call advance_line(unit, line, pos, lnum, stat)
      if (stat /= 0) then
         call io_error(error, "Unexpected end of file", &
            & line, token_type(0, 0), filename(unit), lnum, "missing lattice information")
         return
      end if
      if (stat == 0) &
         call read_next_token(line, pos, token, origin(1), stat)
      if (stat == 0) &
         call read_next_token(line, pos, token, origin(2), stat)
      if (stat == 0) &
         call read_next_token(line, pos, token, origin(3), stat)
      if (stat /= 0) then
         call io_error(error, "Cannot read origin", &
            & line, token, filename(unit), lnum, "expected real value")
         return
         end if
   end if

   if (all(periodic)) then
      do ilat = 1, 3
         call advance_line(unit, line, pos, lnum, stat)
         if (stat == 0) &
            call read_next_token(line, pos, token, coord(1), stat)
         if (stat == 0) &
            call read_next_token(line, pos, token, coord(2), stat)
         if (stat == 0) &
            call read_next_token(line, pos, token, coord(3), stat)
         if (stat /= 0) then
            call io_error(error, "Cannot read lattice vector", &
               & line, token, filename(unit), lnum, "expected real value")
            return
         end if
         lattice(:, ilat) = coord * aatoau
      end do
      if (.not.cartesian) then
         xyz = matmul(lattice, abc)
      end if
   end if

   if (count(periodic) == 1) then
      call advance_line(unit, line, pos, lnum, stat)
      if (stat == 0) &
         call read_next_token(line, pos, token, coord(1), stat)
      if (stat == 0) &
         call read_next_token(line, pos, token, coord(2), stat)
      if (stat == 0) &
         call read_next_token(line, pos, token, coord(3), stat)
      if (stat /= 0) then
         call io_error(error, "Cannot read lattice vector", &
            & line, token, filename(unit), lnum, "expected real value")
         return
      end if
      if (coord(3) < 1) then
         call io_error(error, "Invalid helical axis rotation order", &
            & line, token, filename(unit), lnum, "expected positive value")
         return
      end if

      ! Store helical axis in *first* lattice vector, however it is not an actual
      ! lattice vector as on would expect but a screw axis
      lattice(:, 1) = [coord(1) * aatoau, coord(2) * pi / 180.0_wp, coord(3)]
   end if

   if (any(periodic)) then
      xyz(:, :) = xyz - spread(origin, 2, natoms)
   end if

   info = structure_info(cartesian=cartesian)
   call new(mol, sym, xyz, lattice=lattice, periodic=periodic, info=info)

contains

subroutine advance_line(unit, line, pos, num, stat)
   integer,intent(in) :: unit
   integer, intent(out) :: pos
   integer, intent(inout) :: num
   character(len=:), allocatable, intent(out) :: line
   integer, intent(out) :: stat
   integer :: ihash

   stat = 0
   do while(stat == 0)
      call next_line(unit, line, pos, num, stat)
      ihash = index(line, '#')
      if (ihash > 0) line = line(:ihash-1)
      if (len_trim(line) > 0) exit
   end do
   line = trim(adjustl(line))
end subroutine advance_line

end subroutine read_genformat


end module mctc_io_read_genformat
