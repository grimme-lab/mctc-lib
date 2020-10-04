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
   use mctc_env_error, only : error_type, fatal_error
   use mctc_io_convert, only : aatoau
   use mctc_io_structure, only : structure_type, new
   use mctc_io_structure_info, only : structure_info
   use mctc_io_symbols, only : to_number, symbol_length
   use mctc_io_utils, only : getline
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
   logical :: cartesian, periodic
   real(wp) :: coord(3), lattice(3, 3)
   character(len=1) :: variant
   character(len=symbol_length), allocatable :: species(:), sym(:)
   real(wp), allocatable :: xyz(:, :), abc(:, :)
   type(structure_info) :: info

   call next_line(unit, line, stat)
   read(line, *, iostat=stat) natoms, variant
   if (stat /= 0 .or. natoms < 1) then
      call fatal_error(error, 'could not read number of atoms')
      return
   end if

   allocate(species(natoms))
   allocate(sym(natoms))
   allocate(xyz(3, natoms))
   allocate(abc(3, natoms))

   select case(variant)
   case('c', 'C')
      cartesian = .true.
      periodic = .false.
   case('s', 'S')
      cartesian = .true.
      periodic = .true.
   case('f', 'F')
      cartesian = .false.
      periodic = .true.
   case default
      call fatal_error(error, 'invalid input version')
      return
   endselect

   call next_line(unit, line, stat)
   istart = 1
   iend = 1
   isp = 0
   do while(iend < len_trim(line))
      istart = verify(line(iend:), ' ') - 1 + iend
      iend = scan(line(istart:), ' ') - 1 + istart
      if (iend < istart) iend = len_trim(line)
      isp = isp + 1
      species(isp) = trim(line(istart:iend))
   end do
   nspecies = isp
   if (any(to_number(species(:nspecies)) == 0)) then
      call fatal_error(error, 'unknown atom type present')
      return
   end if

   do iatom = 1, natoms
      call next_line(unit, line, stat)
      read(line, *, iostat=stat) dummy, isp, coord
      if (stat /= 0) then
         call fatal_error(error, 'could not read coordinates from file')
         return
      end if
      sym(iatom) = species(isp)
      if (cartesian) then
         xyz(:, iatom) = coord * aatoau
      else
         abc(:, iatom) = coord
      end if
   end do

   if (periodic) then
      call next_line(unit, line, stat)
      if (stat /= 0) then
         call fatal_error(error, 'missing lattice information')
         return
      end if
      do ilat = 1, 3
         call next_line(unit, line, stat)
         read(line, *, iostat=stat) coord
         if (stat /= 0) then
            call fatal_error(error, 'could not read lattice from file')
            return
         end if
         lattice(:, ilat) = coord * aatoau
      end do
      if (.not.cartesian) then
         xyz = matmul(lattice, abc)
      end if
      info = structure_info(cartesian=cartesian)
      call new(mol, sym, xyz, lattice=lattice, info=info)
   else
      call new(mol, sym, xyz)
   end if


contains

subroutine next_line(unit, line, stat)
   integer,intent(in) :: unit
   character(len=:), allocatable, intent(out) :: line
   integer, intent(out) :: stat
   integer :: ihash

   stat = 0
   do while(stat == 0)
      call getline(unit, line, stat)
      ihash = index(line, '#')
      if (ihash > 0) line = line(:ihash-1)
      if (len_trim(line) > 0) exit
   end do
   line = trim(adjustl(line))
end subroutine next_line

end subroutine read_genformat


end module mctc_io_read_genformat
