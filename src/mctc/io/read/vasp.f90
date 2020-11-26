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

module mctc_io_read_vasp
   use mctc_env_accuracy, only : wp
   use mctc_env_error, only : error_type, fatal_error
   use mctc_io_convert, only : aatoau
   use mctc_io_resize, only : resize
   use mctc_io_structure, only : structure_type, new
   use mctc_io_structure_info, only : structure_info
   use mctc_io_symbols, only : to_number, symbol_length
   use mctc_io_utils, only : getline
   implicit none
   private

   public :: read_vasp


   logical, parameter :: debug = .false.


contains


subroutine read_vasp(self, unit, error)

   !> Instance of the molecular structure data
   type(structure_type), intent(out) :: self

   !> File handle
   integer, intent(in) :: unit

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   logical :: selective, cartesian
   integer :: i, j, k, nn, ntype, natoms, izp, stat
   integer, allocatable :: ncount(:)
   real(wp) :: ddum, latvec(3), scalar, coord(3), lattice(3, 3)
   real(wp), allocatable :: xyz(:, :)
   character(len=:), allocatable :: line, comment
   character(len=2*symbol_length), allocatable :: args(:), args2(:)
   character(len=symbol_length), allocatable :: sym(:)
   type(structure_info) :: info

   selective = .false. ! Selective dynamics
   cartesian = .true.  ! Cartesian or direct
   lattice = 0
   stat = 0

   ntype = 0
   ! first line contains the symbols of different atom types
   call getline(unit, line, stat)
   if (stat /= 0) then
      call fatal_error(error, "Unexpected end of input encountered")
      return
   end if
   if (debug) print'(">", a)', line

   call parse_line(line, args, ntype)
   call move_alloc(line, comment)

   ! this line contains the global scaling factor,
   call getline(unit, line, stat)
   if (stat /= 0) then
      call fatal_error(error, "Unexpected end of input encountered")
      return
   end if
   if (debug) print'(">", a)', line
   read(line, *, iostat=stat) ddum
   if (stat /= 0) then
      call fatal_error(error, "Cannot read scaling factor from input")
      return
   end if
   ! the Ang->au conversion is included in the scaling factor
   if (debug) print'("->", g0)', ddum
   scalar = ddum*aatoau

   ! reading the lattice constants
   do i = 1, 3
      call getline(unit, line, stat)
      if (stat /= 0) then
         call fatal_error(error, "Unexpected end of lattice vectors encountered")
         return
      end if
      if (debug) print'("->", a)', line
      read(line, *, iostat=stat) latvec
      if (stat /= 0) then
         call fatal_error(error, "Cannot read lattice vectors from input")
         return
      end if
      lattice(:, i) = latvec * scalar
   end do
   ! Either here are the numbers of each element,
   ! or (>vasp.5.1) here are the element symbols
   call getline(unit, line, stat)
   if (stat /= 0) then
      call fatal_error(error, "Unexpected end of input encountered")
      return
   end if
   if (debug) print'(">", a)', line

   ! try to verify that first element is actually a number
   i = max(verify(line, ' '), 1)
   j = scan(line(i:), ' ') - 2 + i
   if (j < i) j = len_trim(line)

   ! CONTCAR files have additional Element line here since vasp.5.1
   if (verify(line(i:j), '1234567890') /= 0) then
      call parse_line(line, args, ntype)
      call getline(unit, line, stat)
      if (debug) print'("->", a)', line
      if (stat /= 0) then
         call fatal_error(error, "Unexpected end of input encountered")
         return
      end if
   else
      deallocate(comment)
   end if
   call parse_line(line, args2, nn)
   if (nn /= ntype) then
      call fatal_error(error, 'Number of atom types mismatches the number of counts')
      return
   end if

   allocate(ncount(nn), source = 0)
   do i = 1, nn
      read(args2(i), *, iostat=stat) ncount(i)
      izp = to_number(args(i))
      if (izp < 1 .or. ncount(i) < 1) then
         call fatal_error(error, "Unknown element '"//trim(args(i))//"' encountered")
         return
      end if
   end do

   natoms = sum(ncount)
   allocate(sym(natoms))
   allocate(xyz(3, natoms))

   k = 0
   do i = 1, nn
      do j = 1, ncount(i)
         k = k+1
         sym(k) = trim(args(i))
      end do
   end do

   call getline(unit, line, stat)
   if (stat /= 0) then
      call fatal_error(error, "Could not read POSCAR")
      return
   end if
   if (debug) print'(">", a)', line
   line = adjustl(line)
   if (line(:1).eq.'s' .or. line(:1).eq.'S') then
      selective = .true.
      call getline(unit, line, stat)
      if (debug) print'("->", a)', line
      if (stat /= 0) then
         call fatal_error(error, "Unexpected end of input encountered")
         return
      end if
      line = adjustl(line)
   end if

   cartesian = (line(:1).eq.'c' .or. line(:1).eq.'C' .or. &
      &         line(:1).eq.'k' .or. line(:1).eq.'K')
   do i = 1, natoms
      call getline(unit, line, stat)
      if (stat /= 0) then
         call fatal_error(error, "Unexpected end of geometry encountered")
         return
      end if
      if (debug) print'("-->", a)', line
      read(line, *, iostat=stat) coord
      if (stat /= 0) then
         call fatal_error(error, "Cannot read geometry from input")
         return
      end if

      if (cartesian) then
         xyz(:, i) = coord*scalar
      else
         xyz(:, i) = matmul(lattice, coord)
      end if

   end do

   ! save information about this POSCAR for later
   info = structure_info(scale=ddum, selective=selective, cartesian=cartesian)
   call new(self, sym, xyz, lattice=lattice, info=info)
   if (allocated(comment)) self%comment = comment

end subroutine read_vasp


subroutine parse_line(line, args, nargs)
   character(len=*), intent(in) :: line
   character(len=2*symbol_length), allocatable, intent(out) :: args(:)
   integer, intent(out) :: nargs
   integer, parameter :: p_initial_size = 50
   integer :: istart, iend
   allocate(args(p_initial_size), source=repeat(' ', 2*symbol_length))
   istart = 1
   iend = 1
   nargs = 0
   do while(iend < len_trim(line))
      istart = verify(line(iend:), ' ') - 1 + iend
      iend = scan(line(istart:), ' ') - 1 + istart
      if (iend < istart) iend = len_trim(line)
      if (nargs >= size(args)) then
         call resize(args)
      end if
      nargs = nargs + 1
      args(nargs) = trim(line(istart:iend))
   end do
end subroutine parse_line


end module mctc_io_read_vasp
