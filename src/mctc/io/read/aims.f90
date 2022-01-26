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

module mctc_io_read_aims
   use mctc_env_accuracy, only : wp
   use mctc_env_error, only : error_type, fatal_error
   use mctc_io_convert, only : aatoau
   use mctc_io_resize, only : resize
   use mctc_io_symbols, only : symbol_length, to_number
   use mctc_io_structure, only : structure_type, new
   use mctc_io_utils, only : next_line, token_type, next_token, io_error, filename, &
      read_next_token, to_string
   implicit none
   private

   public :: read_aims

   integer, parameter :: initial_size = 64

contains

subroutine read_aims(mol, unit, error)

   !> Instance of the molecular structure data
   type(structure_type), intent(out) :: mol

   !> File handle
   integer, intent(in) :: unit

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   integer :: stat, pos, lnum, ilt, iat
   type(token_type) :: token
   character(len=:), allocatable :: line
   real(wp) :: x, y, z
   character(len=symbol_length), allocatable :: sym(:)
   real(wp), allocatable :: xyz(:, :), abc(:, :), lattice(:, :)
   logical :: is_frac, periodic(3)
   logical, allocatable :: frac(:)

   allocate(sym(initial_size), source=repeat(' ', symbol_length))
   allocate(xyz(3, initial_size), source=0.0_wp)
   allocate(abc(3, initial_size), source=0.0_wp)
   allocate(frac(initial_size), source=.false.)

   iat = 0
   ilt = 0
   periodic(:) = .false.

   lnum = 0
   stat = 0
   do while(stat == 0)
      call next_line(unit, line, pos, lnum, stat)
      if (stat /= 0) exit
      if (len(line) == 0) cycle
      if (line(1:1) == "#") cycle

      call next_token(line, pos, token)
      select case(line(token%first:token%last))
      case("atom", "atom_frac")
         is_frac = token%last - token%first + 1 > 4
         call read_next_token(line, pos, token, x, stat)
         if (stat == 0) &
            call read_next_token(line, pos, token, y, stat)
         if (stat == 0) &
            call read_next_token(line, pos, token, z, stat)
         if (stat == 0) &
            call next_token(line, pos, token)
         if (stat /= 0) then
            call io_error(error, "Cannot read coordinates", &
               & line, token, filename(unit), lnum, "expected real value")
            exit
         end if

         if (iat >= size(sym)) call resize(sym)
         if (iat >= size(xyz, 2)) call resize(xyz)
         if (iat >= size(abc, 2)) call resize(abc)
         if (iat >= size(frac)) call resize(frac)
         iat = iat + 1

         token%last = min(token%last, token%first + symbol_length - 1)
         sym(iat) = line(token%first:token%last)
         if (to_number(sym(iat)) == 0) then
            call io_error(error, "Cannot map symbol to atomic number", &
               & line, token, filename(unit), lnum, "unknown element")
            exit
         end if
         frac(iat) = is_frac
         if (frac(iat)) then
            abc(:, iat) = [x, y, z]
            xyz(:, iat) = 0.0_wp
         else
            abc(:, iat) = 0.0_wp
            xyz(:, iat) = [x, y, z] * aatoau
         end if

      case("lattice_vector")
         ilt = ilt + 1
         if (ilt > 3) then
            call io_error(error, "Too many lattice vectors", &
               & line, token, filename(unit), lnum, "forth lattice vector found")
            exit
         end if
         call read_next_token(line, pos, token, x, stat)
         if (stat == 0) &
            call read_next_token(line, pos, token, y, stat)
         if (stat == 0) &
            call read_next_token(line, pos, token, z, stat)
         if (stat /= 0) then
            call io_error(error, "Cannot read lattice vectors", &
               & line, token, filename(unit), lnum, "expected real value")
            exit
         end if

         if (.not.allocated(lattice)) allocate(lattice(3, 3), source=0.0_wp)
         lattice(:, ilt) = [x, y, z] * aatoau

      case default
         call io_error(error, "Unexpected keyword found", &
            & line, token, filename(unit), lnum, "invalid in this context")
         exit
      end select

   end do
   if (allocated(error)) return

   if (iat == 0) then
      token = token_type(0, 0)
      call io_error(error, "No atoms found", &
         & line, token, filename(unit), lnum+1, "expected atom specification")
      return
   end if

   if (allocated(lattice)) then
      xyz(ilt+1:3, :iat) = xyz(ilt+1:3, :iat) + abc(ilt+1:3, :iat) * aatoau
      xyz(:ilt, :iat) = xyz(:ilt, :iat) + matmul(lattice(:ilt, :ilt), abc(:ilt, :iat))
      periodic(:ilt) = .true.
   end if

   call new(mol, sym(:iat), xyz, lattice=lattice, periodic=periodic)

end subroutine read_aims

end module mctc_io_read_aims
