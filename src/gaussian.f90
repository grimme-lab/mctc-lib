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

module mctc_io_read_gaussian
   use mctc_env_accuracy, only : wp
   use mctc_env_error, only : error_type, fatal_error
   use mctc_io_structure, only : structure_type, new
   use mctc_io_utils, only : next_line, token_type, next_token, io_error, filename, &
      read_token, to_string
   implicit none
   private

   public :: read_gaussian_external


contains


subroutine read_gaussian_external(self, unit, error)

   !> Instance of the molecular structure data
   type(structure_type), intent(out) :: self

   !> File handle
   integer, intent(in) :: unit

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   integer :: stat, n, mode, chrg, spin, iat, ii, pos, lnum
   type(token_type) :: token, tnat
   character(len=:), allocatable :: line
   integer, allocatable :: at(:)
   real(wp), allocatable :: xyz(:,:)
   real(wp) :: coord(3), q

   lnum = 0
   call next_line(unit, line, pos, lnum, stat)
   if (stat == 0) then
      token = token_type(1, 10)
      tnat = token
      call read_token(line, token, n, stat)
   end if
   if (stat == 0) then
      token = token_type(11, 20)
      call read_token(line, token, mode, stat)
   end if
   if (stat == 0) then
      token = token_type(21, 30)
      call read_token(line, token, chrg, stat)
   end if
   if (stat == 0) then
      token = token_type(31, 40)
      call read_token(line, token, spin, stat)
   end if
   if (stat /= 0) then
      call io_error(error, "Could not read number of atoms", &
         & line, token, filename(unit), lnum, "expected integer value")
      return
   end if

   if (n <= 0) then
      call io_error(error, "Found no atoms, cannot work without atoms!", &
         & line, tnat, filename(unit), lnum, "expected positive integer")
      return
   end if

   allocate(xyz(3, n))
   allocate(at(n))

   ii = 0
   do while (ii < n)
      call next_line(unit, line, pos, lnum, stat)
      if (is_iostat_end(stat)) exit
      if (stat == 0) then
         token = token_type(1, 10)
         tnat = token
         call read_token(line, token, iat, stat)
      end if
      if (stat == 0) then
         token = token_type(11, 30)
         call read_token(line, token, coord(1), stat)
      end if
      if (stat == 0) then
         token = token_type(31, 50)
         call read_token(line, token, coord(2), stat)
      end if
      if (stat == 0) then
         token = token_type(51, 70)
         call read_token(line, token, coord(3), stat)
      end if
      if (stat == 0) then
         token = token_type(71, 90)
         call read_token(line, token, q, stat)
      end if
      if (stat /= 0) then
         call io_error(error, "Could not read geometry from Gaussian file", &
            & line, token, filename(unit), lnum, "unexpected value")
         return
      end if
      if (iat > 0) then
         ii = ii+1
         at(ii) = iat
         xyz(:, ii) = coord
      else
         call io_error(error, "Invalid atomic number", &
            & line, tnat, filename(unit), lnum, "expected positive integer")
         return
      end if
   end do

   call new(self, at, xyz, charge=real(chrg, wp), uhf=spin)

   if (ii /= n) then
      call fatal_error(error, "Atom number missmatch in Gaussian file")
      return
   end if

end subroutine read_gaussian_external


end module mctc_io_read_gaussian
