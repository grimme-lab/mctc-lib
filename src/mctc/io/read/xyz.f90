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

module mctc_io_read_xyz
   use mctc_env_accuracy, only : wp
   use mctc_env_error, only : error_type, fatal_error
   use mctc_io_convert, only : aatoau
   use mctc_io_structure, only : structure_type, new
   use mctc_io_symbols, only : to_number, symbol_length
   use mctc_io_utils, only : getline
   implicit none
   private

   public :: read_xyz


contains


subroutine read_xyz(self, unit, error)

   !> Instance of the molecular structure data
   type(structure_type), intent(out) :: self

   !> File handle
   integer, intent(in) :: unit

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   integer :: ii, n, iat, stat
   real(wp) :: x, y, z, conv
   real(wp), allocatable :: xyz(:, :)
   character(len=symbol_length) :: chdum
   character(len=symbol_length), allocatable :: sym(:)
   character(len=:), allocatable :: line

   conv = aatoau

   read(unit, *, iostat=stat) n
   if (stat /= 0) then
      call fatal_error(error, "Could not read number of atoms, check format!")
      return
   end if

   if (n.lt.1) then
      call fatal_error(error, "Found no atoms, cannot work without atoms!")
      return
   end if

   allocate(sym(n))
   allocate(xyz(3, n))

   ! drop next record
   read(unit, '(a)', iostat=stat)
   if (stat /= 0) then
      call fatal_error(error, "Unexpected end of file")
      return
   end if

   ii = 0
   do while (ii < n)
      call getline(unit, line, stat)
      if (is_iostat_end(stat)) exit
      if (stat /= 0) then
         call fatal_error(error, "Could not read geometry from xyz file")
         return
      end if
      read(line, *, iostat=stat) chdum, x, y, z
      if (stat /= 0) then
         call fatal_error(error, "Could not parse coordinates from xyz file")
         return
      end if

      iat = to_number(chdum)
      if (iat > 0) then
         ii = ii+1
         sym(ii) = trim(chdum)
         xyz(:, ii) = [x, y, z]*conv
      else
         call fatal_error(error, "Unknown element symbol: '"//trim(chdum)//"'")
         return
      end if
   end do

   if (ii /= n) then
      call fatal_error(error, "Atom number missmatch in xyz file")
      return
   end if

   call new(self, sym, xyz)

end subroutine read_xyz


end module mctc_io_read_xyz
