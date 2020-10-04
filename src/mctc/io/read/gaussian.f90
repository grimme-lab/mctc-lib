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

   integer :: stat, n, mode, chrg, spin, iat, ii
   integer, allocatable :: at(:)
   real(wp), allocatable :: xyz(:,:)
   real(wp) :: coord(3), q

   read(unit, '(4i10)', iostat=stat) n, mode, chrg, spin
   if (stat.ne.0) then
      call fatal_error(error, "Could not read number of atoms, check format!")
      return
   end if

   if (n <= 0) then
      call fatal_error(error, "Found no atoms, cannot work without atoms!")
      return
   end if

   allocate(xyz(3, n))
   allocate(at(n))

   ii = 0
   do while (ii < n)
      read(unit, '(i10, 4f20.12)', iostat=stat) iat, coord, q
      if (is_iostat_end(stat)) exit
      if (stat.ne.0) then
         call fatal_error(error, "Could not read geometry from Gaussian file")
         return
      end if
      if (iat > 0) then
         ii = ii+1
         at(ii) = iat
         xyz(:, ii) = coord
      else
         call fatal_error(error, "Invalid atomic number")
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
