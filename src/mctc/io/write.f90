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

module mctc_io_write
   use mctc_env_error, only : error_type, fatal_error
   use mctc_io_filetype, only : filetype, get_filetype
   use mctc_io_write_ctfile, only : write_molfile, write_sdf
   use mctc_io_write_gaussian, only : write_gaussian_external
   use mctc_io_write_genformat, only : write_genformat
   use mctc_io_write_pdb, only : write_pdb
   use mctc_io_write_turbomole, only : write_coord
   use mctc_io_write_vasp, only : write_vasp
   use mctc_io_write_xyz, only : write_xyz
   use mctc_io_structure, only : structure_type, new_structure
   implicit none
   private

   public :: write_structure


   interface write_structure
      module procedure :: write_structure_to_file
      module procedure :: write_structure_to_unit
   end interface write_structure


contains


subroutine write_structure_to_file(self, file, error, format)

   !> Instance of the molecular structure data
   class(structure_type), intent(in) :: self

   !> Name of the file to read
   character(len=*), intent(in) :: file

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   !> File type format hint
   integer, intent(in), optional :: format

   integer :: unit, ftype, stat

   open(file=file, newunit=unit, iostat=stat)
   if (stat /= 0) then
      call fatal_error(error, "Cannot open '"//file//"'")
      return
   end if

   if (present(format)) then
      ftype = format
   else
      ftype = get_filetype(file)
   end if

   ! Unknown file type is inacceptable in this situation,
   ! try to figure something at least something out
   if (ftype == filetype%unknown) then
      if (any(self%periodic)) then
         ftype = filetype%vasp
      else if (allocated(self%sdf)) then
         ftype = filetype%sdf
      else if (allocated(self%pdb)) then
         ftype = filetype%pdb
      else
         ftype = filetype%xyz
      end if
   end if

   call write_structure(self, unit, ftype, error)
   close(unit)

end subroutine write_structure_to_file


subroutine write_structure_to_unit(self, unit, ftype, error)

   !> Instance of the molecular structure data
   class(structure_type), intent(in) :: self

   !> File handle
   integer, intent(in) :: unit

   !> File type to read
   integer, intent(in) :: ftype

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   select case(ftype)
   case default
      call fatal_error(error, "Cannot write unknown file format")

   case(filetype%xyz)
      call write_xyz(self, unit)

   case(filetype%molfile)
      call write_molfile(self, unit)

   case(filetype%pdb)
      call write_pdb(self, unit)

   case(filetype%gen)
      call write_genformat(self, unit)

   case(filetype%sdf)
      call write_sdf(self, unit)

   case(filetype%vasp)
      call write_vasp(self, unit)

   case(filetype%tmol)
      call write_coord(self, unit)

   case(filetype%gaussian)
      call write_gaussian_external(self, unit)

   end select

end subroutine write_structure_to_unit


end module mctc_io_write
