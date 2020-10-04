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

module mctc_io_read
   use mctc_env_error, only : error_type, fatal_error
   use mctc_io_filetype, only : filetype, get_filetype
   use mctc_io_read_ctfile, only : read_molfile, read_sdf
   use mctc_io_read_gaussian, only : read_gaussian_external
   use mctc_io_read_genformat, only : read_genformat
   use mctc_io_read_pdb, only : read_pdb
   use mctc_io_read_turbomole, only : read_coord
   use mctc_io_read_vasp, only : read_vasp
   use mctc_io_read_xyz, only : read_xyz
   use mctc_io_structure, only : structure_type, new_structure
   implicit none
   private

   public :: read_structure
   public :: structure_reader, get_structure_reader


   interface read_structure
      module procedure :: read_structure_from_file
      module procedure :: read_structure_from_unit
   end interface read_structure


   abstract interface
      !> Read molecular structure data from formatted unit
      subroutine structure_reader(self, unit, error)
         import :: structure_type, error_type

         !> Instance of the molecular structure data
         type(structure_type), intent(out) :: self

         !> File handle
         integer, intent(in) :: unit

         !> Error handling
         type(error_type), allocatable, intent(out) :: error

      end subroutine structure_reader
   end interface


contains


subroutine read_structure_from_file(self, file, error, format)

   !> Instance of the molecular structure data
   type(structure_type), intent(out) :: self

   !> Name of the file to read
   character(len=*), intent(in) :: file

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   !> File type format hint
   integer, intent(in), optional :: format

   logical :: exist
   integer :: unit, stat, ftype

   inquire(file=file, exist=exist)
   if (.not.exist) then
      call fatal_error(error, "File '"//file//"' cannot be found")
      return
   end if

   open(file=file, newunit=unit, status='old', iostat=stat)
   if (stat /= 0) then
      call fatal_error(error, "Cannot open '"//file//"'")
      return
   end if

   if (present(format)) then
      ftype = format
   else
      ftype = get_filetype(file)
   end if

   call read_structure(self, unit, ftype, error)
   close(unit)

end subroutine read_structure_from_file


subroutine read_structure_from_unit(self, unit, ftype, error)

   !> Instance of the molecular structure data
   type(structure_type), intent(out) :: self

   !> File handle
   integer, intent(in) :: unit

   !> File type to read
   integer, intent(in) :: ftype

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   procedure(structure_reader), pointer :: reader

   call get_structure_reader(reader, ftype)
   if (.not.associated(reader)) then
      call fatal_error(error, "Cannot read structure from unknown file format")
      return
   end if

   call reader(self, unit, error)

end subroutine read_structure_from_unit


!> Retrieve reader for corresponding file type
subroutine get_structure_reader(reader, ftype)

   !> Reader for the specified file type
   procedure(structure_reader), pointer, intent(out) :: reader

   !> File type to read
   integer, intent(in) :: ftype

   nullify(reader)

   select case(ftype)
   case(filetype%xyz)
      reader => read_xyz

   case(filetype%molfile)
      reader => read_molfile

   case(filetype%pdb)
      reader => read_pdb

   case(filetype%gen)
      reader => read_genformat

   case(filetype%sdf)
      reader => read_sdf

   case(filetype%vasp)
      reader => read_vasp

   case(filetype%tmol)
      reader => read_coord

   case(filetype%gaussian)
      reader => read_gaussian_external

   end select

end subroutine get_structure_reader


end module mctc_io_read
