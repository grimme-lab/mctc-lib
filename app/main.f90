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

!> Example application using tool chain library.
!>
!> This program uses the [[read_structure]] and [[write_structure]] procedures
!> to implement a structure converter.
!> Usually, the input structure can be inferred by the name of the input file.
!> To allow formats with non-standard extensions (because most geometry formats
!> are not really standardized) additional hints can be passed by the command
!> line to determine the read/write formats.
!>
!> To add support for piping standard input and standard output reading and
!> writing from units is combined with the additional format hints.
!>
!> Additional filters or modifications can also be implemented in an intermediary
!> step, this program implements an element symbol normalization. Other filters
!> like folding back to central cells or removing lattice vector could be added
!> in a similar manner.
program main
   use, intrinsic :: iso_fortran_env, only : output_unit, error_unit, input_unit
   use mctc_env, only : error_type, fatal_error, get_argument, wp
   use mctc_io, only : structure_type, read_structure, write_structure, &
      & filetype, get_filetype, to_symbol
   use mctc_version, only : get_mctc_version
   implicit none
   character(len=*), parameter :: prog_name = "mctc-convert"

   character(len=:), allocatable :: input, output
   integer, allocatable :: input_format, output_format
   type(structure_type) :: mol
   type(error_type), allocatable :: error
   logical :: normalize

   call get_arguments(input, input_format, output, output_format, normalize, error)
   if (allocated(error)) then
      write(error_unit, '(a)') error%message
      error stop
   end if

   if (input == "-") then
      if (.not.allocated(input_format)) input_format = filetype%xyz
      call read_structure(mol, input_unit, input_format, error)
   else
      call read_structure(mol, input, error, input_format)
   end if
   if (allocated(error)) then
      write(error_unit, '(a)') error%message
      error stop
   end if

   if (normalize) then
      mol%sym = to_symbol(mol%num)
   end if

   if (output == "-") then
      if (.not.allocated(output_format)) output_format = filetype%xyz
      call write_structure(mol, output_unit, output_format, error)
   else
      call write_structure(mol, output, error, output_format)
   end if
   if (allocated(error)) then
      write(error_unit, '(a)') error%message
      error stop
   end if


contains


subroutine help(unit)
   integer, intent(in) :: unit

   write(unit, '(a, *(1x, a))') &
      "Usage: "//prog_name//" [options] <input> <output>"

   write(unit, '(a)') &
      "", &
      "Read structure from input file and writes it to output file.", &
      "The format is determined by the file extension or the format hint", &
      ""

   write(unit, '(2x, a, t25, a)') &
      "-i, --input <format>", "Hint for the format of the input file", &
      "-o, --output <format>", "Hint for the format of the output file", &
      "--normalize", "Normalize all element symbols to capitalized format", &
      "--version", "Print program version and exit", &
      "--help", "Show this help message"

   write(unit, '(a)')

end subroutine help


subroutine version(unit)
   integer, intent(in) :: unit
   character(len=:), allocatable :: version_string

   call get_mctc_version(string=version_string)
   write(unit, '(a, *(1x, a))') &
      & prog_name, "version", version_string

end subroutine version


subroutine get_arguments(input, input_format, output, output_format, normalize, &
      & error)

   !> Input file name
   character(len=:), allocatable :: input

   !> Input file format
   integer, allocatable, intent(out) :: input_format

   !> Output file name
   character(len=:), allocatable :: output

   !> Output file format
   integer, allocatable, intent(out) :: output_format

   !> Normalize element symbols
   logical, intent(out) :: normalize

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   integer :: iarg, narg
   character(len=:), allocatable :: arg

   normalize = .false.
   iarg = 0
   narg = command_argument_count()
   do while(iarg < narg)
      iarg = iarg + 1
      call get_argument(iarg, arg)
      select case(arg)
      case("--help")
         call help(output_unit)
         stop
      case("--version")
         call version(output_unit)
         stop
      case default
         if (.not.allocated(input)) then
            call move_alloc(arg, input)
            cycle
         end if
         if (.not.allocated(output)) then
            call move_alloc(arg, output)
            cycle
         end if
         call fatal_error(error, "Too many positional arguments present")
         exit
      case("-i", "--input")
         iarg = iarg + 1
         call get_argument(iarg, arg)
         if (.not.allocated(arg)) then
            call fatal_error(error, "Missing argument for input format")
            exit
         end if
         input_format = get_filetype("."//arg)
      case("-o", "--output")
         iarg = iarg + 1
         call get_argument(iarg, arg)
         if (.not.allocated(arg)) then
            call fatal_error(error, "Missing argument for output format")
            exit
         end if
         output_format = get_filetype("."//arg)
      case("--normalize")
         normalize = .true.
      end select
   end do

   if (.not.(allocated(input).and.(allocated(output)))) then
      if (.not.allocated(error)) then
         call help(output_unit)
         error stop
      end if
   end if

end subroutine get_arguments


end program main
