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

!> Readers for `.CHRG` and `.UHF` auxiliary dot-files
module mctc_io_dotfiles
   use, intrinsic :: ieee_arithmetic, only : ieee_is_finite
   use mctc_env_accuracy, only : wp
   use mctc_env_error, only : error_type, fatal_error
   use mctc_env_system, only : dirname, join_path
   use mctc_io_structure, only : structure_type
   use mctc_io_utils, only : filename, getline, io_error, next_token, &
      & read_token, token_type
   implicit none
   private

   public :: read_dotfiles, read_chrg, read_uhf


   !> Read molecular charge from a `.CHRG` file or formatted unit
   interface read_chrg
      module procedure :: read_chrg_from_file
      module procedure :: read_chrg_from_unit
   end interface read_chrg


   !> Read number of unpaired electrons from a `.UHF` file or formatted unit
   interface read_uhf
      module procedure :: read_uhf_from_file
      module procedure :: read_uhf_from_unit
   end interface read_uhf


contains


!> Read `.CHRG` and `.UHF` dot-files from the directory of an input path
!> if they exist on disk.
subroutine read_dotfiles(self, input, error, read_chrg, read_uhf, &
      & chrg_file, uhf_file)

   !> Molecular structure data to update
   type(structure_type), intent(inout) :: self

   !> Input geometry file path (or "-" for current directory)
   character(len=*), intent(in) :: input

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   !> Whether to check and read `.CHRG` (default: `.true.`)
   logical, intent(in), optional :: read_chrg

   !> Whether to check and read `.UHF` (default: `.true.`)
   logical, intent(in), optional :: read_uhf

   !> Resolved `.CHRG` path if the file existed on disk
   character(len=:), allocatable, intent(out), optional :: chrg_file

   !> Resolved `.UHF` path if the file existed on disk
   character(len=:), allocatable, intent(out), optional :: uhf_file

   character(len=:), allocatable :: dir, path, found_chrg, found_uhf
   logical :: do_chrg, do_uhf, exist
   real(wp) :: charge
   integer :: uhf

   do_chrg = .true.
   if (present(read_chrg)) do_chrg = read_chrg
   do_uhf = .true.
   if (present(read_uhf)) do_uhf = read_uhf

   dir = dirname(input)

   if (do_chrg) then
      path = join_path(dir, ".CHRG")
      inquire(file=path, exist=exist)
      if (exist) then
         call read_chrg_from_file(charge, path, error)
         if (allocated(error)) return

         found_chrg = path
      end if
   end if

   if (do_uhf) then
      path = join_path(dir, ".UHF")
      inquire(file=path, exist=exist)
      if (exist) then
         call read_uhf_from_file(uhf, path, error)
         if (allocated(error)) return

         found_uhf = path
      end if
   end if

   if (allocated(found_chrg)) then
      self%charge = charge
      if (present(chrg_file)) chrg_file = found_chrg
   end if

   if (allocated(found_uhf)) then
      self%uhf = uhf
      if (present(uhf_file)) uhf_file = found_uhf
   end if

end subroutine read_dotfiles


!> Read molecular charge from a file
subroutine read_chrg_from_file(charge, file, error)

   !> Molecular charge
   real(wp), intent(out) :: charge

   !> Path to `.CHRG` file
   character(len=*), intent(in) :: file

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   integer :: unit, stat
   logical :: exist

   charge = 0.0_wp
   open(file=file, newunit=unit, status="old", action="read", iostat=stat)
   if (stat /= 0) then
      inquire(file=file, exist=exist)
      if (.not. exist) then
         call fatal_error(error, "File '"//file//"' cannot be found")
      else
         call fatal_error(error, "Cannot open '"//file//"'")
      end if
      return
   end if

   call read_chrg_from_unit(charge, unit, error)
   close(unit)

end subroutine read_chrg_from_file


!> Read molecular charge from an open formatted unit
subroutine read_chrg_from_unit(charge, unit, error)

   !> Molecular charge
   real(wp), intent(out) :: charge

   !> Formatted input unit
   integer, intent(in) :: unit

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=:), allocatable :: line
   type(token_type) :: token
   integer :: lnum, stat

   charge = 0.0_wp
   call next_value_token(unit, "charge", line, token, lnum, error)
   if (allocated(error)) return

   stat = verify(line(token%first:token%last), "+-.0123456789eEdD")
   if (stat == 0) then
      call read_token(line, token, charge, stat)
   end if

   if (stat /= 0 .or. .not. ieee_is_finite(charge)) then
      call io_error(error, "Cannot read charge from file", line, token, &
         & filename(unit), lnum, "expected real value")
   end if

end subroutine read_chrg_from_unit


!> Read number of unpaired electrons from a file
subroutine read_uhf_from_file(uhf, file, error)

   !> Number of unpaired electrons
   integer, intent(out) :: uhf

   !> Path to `.UHF` file
   character(len=*), intent(in) :: file

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   integer :: unit, stat
   logical :: exist

   uhf = 0
   open(file=file, newunit=unit, status="old", action="read", iostat=stat)
   if (stat /= 0) then
      inquire(file=file, exist=exist)
      if (.not. exist) then
         call fatal_error(error, "File '"//file//"' cannot be found")
      else
         call fatal_error(error, "Cannot open '"//file//"'")
      end if
      return
   end if

   call read_uhf_from_unit(uhf, unit, error)
   close(unit)

end subroutine read_uhf_from_file


!> Read number of unpaired electrons from an open formatted unit
subroutine read_uhf_from_unit(uhf, unit, error)

   !> Number of unpaired electrons
   integer, intent(out) :: uhf

   !> Formatted input unit
   integer, intent(in) :: unit

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=:), allocatable :: line
   type(token_type) :: token
   integer :: lnum, stat

   uhf = 0
   call next_value_token(unit, "spin", line, token, lnum, error)
   if (allocated(error)) return

   stat = verify(line(token%first:token%last), "+-0123456789")
   if (stat == 0) then
      call read_token(line, token, uhf, stat)
   end if

   if (stat /= 0) then
      call io_error(error, "Cannot read spin from file", line, token, &
         & filename(unit), lnum, "expected integer value")
   end if

end subroutine read_uhf_from_unit


!> Read the first non-empty, non-comment record and extract its single value token
subroutine next_value_token(unit, quantity, line, token, lnum, error)

   !> Formatted input unit
   integer, intent(in) :: unit

   !> Quantity label ("charge" or "spin") for diagnostics
   character(len=*), intent(in) :: quantity

   !> Line containing the extracted token
   character(len=:), allocatable, intent(out) :: line

   !> Extracted value token bounds
   type(token_type), intent(out) :: token

   !> Physical 1-based line number
   integer, intent(out) :: lnum

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(token_type) :: extra
   integer :: stat, pos, ic, last

   lnum = 0
   token = token_type(0, 0)

   do
      call getline(unit, line, stat)
      if (is_iostat_end(stat) .and. len(line) == 0) exit

      lnum = lnum + 1
      if (stat /= 0 .and. .not. is_iostat_end(stat)) then
         call fatal_error(error, "Cannot read "//quantity//" from '"//filename(unit)//"'")
         return
      end if

      ic = scan(line, "#!")
      last = merge(ic - 1, len(line), ic > 0)

      pos = 0
      call next_token(line(:last), pos, token)
      if (token%first > last) then
         if (is_iostat_end(stat)) exit
         cycle
      end if

      call next_token(line(:last), pos, extra)
      if (extra%first <= last) then
         call io_error(error, "Cannot read "//quantity//" from file", &
            & line, extra, filename(unit), lnum, "unexpected trailing token")
         return
      end if
      return
   end do

   call io_error(error, "Cannot read "//quantity//" from file", &
      & "", token_type(0, 0), filename(unit), max(lnum, 1), "no value found")

end subroutine next_value_token


end module mctc_io_dotfiles
