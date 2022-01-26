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

module mctc_io_utils
   use mctc_env_accuracy, only : wp
   use mctc_env_error, only : error_type, fatal_error
   implicit none
   private

   public :: getline, next_line
   public :: token_type, next_token, read_token, read_next_token
   public :: io_error, io2_error
   public :: filename, to_string


   !> Text token
   type :: token_type
      !> Begin of sequence
      integer :: first
      !> End of sequence
      integer :: last
   end type token_type


   interface read_token
      module procedure :: read_token_int
      module procedure :: read_token_real
   end interface read_token

   interface read_next_token
      module procedure :: read_next_token_int
      module procedure :: read_next_token_real
   end interface read_next_token


contains


subroutine getline(unit, line, iostat, iomsg)

   !> Formatted IO unit
   integer, intent(in) :: unit

   !> Line to read
   character(len=:), allocatable, intent(out) :: line

   !> Status of operation
   integer, intent(out) :: iostat

   !> Error message
   character(len=:), allocatable, optional :: iomsg

   integer, parameter :: bufsize = 512
   character(len=bufsize) :: buffer
   character(len=bufsize) :: msg
   integer :: size
   integer :: stat

   allocate(character(len=0) :: line)
   do
      read(unit, '(a)', advance='no', iostat=stat, iomsg=msg, size=size) &
         & buffer
      if (stat > 0) exit
      line = line // buffer(:size)
      if (stat < 0) then
         if (is_iostat_eor(stat)) then
            stat = 0
         end if
         exit
      end if
   end do

   if (stat /= 0) then
      if (present(iomsg)) iomsg = trim(msg)
   end if
   iostat = stat

end subroutine getline


!> Convenience function to read a line and update associated descriptors
subroutine next_line(unit, line, pos, lnum, iostat, iomsg)

   !> Formatted IO unit
   integer, intent(in) :: unit

   !> Line to read
   character(len=:), allocatable, intent(out) :: line

   !> Current position in line
   integer, intent(out) :: pos

   !> Current line number
   integer, intent(inout) :: lnum

   !> Status of operation
   integer, intent(out) :: iostat

   !> Error message
   character(len=:), allocatable, optional :: iomsg

   pos = 0
   call getline(unit, line, iostat, iomsg)
   if (iostat == 0) lnum = lnum + 1
end subroutine next_line


!> Advance pointer to next text token
subroutine next_token(string, pos, token)

   !> String to check
   character(len=*), intent(in) :: string

   !> Current position in string
   integer, intent(inout) :: pos

   !> Token found
   type(token_type), intent(out) :: token

   integer :: start

   if (pos >= len(string)) then
      token = token_type(len(string)+1, len(string)+1)
      return
   end if

   do while(pos < len(string))
      pos = pos + 1
      select case(string(pos:pos))
      case(" ", achar(9), achar(10), achar(13))
         continue
      case default
         exit
      end select
   end do

   start = pos

   do while(pos < len(string))
      pos = pos + 1
      select case(string(pos:pos))
      case(" ", achar(9), achar(10), achar(13))
         pos = pos - 1
         exit
      case default
         continue
      end select
   end do

   token = token_type(start, pos)
end subroutine next_token


function filename(unit)
   integer, intent(in) :: unit
   character(len=:), allocatable :: filename

   character(len=512) :: buffer
   logical :: opened

   filename = "(input)"
   if (unit /= -1) then
      buffer = ""
      inquire(unit=unit, opened=opened, name=buffer)
      if (opened .and. len_trim(buffer) > 0) then
         filename = trim(buffer)
      end if
   end if
end function


!> Create new IO error
subroutine io_error(error, message, source, token, filename, line, label)

   !> Error handler
   type(error_type), allocatable, intent(out) :: error

   !> Main error message
   character(len=*), intent(in) :: message

   !> String representing the offending input
   character(len=*), intent(in) :: source

   !> Last processed token
   type(token_type), intent(in) :: token

   !> Name of the input file
   character(len=*), intent(in), optional :: filename

   !> Line number
   integer, intent(in), optional :: line

   !> Label of the offending statement
   character(len=*), intent(in), optional :: label

   character(len=*), parameter :: nl = new_line('a')
   integer :: offset, lnum, width
   character(len=:), allocatable :: string

   lnum = 1
   if (present(line)) lnum = line
   offset = integer_width(lnum)
   width = token%last - token%first + 1

   string = "Error: " // message

   if (present(filename)) then
      string = string // nl // &
         repeat(" ", offset)//"--> "//filename

      string = string // ":" // to_string(lnum)
      if (token%first > 0 .and. token%last >= token%first) then
         string = string // &
            ":"//to_string(token%first)
         if (token%last > token%first) string = string//"-"//to_string(token%last)
      end if
   end if

   string = string // nl //&
      repeat(" ", offset+1)//"|"//nl//&
      to_string(lnum)//" | "//source//nl//&
      repeat(" ", offset+1)//"|"//repeat(" ", token%first)//repeat("^", width)

   if (present(label)) then
      string = string // " " // label
   end if

   string = string // nl //&
      repeat(" ", offset+1)//"|"

   call fatal_error(error, string)
end subroutine io_error


!> Create new IO error
subroutine io2_error(error, message, source1, source2, token1, token2, filename, &
      & line1, line2, label1, label2)

   !> Error handler
   type(error_type), allocatable, intent(out) :: error

   !> Main error message
   character(len=*), intent(in) :: message

   !> String representing the offending input
   character(len=*), intent(in) :: source1, source2

   !> Last processed token
   type(token_type), intent(in) :: token1, token2

   !> Name of the input file
   character(len=*), intent(in), optional :: filename

   !> Line number
   integer, intent(in), optional :: line1, line2

   !> Label of the offending statement
   character(len=*), intent(in), optional :: label1, label2

   character(len=*), parameter :: nl = new_line('a')
   integer :: offset, lnum1, lnum2, width1, width2
   character(len=:), allocatable :: string

   lnum1 = 1
   lnum2 = 1
   if (present(line1)) lnum1 = line1
   if (present(line2)) lnum2 = line2
   offset = integer_width(max(lnum1, lnum2))
   width1 = token1%last - token1%first + 1
   width2 = token2%last - token2%first + 1

   string = "Error: " // message

   if (present(filename)) then
      string = string // nl // &
         repeat(" ", offset)//"--> "//filename

      string = string // ":" // to_string(lnum2)
      if (token2%first > 0 .and. token2%last >= token2%first) then
         string = string // &
            ":"//to_string(token2%first)
         if (token2%last > token2%first) string = string//"-"//to_string(token2%last)
      end if
   end if

   string = string // nl //&
      repeat(" ", offset+1)//"|"//nl//&
      to_string(lnum1, offset)//" | "//source1//nl//&
      repeat(" ", offset+1)//"|"//repeat(" ", token1%first)//repeat("-", width1)

   if (present(label1)) then
      string = string // " " // label1
   end if

   string = string // nl //&
      repeat(" ", offset+1)//":"//nl//&
      to_string(lnum2)//" | "//source2//nl//&
      repeat(" ", offset+1)//"|"//repeat(" ", token2%first)//repeat("^", width2)

   if (present(label2)) then
      string = string // " " // label2
   end if

   string = string // nl //&
      repeat(" ", offset+1)//"|"

   call fatal_error(error, string)
end subroutine io2_error


pure function integer_width(input) result(width)
   integer, intent(in) :: input
   integer :: width

   integer :: val

   val = input
   width = 0
   do while (val /= 0)
      val = val / 10
      width = width + 1
   end do

end function integer_width


!> Represent an integer as character sequence.
pure function to_string(val, width) result(string)
   integer, intent(in) :: val
   integer, intent(in), optional :: width
   character(len=:), allocatable :: string
   integer, parameter :: buffer_len = range(val)+2
   character(len=buffer_len) :: buffer
   integer :: pos
   integer :: n
   character(len=1), parameter :: numbers(0:9) = &
      ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]

   if (val == 0) then
      if (present(width)) then
         string = repeat(" ", width-1) // numbers(0)
      else
         string = numbers(0)
      end if
      return
   end if

   n = abs(val)
   buffer = ""

   pos = buffer_len + 1
   do while (n > 0)
      pos = pos - 1
      buffer(pos:pos) = numbers(mod(n, 10))
      n = n/10
   end do
   if (val < 0) then
      pos = pos - 1
      buffer(pos:pos) = '-'
   end if

   if (present(width)) then
      string = repeat(" ", max(width-(buffer_len+1-pos), 0)) // buffer(pos:)
   else
      string = buffer(pos:)
   end if
end function to_string


subroutine read_next_token_int(line, pos, token, val, iostat, iomsg)
   character(len=*), intent(in) :: line
   integer, intent(inout) :: pos
   type(token_type), intent(inout) :: token
   integer, intent(out) :: val
   integer, intent(out) :: iostat
   character(len=:), allocatable, intent(out), optional :: iomsg

   character(len=512) :: msg

   call next_token(line, pos, token)
   call read_token(line, token, val, iostat, iomsg)
end subroutine read_next_token_int


subroutine read_token_int(line, token, val, iostat, iomsg)
   character(len=*), intent(in) :: line
   type(token_type), intent(in) :: token
   integer, intent(out) :: val
   integer, intent(out) :: iostat
   character(len=:), allocatable, intent(out), optional :: iomsg

   character(len=512) :: msg

   if (token%first > 0 .and. token%last <= len(line)) then
      read(line(token%first:token%last), *, iostat=iostat, iomsg=msg) val
   else
      iostat = 1
      msg = "No input found"
   end if
   if (present(iomsg)) iomsg = trim(msg)
end subroutine read_token_int


subroutine read_next_token_real(line, pos, token, val, iostat, iomsg)
   character(len=*), intent(in) :: line
   integer, intent(inout) :: pos
   type(token_type), intent(inout) :: token
   real(wp), intent(out) :: val
   integer, intent(out) :: iostat
   character(len=:), allocatable, intent(out), optional :: iomsg

   call next_token(line, pos, token)
   call read_token(line, token, val, iostat, iomsg)
end subroutine read_next_token_real


subroutine read_token_real(line, token, val, iostat, iomsg)
   character(len=*), intent(in) :: line
   type(token_type), intent(in) :: token
   real(wp), intent(out) :: val
   integer, intent(out) :: iostat
   character(len=:), allocatable, intent(out), optional :: iomsg

   character(len=512) :: msg

   if (token%first > 0 .and. token%last <= len(line)) then
      read(line(token%first:token%last), *, iostat=iostat, iomsg=msg) val
   else
      iostat = 1
      msg = "No input found"
   end if
   if (present(iomsg)) iomsg = trim(msg)
end subroutine read_token_real


end module mctc_io_utils
