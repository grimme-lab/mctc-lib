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

!> Module collecting commands to conveniently interface with system commands
module mctc_env_system
   implicit none
   private

   public :: get_argument, get_variable
   public :: is_windows, is_unix
   public :: dirname, join_path


contains


!> Obtain the command line argument at a given index
subroutine get_argument(idx, arg)

   !> Index of command line argument, range [0:command_argument_count()]
   integer, intent(in) :: idx

   !> Command line argument
   character(len=:), allocatable, intent(out) :: arg

   integer :: length, stat

   call get_command_argument(idx, length=length, status=stat)
   if (stat /= 0) then
      return
   end if

   allocate(character(len=length) :: arg, stat=stat)
   if (stat /= 0) then
      return
   end if

   if (length > 0) then
      call get_command_argument(idx, arg, status=stat)
      if (stat /= 0) then
         deallocate(arg)
         return
      end if
   end if

end subroutine get_argument


!> Obtain the value of an environment variable
subroutine get_variable(var, val)

   !> Name of variable
   character(len=*), intent(in) :: var

   !> Value of variable
   character(len=:), allocatable, intent(out) :: val

   integer :: length, stat

   call get_environment_variable(var, length=length, status=stat)
   if (stat /= 0) then
      return
   end if

   allocate(character(len=length) :: val, stat=stat)
   if (stat /= 0) then
      return
   end if

   if (length > 0) then
      call get_environment_variable(var, val, status=stat)
      if (stat /= 0) then
         deallocate(val)
         return
      end if
   end if

end subroutine get_variable


!> Try to determine if we run on Windows and don't have POSIX compliance around
function is_windows()

   !> Operating system seems to be Windows
   logical :: is_windows

   character(len=:), allocatable :: tmp

   is_windows = .false.
   call get_variable("OS", tmp)
   if (allocated(tmp)) then
      is_windows = index(tmp, "Windows_NT") > 0
   end if
   if (.not.is_windows) then
      call get_variable("OSTYPE", tmp)
      if (allocated(tmp)) then
         is_windows = index(tmp, "win") > 0 .or. index(tmp, "msys") > 0
      end if
   end if

end function is_windows


!> Try to determine if we run on Unix and probably can rely on POSIX compliance
function is_unix()

   !> Operating system seems to be Unix
   logical :: is_unix

   is_unix = .not. is_windows()

end function is_unix


!> Extract directory component from a file path ("." if none is present)
pure function dirname(path) result(dir)

   !> Input file path
   character(len=*), intent(in) :: path

   !> Extracted directory path
   character(len=:), allocatable :: dir

   integer :: isep, last

   isep = scan(trim(path), "/\", back=.true.)
   if (isep == 0) then
      dir = "."
      return
   end if

   last = verify(path(:isep), "/\", back=.true.)
   if (last == 0) then
      dir = path(1:1)
   else if (last == 2 .and. path(2:2) == ":") then
      dir = path(1:3)
   else
      dir = path(1:last)
   end if

end function dirname


!> Join directory and leaf path segments with OS file separator
function join_path(a1, a2) result(path)

   !> Leading path segment
   character(len=*), intent(in) :: a1

   !> Trailing path segment
   character(len=*), intent(in) :: a2

   !> Joined path string
   character(len=:), allocatable :: path

   character :: filesep
   integer :: n1

   n1 = len_trim(a1)
   if (n1 == 0) then
      path = trim(a2)
      return
   end if

   if (a1(n1:n1) == "/" .or. a1(n1:n1) == "\") then
      path = a1(:n1) // trim(a2)
      return
   end if

   filesep = merge("\", "/", is_windows())
   path = a1(:n1) // filesep // trim(a2)

end function join_path


end module mctc_env_system
