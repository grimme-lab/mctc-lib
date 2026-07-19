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

module mctc_env_timer
   use mctc_env_accuracy, only : wp, i8
   implicit none
   private

   public :: timer_type, format_time

   type :: time_record
      character(len=:), allocatable :: label
      logical :: running = .false.
      real(wp) :: time = 0.0_wp
   end type time_record

   type :: timer_type
      integer :: n = 0
      character(len=:), allocatable :: last
      type(time_record), allocatable :: record(:)
   contains
      procedure :: push
      procedure :: pop
      procedure :: get
   end type timer_type

contains

subroutine push(self, label)
   class(timer_type), intent(inout) :: self
   character(len=*), intent(in) :: label

   integer :: it

   if (.not.allocated(self%record)) call resize(self%record)
   it = find(self%record(:self%n), label)

   if (it == 0) then
      if (self%n >= size(self%record)) then
         call resize(self%record)
      end if

      self%n = self%n + 1
      it = self%n
      self%record(it) = time_record(label)
   end if

   associate(record => self%record(it))
      self%last = record%label
      record%time = record%time + timing() * merge(+1, -1, record%running)
      record%running = .not.record%running
   end associate
end subroutine push


subroutine pop(self)
   class(timer_type), intent(inout) :: self

   integer :: it

   if (.not.allocated(self%record)) return
   it = find(self%record(:self%n), self%last)
   if (it == 0) return

   associate(record => self%record(it))
      record%time = record%time + timing() * merge(+1, -1, record%running)
      record%running = .not.record%running
   end associate
   if (allocated(self%last)) deallocate(self%last)
end subroutine pop


function get(self, label) result(time)
   class(timer_type), intent(in) :: self
   character(len=*), intent(in) :: label
   real(wp) :: time

   integer :: it

   time = 0.0_wp
   if (self%n <= 0) return
   it = find(self%record(:self%n), label)
   if (it == 0) return

   associate(record => self%record(it))
      time = record%time
      if (record%running) then
         time = time + timing()
      end if
   end associate
end function get


pure function find(record, label) result(pos)
   type(time_record), intent(in) :: record(:)
   character(len=*), intent(in), optional :: label
   integer :: pos

   integer :: i

   pos = 0
   if (present(label)) then
      do i = size(record), 1, -1
         if (allocated(record(i)%label)) then
            if (label == record(i)%label) then
               pos = i
               exit
            end if
         end if
      end do
   else
      do i = size(record), 1, -1
         if (record(i)%running) then
            pos = i
            exit
         end if
      end do
   end if
end function find


function format_time(time) result(string)
   real(wp), intent(in) :: time
   character(len=:), allocatable :: string

   real(wp) :: secs
   integer :: mins, hours, days

   secs = time
   days = int(secs/86400.0_wp)
   secs = secs - days*86400.0_wp
   hours = int(secs/3600.0_wp)
   secs = secs - hours*3600.0_wp
   mins = int(secs/60.0_wp)
   secs = time - mins*60.0_wp

   if (days > 0) then
      string = format_string_int(days, '(i0, " d,")')
   else
      string = repeat(" ", 4)
   end if
   if (hours > 0) then
      string = string // format_string_int(hours, '(1x, i2, " h,")')
   else
      string = string // repeat(" ", 6)
   end if
   if (mins > 0) then
      string = string // format_string_int(mins, '(1x, i2, " min,")')
   else
      string = string // repeat(" ", 8)
   end if
   string = string // format_string_real_dp(secs, "(f6.3)")//" sec"
end function format_time


function timing() result(time)
   real(wp) :: time

   integer(i8) :: time_count, time_rate, time_max
   call system_clock(time_count, time_rate, time_max)
   time = real(time_count, wp)/real(time_rate, wp)
end function timing


!> Reallocate list of timing records
pure subroutine resize(var, n)
   !> Instance of the array to be resized
   type(time_record), allocatable, intent(inout) :: var(:)
   !> Dimension of the final array size
   integer, intent(in), optional :: n

   type(time_record), allocatable :: tmp(:)
   integer :: this_size, new_size
   integer, parameter :: initial_size = 20

   if (allocated(var)) then
      this_size = size(var, 1)
      call move_alloc(var, tmp)
   else
      this_size = initial_size
   end if

   if (present(n)) then
      new_size = n
   else
      new_size = this_size + this_size/2 + 1
   end if

   allocate(var(new_size))

   if (allocated(tmp)) then
      this_size = min(size(tmp, 1), size(var, 1))
      var(:this_size) = tmp(:this_size)
      deallocate(tmp)
   end if

end subroutine resize

pure function format_string_int(val, format) result(str)
   integer, intent(in) :: val
   character(len=*), intent(in) :: format
   character(len=:), allocatable :: str

   character(len=128) :: buffer
   integer :: stat

   write(buffer, format, iostat=stat) val
   if (stat == 0) then
      str = trim(buffer)
   else
      str = "*"
   end if
end function format_string_int

pure function format_string_real_dp(val, format) result(str)
   real(wp), intent(in) :: val
   character(len=*), intent(in) :: format
   character(len=:), allocatable :: str

   character(len=128) :: buffer
   integer :: stat

   write(buffer, format, iostat=stat) val
   if (stat == 0) then
      str = trim(buffer)
   else
      str = "*"
   end if
end function format_string_real_dp

end module mctc_env_timer
