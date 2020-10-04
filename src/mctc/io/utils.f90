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
   implicit none
   private

   public :: getline


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


end module mctc_io_utils
