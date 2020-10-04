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

!> Reallocation implementation for resizing arrays
module mctc_io_resize
   use mctc_env_accuracy, only : wp
   implicit none
   private

   public :: resize


   !> Overloaded resize interface
   interface resize
      module procedure :: resize_char
      module procedure :: resize_int
      module procedure :: resize_real
      module procedure :: resize_real_2d
   end interface resize


   !> Initial size for dynamic sized arrays
   integer, parameter :: initial_size = 64


contains


!> Reallocate list of integers
pure subroutine resize_int(var, n)

   !> Instance of the array to be resized
   integer, allocatable, intent(inout) :: var(:)

   !> Dimension of the final array size
   integer, intent(in), optional :: n

   integer, allocatable :: tmp(:)
   integer :: this_size, new_size

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

end subroutine resize_int


!> Reallocate list of characters
pure subroutine resize_char(var, n)

   !> Instance of the array to be resized
   character(len=*), allocatable, intent(inout) :: var(:)

   !> Dimension of the final array size
   integer, intent(in), optional :: n

   character(len=:), allocatable :: tmp(:)
   integer :: this_size, new_size

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

end subroutine resize_char


!> Reallocate list of reals
pure subroutine resize_real(var, n)

   !> Instance of the array to be resized
   real(wp), allocatable, intent(inout) :: var(:)

   !> Dimension of the final array size
   integer, intent(in), optional :: n

   real(wp), allocatable :: tmp(:)
   integer :: this_size, new_size

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

end subroutine resize_real


!> Reallocate list of reals
pure subroutine resize_real_2d(var, n)

   !> Instance of the array to be resized
   real(wp), allocatable, intent(inout) :: var(:,:)

   !> Dimension of the final array size
   integer, intent(in), optional :: n

   real(wp), allocatable :: tmp(:,:)
   integer :: order, this_size, new_size

   if (allocated(var)) then
      order = size(var, 1)
      this_size = size(var, 2)
      call move_alloc(var, tmp)
   else
      order = 3
      this_size = initial_size
   end if

   if (present(n)) then
      new_size = n
   else
      new_size = this_size + this_size/2 + 1
   end if

   allocate(var(order, new_size))

   if (allocated(tmp)) then
      this_size = min(size(tmp, 2), size(var, 2))
      var(:, :this_size) = tmp(:, :this_size)
      deallocate(tmp)
   end if

end subroutine resize_real_2d


end module mctc_io_resize
