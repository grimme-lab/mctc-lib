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

module mctc_io
   use mctc_io_filetype, only : filetype, get_filetype
   use mctc_io_read, only : read_structure
   use mctc_io_structure, only : structure_type, new_structure
   use mctc_io_write, only : write_structure
   implicit none
   private

   public :: filetype, get_filetype
   public :: read_structure, write_structure
   public :: structure_type, new_structure


contains
end module mctc_io
