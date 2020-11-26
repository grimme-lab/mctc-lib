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

!> Input and output module of the tool chain library.
!>
!> This module exports the basic [[structure_type]] as well as routines
!> to read it from a file or formatted unit ([[read_structure]]) or write
!> it to a formatted unit ([[write_structure]]).
!>
!> Both [[read_structure]] and [[write_structure]] take format hints from
!> the filetype enumerator. File names can be translated to the respective
!> enumerator by using the [[get_filetype]] function. This can be useful in
!> case the caller routine wants to open the formatted unit itself or uses
!> a non-standard file extension.
module mctc_io
   use mctc_io_filetype, only : filetype, get_filetype
   use mctc_io_read, only : read_structure
   use mctc_io_structure, only : structure_type, new_structure, new
   use mctc_io_write, only : write_structure
   implicit none
   private

   public :: filetype, get_filetype
   public :: read_structure, write_structure
   public :: structure_type, new_structure, new


contains
end module mctc_io
