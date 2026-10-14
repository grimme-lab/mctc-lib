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

!> Writer for crystallographic information files (CIF), not yet implemented
module mctc_io_write_cif
   use mctc_env_error, only : error_type, fatal_error
   use mctc_io_structure, only : structure_type
   implicit none
   private

   public :: write_cif

contains

!> Placeholder for a CIF writer, always reports an error
subroutine write_cif(self, unit, error)

   !> Instance of the molecular structure data
   class(structure_type), intent(in) :: self

   !> File handle
   integer, intent(in) :: unit

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   call fatal_error(error, "Writing CIF files is not supported yet")

end subroutine write_cif

end module mctc_io_write_cif
