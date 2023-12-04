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

module mctc_io_write_qchem
   use mctc_env_accuracy, only : wp
   use mctc_io_convert, only : autoaa
   use mctc_io_structure, only : structure_type
   implicit none
   private

   public :: write_qchem

contains

subroutine write_qchem(self, unit)

   !> Instance of the molecular structure data
   class(structure_type), intent(in) :: self

   !> File handle
   integer, intent(in) :: unit

   integer :: iat
   logical :: expo

   write(unit, '(a)') "$molecule"
   write(unit, '(*(1x, i0))') nint(self%charge), self%uhf + 1

   expo = maxval(self%xyz) > 1.0e+5 .or. minval(self%xyz) < -1.0e+5
   if (expo) then
      do iat = 1, self%nat
         write(unit, '(a, 1x, 3es24.14)') &
           self%sym(self%id(iat)), self%xyz(:, iat) * autoaa
      end do
   else
      do iat = 1, self%nat
         write(unit, '(a, 1x, 3f24.14)') &
           self%sym(self%id(iat)), self%xyz(:, iat) * autoaa
      end do
   end if

   write(unit, '(a)') "$end"

end subroutine write_qchem
   
end module mctc_io_write_qchem
