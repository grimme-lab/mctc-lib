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

module mctc_io_write_aims
   use mctc_env_accuracy, only : wp
   use mctc_io_convert, only : autoaa
   use mctc_io_structure, only : structure_type
   implicit none
   private

   public :: write_aims

contains

subroutine write_aims(self, unit)

   !> Instance of the molecular structure data
   class(structure_type), intent(in) :: self

   !> File handle
   integer, intent(in) :: unit

   integer :: iat, ilt
   logical :: expo

   expo = maxval(self%xyz) > 1.0e+5 .or. minval(self%xyz) < -1.0e+5
   if (expo) then
      do iat = 1, self%nat
         write(unit, '(a, 1x, 3es24.14, 1x, a)') &
            "atom", self%xyz(:, iat) * autoaa, trim(self%sym(self%id(iat)))
      end do
   else
      do iat = 1, self%nat
         write(unit, '(a, 1x, 3f24.14, 1x, a)') &
            "atom", self%xyz(:, iat) * autoaa, trim(self%sym(self%id(iat)))
      end do
   end if

   if (any(self%periodic)) then
      if (size(self%lattice, 2) /= 3) return
      do ilt = 1, 3
         if (self%periodic(ilt)) then
            write(unit, '(a, 1x, 3f24.14)') &
               "lattice_vector", self%lattice(:, ilt) * autoaa
         end if
      end do
   end if

end subroutine write_aims
   
end module mctc_io_write_aims
