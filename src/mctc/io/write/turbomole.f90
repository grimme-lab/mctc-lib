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

module mctc_io_write_turbomole
   use mctc_env_accuracy, only : wp
   use mctc_io_structure, only : structure_type
   implicit none
   private

   public :: write_coord


contains


subroutine write_coord(mol, unit)
   class(structure_type), intent(in) :: mol
   integer, intent(in) :: unit
   integer :: iat, ilt, npbc
   logical :: expo

   write(unit, '(a)') "$coord"
   expo = maxval(mol%xyz) > 1.0e+5 .or. minval(mol%xyz) < -1.0e+5
   if (expo) then
      do iat = 1, mol%nat
         write(unit, '(3es24.14, 6x, a)') mol%xyz(:, iat), trim(mol%sym(mol%id(iat)))
      end do
   else
      do iat = 1, mol%nat
         write(unit, '(3f24.14, 6x, a)') mol%xyz(:, iat), trim(mol%sym(mol%id(iat)))
      end do
   end if
   if (any([nint(mol%charge), mol%uhf] /= 0)) then
      write(unit, '(a, *(1x, a, "=", i0))') &
         "$eht", "charge", nint(mol%charge), "unpaired", mol%uhf
   end if
   if (any(mol%periodic)) then
      write(unit, '(a, 1x, i0)') "$periodic", count(mol%periodic)
      npbc = count(mol%periodic)
      if (size(mol%lattice, 2) == 3) then
         write(unit, '(a)') "$lattice bohr"
         do ilt = 1, npbc
            write(unit, '(3f20.14)') mol%lattice(:npbc, ilt)
         end do
      end if
   end if
   write(unit, '(a)') "$end"

end subroutine write_coord


end module mctc_io_write_turbomole
