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
   use mctc_io_convert, only : autoaa
   implicit none
   private

   public :: write_coord

contains

subroutine write_coord(mol, unit)
   class(structure_type), intent(in) :: mol
   integer, intent(in) :: unit
   integer :: iat, ilt, npbc
   logical :: expo

   real(wp) :: conv_fac
   logical :: angs

   angs = mol%info%angs_coord
   conv_fac = 1.0_wp
   if (angs) conv_fac = autoaa

   if (angs) then
      write(unit, '(a)') "$coord angs"
   else
      write(unit, '(a)') "$coord"
   end if
   expo = maxval(mol%xyz) > 1.0e+5 .or. minval(mol%xyz) < -1.0e+5
   if (expo) then
      do iat = 1, mol%nat
         write(unit, '(3es24.14, 6x, a)') mol%xyz(:, iat) * conv_fac, &
            trim(mol%sym(mol%id(iat)))
      end do
   else
      do iat = 1, mol%nat
         write(unit, '(3f24.14, 6x, a)') mol%xyz(:, iat) * conv_fac, &
            trim(mol%sym(mol%id(iat)))
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
         if (angs) then
            write(unit, '(a)') "$lattice angs"
         else
            write(unit, '(a)') "$lattice bohr"
         end if
         do ilt = 1, npbc
            write(unit, '(3f20.14)') mol%lattice(:npbc, ilt) * conv_fac
         end do
      end if
   end if
   write(unit, '(a)') "$end"

end subroutine write_coord

end module mctc_io_write_turbomole