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
   integer :: iat

   write(unit, '(a)') "$coord"
   do iat = 1, mol%nat
      write(unit, '(3es24.14, 6x, a)') mol%xyz(:, iat), trim(mol%sym(mol%id(iat)))
   enddo
   write(unit, '(a, 1x, i0)') "$periodic", count(mol%periodic)
   if (any(mol%periodic)) then
      write(unit, '(a)') "$lattice bohr"
      write(unit, '(3f20.14)') mol%lattice
   endif
   write(unit, '(a)') "$end"

end subroutine write_coord


end module mctc_io_write_turbomole
