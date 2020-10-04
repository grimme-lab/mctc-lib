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

module mctc_io_write_gaussian
   use mctc_env_accuracy, only : wp
   use mctc_io_structure, only : structure_type
   implicit none
   private

   public :: write_gaussian_external


contains


subroutine write_gaussian_external(mol, unit)
   type(structure_type), intent(in) :: mol
   integer, intent(in) :: unit
   integer :: iat

   write(unit, '(4i10)') mol%nat, 1, nint(mol%charge), mol%uhf
   do iat = 1, mol%nat
      write(unit, '(i10,4f20.12)') mol%num(mol%id(iat)), mol%xyz(:, iat), 0.0_wp
   end do

end subroutine write_gaussian_external


end module mctc_io_write_gaussian
