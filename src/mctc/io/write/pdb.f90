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

module mctc_io_write_pdb
   use mctc_env_accuracy, only : wp
   use mctc_io_convert, only : autoaa
   use mctc_io_structure, only : structure_type
   implicit none
   private

   public :: write_pdb


contains


subroutine write_pdb(mol, unit, number)
   type(structure_type), intent(in) :: mol
   integer, intent(in) :: unit
   integer, intent(in), optional :: number
   character(len=6) :: w1
   character(len=4) :: sym
   character(len=2) :: a_charge
   character(len=1) :: last_chain
   logical :: last_het
   integer :: offset, iat, jat
   real(wp) :: xyz(3)
   character(len=*), parameter :: pdb_format = &
      &  '(a6,i5,1x,a4,a1,a3,1x,a1,i4,a1,3x,3f8.3,2f6.2,6x,a4,a2,a2)'


   if (present(number)) write(unit, '("MODEL ",4x,i4)') number
   if (allocated(mol%pdb)) then
      offset = 0
      last_chain = mol%pdb(1)%chains
      last_het = mol%pdb(1)%het
      do iat = 1, mol%nat

         ! handle the terminator
         if (mol%pdb(iat)%het .neqv. last_het) then
            write(unit, '("TER   ",i5,6x,a3,1x,a1,i4)') iat + offset, &
               &  mol%pdb(iat-1)%residue, last_chain, mol%pdb(iat)%residue_number
            last_het = .not.last_het
            offset = offset+1
         else if (mol%pdb(iat)%chains /= last_chain) then
            write(unit, '("TER   ",i5,6x,a3,1x,a1,i4)') iat + offset, &
               &  mol%pdb(iat-1)%residue, last_chain, mol%pdb(iat)%residue_number
            offset = offset+1
         endif

         jat = iat + offset
         if (mol%pdb(iat)%het) then
            w1 = "HETATM"
         else
            w1 = "ATOM  "
         endif


         sym = adjustr(mol%sym(mol%id(iat))(1:2))
         xyz = mol%xyz(:,iat) * autoaa
         if (mol%pdb(iat)%charge < 0) then
            write(a_charge, '(i1,"-")') abs(mol%pdb(iat)%charge)
         else if (mol%pdb(iat)%charge > 0) then
            write(a_charge, '(i1,"+")') abs(mol%pdb(iat)%charge)
         else
            a_charge = '  '
         endif

         write(unit, pdb_format) &
            &  w1, jat, mol%pdb(iat)%name, mol%pdb(iat)%loc, &
            &  mol%pdb(iat)%residue, mol%pdb(iat)%chains, mol%pdb(iat)%residue_number, &
            &  mol%pdb(iat)%code, xyz, 1.0_wp, 0.0_wp, mol%pdb(iat)%segid, &
            &  sym, a_charge
      enddo
   else
      do iat = 1, mol%nat
         w1 = "HETATM"
         sym = adjustr(mol%sym(mol%id(iat))(1:2))
         xyz = mol%xyz(:,iat) * autoaa
         a_charge = '  '

         write(unit, pdb_format) &
            &  w1, iat, sym, " ", &
            &  "UNK", "A", 1, " ", xyz, 1.0_wp, 0.0_wp, "    ", &
            &  sym, "  "
      enddo
   end if

   if (present(number)) then
      write(unit, '("ENDMDL")')
   else
      write(unit, '("END")')
   endif

end subroutine write_pdb


end module mctc_io_write_pdb
