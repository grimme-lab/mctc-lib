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

module mctc_io_read_pdb
   use mctc_env_accuracy, only : wp
   use mctc_env_error, only : error_type, fatal_error
   use mctc_io_convert, only : aatoau
   use mctc_io_resize, only : resize
   use mctc_io_symbols, only : to_number, symbol_length
   use mctc_io_structure, only : structure_type, new
   use mctc_io_structure_info, only : pdb_data, resize
   use mctc_io_utils, only : getline
   implicit none
   private

   public :: read_pdb


contains


subroutine read_pdb(self, unit, error)

   !> Instance of the molecular structure data
   type(structure_type),intent(out) :: self

   !> File handle
   integer,intent(in) :: unit

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: pdb_format = &
      &  '(6x,i5,1x,a4,a1,a3,1x,a1,i4,a1,3x,3f8.3,2f6.2,6x,a4,a2,a2)'
   integer, parameter :: p_initial_size = 1000 ! this is going to be a proteine

   integer :: iatom, jatom, iresidue, try, stat, atom_type
   real(wp) :: occ, temp, coords(3)
   real(wp), allocatable :: xyz(:,:)
   character(len=4) :: a_charge
   character(len=:), allocatable :: line
   character(len=symbol_length), allocatable :: sym(:)
   type(pdb_data), allocatable :: pdb(:)

   allocate(sym(p_initial_size), source=repeat(' ', symbol_length))
   allocate(xyz(3, p_initial_size), source=0.0_wp)
   allocate(pdb(p_initial_size), source=pdb_data())

   iatom = 0
   iresidue = 0

   stat = 0
   do while(stat == 0)
      call getline(unit, line, stat)
      if (index(line, 'END') == 1) exit
      if (index(line, 'ATOM') == 1 .or. index(line, 'HETATM') == 1) then
         if (iatom >= size(xyz, 2)) call resize(xyz)
         if (iatom >= size(sym)) call resize(sym)
         if (iatom >= size(pdb)) call resize(pdb)
         iatom = iatom + 1
         pdb(iatom)%het = index(line, 'HETATM') == 1
         read(line, pdb_format) &
            & jatom, pdb(iatom)%name, pdb(iatom)%loc, pdb(iatom)%residue, &
            & pdb(iatom)%chains, pdb(iatom)%residue_number, pdb(iatom)%code, &
            & coords, occ, temp, pdb(iatom)%segid, sym(iatom), a_charge
         xyz(:,iatom) = coords * aatoau
         atom_type = to_number(sym(iatom))
         if (atom_type == 0) then
            try = scan(pdb(iatom)%name, 'HCNOSPF')
            if (try > 0) sym(iatom) = pdb(iatom)%name(try:try)//' '
            pdb(iatom)%charge = 0
         else
            read(a_charge(1:1), *, iostat=stat) pdb(iatom)%charge
            if (stat /= 0) then
               stat = 0
               pdb(iatom)%charge = 0
            else
               if (a_charge(2:2) == '-') pdb(iatom)%charge = -pdb(iatom)%charge
            end if
         end if         
      end if
   end do
   if (stat /= 0) then
      call fatal_error(error, "could not read in coordinates, last line was: '"//line//"'")
      return
   end if

   call new(self, sym(:iatom), xyz(:, :iatom))
   self%pdb = pdb(:iatom)
   self%charge = sum(pdb(:iatom)%charge)

   if (.not.all(self%num > 0)) then
      call fatal_error(error, "invalid atom type found")
      return
   end if

   ! since PDB is used for biomolecules, this is a sensible check (prevents GIGO)
   if (.not.any(self%num == 1)) then
      call fatal_error(error, "You get no calculation today, please add hydrogen atoms first")
      return
   end if

end subroutine read_pdb


end module mctc_io_read_pdb
