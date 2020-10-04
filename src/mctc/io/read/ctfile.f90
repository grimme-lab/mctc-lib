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

module mctc_io_read_ctfile
   use mctc_env_accuracy, only : wp
   use mctc_env_error, only : error_type, fatal_error
   use mctc_io_convert, only : aatoau
   use mctc_io_structure, only : structure_type, new
   use mctc_io_structure_info, only : sdf_data, structure_info
   use mctc_io_symbols, only : to_number, symbol_length
   use mctc_io_utils, only : getline
   implicit none
   private

   public :: read_sdf, read_molfile


contains


subroutine read_sdf(mol, unit, error)

   !> Instance of the molecular structure data
   type(structure_type), intent(out) :: mol

   !> File handle
   integer, intent(in) :: unit

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=:), allocatable :: line
   integer :: stat

   call read_molfile(mol, unit, error)
   if (allocated(error)) return

   stat = 0
   do while(stat == 0)
      call getline(unit, line, stat)
      if (index(line, '$$$$') == 1) exit
   end do
   if (stat /= 0) then
      call fatal_error(error, "Failed while reading SDF key-value pairs")
      return
   end if

end subroutine read_sdf


subroutine read_molfile(mol, unit, error)

   !> Instance of the molecular structure data
   type(structure_type), intent(out) :: mol

   !> File handle
   integer, intent(in) :: unit

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=:), allocatable :: line
   character(len=:), allocatable :: name
   integer :: i, iatom, jatom, ibond, btype, atomtype
   integer :: stat, length, charge(2, 15)
   integer :: number_of_atoms, number_of_bonds, number_of_atom_lists, &
      &       chiral_flag, number_of_stext_entries, i999
   integer :: list4(4), list12(12)
   real(wp) :: x, y, z
   character(len=2) :: sdf_dim
   character(len=3) :: symbol
   character(len=5) :: v2000
   integer, parameter :: ccc_to_charge(0:7) = [0, +3, +2, +1, 0, -1, -2, -3]
   logical :: two_dim
   character(len=symbol_length), allocatable :: sym(:)
   type(sdf_data), allocatable :: sdf(:)
   type(structure_info) :: info
   real(wp), allocatable :: xyz(:, :)

   two_dim = .false.

   call getline(unit, name, stat)
   call getline(unit, line, stat)
   read(line, '(20x, a2)', iostat=stat) sdf_dim
   if (stat == 0) then
      two_dim = sdf_dim == '2D' .or. sdf_dim == '2d'
   end if
   call getline(unit, line, stat)
   call getline(unit, line, stat)
   read(line, '(3i3, 3x, 2i3, 12x, i3, 1x, a5)', iostat=stat) &
      & number_of_atoms, number_of_bonds, number_of_atom_lists, &
      & chiral_flag, number_of_stext_entries, i999, v2000
   if (stat /= 0) then
      call fatal_error(error, "Cannot read header of molfile")
      return
   end if

   allocate(sdf(number_of_atoms))
   allocate(xyz(3, number_of_atoms))
   allocate(sym(number_of_atoms))

   do iatom = 1, number_of_atoms
      call getline(unit, line, stat)
      read(line, '(3f10.4, 1x, a3, i2, 11i3)', iostat=stat) &
         & x, y, z, symbol, list12
      if (stat /= 0) then
         call fatal_error(error, "Cannot read coordinates from connection table")
         return
      end if
      atomtype = to_number(symbol)
      if (atomtype == 0) then
         call fatal_error(error, "Unknown atom type '"//trim(symbol)//"' in connection table")
         return
      end if
      xyz(:, iatom) = [x, y, z] * aatoau
      sym(iatom) = trim(symbol)
      sdf(iatom)%isotope = list12(1)
      sdf(iatom)%charge = ccc_to_charge(list12(2)) ! drop doublet radical
      sdf(iatom)%hydrogens = list12(4)
      sdf(iatom)%valence = list12(6)
   end do

   !call mol%bonds%allocate(size=number_of_bonds, order=3)
   do ibond = 1, number_of_bonds
      call getline(unit, line, stat)
      read(line, '(7i3)', iostat=stat) &
         & iatom, jatom, btype, list4
      if (stat /= 0) then
         call fatal_error(error, "Cannot read topology from connection table")
         return
      end if
      !call mol%bonds%push_back([iatom, jatom, btype])
   end do

   do while(stat == 0)
      call getline(unit, line, stat)
      if (index(line, 'M  END') == 1) exit
      if (index(line, 'M  CHG') == 1) then
         read(line(7:9), *) length
         read(line(10:), '(*(1x, i3, 1x, i3))') (charge(:, i), i=1, length)
         do i = 1, length
            sdf(charge(1, i))%charge = charge(2, i)
         end do
      end if
   end do
   if (stat /= 0) then
      call fatal_error(error, "Cannot read connection table")
      return
   end if

   info = structure_info(two_dimensional=two_dim, &
      & missing_hydrogen=any(sdf%hydrogens > 1))
   call new(mol, sym, xyz, charge=real(sum(sdf%charge), wp), info=info)
   call move_alloc(sdf, mol%sdf)
   !if (len(name) > 0) mol%name = name

end subroutine read_molfile


end module mctc_io_read_ctfile
