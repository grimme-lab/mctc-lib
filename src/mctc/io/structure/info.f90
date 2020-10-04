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

module mctc_io_structure_info
   use mctc_env_accuracy, only : wp
   implicit none
   private

   public :: pdb_data, sdf_data, structure_info
   public :: resize


   !> Atomic pdb data type.
   !>
   !> keeps information from PDB input that is currently not used by the
   !> caller program (like residues or chains) but is needed to write
   !> the PDB output eventually
   !>
   !>     ATOM   2461  HA3 GLY A 153     -10.977  -7.661   2.011  1.00  0.00           H
   !>     TER    2462      GLY A 153
   !>     a6----i5---xa4--aa3-xai4--axxxf8.3----f8.3----f8.3----f6.2--f6.2--xxxxxxa4--a2a2
   !>     HETATM 2463  CHA HEM A 154       9.596 -13.100  10.368  1.00  0.00           C
   type :: pdb_data
      logical :: het = .false.
      integer :: charge = 0
      integer :: residue_number = 0
      character(len=4) :: name = ' '
      character(len=1) :: loc = ' '
      character(len=3) :: residue = ' '
      character(len=1) :: chains = ' '
      character(len=1) :: code = ' '
      character(len=4) :: segid = ' '
   end type pdb_data


   !> SDF atomic data.
   !>
   !> We only support some entries, the rest is simply dropped.
   !> the format is: ddcccssshhhbbbvvvHHHrrriiimmmnnneee
   type :: sdf_data
      integer :: isotope = 0   !< d field
      integer :: charge = 0    !< c field
      integer :: hydrogens = 0 !< h field
      integer :: valence = 0   !< v field
   end type sdf_data


   !> structure input info
   !>
   !> contains informations from different input file formats
   type :: structure_info

      !> Vasp coordinate scaling information
      real(wp) :: scale = 1.0_wp

      !> Vasp selective dynamics keyword is present
      logical :: selective = .false.

      !> SDF 2D structure present
      logical :: two_dimensional = .false.

      !> SDF hydrogen query present or PDB without hydrogen atoms found
      logical :: missing_hydrogen = .false.

      !> Periodic coordinates should use preferrably cartesian coordinates
      logical :: cartesian = .true.

      !> Lattice information should use preferrably lattice vectors
      logical :: lattice = .true.

      !> Unit of the lattice vectors should be in Angstrom if possible
      logical :: angs_lattice = .false.

      !> Unit of the atomic coordinates should be in Angstrom if possible
      logical :: angs_coord = .false.

   end type structure_info


   interface resize
      module procedure resize_pdb_data
   end interface


contains


subroutine resize_pdb_data(var, n)
   type(pdb_data), allocatable, intent(inout) :: var(:)
   integer, intent(in), optional :: n
   type(pdb_data), allocatable :: tmp(:)
   integer :: length, current_length
   current_length = size(var)
   if (current_length > 0) then
      if (present(n)) then
         if (n <= current_length) return
         length = n
      else
         length = current_length + current_length/2 + 1
      endif
      allocate(tmp(length), source=pdb_data())
      tmp(:current_length) = var(:current_length)
      deallocate(var)
      call move_alloc(tmp, var)
   else
      if (present(n)) then
         length = n
      else
         length = 64
      endif
      allocate(var(length), source=pdb_data())
   endif
end subroutine resize_pdb_data


end module mctc_io_structure_info
