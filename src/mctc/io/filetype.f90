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

!> File type support
!>
!> This module provides file type identification for molecular structure files.
!> Use [[get_filetype]] to determine the format from a filename, or use the
!> [[mctc_io_filetype:filetype]] enumerator directly when the format is known.
module mctc_io_filetype
   use mctc_io_utils, only : to_lower
   implicit none
   private

   public :: filetype, get_filetype


   !> Possible file types
   type :: enum_filetype

      !> Unknown file type
      integer :: unknown = 0

      !> xyz-format
      integer :: xyz = 1

      !> Turbomole coordinate format
      integer :: tmol = 2

      !> mol-format
      integer :: molfile = 3

      !> Vasp coordinate input
      integer :: vasp = 4

      !> Protein database format
      integer :: pdb = 5

      !> Structure data format
      integer :: sdf = 6

      !> GenFormat of DFTB+
      integer :: gen = 7

      !> Gaussian external format
      integer :: gaussian = 8

      !> QCSchema JSON file
      integer :: qcschema = 9

      !> FHI-aims geometry.in format
      integer :: aims = 10

      !> Q-Chem molecule format
      integer :: qchem = 11

      !> Chemical JSON format (avogadro)
      integer :: cjson = 12

      !> Pymatgen JSON format
      integer :: pymatgen = 13

      !> General JSON format
      integer :: json = 14

   end type enum_filetype

   !> File type enumerator
   !>
   !> | Enumerator | Format | Description |
   !> |------------|--------|-------------|
   !> | `filetype%xyz` | xyz | Xmol/xyz format |
   !> | `filetype%tmol` | Turbomole | Turbomole coord format |
   !> | `filetype%molfile` | MOL | MDL Molfile V2000/V3000 |
   !> | `filetype%sdf` | SDF | Structure Data File |
   !> | `filetype%vasp` | VASP | POSCAR/CONTCAR format |
   !> | `filetype%pdb` | PDB | Protein Data Bank format |
   !> | `filetype%gen` | gen | DFTB+ genFormat |
   !> | `filetype%gaussian` | Gaussian | External program format |
   !> | `filetype%qcschema` | QCSchema | MolSSI QCSchema JSON |
   !> | `filetype%cjson` | Chemical JSON | Avogadro Chemical JSON |
   !> | `filetype%pymatgen` | Pymatgen | Pymatgen JSON format |
   !> | `filetype%aims` | FHI-aims | geometry.in format |
   !> | `filetype%qchem` | Q-Chem | Molecule block format |
   type(enum_filetype), parameter :: filetype = enum_filetype()


contains


elemental function get_filetype(file) result(ftype)

   !> Name of the file
   character(len=*), intent(in) :: file

   !> File type from extension
   integer :: ftype

   integer :: iext, isep

   ftype = filetype%unknown
   iext = index(file, '.', back=.true.)
   isep = scan(file, '\/', back=.true.)

   if (iext > isep .and. iext > 0) then
      select case(to_lower(file(iext+1:)))
      case('coord', 'tmol')
         ftype = filetype%tmol
      case('xyz', 'log')
         ftype = filetype%xyz
      case('mol')
         ftype = filetype%molfile
      case('sdf')
         ftype = filetype%sdf
      case('poscar', 'contcar', 'vasp')
         ftype = filetype%vasp
      case('pdb')
         ftype = filetype%pdb
      case('gen')
         ftype = filetype%gen
      case('ein')
         ftype = filetype%gaussian
      case('qcjson')
         ftype = filetype%qcschema
      case('cjson')
         ftype = filetype%cjson
      case('qchem')
         ftype = filetype%qchem
      case('pmgjson')
         ftype = filetype%pymatgen
      case('json')
         ftype = filetype%json
      end select
      if (ftype /= filetype%unknown) return
   else
      iext = len(file) + 1
   end if

   if (iext > isep) then
      if (file(isep+1:) == 'geometry.in') then
         ftype = filetype%aims
      end if

      select case(to_lower(file(isep+1:iext-1)))
      case('geometry.in')
         ftype = filetype%aims
      case('coord')
         ftype = filetype%tmol
      case('poscar', 'contcar')
         ftype = filetype%vasp
      end select
   end if

end function get_filetype


end module mctc_io_filetype
