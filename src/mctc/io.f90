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

!> Input and output module for molecular structure data.
!>
!> This module provides the main interface for reading and writing molecular
!> structures in various geometry file formats. It exports:
!>
!> - [[structure_type]]: Central data structure for molecular and periodic systems
!> - [[read_structure]]: Read structures from files or formatted units
!> - [[write_structure]]: Write structures to files or formatted units
!> - [[filetype]]: Enumerator for supported file formats
!> - [[to_symbol]] / [[to_number]]: Convert between element symbols and atomic numbers
!>
!> The file format is automatically detected from file extensions, or can be
!> explicitly specified using the [[filetype]] enumerator. Use [[get_filetype]]
!> to translate file names to format identifiers.
!>
!> Supported formats include xyz, mol/sdf, pdb, Turbomole coord, VASP POSCAR,
!> DFTB+ gen, Gaussian external, QCSchema JSON, Chemical JSON, Pymatgen JSON,
!> FHI-aims geometry.in, and Q-Chem molecule blocks.
!>
!> Example usage:
!>
!>```f90
!> use mctc_io
!> use mctc_env
!> type(structure_type) :: mol
!> type(error_type), allocatable :: error
!>
!> call read_structure(mol, "input.xyz", error)
!> if (allocated(error)) stop error%message
!>
!> call write_structure(mol, "output.mol", error)
!>```
module mctc_io
   use mctc_io_filetype, only : filetype, get_filetype
   use mctc_io_read, only : read_structure
   use mctc_io_structure, only : structure_type, new_structure, new
   use mctc_io_symbols, only : to_symbol, to_number
   use mctc_io_write, only : write_structure
   implicit none
   private

   public :: filetype, get_filetype
   public :: read_structure, write_structure
   public :: structure_type, new_structure, new
   public :: to_symbol, to_number


contains
end module mctc_io
