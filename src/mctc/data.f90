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

!> @dir mctc/data
!> Contains element data used for defining interactions.

!> @file mctc/data.f90
!> Reexports access to element-specific data.

!> Element data module providing atomic properties.
!>
!> This module provides access to element-specific data indexed by atomic number.
!> All radii are returned in atomic units (Bohr).
!>
!> Available functions:
!>
!> - [[get_covalent_rad]]: Covalent radii in Bohr
!> - [[get_vdw_rad]]: van der Waals radii in Bohr
!> - [[get_atomic_rad]]: Atomic radii in Bohr
!> - [[get_pauling_en]]: Pauling electronegativities
!>
!> Example usage:
!>
!>```f90
!> use mctc_data
!> use mctc_env, only : wp
!> real(wp) :: radius
!>
!> ! Get covalent radius for carbon (Z=6)
!> radius = get_covalent_rad(6)
!>```
module mctc_data
   use mctc_data_atomicrad, only : get_atomic_rad
   use mctc_data_covrad, only : get_covalent_rad
   use mctc_data_mass, only : get_atomic_mass
   use mctc_data_paulingen, only : get_pauling_en
   use mctc_data_vdwrad, only : get_vdw_rad
   implicit none

   public :: get_atomic_rad, get_covalent_rad, get_pauling_en, get_vdw_rad, get_atomic_mass
end module mctc_data
