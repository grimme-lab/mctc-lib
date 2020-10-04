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

!> Conversion factors
module mctc_io_convert
   use mctc_env_accuracy, only : wp
   use mctc_io_constants, only : pi, codata
   implicit none
   private

   !> Reducted Planck's constant
   real(wp), parameter :: hbar = codata%h/(2.0_wp*pi) ! J·s = kg·m²·s⁻¹

   !> Bohr radius
   real(wp), parameter :: bohr = hbar/(codata%me*codata%c*codata%alpha) ! m

   !> Hartree energy
   real(wp), parameter :: hartree = codata%me*codata%c**2*codata%alpha**2 ! J = kg·m²·s⁻²

   !> Conversion factor from bohr to Ångström
   real(wp), public, parameter :: autoaa = bohr * 1e10_wp

   !> Conversion factor from Ångström to bohr
   real(wp), public, parameter :: aatoau = 1.0_wp/autoaa

   !> Conversion factor from hartree to electron volts
   real(wp), public, parameter :: autoeV = hartree/codata%e

   !> Conversion factor from electron volts to hartree
   real(wp), public, parameter :: evtoau = 1.0_wp/autoev

   !> Coversion factor between calorine and joule
   real(wp), public, parameter :: caltoj = 4.184_wp

   !> Coversion factor between joule and calorine
   real(wp), public, parameter :: jtocal = 1.0_wp/caltoj

   !> Conversion from hartree to kJ/mol
   real(wp), public, parameter :: autokj = hartree*codata%na*1e-3_wp

   !> Conversion from kJ/mol to hartree
   real(wp), public, parameter :: kjtoau = 1.0_wp/autokj

   !> Conversion from hartree to kcal/mol
   real(wp), public, parameter :: autokcal = autokJ*Jtocal

   !> Conversion from kcal/mol to hartree
   real(wp), public, parameter :: kcaltoau = 1.0_wp/autokcal

   !> Conversion from hartree to reciprocal centimeters
   real(wp), public, parameter :: autorcm = hartree/(codata%h*codata%c)*1e-2_wp

   !> Conversion from reciprocal centimeters to hartree
   real(wp), public, parameter :: rcmtoau = 1.0_wp/autorcm

   !> Conversion from hartree to nanometers (wavelength)
   real(wp), public, parameter :: autonm = codata%h*codata%c/hartree * 1e+9_wp

   !> Conversion from nanometers (wavelength) to hartree
   real(wp), public, parameter :: nmtoau = 1.0_wp/autonm

   !> Conversion from electron mass (a.u.) to kg
   real(wp), public, parameter :: autokg = codata%me

   !> Conversion from kg to electron mass (a.u.)
   real(wp), public, parameter :: kgtoau = 1.0_wp/autokg

   !> Molecular mass per mole (g/mol) to electron mass (a.u.)
   real(wp), public, parameter :: autogmol = codata%me*codata%na*1e+3_wp

   !> Electron mass (a.u.) to molecular mass per mole (g/mol)
   real(wp), public, parameter :: gmoltoau = 1.0_wp/autogmol

   !> Molecular mass per mole (g/mol) to kg
   real(wp), public, parameter :: gmoltokg = gmoltoau*autokg

   !> kg to molecular mass per mole (g/mol)
   real(wp), public, parameter :: kgtogmol = 1.0_wp/gmoltokg

   !> Coulomb to atomic charge units
   real(wp), public, parameter :: autoc = codata%e

   !> Atomic charge units to Coulomb
   real(wp), public, parameter :: ctoau = 1.0_wp/autoc


end module mctc_io_convert
