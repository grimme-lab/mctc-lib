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

!> Numerical constants
module mctc_io_constants
   use mctc_env_accuracy, only : wp
   implicit none
   private

   public :: pi, codata


   !> Ratio between a circles diameter and its circumfence
   real(wp), parameter :: pi = 3.1415926535897932384626433832795029_wp


   !> Natural constants defining the SI unit base
   type :: enum_codata

      !> Planck's constant
      real(wp) :: h = 6.6260715e-34_wp ! J·s = kg·m²·s⁻¹

      !> Speed of light in vacuum
      real(wp) :: c = 299792458.0_wp ! m·s⁻¹

      !> Boltzmann's constant
      real(wp) :: kb = 1.380649e-23_wp ! J·K⁻¹ = kg·m²·s⁻²·K⁻¹

      !> Avogadro's number
      real(wp) :: NA = 6.02214076e23_wp ! mol⁻¹

      !> Elementary charge
      real(wp) :: e = 1.602176634e-19_wp ! C

      !> fine structure constant (CODATA2018)
      real(wp) :: alpha = 1.0_wp/137.035999046_wp ! dimensionless

      !> electron rest mass
      real(wp) :: me = 9.10938356e-31_wp ! kg

   end type enum_codata

   !> Actual collection of natural constants
   type(enum_codata), parameter :: codata = enum_codata()


end module mctc_io_constants
