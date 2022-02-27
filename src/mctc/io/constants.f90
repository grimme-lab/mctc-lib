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
   use mctc_io_codata2018, only : planck_constant, speed_of_light_in_vacuum, &
      & boltzmann_constant, avogadro_constant, elementary_charge, fine_structure_constant, &
      & electron_mass
   implicit none
   private

   public :: pi, codata


   !> Ratio between a circles diameter and its circumfence
   real(wp), parameter :: pi = 3.1415926535897932384626433832795029_wp


   !> Natural constants defining the SI unit base
   type :: enum_codata

      !> Planck's constant
      real(wp) :: h = planck_constant

      !> Speed of light in vacuum
      real(wp) :: c = speed_of_light_in_vacuum

      !> Boltzmann's constant
      real(wp) :: kb = boltzmann_constant

      !> Avogadro's number
      real(wp) :: NA = avogadro_constant

      !> Elementary charge
      real(wp) :: e = elementary_charge

      !> fine structure constant (CODATA2018)
      real(wp) :: alpha = fine_structure_constant

      !> electron rest mass
      real(wp) :: me = electron_mass

   end type enum_codata

   !> Actual collection of natural constants
   type(enum_codata), parameter :: codata = enum_codata()


end module mctc_io_constants
