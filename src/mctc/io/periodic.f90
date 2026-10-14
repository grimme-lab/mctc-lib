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

!> Utilities for periodic structures
module mctc_io_periodic
   use mctc_env_accuracy, only : wp
   implicit none
   private

   public :: cell_to_dlat


contains


!> Calculate the lattice vectors from a set of cell parameters
pure subroutine cell_to_dlat(cellpar, lattice)

   !> Cell parameters
   real(wp), intent(in)  :: cellpar(6)

   !> Direct lattice
   real(wp), intent(out) :: lattice(:, :)

   real(wp) :: dvol

   dvol = cell_to_dvol(cellpar)

   associate(alen => cellpar(1), blen => cellpar(2), clen => cellpar(3), &
         &   alp  => cellpar(4), bet  => cellpar(5), gam  => cellpar(6))

      lattice(1, 1) = alen
      lattice(2, 1) = 0.0_wp
      lattice(3, 1) = 0.0_wp
      lattice(3, 2) = 0.0_wp
      lattice(1, 2) = blen*cos(gam)
      lattice(2, 2) = blen*sin(gam)
      lattice(1, 3) = clen*cos(bet)
      lattice(2, 3) = clen*(cos(alp) - cos(bet)*cos(gam))/sin(gam)
      lattice(3, 3) = dvol/(alen*blen*sin(gam))

   end associate

end subroutine cell_to_dlat


!> Calculate the cell volume from a set of cell parameters
pure function cell_to_dvol(cellpar) result(dvol)

   !> Cell parameters
   real(wp), intent(in) :: cellpar(6)

   !> Cell volume
   real(wp) :: dvol

   real(wp) :: vol2

   associate(alen => cellpar(1), blen => cellpar(2), clen => cellpar(3), &
         &   alp  => cellpar(4), bet  => cellpar(5), gam  => cellpar(6) )

      vol2 = 1.0_wp - cos(alp)**2 - cos(bet)**2 - cos(gam)**2 &
         & + 2.0_wp*cos(alp)*cos(bet)*cos(gam)

      dvol = sqrt(abs(vol2))*alen*blen*clen
      ! return negative volume instead of imaginary one (means bad cell parameters)
      if (vol2 < 0.0_wp) dvol = -dvol ! this should not happen, but who knows...

   end associate
end function cell_to_dvol


end module mctc_io_periodic
