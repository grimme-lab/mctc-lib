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

#include "mctc/defs.h"

module mctc_io_read_cjson
   use mctc_env_accuracy, only : wp
   use mctc_env_error, only : error_type, fatal_error
   use mctc_io_constants, only : pi
   use mctc_io_convert, only : aatoau
   use mctc_io_structure, only : structure_type, new
   use mctc_io_symbols, only : to_number, symbol_length
   use mctc_io_utils, only : getline
#if WITH_JSON
   use json_value_module, only : json_core, json_value
#endif
   implicit none
   private

   public :: read_cjson


contains


subroutine read_cjson(self, unit, error)

   !> Instance of the molecular structure data
   type(structure_type), intent(out) :: self

   !> File handle
   integer, intent(in) :: unit

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

#if WITH_JSON
   type(json_core) :: json
   type(json_value), pointer :: root, val, child, array

   logical :: cartesian, found
   integer :: stat, schema_version, charge, multiplicity, ibond
   character(len=:), allocatable :: input, line, message, comment
   integer, allocatable :: num(:), bond(:, :), list(:), order(:)
   real(wp) :: cellpar(6)
   real(wp), allocatable :: lattice(:, :)
   real(wp), allocatable, target :: geo(:)
   real(wp), pointer :: xyz(:, :)

   stat = 0
   input = ""
   do
      call getline(unit, line, stat)
      if (stat /= 0) exit
      input = input // line
   end do

   call json%deserialize(root, input)
   if (json%failed()) then
      call json%check_for_errors(error_msg=message)
      call fatal_error(error, message)
      call json%destroy(root)
      return
   end if
   val => root

   call cjson_get(json, val, "chemicalJson", "chemical json", child)
   if (.not.associated(child)) then
      call fatal_error(error, "No 'chemical json' key found")
      call json%destroy(root)
      return
   end if

   call json%get(child, schema_version)

   ! There seems to be no actual difference between version 0 and 1, though
   if (all(schema_version /= [0, 1])) then
      call fatal_error(error, "Unsupported schema version for 'chemical json'")
      call json%destroy(root)
      return
   end if

   call json%get(val, "atoms.elements.number", num)
   if (.not.allocated(num) .or. json%failed()) then
      call fatal_error(error, "List of atomic symbols must be provided")
      call json%destroy(root)
      return
   end if

   call cjson_get(json, val, "unitCell", "unit cell", child)
   if (associated(child)) then
      call json%get(child, "a", cellpar(1))
      call json%get(child, "b", cellpar(2))
      call json%get(child, "c", cellpar(3))
      call json%get(child, "alpha", cellpar(4))
      call json%get(child, "beta",  cellpar(5))
      call json%get(child, "gamma", cellpar(6))

      if (json%failed()) then
         call json%check_for_errors(error_msg=message)
         call fatal_error(error, message)
         call json%destroy(root)
         return
      end if
      cellpar(1:3) = cellpar(1:3) * aatoau
      cellpar(4:6) = cellpar(4:6) * (pi / 180)
      allocate(lattice(3, 3))
      call cell_to_dlat(cellpar, lattice)
   end if

   call json%get(val, "atoms.coords.3d", geo, found=cartesian)
   if (.not.cartesian .and. allocated(lattice)) then
      call cjson_get(json, val, "atoms.coords.3dFractional", "atoms.coords.3d fractional", &
         & child)
      if (associated(child)) call json%get(child, geo)
   end if
   if (.not.allocated(geo) .or. json%failed()) then
      call fatal_error(error, "Cartesian coordinates must be provided")
      call json%destroy(root)
      return
   end if

   if (3*size(num) /= size(geo)) then
      call fatal_error(error, "Number of atomic numbers and coordinate triples must match")
      call json%destroy(root)
      return
   end if

   call json%get(val, "bonds.connections.index", list, found=found)
   call json%get(val, "bonds.order", order, found=found)
   if (.not.allocated(order) .and. allocated(list)) &
      allocate(order(size(list)/2), source=1)

   if (json%failed()) then
      call fatal_error(error, "Cannot read entries from 'bonds'")
      call json%destroy(root)
      return
   end if

   if (allocated(list)) then
      allocate(bond(3, size(list)/2))
      do ibond = 1, size(bond, 2)
         bond(:, ibond) = [list(2*ibond-1) + 1, list(2*ibond) + 1, order(ibond)]
      end do
   end if

   call json%get(val, "name", comment, default="")
   call json%get(val, "properties.totalCharge", charge, found=found)
   if (.not.found) then
      call json%get(val, "atoms.formalCharges", list, found=found)
      charge  = 0
      if (allocated(list)) charge = sum(list)
   end if
   call json%get(val, "properties.totalSpinMultiplicity", multiplicity, found=found)
   if (.not.found) multiplicity = 1

   if (json%failed()) then
      call json%check_for_errors(error_msg=message)
      call fatal_error(error, message)
      call json%destroy(root)
      return
   end if

   xyz(1:3, 1:size(geo)/3) => geo
   xyz(:, :) = xyz * aatoau
   if (.not.cartesian) then
      xyz(:, :) = matmul(lattice, xyz(:, :))
   end if
   call new(self, num, xyz, lattice=lattice, charge=real(charge, wp), uhf=multiplicity - 1)
   if (len(comment) > 0) self%comment = comment
   if (allocated(bond)) then
      self%nbd = size(bond, 2)
      call move_alloc(bond, self%bond)
   end if

   call json%destroy(root)

contains

   subroutine cjson_get(json, val, key1, key2, child)
      type(json_core), intent(inout) :: json
      type(json_value), pointer, intent(in) :: val
      type(json_value), pointer, intent(out) :: child
      character(*), intent(in) :: key1, key2

      logical :: found

      call json%get(val, key1, child, found=found)
      if (.not.found) then
         call json%get(val, key2, child, found=found)
      end if
   end subroutine cjson_get

#else
   call fatal_error(error, "JSON support not enabled")
#endif
end subroutine read_cjson


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
      lattice(2, 3) = clen*(cos(alp) - cos(bet)*cos(gam))/sin(gam);
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


end module mctc_io_read_cjson
