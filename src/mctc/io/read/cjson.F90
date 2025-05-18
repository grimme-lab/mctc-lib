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
   use mctc_io_utils, only : getline, to_string
#if WITH_JSON
   use jonquil, only : json_value, json_object, json_array, json_keyval, &
      & json_load, json_error, json_context, json_stat, json_path, get_value, &
      & json_parser_config, json_context, cast_to_object, len
#endif
   implicit none
   private

   public :: read_cjson
#if WITH_JSON
   public :: read_cjson_from_object

   interface read_cjson
      module procedure :: read_cjson
      module procedure :: read_cjson_from_object
   end interface read_cjson
#endif


contains


subroutine read_cjson(self, unit, error)

   !> Instance of the molecular structure data
   type(structure_type), intent(out) :: self

   !> File handle
   integer, intent(in) :: unit

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

#if WITH_JSON
   class(json_value), allocatable :: root
   type(json_error), allocatable :: parse_error
   type(json_context) :: ctx

   call json_load(root, unit, config=json_parser_config(context_detail=1), &
      & context=ctx, error=parse_error)
   if (allocated(parse_error)) then
      allocate(error)
      call move_alloc(parse_error%message, error%message)
      return
   end if
   call read_cjson_from_object(self, root, ctx, error)
#else
   call fatal_error(error, "JSON support not enabled")
#endif
end subroutine read_cjson

#if WITH_JSON
subroutine read_cjson_from_object(self, root, context, error)

   !> Instance of the molecular structure data
   type(structure_type), intent(out) :: self

   !> Top-level object
   class(json_value), intent(inout) :: root

   !> Context for error reporting
   type(json_context), intent(in), optional :: context

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(json_object), pointer :: object, child, child2
   type(json_array), pointer :: array, child_array
   type(json_keyval), pointer :: val
   type(json_error), allocatable :: parse_error
   type(json_context) :: ctx

   logical :: cartesian, found
   integer :: stat, origin, schema_version, charge, multiplicity, ibond
   integer :: origin_elements, origin_coords
   character(len=:), allocatable :: input, line, message, comment
   integer, allocatable :: num(:), bond(:, :), list(:), order(:)
   real(wp) :: cellpar(6)
   real(wp), allocatable :: lattice(:, :)
   real(wp), allocatable, target :: geo(:)
   real(wp), pointer :: xyz(:, :)

   if (present(context)) ctx = context

   object => cast_to_object(root)
   if (.not.associated(object)) then
      call fatal_error(error, ctx%report("Invalid JSON object", root%origin, "Expected JSON object"))
      return
   end if

   call cjson_get_value(object, "chemicalJson", "chemical json", val, stat=stat, origin=origin)
   if (.not.associated(val) .or. stat /= json_stat%success) then
      call fatal_error(error, ctx%report("Could find chemical json", origin))
      return
   end if

   call get_value(val, schema_version, stat=stat, origin=origin)
   if (stat /= json_stat%success) then
      call fatal_error(error, ctx%report("Could not read schema version", origin, &
         & "Expected integer value"))
      return
   end if

   ! There seems to be no actual difference between version 0 and 1, though
   if (all(schema_version /= [0, 1])) then
      call fatal_error(error, ctx%report("Invalid schema version", val%origin, &
         & "Expected 0 or 1"))
      return
   end if

   call cjson_get_child(object, "unitCell", "unit cell", child, stat=stat, origin=origin)
   if (stat /= json_stat%success) then
      call fatal_error(error, ctx%report("Could not read unit cell", origin))
      return
   end if
   if (associated(child)) then
      call get_value(child, "a", cellpar(1), stat=stat, origin=origin)
      if (stat == json_stat%success) then
         call get_value(child, "b", cellpar(2), stat=stat, origin=origin)
      end if
      if (stat == json_stat%success) then
         call get_value(child, "c", cellpar(3), stat=stat, origin=origin)
      end if
      if (stat == json_stat%success) then
         call get_value(child, "alpha", cellpar(4), stat=stat, origin=origin)
      end if
      if (stat == json_stat%success) then
         call get_value(child, "beta",  cellpar(5), stat=stat, origin=origin)
      end if
      if (stat == json_stat%success) then
         call get_value(child, "gamma", cellpar(6), stat=stat, origin=origin)
      end if
      if (stat /= json_stat%success) then
         call fatal_error(error, ctx%report("Could not read unit cell parameters", origin, &
            & "Expected float values"))
         return
      end if

      cellpar(1:3) = cellpar(1:3) * aatoau
      cellpar(4:6) = cellpar(4:6) * (pi / 180)
      allocate(lattice(3, 3))
      call cell_to_dlat(cellpar, lattice)
   end if

   call get_value(object, "atoms", child, stat=stat, origin=origin)
   if (stat /= json_stat%success) then
      call fatal_error(error, ctx%report("Could not read atoms", origin))
      return
   end if
   call get_value(child, "elements", child2, stat=stat, origin=origin)
   if (stat == json_stat%success) then
      call get_value(child2, "number", array, stat=stat, origin=origin)
   end if
   if (stat == json_stat%success) then
      call get_value(array, num, stat=stat, origin=origin)
   end if
   if (stat /= json_stat%success) then
      call fatal_error(error, ctx%report("Could not read atomic numbers", origin))
      return
   end if
   origin_elements = array%origin

   call get_value(child, "coords", child2, stat=stat, origin=origin)
   if (stat /= json_stat%success) then
      call fatal_error(error, ctx%report("Could not read coordinates", origin, "Expected object"))
      return
   end if
   call get_value(child2, "3d", array, requested=.false., stat=stat, origin=origin)
   if (stat /= json_stat%success) then
      call fatal_error(error, ctx%report("Could not read 3D coordinates", origin, &
         & "Expected array"))
      return
   end if
   cartesian = associated(array)
   if (.not.cartesian .and. allocated(lattice)) then
      call cjson_get_array(child2, "3dFractional", "3d fractional", &
         & array, stat=stat, origin=origin)
      if (stat /= json_stat%success) then
         call fatal_error(error, ctx%report("Could not read fractional coordinates", origin, &
            & "Expected array"))
         return
      end if
   end if
   if (associated(array)) then
      call get_value(array, geo, stat=stat, origin=origin)
      if (stat /= json_stat%success) then
         call fatal_error(error, ctx%report("Could not read coordinates", origin))
         return
      end if
   else
      call fatal_error(error, ctx%report("Could not read coordinates", child2%origin, &
         & "Coordinates not found"))
      return
   end if
   origin_coords = array%origin

   if (3*size(num) /= size(geo)) then
      call fatal_error(error, ctx%report("Number of coordinates and atomic numbers do not match", &
         & origin_elements, origin_coords, "Got "//to_string(size(num))//" elements", &
         & "Got "//to_string(size(geo))//" coordinates, expected "//to_string(3*size(num))))
      return
   end if

   call get_value(object, "bonds", child, stat=stat, origin=origin)
   if (stat /= json_stat%success) then
      call fatal_error(error, ctx%report("Could not read bonds", origin))
      return
   end if
   call get_value(child, "order", array, requested=.false., stat=stat, origin=origin)
   if (associated(array)) then
      call get_value(array, order, stat=stat, origin=origin)
   end if
   call get_value(child, json_path("connections", "index"), array, requested=.false., &
      & stat=stat, origin=origin)
   if (associated(array)) then
      call get_value(array, list, stat=stat, origin=origin)
   end if
   if (.not.allocated(order) .and. allocated(list)) &
      allocate(order(size(list)/2), source=1)

   if (allocated(list)) then
      if (2*size(order) /= size(list)) then
         call fatal_error(error, "Number of bond orders and connectivity indices must match")
         return
      end if
      allocate(bond(3, size(list)/2))
      do ibond = 1, size(bond, 2)
         bond(:, ibond) = [list(2*ibond-1) + 1, list(2*ibond) + 1, order(ibond)]
      end do
   end if

   call get_value(object, "name", comment, default="", stat=stat, origin=origin)
   call get_value(object, "properties", child, stat=stat, origin=origin)
   call get_value(child, "totalCharge", charge, stat=stat, origin=origin)
   if (stat == json_stat%missing_key) then
      call get_value(object, json_path("atoms", "formalCharges"), array, stat=stat, origin=origin)
      charge  = 0
      if (associated(array)) then
          call get_value(array, list, stat=stat, origin=origin)
          if (allocated(list)) then
            charge = sum(list)
          end if
         end if
   end if
   call get_value(child, "totalSpinMultiplicity", multiplicity, default=1, stat=stat, &
      & origin=origin)

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

end subroutine read_cjson_from_object
#endif

#if WITH_JSON
subroutine cjson_get_child(object, key1, key2, child, stat, origin)
   type(json_object), intent(inout) :: object
   type(json_object), pointer, intent(out) :: child
   character(len=*), intent(in) :: key1, key2
   integer, intent(out) :: stat, origin

   call get_value(object, key1, child, requested=.false., &
      & stat=stat, origin=origin)
   if (stat == json_stat%missing_key .or. .not.associated(child)) then
      call get_value(object, key2, child, requested=.false., &
         & stat=stat, origin=origin)
   end if
end subroutine cjson_get_child

subroutine cjson_get_array(object, key1, key2, array, stat, origin)
   type(json_object), intent(inout) :: object
   type(json_array), pointer, intent(out) :: array
   character(len=*), intent(in) :: key1, key2
   integer, intent(out) :: stat, origin

   call get_value(object, key1, array, requested=.false., &
      & stat=stat, origin=origin)
   if (stat == json_stat%missing_key .or. .not.associated(array)) then
      call get_value(object, key2, array, requested=.false., &
         & stat=stat, origin=origin)
   end if
end subroutine cjson_get_array

subroutine cjson_get_value(object, key1, key2, val, stat, origin)
   type(json_object), intent(inout) :: object
   type(json_keyval), pointer, intent(out) :: val
   character(len=*), intent(in) :: key1, key2
   integer, intent(out) :: stat, origin

   call get_value(object, key1, val, requested=.false., &
      & stat=stat, origin=origin)
   if (stat == json_stat%missing_key .or. .not.associated(val)) then
      call get_value(object, key2, val, requested=.false., &
         & stat=stat, origin=origin)
   end if
end subroutine cjson_get_value
#endif

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
