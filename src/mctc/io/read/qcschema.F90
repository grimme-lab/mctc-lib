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

module mctc_io_read_qcschema
   use mctc_env_accuracy, only : wp
   use mctc_env_error, only : error_type, fatal_error
   use mctc_io_structure, only : structure_type, new
   use mctc_io_symbols, only : to_number, symbol_length
   use mctc_io_utils, only : to_string
#if WITH_JSON
   use jonquil, only : json_value, json_object, json_array, json_keyval, &
      & json_load, json_error, json_context, json_stat, get_value, &
      & json_parser_config, json_context, cast_to_object, len
#endif
   implicit none
   private

   public :: read_qcschema


contains


subroutine read_qcschema(self, unit, error)

   !> Instance of the molecular structure data
   type(structure_type), intent(out) :: self

   !> File handle
   integer, intent(in) :: unit

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

#if WITH_JSON
   class(json_value), allocatable :: root
   type(json_object), pointer :: object, child
   type(json_array), pointer :: array, child_array
   type(json_error), allocatable :: parse_error
   type(json_keyval), pointer :: val
   type(json_context) :: ctx

   integer :: stat, origin, schema_version, charge, multiplicity, iat, ibond
   integer :: origin_symbols, origin_geometry
   character(len=:), allocatable :: symbol, message, schema_name, comment
   character(len=symbol_length), allocatable :: sym(:)
   integer, allocatable :: bond(:, :), list(:)
   real(wp), allocatable, target :: geo(:)
   real(wp), pointer :: xyz(:, :)

   call json_load(root, unit, config=json_parser_config(context_detail=1), &
      & context=ctx, error=parse_error)
   if (allocated(parse_error)) then
      allocate(error)
      call move_alloc(parse_error%message, error%message)
      return
   end if
   object => cast_to_object(root)
   if (.not.associated(object)) then
      call fatal_error(error, ctx%report("Invalid JSON object", root%origin, "Expected JSON object"))
      return
   end if

   call get_value(object, "schema_version", schema_version, default=2, stat=stat, origin=origin)
   if (stat /= json_stat%success) then
      call fatal_error(error, ctx%report("Could not read schema version", origin=origin))
      return
   end if
   call get_value(object, "schema_name", schema_name, default="qcschema_molecule", stat=stat, origin=origin)
   if (stat /= json_stat%success) then
      call fatal_error(error, ctx%report("Could not read schema name", origin=origin))
      return
   end if

   if (schema_name /= "qcschema_molecule" .and. schema_name /= "qcschema_input") then
      call get_value(object, "schema_name", val, requested=.false.)
      if (associated(val)) origin = val%origin_value
      call fatal_error(error, ctx%report("Invalid schema name '"//schema_name//"'", &
         & origin, "Expected 'qcschema_molecule' or 'qcschema_input'"))
      return
   end if

   if (schema_name == "qcschema_input") then
      select case(schema_version)
      case(1)
         call get_value(object, "molecule", child, stat=stat, origin=origin)
         if (stat /= json_stat%success) then
            call fatal_error(error, ctx%report("Could not read molecule", origin=origin))
            return
         end if
      case default
         call get_value(object, "schema_version", val, requested=.false.)
         if (associated(val)) origin = val%origin_value
         call fatal_error(error, ctx%report("Unsupported schema version for 'qcschema_input'", &
            & origin, "Expected 1"))
         return
      end select
      call get_value(child, "schema_version", schema_version, default=2, stat=stat, origin=origin)
      if (stat /= json_stat%success) then
         call fatal_error(error, ctx%report("Could not read schema version", origin=origin))
         return
      end if
      call get_value(child, "schema_name", schema_name, default="qcschema_molecule", stat=stat, origin=origin)
      if (stat /= json_stat%success) then
         call fatal_error(error, ctx%report("Could not read schema name", origin=origin))
         return
      end if

      if (schema_name /= "qcschema_molecule") then
         call get_value(child, "schema_name", val, requested=.false.)
         if (associated(val)) origin = val%origin_value
         call fatal_error(error, ctx%report("Invalid schema name '"//schema_name//"'", &
            & origin, "Expected 'qcschema_molecule'"))
         return
      end if

      object => child
   end if

   select case(schema_version)
   case(1)
      call get_value(object, "molecule", child, stat=stat, origin=origin)
      if (stat /= json_stat%success) then
         call fatal_error(error, ctx%report("Could not read molecule", origin=origin))
         return
      end if
   case(2)
      child => object
   case default
      call get_value(object, "schema_version", val, requested=.false.)
      if (associated(val)) origin = val%origin_value
      call fatal_error(error, ctx%report("Unsupported schema version for 'qcschema_molecule'", &
         & origin, "Expected 1 or 2"))
      return
   end select

   call get_value(child, "symbols", array, requested=.false., stat=stat, origin=origin)
   if (stat /= json_stat%success .or. .not.associated(array)) then
      call fatal_error(error, ctx%report("Could not read symbols", origin=origin))
      return
   end if
   allocate(sym(len(array)))
   do iat = 1, size(sym)
      call get_value(array, iat, symbol, stat=stat, origin=origin)
      if (stat /= json_stat%success) then
         call fatal_error(error, ctx%report("Could not read symbol", origin=origin))
         return
      end if
      sym(iat) = symbol
   end do
   origin_symbols = array%origin
   call get_value(child, "geometry", array, requested=.false., stat=stat, origin=origin)
   if (stat == json_stat%success .and. associated(array)) then
      call get_value(array, geo, stat=stat, origin=origin)
   end if
   if (stat /= json_stat%success .or. .not.associated(array)) then
      call fatal_error(error, ctx%report("Could not read geometry", origin=origin))
      return
   end if
   origin_geometry = array%origin

   if (3*size(sym) /= size(geo)) then
      call fatal_error(error, ctx%report("Number of coordinates and elements do not match", &
         & origin_symbols, origin_geometry, "Got "//to_string(size(sym))//" elements", &
         & "Got "//to_string(size(geo))//" coordinates, expected "//to_string(3*size(sym))))
      return
   end if

   call get_value(child, "comment", comment, default="", stat=stat, origin=origin)
   if (stat /= json_stat%success) then
      call fatal_error(error, ctx%report("Could not read comment", origin=origin))
      return
   end if
   call get_value(child, "molecular_charge", charge, default=0, stat=stat, origin=origin)
   if (stat /= json_stat%success) then
      call fatal_error(error, ctx%report("Could not read charge", origin=origin))
      return
   end if
   call get_value(child, "molecular_multiplicity", multiplicity, default=1, stat=stat, origin=origin)
   if (stat /= json_stat%success) then
      call fatal_error(error, ctx%report("Could not read multiplicity", origin=origin))
      return
   end if

   call get_value(child, "connectivity", array, stat=stat, origin=origin)
   if (stat /= json_stat%success) then
      call fatal_error(error, ctx%report("Could not read connectivity", origin=origin))
      return
   end if
   if (associated(array)) then
      allocate(bond(3, len(array)))
      do ibond = 1, size(bond, 2)
         call get_value(array, ibond, child_array, stat=stat, origin=origin)
         if (stat == json_stat%success) then
            call get_value(child_array, list, stat=stat, origin=origin)
         end if
         if (stat /= json_stat%success) then
            call fatal_error(error, ctx%report("Could not read bond", origin=origin))
            return
         end if
         if (allocated(list)) then
            bond(:, ibond) = [list(1)+1, list(2)+1, list(3)]
         end if
      end do
   end if

   xyz(1:3, 1:size(geo)/3) => geo
   call new(self, sym, xyz, charge=real(charge, wp), uhf=multiplicity-1)
   if (len(comment) > 0) self%comment = comment
   if (allocated(bond)) then
      self%nbd = size(bond, 2)
      call move_alloc(bond, self%bond)
   end if

#else
   call fatal_error(error, "JSON support not enabled")
#endif
end subroutine read_qcschema


end module mctc_io_read_qcschema
