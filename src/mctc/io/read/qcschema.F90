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
   use mctc_io_utils, only : getline
#if WITH_JSON
   use json_value_module, only : json_core, json_value
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
   type(json_core) :: json
   type(json_value), pointer :: root, val, child, array

   integer :: stat, schema_version, charge, multiplicity, ibond
   character(len=:), allocatable :: input, line, message, schema_name, comment
   character(len=symbol_length), allocatable :: sym(:)
   integer, allocatable :: bond(:, :), list(:)
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

   call json%get(val, "schema_version", schema_version, default=2)
   call json%get(val, "schema_name", schema_name, default="qcschema_molecule")

   if (schema_name /= "qcschema_molecule" .and. schema_name /= "qcschema_input" &
      & .or. json%failed()) then
      call fatal_error(error, "Invalid schema name '"//schema_name//"'")
      call json%destroy(root)
      return
   end if

   if (schema_name == "qcschema_input") then
      select case(schema_version)
      case(1)
         call json%get(val, "molecule", child)
      case default
         call fatal_error(error, "Unsupported schema version for 'qcschema_input'")
         call json%destroy(root)
         return
      end select
      call json%get(child, "schema_version", schema_version, default=2)
      call json%get(child, "schema_name", schema_name, default="qcschema_molecule")

      if (schema_name /= "qcschema_molecule" .or. json%failed()) then
         call fatal_error(error, "Invalid schema name '"//schema_name//"'")
         call json%destroy(root)
         return
      end if

      val => child
   end if

   select case(schema_version)
   case(1)
      call json%get(val, "molecule", child)
   case(2)
      child => val
   case default
      call fatal_error(error, "Unsupported schema version for 'qcschema_molecule'")
      call json%destroy(root)
      return
   end select

   call json%get(child, "symbols", sym)
   if (.not.allocated(sym) .or. json%failed()) then
      call fatal_error(error, "List of atomic symbols must be provided")
      call json%destroy(root)
      return
   end if
   call json%get(child, "geometry", geo)
   if (.not.allocated(geo) .or. json%failed()) then
      call fatal_error(error, "Cartesian coordinates must be provided")
      call json%destroy(root)
      return
   end if

   if (3*size(sym) /= size(geo)) then
      call fatal_error(error, "Number of symbols and coordinate triples must match")
      call json%destroy(root)
      return
   end if

   call json%get(child, "comment", comment, default="")
   call json%get(child, "molecular_charge", charge, default=0)
   call json%get(child, "molecular_multiplicity", multiplicity, default=1)

   if (json%failed()) then
      call json%check_for_errors(error_msg=message)
      call fatal_error(error, message)
      call json%destroy(root)
      return
   end if

   call json%get_child(child, "connectivity", array)
   if (associated(array)) then
      allocate(bond(3, json%count(array)))
      do ibond = 1, size(bond, 2)
         call json%get_child(array, ibond, child)
         call json%get(child, "", list)
         if (allocated(list)) then
            bond(:, ibond) = [list(1)+1, list(2)+1, list(3)]
         end if
      end do

      if (json%failed()) then
         call json%check_for_errors(error_msg=message)
         call fatal_error(error, message)
         call json%destroy(root)
         return
      end if
   end if

   xyz(1:3, 1:size(geo)/3) => geo
   call new(self, sym, xyz, charge=real(charge, wp), uhf=multiplicity-1)
   if (len(comment) > 0) self%comment = comment
   if (allocated(bond)) then
      self%nbd = size(bond, 2)
      call move_alloc(bond, self%bond)
   end if

   call json%destroy(root)
#else
   call fatal_error(error, "JSON support not enabled")
#endif
end subroutine read_qcschema


end module mctc_io_read_qcschema
