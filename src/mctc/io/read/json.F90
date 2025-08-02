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

module mctc_io_read_json
   use mctc_env_accuracy, only : wp
   use mctc_env_error, only : error_type, fatal_error
   use mctc_io_structure, only : structure_type, new
   use mctc_io_symbols, only : to_number, symbol_length
   use mctc_io_utils, only : to_string
   use mctc_io_read_cjson, only : read_cjson
   use mctc_io_read_qcschema, only : read_qcschema
   use mctc_io_read_pymatgen, only : read_pymatgen
#if WITH_JSON
   use jonquil, only : json_value, json_object, json_array, json_keyval, &
      & json_load, json_error, json_context, json_stat, get_value, &
      & json_parser_config, json_context, cast_to_object, len
#endif
   implicit none
   private

   public :: read_json


contains


subroutine read_json(self, unit, error)

   !> Instance of the molecular structure data
   type(structure_type), intent(out) :: self

   !> File handle
   integer, intent(in) :: unit

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

#if WITH_JSON
   class(json_value), allocatable :: root
   type(json_object), pointer :: object
   type(json_error), allocatable :: parse_error
   type(json_context) :: ctx

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

   ! QCSchema JSON uses "schema_name" and "schema_version" keys
   if (object%has_key("schema_name") .or. object%has_key("schema_version")) then
      call read_qcschema(self, object, ctx, error)
      return
   end if

   ! Pymatgen serialized via monty adds "@module" and "@class" keys
   if (object%has_key("@module") .or. object%has_key("@class")) then
      call read_pymatgen(self, object, ctx, error)
      return
   end if

   ! Chemical JSON (cjson) tracks version via "chemical json" or "chemicalJson" keys
   if (object%has_key("chemical json") .or. object%has_key("chemicalJson")) then
      call read_cjson(self, object, ctx, error)
      return
   end if

   ! Default to QCSchema if no specific schema is detected
   call read_qcschema(self, object, ctx, error)
#else
   call fatal_error(error, "JSON support not enabled")
#endif
end subroutine read_json


end module mctc_io_read_json
