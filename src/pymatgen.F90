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

module mctc_io_read_pymatgen
   use mctc_env_accuracy, only : wp
   use mctc_env_error, only : error_type, fatal_error
   use mctc_io_structure, only : structure_type, new
   use mctc_io_symbols, only : to_number, symbol_length
   use mctc_io_utils, only : to_string
   use mctc_io_convert, only : aatoau
#if WITH_JSON
   use jonquil, only : json_value, json_object, json_array, json_keyval, &
      & json_load, json_error, json_context, json_stat, get_value, &
      & json_parser_config, json_context, cast_to_object, len
#endif
   implicit none
   private

   public :: read_pymatgen

#if WITH_JSON
   interface read_pymatgen
      module procedure read_pymatgen
      module procedure load_pymatgen
   end interface read_pymatgen
#endif


contains


subroutine read_pymatgen(self, unit, error)

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

   call load_pymatgen(self, object, ctx, error)
#else
   call fatal_error(error, "JSON support not enabled")
#endif
end subroutine read_pymatgen


#if WITH_JSON
subroutine load_pymatgen(self, object, ctx, error)

   !> Instance of the molecular structure data
   type(structure_type), intent(out) :: self

   !> JSON object representing the structure
   type(json_object), intent(inout) :: object

   !> JSON context for error reporting
   type(json_context), intent(inout) :: ctx

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(json_object), pointer :: child
   type(json_array), pointer :: array, child_array
   type(json_keyval), pointer :: val

   integer :: stat, origin, multiplicity, iat, ilt
   character(len=:), allocatable :: symbol, module_name, class_name
   character(len=symbol_length), allocatable :: sym(:)
   logical :: periodic
   real(wp) :: charge
   real(wp), allocatable :: xyz(:, :), lattice(:, :), vec(:)

   call get_value(object, "@module", module_name, stat=stat, origin=origin)
   if (stat /= json_stat%success .or. .not.allocated(module_name)) then
      call fatal_error(error, ctx%report("Could not read module name", origin=origin))
      return
   end if
   call get_value(object, "@class", class_name, stat=stat, origin=origin)
   if (stat /= json_stat%success .or. .not.allocated(class_name)) then
      call fatal_error(error, ctx%report("Could not read class name", origin=origin))
      return
   end if

   if (module_name /= "pymatgen.core.structure") then
      call get_value(object, "@module", val, requested=.false.)
      call fatal_error(error, ctx%report("Invalid pymatgen object", origin=val%origin_value, &
         & label="Expected pymatgen.core.structure"))
      return
   end if

   if (class_name /= "Structure" .and. class_name /= "Molecule") then
      call get_value(object, "@class", val, requested=.false.)
      call fatal_error(error, ctx%report("Invalid pymatgen object", origin=val%origin_value, &
         & label="Expected Structure or Molecule"))
      return
   end if
   periodic = class_name == "Structure"

   call get_value(object, "charge", charge, default=0.0_wp, stat=stat, origin=origin)
   if (stat /= json_stat%success) then
      call fatal_error(error, ctx%report("Could not read charge", origin=origin, &
         & label="Expected integer value"))
      return
   end if

   if (.not.periodic) then
      call get_value(object, "spin_multiplicity", multiplicity, default=1, &
         & stat=stat, origin=origin)
      if (stat /= json_stat%success) then
         call fatal_error(error, ctx%report("Could not read spin multiplicity", origin=origin, &
            & label="Expected integer value"))
         return
      end if
      if (multiplicity < 1) then
         call get_value(object, "spin_multiplicity", val, requested=.false.)
         call fatal_error(error, ctx%report("Invalid spin multiplicity", &
            & origin=val%origin_value, label="Expected integer value >= 1"))
         return
      end if
   else
      charge = 0
      multiplicity = 1
   end if

   call get_value(object, "sites", array, stat=stat, origin=origin)
   if (stat /= json_stat%success) then
      call fatal_error(error, ctx%report("Could not read sites", origin=origin, &
         & label="Expected array of sites"))
      return
   end if

   allocate(sym(len(array)))
   allocate(xyz(3, len(array)))

   do iat = 1, len(array)
      call get_value(array, iat, child, stat=stat, origin=origin)
      if (stat /= json_stat%success) then
         call fatal_error(error, ctx%report("Could not read site", origin=origin))
         return
      end if

      call get_value(child, "label", symbol, stat=stat, origin=origin)
      if (stat /= json_stat%success) then
         call fatal_error(error, ctx%report("Could not read site label", origin=origin, &
            & label="Expected string value"))
         return
      end if
      sym(iat) = symbol

      call get_value(child, "xyz", child_array, stat=stat, origin=origin)
      if (stat /= json_stat%success) then
         call fatal_error(error, ctx%report("Could not read site coordinates", origin=origin, &
            & label="Expected array of coordinates"))
         return
      end if
      call get_value(child_array, vec, stat=stat, origin=origin)
      if (stat /= json_stat%success) then
         call fatal_error(error, ctx%report("Could not read site coordinates", origin=origin, &
            & label="Expected array of coordinates"))
         return
      end if
      if (size(vec) /= 3) then
         call fatal_error(error, ctx%report("Invalid site coordinates size", origin=child_array%origin, &
            & label="Expected 3 coordinates"))
         return
      end if
      xyz(:, iat) = vec * aatoau
   end do

   if (periodic) then
      allocate(lattice(3, 3))
      call get_value(object, "lattice", child, stat=stat, origin=origin)
      if (stat /= json_stat%success) then
         call fatal_error(error, ctx%report("Could not read lattice", origin=origin, &
            & label="Expected lattice object"))
         return
      end if

      call get_value(child, "matrix", array, stat=stat, origin=origin)
      if (stat /= json_stat%success) then
         call fatal_error(error, ctx%report("Could not read lattice matrix", origin=origin, &
            & label="Expected array of lattice vectors"))
         return
      end if
      if (len(array) /= 3) then
         call fatal_error(error, ctx%report("Invalid lattice matrix size", origin=origin, &
            & label="Expected 3x3 matrix"))
         return
      end if

      do ilt = 1, 3
         call get_value(array, ilt, child_array, stat=stat, origin=origin)
         if (stat /= json_stat%success) then
            call fatal_error(error, ctx%report("Could not read lattice vector", origin=origin))
            return
         end if
         call get_value(child_array, vec, stat=stat, origin=origin)
         if (stat /= json_stat%success) then
            call fatal_error(error, ctx%report("Could not read lattice vector", origin=origin, &
               & label="Expected array of coordinates"))
            return
         end if
         if (size(vec) /= 3) then
            call fatal_error(error, ctx%report("Invalid lattice vector size", &
               & origin=child_array%origin, label="Expected 3 coordinates"))
            return
         end if
         lattice(:, ilt) = vec * aatoau
      end do
   end if

   call new(self, sym, xyz, charge=charge, uhf=multiplicity-1, lattice=lattice)

end subroutine load_pymatgen
#endif


end module mctc_io_read_pymatgen