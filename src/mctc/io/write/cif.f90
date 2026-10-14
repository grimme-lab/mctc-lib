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

!> Writer for crystallographic information files (CIF)
module mctc_io_write_cif
   use mctc_env_accuracy, only : wp
   use mctc_env_error, only : error_type, fatal_error
   use mctc_io_constants, only : pi
   use mctc_io_convert, only : autoaa
   use mctc_io_math, only : matdet_3x3, matinv_3x3
   use mctc_io_periodic, only : dlat_to_cell
   use mctc_io_structure, only : structure_type
   use mctc_io_symbols, only : to_symbol
   use mctc_io_utils, only : to_string
   implicit none
   private

   public :: write_cif

contains

!> Write a structure as CIF data block in space group P1
subroutine write_cif(self, unit, error)

   !> Instance of the molecular structure data
   class(structure_type), intent(in) :: self

   !> File handle
   integer, intent(in) :: unit

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=:), allocatable :: block
   integer :: iat, i
   real(wp) :: cellpar(6)
   real(wp), allocatable :: abc(:, :)

   if (size(self%periodic) /= 3 .or. .not.all(self%periodic)) then
      call fatal_error(error, "CIF output requires a 3D periodic structure")
      return
   end if

   ! the negated comparison also rejects NaN entries
   if (.not.abs(matdet_3x3(self%lattice)) > &
      & sqrt(epsilon(1.0_wp)) * product(norm2(self%lattice, 1))) then
      call fatal_error(error, "CIF output requires a non-degenerate lattice")
      return
   end if

   block = "structure"
   if (allocated(self%comment)) then
      if (len_trim(self%comment) > 0) then
         block = trim(adjustl(self%comment))
         ! CIF data block names may not contain whitespace
         do i = 1, len(block)
            if (block(i:i) == " " .or. block(i:i) == achar(9)) block(i:i) = "_"
         end do
      end if
   end if
   write(unit, '(a)') "data_" // block

   call dlat_to_cell(self%lattice, cellpar)
   cellpar(1:3) = cellpar(1:3) * autoaa
   cellpar(4:6) = cellpar(4:6) * 180.0_wp / pi
   write(unit, '(a, f18.10)') &
      & "_cell_length_a    ", cellpar(1), &
      & "_cell_length_b    ", cellpar(2), &
      & "_cell_length_c    ", cellpar(3), &
      & "_cell_angle_alpha ", cellpar(4), &
      & "_cell_angle_beta  ", cellpar(5), &
      & "_cell_angle_gamma ", cellpar(6)

   write(unit, '(a)') &
      & "_symmetry_space_group_name_H-M 'P 1'", &
      & "loop_", &
      & "_space_group_symop_operation_xyz", &
      & "'x, y, z'"

   write(unit, '(a)') &
      & "loop_", &
      & "_atom_site_label", &
      & "_atom_site_type_symbol", &
      & "_atom_site_fract_x", &
      & "_atom_site_fract_y", &
      & "_atom_site_fract_z"
   abc = matmul(matinv_3x3(self%lattice), self%xyz)
   ! cell parameters always describe a right-handed lattice, inverting the
   ! fractional coordinates keeps the handedness of a left-handed lattice
   if (matdet_3x3(self%lattice) < 0.0_wp) abc = -abc
   do iat = 1, self%nat
      write(unit, '(a, 1x, a, 3(1x, f20.12))') &
         & trim(to_symbol(self%num(self%id(iat)))) // to_string(iat), &
         & trim(to_symbol(self%num(self%id(iat)))), abc(:, iat)
   end do

end subroutine write_cif

end module mctc_io_write_cif
