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

module test_read_cif
   use mctc_env, only : wp
   use mctc_env_testing, only : new_unittest, unittest_type, error_type, check
   use mctc_io_constants, only : pi
   use mctc_io_convert, only : aatoau
   use mctc_io_read_cif, only : read_cif
   use mctc_io_structure, only : structure_type
   implicit none
   private

   public :: collect_read_cif

   real(wp), parameter :: thr = 1.0e-8_wp


contains


!> Collect all exported unit tests
subroutine collect_read_cif(testsuite)

   !> Collection of tests
   type(unittest_type), allocatable, intent(out) :: testsuite(:)

   testsuite = [ &
      & new_unittest("valid1-cif", test_valid1_cif), &
      & new_unittest("valid2-cif", test_valid2_cif), &
      & new_unittest("valid3-cif", test_valid3_cif), &
      & new_unittest("valid4-cif", test_valid4_cif), &
      & new_unittest("valid5-cif", test_valid5_cif), &
      & new_unittest("valid6-cif", test_valid6_cif), &
      & new_unittest("invalid1-cif", test_invalid1_cif, should_fail=.true.), &
      & new_unittest("invalid2-cif", test_invalid2_cif, should_fail=.true.), &
      & new_unittest("invalid3-cif", test_invalid3_cif, should_fail=.true.), &
      & new_unittest("invalid4-cif", test_invalid4_cif, should_fail=.true.), &
      & new_unittest("invalid5-cif", test_invalid5_cif, should_fail=.true.), &
      & new_unittest("invalid6-cif", test_invalid6_cif, should_fail=.true.), &
      & new_unittest("invalid7-cif", test_invalid7_cif, should_fail=.true.), &
      & new_unittest("invalid8-cif", test_invalid8_cif, should_fail=.true.), &
      & new_unittest("invalid9-cif", test_invalid9_cif, should_fail=.true.), &
      & new_unittest("invalid10-cif", test_invalid10_cif, should_fail=.true.), &
      & new_unittest("invalid11-cif", test_invalid11_cif, should_fail=.true.), &
      & new_unittest("invalid12-cif", test_invalid12_cif, should_fail=.true.), &
      & new_unittest("invalid13-cif", test_invalid13_cif, should_fail=.true.) &
      & ]

end subroutine collect_read_cif


subroutine test_valid1_cif(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status="scratch", newunit=unit)
   write(unit, "(a)") &
      "data_OPT_STEP_1", &
      "_symmetry_space_group_name_H-M 'P 1'", &
      "_cell_length_a    5.56900000", &
      "_cell_length_b    5.56900000", &
      "_cell_length_c    5.56900000", &
      "_cell_angle_alpha 90.000000", &
      "_cell_angle_beta  90.000000", &
      "_cell_angle_gamma 90.000000", &
      "", &
      "loop_", &
      "_atom_site_label", &
      "_atom_site_fract_x", &
      "_atom_site_fract_y", &
      "_atom_site_fract_z", &
      "Na -0.0000000000 -0.0000000000 -0.0000000000", &
      "Na -0.5000000000 -0.5000000000 -0.0000000000", &
      "Na -0.5000000000 -0.0000000000 -0.5000000000", &
      "Na 0.0000000000 -0.5000000000 -0.5000000000", &
      "Cl -0.0000000000 -0.0000000000 -0.5000000000", &
      "Cl -0.5000000000 -0.5000000000 -0.5000000000", &
      "Cl -0.5000000000 -0.0000000000 0.0000000000", &
      "Cl 0.0000000000 -0.5000000000 0.0000000000"
   rewind(unit)

   call read_cif(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, struc%nat, 8, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, 2, "Number of species does not match")
   if (allocated(error)) return
   call check(error, allocated(struc%comment), "Data block name should be saved")
   if (allocated(error)) return
   call check(error, struc%comment == "OPT_STEP_1", "Comment does not match data block name")
   if (allocated(error)) return
   call check(error, all(struc%periodic), "Structure should be periodic")
   if (allocated(error)) return
   call check(error, struc%lattice(1, 1), 5.569_wp*aatoau, thr=thr)
   if (allocated(error)) return
   call check(error, struc%xyz(1, 2), -0.5_wp*5.569_wp*aatoau, thr=thr)
   if (allocated(error)) return

end subroutine test_valid1_cif


subroutine test_valid2_cif(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status="scratch", newunit=unit)
   write(unit, "(a)") &
      "# generated using pymatgen", &
      "data_NaCl", &
      "_symmetry_space_group_name_H-M   'P 1'", &
      "_cell_length_a   3.95140210", &
      "_cell_length_b   3.95140210", &
      "_cell_length_c   3.95140210", &
      "_cell_angle_alpha   60.00000000", &
      "_cell_angle_beta   60.00000000", &
      "_cell_angle_gamma   60.00000000", &
      "_symmetry_Int_Tables_number   1", &
      "_chemical_formula_structural   NaCl", &
      "_chemical_formula_sum   'Na1 Cl1'", &
      "_cell_volume   43.62532547", &
      "_cell_formula_units_Z   1", &
      "loop_", &
      " _symmetry_equiv_pos_site_id", &
      " _symmetry_equiv_pos_as_xyz", &
      "  1  'x, y, z'", &
      "loop_", &
      " _atom_site_type_symbol", &
      " _atom_site_label", &
      " _atom_site_symmetry_multiplicity", &
      " _atom_site_fract_x", &
      " _atom_site_fract_y", &
      " _atom_site_fract_z", &
      " _atom_site_occupancy", &
      "  Na  Na0  1  0.00000000  0.00000000  0.00000000  1", &
      "  Cl  Cl1  1  0.50000000  0.50000000  0.50000000  1"
   rewind(unit)

   call read_cif(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, struc%nat, 2, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%num(struc%id(2)), 17, "Second atom should be chlorine")
   if (allocated(error)) return
   call check(error, struc%lattice(1, 2), 3.95140210_wp*cos(60*pi/180)*aatoau, thr=thr)
   if (allocated(error)) return
   call check(error, struc%lattice(2, 2), 3.95140210_wp*sin(60*pi/180)*aatoau, thr=thr)
   if (allocated(error)) return

end subroutine test_valid2_cif


subroutine test_valid3_cif(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status="scratch", newunit=unit)
   write(unit, "(a)") &
      "# periclase with explicit symmetry operations", &
      "data_rocksalt", &
      "_publ_section_title", &
      ";Text field containing loop_", &
      "_cell_length_a 99", &
      ";", &
      "_journal_name_full 'Phys. Chem. Minerals'", &
      "_cell_length_a 4.1749(3)", &
      "_cell_length_b 4.1749", &
      "_cell_length_c 4.1749", &
      "_symmetry_space_group_name_h-m 'F m 3 m'", &
      "loop_", &
      "_symmetry_equiv_pos_as_xyz", &
      "x,y,z", &
      "x,1/2+y,1/2+z", &
      "1/2+x,y,1/2+z", &
      "1/2+x,1/2+y,z", &
      "-x,-y,-z", &
      "loop_", &
      "_atom_site_label", &
      "_atom_site_fract_x", &
      "_atom_site_fract_y", &
      "_atom_site_fract_z", &
      "Mg 0.00000 0.00000 0.00000", &
      "O 0.50000 0.50000 0.50000"
   rewind(unit)

   call read_cif(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, struc%nat, 8, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, count(struc%num(struc%id) == 12), 4, "Number of magnesium atoms does not match")
   if (allocated(error)) return
   call check(error, struc%lattice(1, 1), 4.1749_wp*aatoau, thr=thr)
   if (allocated(error)) return

end subroutine test_valid3_cif


subroutine test_valid4_cif(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status="scratch", newunit=unit)
   write(unit, "(a)") &
      "data_global", &
      "loop_", &
      "_publ_author_name", &
      "_publ_author_address", &
      "'Clegg, William'", &
      ";", &
      "School of Chemistry", &
      ";", &
      "data_I", &
      "_cell_length_a 5.8166(16)", &
      "_cell_length_b 6.8339(19)", &
      "_cell_length_c 12.263(3)", &
      "_cell_angle_alpha 87.642(5)", &
      "_cell_angle_beta 81.726(5)", &
      "_cell_angle_gamma 73.017(5)", &
      "loop_", &
      "_symmetry_equiv_pos_as_xyz", &
      "'x, y, z'", &
      "'-x, -y, -z'", &
      "loop_", &
      "_atom_site_label", &
      "_atom_site_fract_x", &
      "_atom_site_fract_y", &
      "_atom_site_fract_z", &
      "_atom_site_occupancy", &
      "Co 0.5000 0.5000 0.5000 1", &
      "Cl1 0.26740(13) 0.26735(11) 0.45695(6) 1", &
      "H1O 0.095(4) 0.608(5) 0.613(3) 1", &
      "loop_", &
      "_atom_site_aniso_label", &
      "_atom_site_aniso_U_11", &
      "Co 0.01"
   rewind(unit)

   call read_cif(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, struc%nat, 5, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, all(struc%num(struc%id) == [27, 17, 17, 1, 1]), &
      & "Elements of expanded atom sites do not match")
   if (allocated(error)) return
   call check(error, allocated(struc%comment), "Data block name should be saved")
   if (allocated(error)) return
   call check(error, struc%comment == "I", "Comment does not match data block name")
   if (allocated(error)) return
   call check(error, all(abs(struc%xyz(:, 3) - matmul(struc%lattice, &
      & -[0.26740_wp, 0.26735_wp, 0.45695_wp])) < thr), &
      & "Symmetry image should not be wrapped into the cell")
   if (allocated(error)) return

end subroutine test_valid4_cif


subroutine test_valid5_cif(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status="scratch", newunit=unit)
   write(unit, "(a)") &
      "data_HgI", &
      "_cell_length_a 5.0", &
      "_cell_length_b 5.0", &
      "_cell_length_c 5.0", &
      "_cell_angle_alpha 90", &
      "_cell_angle_beta 90", &
      "_cell_angle_gamma 90", &
      "_symmetry_space_group_name_H-M 'P 1'", &
      "loop_", &
      "_atom_site_label", &
      "_atom_site_type_symbol", &
      "_atom_site_fract_x", &
      "_atom_site_fract_y", &
      "_atom_site_fract_z", &
      "X1 Hg2+ 0 0 0", &
      "X2 I1- 0.5 0.5 0.5"
   rewind(unit)

   call read_cif(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, struc%nat, 2, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, all(struc%num(struc%id) == [80, 53]), &
      & "Type symbols should determine the elements")
   if (allocated(error)) return

end subroutine test_valid5_cif


subroutine test_invalid1_cif(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status="scratch", newunit=unit)
   write(unit, "(a)") &
      "data_missing_b", &
      "_cell_length_a 5.0", &
      "_cell_length_c 5.0", &
      "loop_", &
      "_atom_site_label", &
      "_atom_site_fract_x", &
      "_atom_site_fract_y", &
      "_atom_site_fract_z", &
      "Na 0 0 0"
   rewind(unit)

   call read_cif(struc, unit, error)
   close(unit)

end subroutine test_invalid1_cif


subroutine test_invalid2_cif(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status="scratch", newunit=unit)
   write(unit, "(a)") &
      "data_no_atoms", &
      "_cell_length_a 5.0", &
      "_cell_length_b 5.0", &
      "_cell_length_c 5.0"
   rewind(unit)

   call read_cif(struc, unit, error)
   close(unit)

end subroutine test_invalid2_cif


subroutine test_invalid3_cif(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status="scratch", newunit=unit)
   write(unit, "(a)") &
      "data_unknown_element", &
      "_cell_length_a 5.0", &
      "_cell_length_b 5.0", &
      "_cell_length_c 5.0", &
      "loop_", &
      "_atom_site_label", &
      "_atom_site_fract_x", &
      "_atom_site_fract_y", &
      "_atom_site_fract_z", &
      "X1 0 0 0"
   rewind(unit)

   call read_cif(struc, unit, error)
   close(unit)

end subroutine test_invalid3_cif


subroutine test_invalid4_cif(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status="scratch", newunit=unit)
   write(unit, "(a)") &
      "data_bad_symop", &
      "_cell_length_a 5.0", &
      "_cell_length_b 5.0", &
      "_cell_length_c 5.0", &
      "loop_", &
      "_symmetry_equiv_pos_as_xyz", &
      "'x, y'", &
      "loop_", &
      "_atom_site_label", &
      "_atom_site_fract_x", &
      "_atom_site_fract_y", &
      "_atom_site_fract_z", &
      "Na 0 0 0"
   rewind(unit)

   call read_cif(struc, unit, error)
   close(unit)

end subroutine test_invalid4_cif


subroutine test_invalid5_cif(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status="scratch", newunit=unit)
   write(unit, "(a)") &
      "data_no_symops", &
      "_symmetry_space_group_name_H-M 'P 21/c'", &
      "_cell_length_a 5.0", &
      "_cell_length_b 5.0", &
      "_cell_length_c 5.0", &
      "_cell_angle_beta 100.0", &
      "loop_", &
      "_atom_site_label", &
      "_atom_site_fract_x", &
      "_atom_site_fract_y", &
      "_atom_site_fract_z", &
      "Na 0.1 0.2 0.3"
   rewind(unit)

   call read_cif(struc, unit, error)
   close(unit)

end subroutine test_invalid5_cif


subroutine test_invalid6_cif(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status="scratch", newunit=unit)
   write(unit, "(a)") &
      "data_unknown_coordinate", &
      "_cell_length_a 5.0", &
      "_cell_length_b 5.0", &
      "_cell_length_c 5.0", &
      "loop_", &
      "_atom_site_label", &
      "_atom_site_fract_x", &
      "_atom_site_fract_y", &
      "_atom_site_fract_z", &
      "Na 0 ? 0"
   rewind(unit)

   call read_cif(struc, unit, error)
   close(unit)

end subroutine test_invalid6_cif


subroutine test_invalid7_cif(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status="scratch", newunit=unit)
   write(unit, "(a)") &
      "data_cartesian", &
      "_cell_length_a 5.0", &
      "_cell_length_b 5.0", &
      "_cell_length_c 5.0", &
      "loop_", &
      "_atom_site_label", &
      "_atom_site_Cartn_x", &
      "_atom_site_Cartn_y", &
      "_atom_site_Cartn_z", &
      "Na 0 0 0"
   rewind(unit)

   call read_cif(struc, unit, error)
   close(unit)

end subroutine test_invalid7_cif


subroutine test_valid6_cif(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status="scratch", newunit=unit)
   write(unit, "(a)") &
      "data_full_occupancy", &
      "_cell_length_a 5.0", &
      "_cell_length_b 5.0", &
      "_cell_length_c 5.0", &
      "loop_", &
      "_atom_site_label", &
      "_atom_site_fract_x", &
      "_atom_site_fract_y", &
      "_atom_site_fract_z", &
      "_atom_site_occupancy", &
      "_atom_site_disorder_group", &
      "Na1 0 0 0 1.000(2) .", &
      "Cl1 0.5 0.5 0.5 ? ?"
   rewind(unit)

   call read_cif(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, struc%nat, 2, "Number of atoms does not match")
   if (allocated(error)) return

end subroutine test_valid6_cif


subroutine test_invalid8_cif(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status="scratch", newunit=unit)
   write(unit, "(a)") &
      "data_partial_occupancy", &
      "_cell_length_a 5.0", &
      "_cell_length_b 5.0", &
      "_cell_length_c 5.0", &
      "loop_", &
      "_atom_site_label", &
      "_atom_site_fract_x", &
      "_atom_site_fract_y", &
      "_atom_site_fract_z", &
      "_atom_site_occupancy", &
      "Fe1 0 0 0 0.5", &
      "Mg1 0 0 0 0.5"
   rewind(unit)

   call read_cif(struc, unit, error)
   close(unit)

end subroutine test_invalid8_cif


subroutine test_invalid9_cif(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status="scratch", newunit=unit)
   write(unit, "(a)") &
      "data_disorder", &
      "_cell_length_a 5.0", &
      "_cell_length_b 5.0", &
      "_cell_length_c 5.0", &
      "loop_", &
      "_atom_site_label", &
      "_atom_site_fract_x", &
      "_atom_site_fract_y", &
      "_atom_site_fract_z", &
      "_atom_site_occupancy", &
      "_atom_site_disorder_group", &
      "C1A 0.10 0 0 1 1", &
      "C1B 0.12 0 0 1 2"
   rewind(unit)

   call read_cif(struc, unit, error)
   close(unit)

end subroutine test_invalid9_cif


subroutine test_invalid10_cif(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status="scratch", newunit=unit)
   write(unit, "(a)") &
      "data_zero_length", &
      "_cell_length_a 5.0", &
      "_cell_length_b 5.0", &
      "_cell_length_c 0.0", &
      "loop_", &
      "_atom_site_label", &
      "_atom_site_fract_x", &
      "_atom_site_fract_y", &
      "_atom_site_fract_z", &
      "Na 0 0 0"
   rewind(unit)

   call read_cif(struc, unit, error)
   close(unit)

end subroutine test_invalid10_cif


subroutine test_invalid11_cif(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status="scratch", newunit=unit)
   write(unit, "(a)") &
      "data_negative_length", &
      "_cell_length_a -5.0", &
      "_cell_length_b 5.0", &
      "_cell_length_c 5.0", &
      "loop_", &
      "_atom_site_label", &
      "_atom_site_fract_x", &
      "_atom_site_fract_y", &
      "_atom_site_fract_z", &
      "Na 0 0 0"
   rewind(unit)

   call read_cif(struc, unit, error)
   close(unit)

end subroutine test_invalid11_cif


subroutine test_invalid12_cif(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status="scratch", newunit=unit)
   write(unit, "(a)") &
      "data_flat_cell", &
      "_cell_length_a 5.0", &
      "_cell_length_b 5.0", &
      "_cell_length_c 5.0", &
      "_cell_angle_alpha 45.0", &
      "_cell_angle_beta 45.0", &
      "_cell_angle_gamma 90.0", &
      "loop_", &
      "_atom_site_label", &
      "_atom_site_fract_x", &
      "_atom_site_fract_y", &
      "_atom_site_fract_z", &
      "Na 0 0 0"
   rewind(unit)

   call read_cif(struc, unit, error)
   close(unit)

end subroutine test_invalid12_cif


subroutine test_invalid13_cif(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status="scratch", newunit=unit)
   write(unit, "(a)") &
      "data_nan_angle", &
      "_cell_length_a 5.0", &
      "_cell_length_b 5.0", &
      "_cell_length_c 5.0", &
      "_cell_angle_alpha NaN", &
      "loop_", &
      "_atom_site_label", &
      "_atom_site_fract_x", &
      "_atom_site_fract_y", &
      "_atom_site_fract_z", &
      "Na 0 0 0"
   rewind(unit)

   call read_cif(struc, unit, error)
   close(unit)

end subroutine test_invalid13_cif


end module test_read_cif
