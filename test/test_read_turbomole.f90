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

module test_read_turbomole
   use mctc_env_testing, only : new_unittest, unittest_type, error_type, check
   use mctc_io_read_turbomole
   use mctc_io_structure
   implicit none
   private

   public :: collect_read_turbomole


contains


!> Collect all exported unit tests
subroutine collect_read_turbomole(testsuite)

   !> Collection of tests
   type(unittest_type), allocatable, intent(out) :: testsuite(:)

   testsuite = [ &
      & new_unittest("valid1-coord", test_valid1_coord), &
      & new_unittest("valid2-coord", test_valid2_coord), &
      & new_unittest("valid3-coord", test_valid3_coord), &
      & new_unittest("valid4-coord", test_valid4_coord), &
      & new_unittest("valid5-coord", test_valid5_coord), &
      & new_unittest("valid6-coord", test_valid6_coord), &
      & new_unittest("valid7-coord", test_valid7_coord), &
      & new_unittest("invalid1-coord", test_invalid1_coord, should_fail=.true.), &
      & new_unittest("invalid2-coord", test_invalid2_coord, should_fail=.true.), &
      & new_unittest("invalid3-coord", test_invalid3_coord, should_fail=.true.), &
      & new_unittest("invalid4-coord", test_invalid4_coord, should_fail=.true.), &
      & new_unittest("invalid5-coord", test_invalid5_coord, should_fail=.true.), &
      & new_unittest("invalid6-coord", test_invalid6_coord, should_fail=.true.), &
      & new_unittest("invalid7-coord", test_invalid7_coord, should_fail=.true.) &
      & ]

end subroutine collect_read_turbomole


subroutine test_valid1_coord(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "$coord angs", &
      " 1.1847029  1.1150792 -0.0344641 O", &
      " 0.4939088  0.9563767  0.6340089 H", &
      " 2.0242676  1.0811246  0.4301417 H", &
      "-1.1469443  0.0697649  1.1470196 O", &
      "-1.2798308 -0.5232169  1.8902833 H", &
      "-1.0641398 -0.4956693  0.3569250 H", &
      "-0.1633508 -1.0289346 -1.2401808 O", &
      " 0.4914771 -0.3248733 -1.0784838 H", &
      "-0.5400907 -0.8496512 -2.1052499 H", &
      "$end"
   rewind(unit)

   call read_coord(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, struc%nat, 9, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, 2, "Number of species does not match")
   if (allocated(error)) return

end subroutine test_valid1_coord


subroutine test_valid2_coord(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "$coord", &
      "    1.36794785746435     13.45808943446053      8.83754983226359      c", &
      "    3.69183290816438     13.13552229161569     10.16652201690950      c", &
      "    1.36792668081267     10.38660504434782     13.04411926632965      c", &
      "    3.69180534781206     11.55414582295511     12.33193380846742      c", &
      "    1.36791549262702      3.53066844289674     10.38660588677206      c", &
      "    1.36792046664920      7.73723910626293     13.45809224934817      c", &
      "    3.69181279359489      6.40826717723392     13.13552570942280      c", &
      "    1.36792009865062      3.11669712338516      7.73723850632628      c", &
      "    3.69181515738094      3.43926499914873      6.40826580885474      c", &
      "    3.69178443989294      4.24285720771059     11.55415026712869      c", &
      "    1.36790824853106      6.18818490375705      3.53066863732142      c", &
      "    3.69178194163078      5.02063901427657      4.24285736953327      c", &
      "    1.36794124909207     13.04411858182861      6.18818324080182      c", &
      "    1.36792249732236      8.83755133592807      3.11669686076913      c", &
      "    3.69182456413952     10.16652118921143      3.43926084011816      c", &
      "    3.69181444966104     12.33193631088573      5.02063847821044      c", &
      "    6.01572566324028     13.45790756713123      8.83752222635545      c", &
      "    8.33965926123256     13.13576644753615     10.16660228658307      c", &
      "    6.01574747573805     10.38654070512969     13.04391961251944      c", &
      "    8.33964066450677     11.55427002850905     12.33211653730939      c", &
      "    6.01574728097580      3.53087013230607     10.38654217813321      c", &
      "    6.01568913853645      7.73726406411719     13.45790864082374      c", &
      "    8.33963586549168      6.40818371470975     13.13576911116618      c", &
      "    6.01568179676984      3.11688332536281      7.73726611148835      c", &
      "    8.33963704688671      3.43902559351770      6.40818390180453      c", &
      "    8.33962496288127      4.24267007149867     11.55427031066552      c", &
      "    6.01573464280675      6.18824653544318      3.53086861480278      c", &
      "    8.33961857277245      5.02052001792996      4.24267413625204      c", &
      "    6.01575677304189     13.04392044501564      6.18824448603611      c", &
      "    6.01568344836224      8.83752193432504      3.11688171781516      c", &
      "    8.33964228963694     10.16660428027860      3.43902155668011      c", &
      "    8.33965118613331     12.33211762632282      5.02051902430387      c", &
      "$periodic 1", &
      "$cell", &
      " 9.29556285275863798006", &
      "$end"
   rewind(unit)

   call read_coord(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, struc%nat, 32, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, 1, "Number of species does not match")
   if (allocated(error)) return

end subroutine test_valid2_coord


subroutine test_valid3_coord(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "$coord", &
      "   -0.12918412100093      0.06210659750976     -2.13384498734326  c", &
      "    0.12856915667443     -0.07403227791901      4.02358027265954  c", &
      "   -0.12317720857511      2.75170732207802     -2.13345350602279  c", &
      "    2.44816466162280      1.28612566399214      4.02317048854901  c", &
      "$periodic 2", &
      "$cell  angs", &
      "    2.4809835980     2.4811430162   120.2612191150", &
      "$end"
   rewind(unit)

   call read_coord(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, struc%nat, 4, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, 1, "Number of species does not match")
   if (allocated(error)) return

end subroutine test_valid3_coord


subroutine test_valid4_coord(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "$cell", &
      "  4.766080896955 4.766080896955 4.766080896955 60. 60. 60.", &
      "$coord", &
      "    0.00000000000000      0.00000000000000      0.00000000000000      c", &
      "    2.38304045219106      1.39084904447079      0.97287218605834      c", &
      "$periodic 3", &
      "$end"
   rewind(unit)

   call read_coord(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, struc%nat, 2, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, 1, "Number of species does not match")
   if (allocated(error)) return

end subroutine test_valid4_coord


subroutine test_valid5_coord(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "$coord frac", &
      "    0.25000000000000      0.25000000000000      0.25000000000000      f", &
      "    0.75000000000000      0.75000000000000      0.75000000000000      f", &
      "    0.00000000000000      0.00000000000000      0.00000000000000      ca", &
      "$user-defined bonds", &
      "$lattice angs", &
      "       3.153833580475253       1.115048555743951       1.931320751454818", &
      "       0.000000000000000       3.345145667231851       1.931320751454818", &
      "       0.000000000000000       0.000000000000000       3.862641502909638", &
      "$periodic 3", &
      "$end"
   rewind(unit)

   call read_coord(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, struc%nat, 3, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, 2, "Number of species does not match")
   if (allocated(error)) return

end subroutine test_valid5_coord


subroutine test_valid6_coord(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "$cell", &
      " 9.09903133 9.09903130512 30.4604956 90.0 90.0 120.000000127", &
      "$coord", &
      "   -0.57949455800000      0.06835893310000     -7.51993484000000      ca", &
      "   -0.57949455800000      0.06835893310000      7.71031294000000      mg", &
      "   -0.57949455800000      0.06835893310000     -0.10280417200000      c", &
      "    1.73848367000000     -0.20507679900000     -0.08757392470000      o", &
      "$periodic 3", &
      "$end"
   rewind(unit)

   call read_coord(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, struc%nat, 4, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, 4, "Number of species does not match")
   if (allocated(error)) return

end subroutine test_valid6_coord


subroutine test_valid7_coord(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "$coord", &
      "    2.82781861325240      2.96439280874170      3.12827803849279  o", &
      "    7.19124230791576      0.98723342603994      4.89004701836746  o", &
      "    4.95491880597601      4.82830910314898      8.74847811174740  o", &
      "    0.19290883043307      2.30645007856310      8.72969832061507  o", &
      "   -2.01592208020090      6.16478744235115      4.87273962147340  o", &
      "    0.66183062221384      7.07392578563696      0.27767968372345  o", &
      "    4.55701736204879      0.06291337111965      3.31745840478609  si", &
      "   -2.10064209975148      3.63969476409878      6.81014625000326  si", &
      "    2.31009832827224      4.12572862149043      0.08842485276656  si", &
      "$user-defined bonds", &
      "$cell", &
      "  9.28422449595511046    9.28422449595511046    10.21434769907115   90.0000   90.0000  120.0000", &
      "$periodic 3", &
      "$end"
   rewind(unit)

   call read_coord(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, struc%nat, 9, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, 2, "Number of species does not match")
   if (allocated(error)) return

end subroutine test_valid7_coord


subroutine test_invalid1_coord(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "$end"
   rewind(unit)

   call read_coord(struc, unit, error)
   close(unit)
   if (allocated(error)) return

end subroutine test_invalid1_coord


subroutine test_invalid2_coord(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "$coord", &
      "    2.82781861325240      2.96439280874170      3.12827803849279  o", &
      "    7.19124230791576      0.98723342603994      4.89004701836746  o", &
      "    4.95491880597601      4.82830910314898      8.74847811174740  o", &
      "    0.19290883043307      2.30645007856310      8.72969832061507  o", &
      "   -2.01592208020090      6.16478744235115      4.87273962147340  o", &
      "    0.66183062221384      7.07392578563696      0.27767968372345  o", &
      "    4.55701736204879      0.06291337111965      3.31745840478609  si", &
      "   -2.10064209975148      3.63969476409878      6.81014625000326  si", &
      "    2.31009832827224      4.12572862149043      0.08842485276656  si", &
      "$periodic 3", &
      "$end"
   rewind(unit)

   call read_coord(struc, unit, error)
   close(unit)
   if (allocated(error)) return

end subroutine test_invalid2_coord


subroutine test_invalid3_coord(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "$coord", &
      "    2.82781861325240      2.96439280874170      3.12827803849279  o", &
      "    7.19124230791576      0.98723342603994      4.89004701836746  o", &
      "    4.95491880597601      4.82830910314898      8.74847811174740  o", &
      "    0.19290883043307      2.30645007856310      8.72969832061507  o", &
      "   -2.01592208020090      6.16478744235115      4.87273962147340  o", &
      "    0.66183062221384      7.07392578563696      0.27767968372345  o", &
      "    4.55701736204879      0.06291337111965      3.31745840478609  si", &
      "   -2.10064209975148      3.63969476409878      6.81014625000326  si", &
      "    2.31009832827224      4.12572862149043      0.08842485276656  si", &
      "$user-defined bonds", &
      "$cell", &
      "  9.28422449595511046    9.28422449595511046    10.21434769907115   90.0000   90.0000  120.0000", &
      "$end"
   rewind(unit)

   call read_coord(struc, unit, error)
   close(unit)
   if (allocated(error)) return

end subroutine test_invalid3_coord


subroutine test_invalid4_coord(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "$coord angs", &
      "-1.1469443  0.0697649  1.1470196 ***", &
      "-1.2798308 -0.5232169  1.8902833 H", &
      "-1.0641398 -0.4956693  0.3569250 H", &
      "$end"
   rewind(unit)

   call read_coord(struc, unit, error)
   close(unit)
   if (allocated(error)) return

end subroutine test_invalid4_coord


subroutine test_invalid5_coord(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "$coord frac", &
      "    0.25000000000000      0.25000000000000      0.25000000000000      f", &
      "    0.75000000000000      0.75000000000000      0.75000000000000      f", &
      "    0.00000000000000      0.00000000000000      0.00000000000000      ca", &
      "$user-defined bonds", &
      "$lattice angs", &
      "       3.153833580475253       1.115048555743951       1.931320751454818", &
      "       0.000000000000000       3.345145667231851       1.931320751454818", &
      "$periodic 3", &
      "$end"
   rewind(unit)

   call read_coord(struc, unit, error)
   close(unit)
   if (allocated(error)) return

end subroutine test_invalid5_coord


subroutine test_invalid6_coord(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "$coord frac", &
      "    0.25000000000000      0.25000000000000      0.25000000000000      f", &
      "    0.75000000000000      0.75000000000000      0.75000000000000      f", &
      "    0.00000000000000      0.00000000000000      0.00000000000000      ca", &
      "$periodic 0", &
      "$end"
   rewind(unit)

   call read_coord(struc, unit, error)
   close(unit)
   if (allocated(error)) return

end subroutine test_invalid6_coord


subroutine test_invalid7_coord(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "$coord", &
      "    1.36794785746435     13.45808943446053      8.83754983226359      c", &
      "    3.69183290816438     13.13552229161569     10.16652201690950      c", &
      "    1.36792668081267     10.38660504434782     13.04411926632965      c", &
      "    3.69180534781206     11.55414582295511     12.33193380846742      c", &
      "    1.36791549262702      3.53066844289674     10.38660588677206      c", &
      "    1.36792046664920      7.73723910626293     13.45809224934817      c", &
      "    3.69181279359489      6.40826717723392     13.13552570942280      c", &
      "    1.36792009865062      3.11669712338516      7.73723850632628      c", &
      "    3.69181515738094      3.43926499914873      6.40826580885474      c", &
      "    3.69178443989294      4.24285720771059     11.55415026712869      c", &
      "    1.36790824853106      6.18818490375705      3.53066863732142      c", &
      "    3.69178194163078      5.02063901427657      4.24285736953327      c", &
      "    1.36794124909207     13.04411858182861      6.18818324080182      c", &
      "    1.36792249732236      8.83755133592807      3.11669686076913      c", &
      "    3.69182456413952     10.16652118921143      3.43926084011816      c", &
      "    3.69181444966104     12.33193631088573      5.02063847821044      c", &
      "    6.01572566324028     13.45790756713123      8.83752222635545      c", &
      "    8.33965926123256     13.13576644753615     10.16660228658307      c", &
      "    6.01574747573805     10.38654070512969     13.04391961251944      c", &
      "    8.33964066450677     11.55427002850905     12.33211653730939      c", &
      "    6.01574728097580      3.53087013230607     10.38654217813321      c", &
      "    6.01568913853645      7.73726406411719     13.45790864082374      c", &
      "    8.33963586549168      6.40818371470975     13.13576911116618      c", &
      "    6.01568179676984      3.11688332536281      7.73726611148835      c", &
      "    8.33963704688671      3.43902559351770      6.40818390180453      c", &
      "    8.33962496288127      4.24267007149867     11.55427031066552      c", &
      "    6.01573464280675      6.18824653544318      3.53086861480278      c", &
      "    8.33961857277245      5.02052001792996      4.24267413625204      c", &
      "    6.01575677304189     13.04392044501564      6.18824448603611      c", &
      "    6.01568344836224      8.83752193432504      3.11688171781516      c", &
      "    8.33964228963694     10.16660428027860      3.43902155668011      c", &
      "    8.33965118613331     12.33211762632282      5.02051902430387      c", &
      "$periodic 1", &
      "$lattice", &
      " 9.29556285275863798006", &
      "$cell", &
      " 9.29556285275863798006", &
      "$end"
   rewind(unit)

   call read_coord(struc, unit, error)
   close(unit)
   if (allocated(error)) return

end subroutine test_invalid7_coord


end module test_read_turbomole
