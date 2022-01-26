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
   use mctc_env, only : wp
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
      & new_unittest("valid8-coord", test_valid8_coord), &
      & new_unittest("valid9-coord", test_valid9_coord), &
      & new_unittest("valid10-coord", test_valid10_coord), &
      & new_unittest("valid11-coord", test_valid11_coord), &
      & new_unittest("invalid1-coord", test_invalid1_coord, should_fail=.true.), &
      & new_unittest("invalid2-coord", test_invalid2_coord, should_fail=.true.), &
      & new_unittest("invalid3-coord", test_invalid3_coord, should_fail=.true.), &
      & new_unittest("invalid4-coord", test_invalid4_coord, should_fail=.true.), &
      & new_unittest("invalid5-coord", test_invalid5_coord, should_fail=.true.), &
      & new_unittest("invalid6-coord", test_invalid6_coord, should_fail=.true.), &
      & new_unittest("invalid7-coord", test_invalid7_coord, should_fail=.true.), &
      & new_unittest("invalid8-coord", test_invalid8_coord, should_fail=.true.), &
      & new_unittest("invalid9-coord", test_invalid9_coord, should_fail=.true.), &
      & new_unittest("invalid10-coord", test_invalid10_coord, should_fail=.true.), &
      & new_unittest("invalid11-coord", test_invalid11_coord, should_fail=.true.), &
      & new_unittest("invalid12-coord", test_invalid12_coord, should_fail=.true.), &
      & new_unittest("invalid13-coord", test_invalid13_coord, should_fail=.true.), &
      & new_unittest("invalid14-coord", test_invalid14_coord, should_fail=.true.), &
      & new_unittest("invalid15-coord", test_invalid15_coord, should_fail=.true.), &
      & new_unittest("invalid16-coord", test_invalid16_coord, should_fail=.true.) &
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
      "$eht charge=0 unpaired=0", &
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
      "$eht unpaired=0 charge=0", &
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


subroutine test_valid8_coord(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "$coord", &
      "   -1.79537625851198     -3.77866422935275     -1.07883558363403      h", &
      "   -2.68278833302782      0.38892666265890      1.66214865238427      s", &
      "    0.11484649791305      1.48857933226955      3.65660396510375      b", &
      "   -1.07998879593946     -0.16259121615748     -4.55703065871422      o", &
      "    0.60302832999383      4.08816149622342     -0.02589373148029      mg", &
      "   -1.22534089315880     -1.79981382478068     -3.70773173318592      h", &
      "   -1.33460982049866     -4.24819082475503      2.72791902701083      h", &
      "   -0.16278082578516      2.41267994179303      5.69030695190570      h", &
      "    2.87802444057103     -0.33120525058830      1.88311373530297      si", &
      "    0.68489327931487      0.32790204044961     -4.20547693710673      h", &
      "   -1.20919773588330     -2.87253762561437      0.94064204223101      b", &
      "   -3.25572604597922      2.21241092990940     -2.86715549314771      li", &
      "   -1.83147468262373      5.20527293771933     -2.26976270603341      f", &
      "    4.90885865772880     -1.92576561961811      2.99069919443735      h", &
      "    1.26806242248758     -2.60409341782411      0.55162805282247      h", &
      "    4.11956976339902      1.59892866766766     -1.39117477789609      s", &
      "$eht unpaired=1", &
      "$end"
   rewind(unit)

   call read_coord(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, struc%nat, 16, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, 8, "Number of species does not match")
   if (allocated(error)) return
   call check(error, struc%uhf, 1, "Number of unpaired electrons does not match")
   if (allocated(error)) return

end subroutine test_valid8_coord


subroutine test_valid9_coord(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "$coord", &
      "    4.82824919102333E-02    5.71831000079710E-02    1.73514614763116E-01      C", &
      "    4.82824919102333E-02    5.71831000079710E-02    2.78568246476372E+00      N", &
      "    2.46093310136750E+00    5.71831000079710E-02    3.59954953387915E+00      C", &
      "    3.99138416000780E+00   -2.21116805417472E-01    1.58364683739854E+00      N", &
      "    2.54075511539052E+00   -1.18599185608072E-01   -5.86344093538442E-01      C", &
      "   -2.06104824371096E+00    8.28021114689117E-01    4.40357113204146E+00      C", &
      "    6.72173545596011E+00    2.10496546922931E-01    1.72565972456309E+00      C", &
      "    3.05878562448454E+00    7.09403031823937E-02    5.55721088395376E+00      H", &
      "    3.36822820962351E+00   -2.07680855613880E-01   -2.46191575873710E+00      H", &
      "   -1.68465267663933E+00    1.48551338123814E-01   -9.21486948343917E-01      H", &
      "   -3.83682349412373E+00    3.78984491295393E-01    3.43261116458953E+00      H", &
      "   -1.96215889726624E+00   -2.17412943024358E-01    6.19219651728748E+00      H", &
      "   -1.85966017471395E+00    2.87036107386343E+00    4.74746341688781E+00      H", &
      "    7.49947096948557E+00   -8.77758695396645E-01    3.31081834253025E+00      H", &
      "    7.58490546886959E+00   -4.29156708916399E-01   -4.73754235690626E-02      H", &
      "    7.00829346274163E+00    2.24769645216395E+00    2.03795579552532E+00      H", &
      "$eht charge=1", &
      "$end"
   rewind(unit)

   call read_coord(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, struc%nat, 16, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, 3, "Number of species does not match")
   if (allocated(error)) return
   call check(error, struc%charge, 1.0_wp, "Total charge does not match")
   if (allocated(error)) return

end subroutine test_valid9_coord


subroutine test_valid10_coord(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "$cell", &
      " 8.00000006 ", &
      "$periodic 1", &
      "$coord", &
      "   -2.00000001000000      3.50586945000000      0.00000000000000      b", &
      "   -2.00000001000000      2.98408124000000      2.61870552000000      n", &
      "   -2.00000001000000      1.49889804000000     -1.76123460000000      n", &
      "   -2.00000001000000      5.56753332000000     -0.69908457400000      h", &
      "   -2.00000001000000      0.45532164600000      3.47617645000000      b", &
      "   -2.00000001000000     -1.02986157000000     -0.90376369200000      b", &
      "   -2.00000001000000     -1.55164977000000      1.71494186000000      n", &
      "   -2.00000001000000      0.02991464770000      5.61117202000000      h", &
      "   -2.00000001000000     -2.66611845000000     -2.33967477000000      h", &
      "   -2.00000001000000      4.53085539000000      3.97609015000000      h", &
      "   -2.00000001000000     -3.50056641000000      2.37579525000000      h", &
      "   -2.00000001000000      1.90104056000000     -3.77947261000000      h", &
      "    2.00000001000000      1.55164977000000     -1.71494183000000      b", &
      "    2.00000001000000      1.02986156000000      0.90376368700000      n", &
      "    2.00000001000000     -0.45532163900000     -3.47617644000000      n", &
      "    2.00000001000000      3.61331364000000     -2.41402641000000      h", &
      "    2.00000001000000     -1.49889804000000      1.76123461000000      b", &
      "    2.00000001000000     -2.98408125000000     -2.61870553000000      b", &
      "    2.00000001000000     -3.50586946000000      0.00000002473480      n", &
      "    2.00000001000000     -1.92430504000000      3.89623019000000      h", &
      "    2.00000001000000     -4.62033813000000     -4.05461660000000      h", &
      "    2.00000001000000      2.57663570000000      2.26114832000000      h", &
      "    2.00000001000000     -5.45478609000000      0.66085341700000      h", &
      "    2.00000001000000     -0.05317912580000     -5.49441445000000      h", &
      "$user-defined bonds", &
      "$end"
   rewind(unit)

   call read_coord(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, struc%nat, 24, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, 3, "Number of species does not match")
   if (allocated(error)) return
   call check(error, count(struc%periodic), 1, "Periodic of system does not match")
   if (allocated(error)) return

end subroutine test_valid10_coord


subroutine test_valid11_coord(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "$coord frac", &
      "    0.00000000000000      0.00000000000000      0.00000000000000      mg", &
      "    0.50000000000000      0.00000000000000      0.00000000000000      o", &
      "    0.00000000000000      0.50000000000000      0.00000000000000      o", &
      "    0.00000000000000      0.00000000000000      3.97881835572287      o", &
      "    0.50000000000000      0.50000000000000      0.00000000000000      mg", &
      "    0.50000000000000      0.00000000000000      3.97881835572287      mg", &
      "    0.00000000000000      0.50000000000000      3.97881835572287      mg", &
      "    0.50000000000000      0.50000000000000      3.97881835572287      o", &
      "$periodic 2", &
      "$lattice", &
      " 5.626898880882 -5.626898880882", &
      " 5.626898880882  5.626898880882", &
      "$end"
   rewind(unit)

   call read_coord(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, struc%nat, 8, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, 2, "Number of species does not match")
   if (allocated(error)) return
   call check(error, count(struc%periodic), 2, "Periodic of system does not match")
   if (allocated(error)) return

end subroutine test_valid11_coord


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
      "-1.1469443  0.0697649  1.1470196 --->o", &
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


subroutine test_invalid8_coord(error)

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
      "-1.1469443  abcd.efgh  1.1470196 O", &
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

end subroutine test_invalid8_coord


subroutine test_invalid9_coord(error)

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
      "$periodic 4", &
      "$end"
   rewind(unit)

   call read_coord(struc, unit, error)
   close(unit)

end subroutine test_invalid9_coord


subroutine test_invalid10_coord(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "$coord", &
      "    4.82824919102333E-02    5.71831000079710E-02    1.73514614763116E-01      C", &
      "    4.82824919102333E-02    5.71831000079710E-02    2.78568246476372E+00      N", &
      "    2.46093310136750E+00    5.71831000079710E-02    3.59954953387915E+00      C", &
      "    3.99138416000780E+00   -2.21116805417472E-01    1.58364683739854E+00      N", &
      "    2.54075511539052E+00   -1.18599185608072E-01   -5.86344093538442E-01      C", &
      "   -2.06104824371096E+00    8.28021114689117E-01    4.40357113204146E+00      C", &
      "    6.72173545596011E+00    2.10496546922931E-01    1.72565972456309E+00      C", &
      "    3.05878562448454E+00    7.09403031823937E-02    5.55721088395376E+00      H", &
      "    3.36822820962351E+00   -2.07680855613880E-01   -2.46191575873710E+00      H", &
      "   -1.68465267663933E+00    1.48551338123814E-01   -9.21486948343917E-01      H", &
      "   -3.83682349412373E+00    3.78984491295393E-01    3.43261116458953E+00      H", &
      "   -1.96215889726624E+00   -2.17412943024358E-01    6.19219651728748E+00      H", &
      "   -1.85966017471395E+00    2.87036107386343E+00    4.74746341688781E+00      H", &
      "    7.49947096948557E+00   -8.77758695396645E-01    3.31081834253025E+00      H", &
      "    7.58490546886959E+00   -4.29156708916399E-01   -4.73754235690626E-02      H", &
      "    7.00829346274163E+00    2.24769645216395E+00    2.03795579552532E+00      H", &
      "$eht charge=one unpaired=0", &
      "$end"
   rewind(unit)

   call read_coord(struc, unit, error)
   close(unit)

end subroutine test_invalid10_coord


subroutine test_invalid11_coord(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "$coord", &
      "    4.82824919102333E-02    5.71831000079710E-02    1.73514614763116E-01      C", &
      "    4.82824919102333E-02    5.71831000079710E-02    2.78568246476372E+00      N", &
      "    2.46093310136750E+00    5.71831000079710E-02    3.59954953387915E+00      C", &
      "    3.99138416000780E+00   -2.21116805417472E-01    1.58364683739854E+00      N", &
      "    2.54075511539052E+00   -1.18599185608072E-01   -5.86344093538442E-01      C", &
      "   -2.06104824371096E+00    8.28021114689117E-01    4.40357113204146E+00      C", &
      "    6.72173545596011E+00    2.10496546922931E-01    1.72565972456309E+00      C", &
      "    3.05878562448454E+00    7.09403031823937E-02    5.55721088395376E+00      H", &
      "    3.36822820962351E+00   -2.07680855613880E-01   -2.46191575873710E+00      H", &
      "   -1.68465267663933E+00    1.48551338123814E-01   -9.21486948343917E-01      H", &
      "   -3.83682349412373E+00    3.78984491295393E-01    3.43261116458953E+00      H", &
      "   -1.96215889726624E+00   -2.17412943024358E-01    6.19219651728748E+00      H", &
      "   -1.85966017471395E+00    2.87036107386343E+00    4.74746341688781E+00      H", &
      "    7.49947096948557E+00   -8.77758695396645E-01    3.31081834253025E+00      H", &
      "    7.58490546886959E+00   -4.29156708916399E-01   -4.73754235690626E-02      H", &
      "    7.00829346274163E+00    2.24769645216395E+00    2.03795579552532E+00      H", &
      "$eht unpaired=", &
      "$end"
   rewind(unit)

   call read_coord(struc, unit, error)
   close(unit)

end subroutine test_invalid11_coord


subroutine test_invalid12_coord(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "$coord", &
      "   -0.12918412100093      0.06210659750976     -2.13384498734326  c", &
      "    0.12856915667443     -0.07403227791901      4.02358027265954  c", &
      "$eht unpaired=0 charge=0", &
      "$periodic 2", &
      "$cell  angs", &
      "    2.4809835980     2.4811430162   120.2612191150", &
      "$coord", &
      "   -0.12317720857511      2.75170732207802     -2.13345350602279  c", &
      "    2.44816466162280      1.28612566399214      4.02317048854901  c", &
      "$end"
   rewind(unit)

   call read_coord(struc, unit, error)
   close(unit)

end subroutine test_invalid12_coord


subroutine test_invalid13_coord(error)

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
      "$cell  angs", &
      "    2.4809835980     2.4811430162   120.2612191150", &
      "$eht unpaired=0 charge=0", &
      "$periodic 2", &
      "$cell  angs", &
      "    2.4809835980     2.4811430162   120.2612191150", &
      "$end"
   rewind(unit)

   call read_coord(struc, unit, error)
   close(unit)

end subroutine test_invalid13_coord


subroutine test_invalid14_coord(error)

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
      "$eht unpaired=0 charge=0", &
      "$periodic 2", &
      "$cell  angs", &
      "    2.4809835980     2.4811430162   120.2612191150", &
      "$periodic 2", &
      "$end"
   rewind(unit)

   call read_coord(struc, unit, error)
   close(unit)

end subroutine test_invalid14_coord


subroutine test_invalid15_coord(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "$eht charge=0", &
      "$coord", &
      "   -0.12918412100093      0.06210659750976     -2.13384498734326  c", &
      "    0.12856915667443     -0.07403227791901      4.02358027265954  c", &
      "   -0.12317720857511      2.75170732207802     -2.13345350602279  c", &
      "    2.44816466162280      1.28612566399214      4.02317048854901  c", &
      "$eht unpaired=0", &
      "$periodic 2", &
      "$cell  angs", &
      "    2.4809835980     2.4811430162   120.2612191150", &
      "$end"
   rewind(unit)

   call read_coord(struc, unit, error)
   close(unit)

end subroutine test_invalid15_coord


subroutine test_invalid16_coord(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "$lattice angs", &
      "       3.153833580475253       1.115048555743951       1.931320751454818", &
      "       0.000000000000000       3.345145667231851       1.931320751454818", &
      "       0.000000000000000       0.000000000000000       3.862641502909638", &
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

end subroutine test_invalid16_coord


end module test_read_turbomole
