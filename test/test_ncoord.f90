! This file is part of tblite.
! SPDX-Identifier: LGPL-3.0-or-later
!
! tblite is free software: you can redistribute it and/or modify it under
! the terms of the GNU Lesser General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! tblite is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public License
! along with tblite.  If not, see <https://www.gnu.org/licenses/>.

module test_ncoord
   use mctc_env, only : wp
   use mctc_env_testing, only : new_unittest, unittest_type, error_type, &
   & test_failed
   use mctc_io_structure, only : structure_type
   use testsuite_structure, only : get_structure
   use mctc_cutoff, only : get_lattice_points
   use mctc_data_covrad, only : get_covalent_rad
   use mctc_data_paulingen, only : get_pauling_en
   use mctc_ncoord_gfn
   use mctc_ncoord_exp
   use mctc_ncoord_erf
   use mctc_ncoord_erf_en
   use mctc_ncoord_type !, only : get_coordination_number
   implicit none
   private

   public :: collect_ncoord

   real(wp), parameter :: thr = 100*epsilon(1.0_wp)
   real(wp), parameter :: thr2 = sqrt(epsilon(1.0_wp))


contains


   !> Collect all exported unit tests
   subroutine collect_ncoord(testsuite)

      !> Collection of tests
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
      & new_unittest("cn-mb01_gfn", test_cn_mb01_gfn), &
      & new_unittest("cn-mb02_gfn", test_cn_mb02_gfn), &
      & new_unittest("cn-mb03_gfn", test_cn_mb03_gfn), &
      & new_unittest("cn-acetic_gfn", test_cn_acetic_gfn), &
      & new_unittest("dcndr-mb04_gfn", test_dcndr_mb04_gfn), &
      & new_unittest("dcndr-mb05_gfn", test_dcndr_mb05_gfn), &
      & new_unittest("dcndr-ammonia_gfn", test_dcndr_ammonia_gfn), &
      & new_unittest("dcndL-mb06_gfn", test_dcndL_mb06_gfn), &
      & new_unittest("dcndL-mb07_gfn", test_dcndL_mb07_gfn), &
      & new_unittest("dcndL-antracene_gfn", test_dcndL_anthracene_gfn), &
      & new_unittest("cn-mb01_exp", test_cn_mb01_exp), &
      & new_unittest("cn-mb02_exp", test_cn_mb02_exp), &
      & new_unittest("cn-mb03_exp", test_cn_mb03_exp), &
      & new_unittest("cn-acetic_exp", test_cn_acetic_exp), &
      & new_unittest("dcndr-mb04_exp", test_dcndr_mb04_exp), &
      & new_unittest("dcndr-mb05_exp", test_dcndr_mb05_exp), &
      & new_unittest("dcndr-ammonia_exp", test_dcndr_ammonia_exp), &
      & new_unittest("dcndL-mb06_exp", test_dcndL_mb06_exp), &
      & new_unittest("dcndL-mb07_exp", test_dcndL_mb07_exp), &
      & new_unittest("dcndL-antracene_exp", test_dcndL_anthracene_exp), &
      & new_unittest("cn-mb01_erf", test_cn_mb01_erf), &
      & new_unittest("cn-mb02_erf", test_cn_mb02_erf), &
      & new_unittest("cn-mb03_erf", test_cn_mb03_erf), &
      & new_unittest("cn-acetic_erf", test_cn_acetic_erf), &
      & new_unittest("dcndr-mb04_erf", test_dcndr_mb04_erf), &
      & new_unittest("dcndr-mb05_erf", test_dcndr_mb05_erf), &
      & new_unittest("dcndr-ammonia_erf", test_dcndr_ammonia_erf), &
      & new_unittest("dcndL-mb06_erf", test_dcndL_mb06_erf), &
      & new_unittest("dcndL-mb07_erf", test_dcndL_mb07_erf), &
      & new_unittest("dcndL-antracene_erf", test_dcndL_anthracene_erf), &
      & new_unittest("cn-mb01_erf_en", test_cn_mb01_erf_en), &
      & new_unittest("cn-mb02_erf_en", test_cn_mb02_erf_en), &
      & new_unittest("cn-mb03_erf_en", test_cn_mb03_erf_en), &
      & new_unittest("cn-acetic_erf_en", test_cn_acetic_erf_en), &
      & new_unittest("dcndr-mb04_erf_en", test_dcndr_mb04_erf_en), &
      & new_unittest("dcndr-mb05_erf_en", test_dcndr_mb05_erf_en), &
      & new_unittest("dcndr-ammonia_erf_en", test_dcndr_ammonia_erf_en), &
      & new_unittest("dcndL-mb06_erf_en", test_dcndL_mb06_erf_en), &
      & new_unittest("dcndL-mb07_erf_en", test_dcndL_mb07_erf_en), &
      & new_unittest("dcndL-antracene_erf_en", test_dcndL_anthracene_erf_en) &
      & ]

   end subroutine collect_ncoord


   subroutine test_cn_gen(error, mol, ncoord, cutoff, ref)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      !> Molecular structure data
      type(structure_type) :: mol

      !> Coordination number type
      class(ncoord_type)   :: ncoord

      real(wp), intent(in) :: cutoff

      !> Reference CNs
      real(wp), intent(in) :: ref(:)
      real(wp), allocatable :: cn(:)
      real(wp), allocatable :: lattr(:, :)

      allocate(cn(mol%nat))

      call get_lattice_points(mol%periodic, mol%lattice, cutoff, lattr)
      call get_coordination_number(ncoord, mol, lattr, cutoff, cn)

      if (any(abs(cn - ref) > thr)) then
         call test_failed(error, "Coordination numbers do not match")
         print'(3es21.14)', cn
      end if

   end subroutine test_cn_gen


   subroutine test_numgrad(error, mol, ncoord, cutoff)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      !> Molecular structure data
      type(structure_type), intent(inout) :: mol

      !> Coordination number type
      class(ncoord_type)   :: ncoord

      real(wp), intent(in) :: cutoff

      integer :: iat, ic
      real(wp), allocatable :: cn(:), cnr(:), cnl(:)
      real(wp), allocatable :: dcndr(:, :, :), dcndL(:, :, :)
      real(wp), allocatable :: numdr(:, :, :)
      real(wp), allocatable :: lattr(:, :)
      real(wp), parameter :: step = 1.0e-6_wp

      allocate(cn(mol%nat), cnr(mol%nat), cnl(mol%nat), &
      & dcndr(3, mol%nat, mol%nat), dcndL(3, 3, mol%nat), &
      & numdr(3, mol%nat, mol%nat))

      call get_lattice_points(mol%periodic, mol%lattice, cutoff, lattr)

      do iat = 1, mol%nat
         do ic = 1, 3
            mol%xyz(ic, iat) = mol%xyz(ic, iat) + step
            call get_coordination_number(ncoord, mol, lattr, cutoff, cnr)
            mol%xyz(ic, iat) = mol%xyz(ic, iat) - 2*step
            call get_coordination_number(ncoord, mol, lattr, cutoff, cnl)
            mol%xyz(ic, iat) = mol%xyz(ic, iat) + step
            numdr(ic, iat, :) = 0.5_wp*(cnr - cnl)/step
         end do
      end do

      call get_coordination_number(ncoord, mol, lattr, cutoff, cn, dcndr, dcndL)

      if (any(abs(dcndr - numdr) > thr2)) then
         call test_failed(error, "Derivative of coordination number does not match")
      end if

   end subroutine test_numgrad


   subroutine test_numsigma(error, mol, ncoord, cutoff)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      !> Molecular structure data
      type(structure_type), intent(inout) :: mol

      !> Coordination number type
      class(ncoord_type)   :: ncoord

      real(wp), intent(in) :: cutoff

      integer :: ic, jc
      real(wp) :: eps(3, 3)
      real(wp), allocatable :: cn(:), cnr(:), cnl(:), xyz(:, :)
      real(wp), allocatable :: dcndr(:, :, :), dcndL(:, :, :)
      real(wp), allocatable :: numdL(:, :, :)
      real(wp), allocatable :: lattr(:, :), trans(:, :)
      real(wp), parameter :: unity(3, 3) = reshape(&
      & [1, 0, 0, 0, 1, 0, 0, 0, 1], shape(unity))
      real(wp), parameter :: step = 1.0e-6_wp

      allocate(cn(mol%nat), cnr(mol%nat), cnl(mol%nat), &
      & dcndr(3, mol%nat, mol%nat), dcndL(3, 3, mol%nat), xyz(3, mol%nat), &
      & numdL(3, 3, mol%nat))
      call get_lattice_points(mol%periodic, mol%lattice, cutoff, lattr)

      eps(:, :) = unity
      xyz(:, :) = mol%xyz
      trans = lattr
      do ic = 1, 3
         do jc = 1, 3
            eps(jc, ic) = eps(jc, ic) + step
            mol%xyz(:, :) = matmul(eps, xyz)
            lattr(:, :) = matmul(eps, trans)
            call get_coordination_number(ncoord, mol, lattr, cutoff, cnr)
            eps(jc, ic) = eps(jc, ic) - 2*step
            mol%xyz(:, :) = matmul(eps, xyz)
            lattr(:, :) = matmul(eps, trans)
            call get_coordination_number(ncoord, mol, lattr, cutoff, cnl)
            eps(jc, ic) = eps(jc, ic) + step
            mol%xyz(:, :) = xyz
            lattr(:, :) = trans
            numdL(jc, ic, :) = 0.5_wp*(cnr - cnl)/step
         end do
      end do

      call get_coordination_number(ncoord, mol, lattr, cutoff, cn, dcndr, dcndL)

      if (any(abs(dcndL - numdL) > 10.0_wp * thr2)) then
         call test_failed(error, "Derivative of coordination number does not match")
      end if

   end subroutine test_numsigma


   !> ----------------------------------------------------
   !> Tests for double-exponential (GFN) coordination number
   !> ----------------------------------------------------
   subroutine test_cn_mb01_gfn(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      type(gfn_ncoord_type) :: gfn_ncoord
      real(wp), allocatable :: rcov(:)

      real(wp), parameter :: cutoff = 30.0_wp
      real(wp), parameter :: ref(16) = [&
      & 4.11453659059991E+0_wp, 9.32058998762811E-1_wp, 2.03554597140311E+0_wp, &
      & 1.42227835389358E+0_wp, 1.12812426574031E+0_wp, 1.05491602558828E+0_wp, &
      & 1.52709064704269E+0_wp, 1.95070367247232E+0_wp, 3.83759889196540E+0_wp, &
      & 1.09388314007182E+0_wp, 1.07090773695340E+0_wp, 2.00285254082830E+0_wp, &
      & 4.36400837813955E+0_wp, 3.83469860546080E+0_wp, 3.91542517673963E+0_wp, &
      & 5.58571682419960E+0_wp]

      call get_structure(mol, "mindless01")

      allocate(rcov(mol%nid))
      rcov(:) = get_covalent_rad(mol%num)

      call new_gfn_ncoord(gfn_ncoord, mol, cutoff=cutoff, rcov=rcov)
      call test_cn_gen(error, mol, gfn_ncoord, cutoff, ref)

   end subroutine test_cn_mb01_gfn


   subroutine test_cn_mb02_gfn(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      type(gfn_ncoord_type) :: gfn_ncoord
      real(wp), allocatable :: rcov(:)

      real(wp), parameter :: cutoff = 30.0_wp
      real(wp), parameter :: ref(16) = [&
      & 9.16720394154780E-1_wp, 3.81678481096046E+0_wp, 3.58669504034282E+0_wp, &
      & 2.90017371823910E+0_wp, 5.09896950232009E+0_wp, 1.13810142820063E+0_wp, &
      & 9.35735928561309E-1_wp, 9.43324181507483E-1_wp, 4.86704300506033E+0_wp, &
      & 1.22024405676881E+0_wp, 3.86661472534511E+0_wp, 4.02897853934353E+0_wp, &
      & 1.96869999324996E+0_wp, 9.04495366214972E-1_wp, 1.49977675358986E+0_wp, &
      & 2.04937665719480E+0_wp]

      call get_structure(mol, "mindless02")

      allocate(rcov(mol%nid))
      rcov(:) = get_covalent_rad(mol%num)

      call new_gfn_ncoord(gfn_ncoord, mol, cutoff=cutoff, rcov=rcov)
      call test_cn_gen(error, mol,  gfn_ncoord, cutoff, ref)

   end subroutine test_cn_mb02_gfn


   subroutine test_cn_mb03_gfn(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      type(gfn_ncoord_type) :: gfn_ncoord
      real(wp), allocatable :: rcov(:)

      real(wp), parameter :: cutoff = 30.0_wp
      real(wp), parameter :: ref(16) = [&
      & 4.03003984525826E+0_wp, 2.88404829383272E+0_wp, 1.12030447706437E+0_wp, &
      & 4.89326653038565E+0_wp, 6.97177319120287E+0_wp, 4.43722632690247E+0_wp, &
      & 4.74866591812568E+0_wp, 1.30338583081493E+0_wp, 1.06130190313902E+0_wp, &
      & 1.04048373066340E+0_wp, 1.92270060977307E+0_wp, 2.98737904327655E+0_wp, &
      & 4.69081207436918E+0_wp, 1.26260125612717E+0_wp, 3.36368006785498E+0_wp, &
      & 1.35743886389476E+0_wp]

      call get_structure(mol, "mindless03")

      allocate(rcov(mol%nid))
      rcov(:) = get_covalent_rad(mol%num)

      call new_gfn_ncoord(gfn_ncoord, mol, cutoff=cutoff, rcov=rcov)
      call test_cn_gen(error, mol, gfn_ncoord, cutoff, ref)

   end subroutine test_cn_mb03_gfn


   subroutine test_cn_acetic_gfn(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      type(gfn_ncoord_type) :: gfn_ncoord
      real(wp), allocatable :: rcov(:)

      real(wp), parameter :: cutoff = 30.0_wp
      real(wp), parameter :: ref(32) = [&
      & 1.29670485827103E+0_wp, 2.12821173792238E+0_wp, 1.29689958901536E+0_wp, &
      & 2.12819492380168E+0_wp, 2.12820788692351E+0_wp, 1.29730145110972E+0_wp, &
      & 1.29709466512645E+0_wp, 2.12822682780515E+0_wp, 3.12090335292641E+0_wp, &
      & 4.03264857560058E+0_wp, 3.12090536181014E+0_wp, 4.03267505042477E+0_wp, &
      & 4.03270042180352E+0_wp, 3.12093082635011E+0_wp, 3.12100563126373E+0_wp, &
      & 4.03263801987003E+0_wp, 1.00224480975784E+0_wp, 1.11740483416248E+0_wp, &
      & 1.00446562107381E+0_wp, 1.00138537125699E+0_wp, 1.00139516076697E+0_wp, &
      & 1.00452681395494E+0_wp, 1.00222513359088E+0_wp, 1.11751807844285E+0_wp, &
      & 1.00457316822347E+0_wp, 1.00217171056793E+0_wp, 1.11725423574458E+0_wp, &
      & 1.00146807045654E+0_wp, 1.00452614353119E+0_wp, 1.00217802827893E+0_wp, &
      & 1.00141181936828E+0_wp, 1.11720394408805E+0_wp]

      call get_structure(mol, "x02")

      allocate(rcov(mol%nid))
      rcov(:) = get_covalent_rad(mol%num)

      call new_gfn_ncoord(gfn_ncoord, mol, cutoff=cutoff, rcov=rcov)
      call test_cn_gen(error, mol, gfn_ncoord, cutoff, ref)

   end subroutine test_cn_acetic_gfn


   subroutine test_dcndr_mb04_gfn(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      type(gfn_ncoord_type) :: gfn_ncoord
      real(wp), allocatable :: rcov(:)

      real(wp), parameter :: cutoff = 30.0_wp

      call get_structure(mol, "mindless04")

      allocate(rcov(mol%nid))
      rcov(:) = get_covalent_rad(mol%num)

      call new_gfn_ncoord(gfn_ncoord, mol, cutoff=cutoff, rcov=rcov)
      call test_numgrad(error, mol, gfn_ncoord, cutoff)

   end subroutine test_dcndr_mb04_gfn


   subroutine test_dcndr_mb05_gfn(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      type(gfn_ncoord_type) :: gfn_ncoord
      real(wp), allocatable :: rcov(:)

      real(wp), parameter :: cutoff = 30.0_wp

      call get_structure(mol, "mindless05")

      allocate(rcov(mol%nid))
      rcov(:) = get_covalent_rad(mol%num)

      call new_gfn_ncoord(gfn_ncoord, mol, cutoff=cutoff, rcov=rcov)
      call test_numgrad(error, mol, gfn_ncoord, cutoff)

   end subroutine test_dcndr_mb05_gfn


   subroutine test_dcndr_ammonia_gfn(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      type(gfn_ncoord_type) :: gfn_ncoord
      real(wp), allocatable :: rcov(:)

      real(wp), parameter :: cutoff = 30.0_wp

      call get_structure(mol, "x04")

      allocate(rcov(mol%nid))
      rcov(:) = get_covalent_rad(mol%num)

      call new_gfn_ncoord(gfn_ncoord, mol, cutoff=cutoff, rcov=rcov)
      call test_numgrad(error, mol, gfn_ncoord, cutoff)

   end subroutine test_dcndr_ammonia_gfn


   subroutine test_dcndL_mb06_gfn(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      type(gfn_ncoord_type) :: gfn_ncoord
      real(wp), allocatable :: rcov(:)

      real(wp), parameter :: cutoff = 30.0_wp

      call get_structure(mol, "mindless06")

      allocate(rcov(mol%nid))
      rcov(:) = get_covalent_rad(mol%num)

      call new_gfn_ncoord(gfn_ncoord, mol, cutoff=cutoff, rcov=rcov)
      call test_numsigma(error, mol, gfn_ncoord, cutoff)

   end subroutine test_dcndL_mb06_gfn


   subroutine test_dcndL_mb07_gfn(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      type(gfn_ncoord_type) :: gfn_ncoord
      real(wp), allocatable :: rcov(:)

      real(wp), parameter :: cutoff = 30.0_wp

      call get_structure(mol, "mindless07")

      allocate(rcov(mol%nid))
      rcov(:) = get_covalent_rad(mol%num)

      call new_gfn_ncoord(gfn_ncoord, mol, cutoff=cutoff, rcov=rcov)
      call test_numsigma(error, mol, gfn_ncoord, cutoff)

   end subroutine test_dcndL_mb07_gfn


   subroutine test_dcndL_anthracene_gfn(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      type(gfn_ncoord_type) :: gfn_ncoord
      real(wp), allocatable :: rcov(:)

      real(wp), parameter :: cutoff = 30.0_wp

      call get_structure(mol, "x04")

      allocate(rcov(mol%nid))
      rcov(:) = get_covalent_rad(mol%num)

      call new_gfn_ncoord(gfn_ncoord, mol, cutoff=cutoff, rcov=rcov)
      call test_numsigma(error, mol, gfn_ncoord, cutoff)

   end subroutine test_dcndL_anthracene_gfn


   !> ----------------------------------------------------
   !> Tests for mono-exponential coordination number
   !> ----------------------------------------------------
   subroutine test_cn_mb01_exp(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      type(exp_ncoord_type) :: exp_ncoord
      real(wp), allocatable :: rcov(:)

      real(wp), parameter :: cutoff = 30.0_wp
      real(wp), parameter :: ref(16) = [&
      & 4.15066368951397E+0_wp, 9.78868026389781E-1_wp, 2.01080985633859E+0_wp, &
      & 1.47865697827818E+0_wp, 1.03577822442117E+0_wp, 1.01206994314781E+0_wp, &
      & 1.50329777127401E+0_wp, 1.99858468272609E+0_wp, 3.89181927539324E+0_wp, &
      & 1.04323373360740E+0_wp, 1.01526584450636E+0_wp, 1.99315213227354E+0_wp, &
      & 4.63526560889683E+0_wp, 3.87312260639335E+0_wp, 3.99316800677884E+0_wp, &
      & 5.45068226903888E+0_wp]

      call get_structure(mol, "mindless01")

      allocate(rcov(mol%nid))
      rcov(:) = get_covalent_rad(mol%num)

      call new_exp_ncoord(exp_ncoord, mol, cutoff=cutoff, rcov=rcov)
      call test_cn_gen(error, mol, exp_ncoord, cutoff, ref)

   end subroutine test_cn_mb01_exp


   subroutine test_cn_mb02_exp(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      type(exp_ncoord_type) :: exp_ncoord
      real(wp), allocatable :: rcov(:)

      real(wp), parameter :: cutoff = 30.0_wp
      real(wp), parameter :: ref(16) = [&
      & 9.68434565947196E-1_wp, 3.94488220883276E+0_wp, 3.82701677880409E+0_wp, &
      & 2.99161201243234E+0_wp, 5.46904892971914E+0_wp, 1.06438740698832E+0_wp, &
      & 9.77627896999762E-1_wp, 9.81715643929621E-1_wp, 5.06702924169705E+0_wp, &
      & 1.08324093335500E+0_wp, 4.00161384251868E+0_wp, 4.03067393311321E+0_wp, &
      & 2.00249301823990E+0_wp, 9.73399178780401E-1_wp, 1.66868575900646E+0_wp, &
      & 2.03273936244268E+0_wp]

      call get_structure(mol, "mindless02")

      allocate(rcov(mol%nid))
      rcov(:) = get_covalent_rad(mol%num)

      call new_exp_ncoord(exp_ncoord, mol, cutoff=cutoff, rcov=rcov)
      call test_cn_gen(error, mol,  exp_ncoord, cutoff, ref)

   end subroutine test_cn_mb02_exp


   subroutine test_cn_mb03_exp(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      type(exp_ncoord_type) :: exp_ncoord
      real(wp), allocatable :: rcov(:)

      real(wp), parameter :: cutoff = 30.0_wp
      real(wp), parameter :: ref(16) = [&
      & 4.04992403668766E+0_wp, 2.98487291056010E+0_wp, 1.03236609075831E+0_wp, &
      & 4.86782876076800E+0_wp, 7.48154833549122E+0_wp, 4.76588477343835E+0_wp, &
      & 4.92432613481557E+0_wp, 1.19761963808520E+0_wp, 1.01809037129368E+0_wp, &
      & 1.01042713594272E+0_wp, 1.99186789992505E+0_wp, 3.03157225205500E+0_wp, &
      & 4.89702969622395E+0_wp, 1.11663432026701E+0_wp, 3.52903544775683E+0_wp, &
      & 1.33563958333433E+0_wp]

      call get_structure(mol, "mindless03")

      allocate(rcov(mol%nid))
      rcov(:) = get_covalent_rad(mol%num)

      call new_exp_ncoord(exp_ncoord, mol, cutoff=cutoff, rcov=rcov)
      call test_cn_gen(error, mol, exp_ncoord, cutoff, ref)

   end subroutine test_cn_mb03_exp


   subroutine test_cn_acetic_exp(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      type(exp_ncoord_type) :: exp_ncoord
      real(wp), allocatable :: rcov(:)

      real(wp), parameter :: cutoff = 30.0_wp
      real(wp), parameter :: ref(32) = [&
      & 1.08784433872404E+0_wp, 2.04822026776264E+0_wp, 1.08792777906791E+0_wp, &
      & 2.04820799570728E+0_wp, 2.04821951599101E+0_wp, 1.08811939753571E+0_wp, &
      & 1.08802819063717E+0_wp, 2.04823763269824E+0_wp, 3.03590173052949E+0_wp, &
      & 4.04097072127914E+0_wp, 3.03590425814990E+0_wp, 4.04098287934520E+0_wp, &
      & 4.04097935270722E+0_wp, 3.03590447694319E+0_wp, 3.03593501518861E+0_wp, &
      & 4.04096875806481E+0_wp, 1.00135680977725E+0_wp, 1.03683422933766E+0_wp, &
      & 1.00190020398973E+0_wp, 1.00114101857689E+0_wp, 1.00114344249343E+0_wp, &
      & 1.00191213773267E+0_wp, 1.00135278116294E+0_wp, 1.03690135098048E+0_wp, &
      & 1.00192295753089E+0_wp, 1.00133927917777E+0_wp, 1.03675374108303E+0_wp, &
      & 1.00115868342640E+0_wp, 1.00191299668631E+0_wp, 1.00134039650187E+0_wp, &
      & 1.00114761354986E+0_wp, 1.03670619005681E+0_wp]

      call get_structure(mol, "x02")

      allocate(rcov(mol%nid))
      rcov(:) = get_covalent_rad(mol%num)

      call new_exp_ncoord(exp_ncoord, mol, cutoff=cutoff, rcov=rcov)
      call test_cn_gen(error, mol, exp_ncoord, cutoff, ref)

   end subroutine test_cn_acetic_exp


   subroutine test_dcndr_mb04_exp(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      type(exp_ncoord_type) :: exp_ncoord
      real(wp), allocatable :: rcov(:)

      real(wp), parameter :: cutoff = 30.0_wp

      call get_structure(mol, "mindless04")

      allocate(rcov(mol%nid))
      rcov(:) = get_covalent_rad(mol%num)

      call new_exp_ncoord(exp_ncoord, mol, cutoff=cutoff, rcov=rcov)
      call test_numgrad(error, mol, exp_ncoord, cutoff)

   end subroutine test_dcndr_mb04_exp


   subroutine test_dcndr_mb05_exp(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      type(exp_ncoord_type) :: exp_ncoord
      real(wp), allocatable :: rcov(:)

      real(wp), parameter :: cutoff = 30.0_wp

      call get_structure(mol, "mindless05")

      allocate(rcov(mol%nid))
      rcov(:) = get_covalent_rad(mol%num)

      call new_exp_ncoord(exp_ncoord, mol, cutoff=cutoff, rcov=rcov)
      call test_numgrad(error, mol, exp_ncoord, cutoff)

   end subroutine test_dcndr_mb05_exp


   subroutine test_dcndr_ammonia_exp(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      type(exp_ncoord_type) :: exp_ncoord
      real(wp), allocatable :: rcov(:)

      real(wp), parameter :: cutoff = 30.0_wp

      call get_structure(mol, "x04")

      allocate(rcov(mol%nid))
      rcov(:) = get_covalent_rad(mol%num)

      call new_exp_ncoord(exp_ncoord, mol, cutoff=cutoff, rcov=rcov)
      call test_numgrad(error, mol, exp_ncoord, cutoff)

   end subroutine test_dcndr_ammonia_exp


   subroutine test_dcndL_mb06_exp(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      type(exp_ncoord_type) :: exp_ncoord
      real(wp), allocatable :: rcov(:)

      real(wp), parameter :: cutoff = 30.0_wp

      call get_structure(mol, "mindless06")

      allocate(rcov(mol%nid))
      rcov(:) = get_covalent_rad(mol%num)

      call new_exp_ncoord(exp_ncoord, mol, cutoff=cutoff, rcov=rcov)
      call test_numsigma(error, mol, exp_ncoord, cutoff)

   end subroutine test_dcndL_mb06_exp


   subroutine test_dcndL_mb07_exp(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      type(exp_ncoord_type) :: exp_ncoord
      real(wp), allocatable :: rcov(:)

      real(wp), parameter :: cutoff = 30.0_wp

      call get_structure(mol, "mindless07")

      allocate(rcov(mol%nid))
      rcov(:) = get_covalent_rad(mol%num)

      call new_exp_ncoord(exp_ncoord, mol, cutoff=cutoff, rcov=rcov)
      call test_numsigma(error, mol, exp_ncoord, cutoff)

   end subroutine test_dcndL_mb07_exp


   subroutine test_dcndL_anthracene_exp(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      type(exp_ncoord_type) :: exp_ncoord
      real(wp), allocatable :: rcov(:)

      real(wp), parameter :: cutoff = 30.0_wp

      call get_structure(mol, "x04")

      allocate(rcov(mol%nid))
      rcov(:) = get_covalent_rad(mol%num)

      call new_exp_ncoord(exp_ncoord, mol, cutoff=cutoff, rcov=rcov)
      call test_numsigma(error, mol, exp_ncoord, cutoff)

   end subroutine test_dcndL_anthracene_exp


   !> ----------------------------------------------------
   !> Tests for error-function based CEH/GP3 coordination number 
   !> using the Pyykko covalent radii and Pauling EN
   !> ----------------------------------------------------
   subroutine test_cn_mb01_erf(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      type(erf_ncoord_type) :: erf_ncoord
      real(wp), allocatable :: rcov(:)

      real(wp), parameter :: cutoff = 30.0_wp
      real(wp), parameter :: kcn = 2.60_wp
      real(wp), parameter :: ref(16) = [&
      & +4.02932551939856E+00_wp, +7.85103341426644E-01_wp, +1.81014102249862E+00_wp, &
      & +1.27570668822915E+00_wp, +1.07819362139425E+00_wp, +9.25571697114998E-01_wp, &
      & +1.46154464565793E+00_wp, +1.68476840890299E+00_wp, +3.36480649393304E+00_wp, &
      & +1.01246571460105E+00_wp, +9.41673850989885E-01_wp, +1.86611893141095E+00_wp, &
      & +3.82207104984211E+00_wp, +3.43775046556670E+00_wp, +3.43498366851429E+00_wp, &
      & +5.23834442955556E+00_wp]

      call get_structure(mol, "mindless01")

      allocate(rcov(mol%nid))
      rcov(:) = get_covalent_rad(mol%num)

      call new_erf_ncoord(erf_ncoord, mol, kcn=kcn, cutoff=cutoff, rcov=rcov)
      call test_cn_gen(error, mol, erf_ncoord, cutoff, ref)

   end subroutine test_cn_mb01_erf


   subroutine test_cn_mb02_erf(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      type(erf_ncoord_type) :: erf_ncoord
      real(wp), allocatable :: rcov(:)

      real(wp), parameter :: cutoff = 30.0_wp
      real(wp), parameter :: kcn = 2.60_wp
      real(wp), parameter :: ref(16) = [&
      & +7.69338455488404E-01_wp, +3.43906282200685E+00_wp, +3.09682161007250E+00_wp, &
      & +2.51887440744333E+00_wp, +4.48473180889284E+00_wp, +1.04427988033896E+00_wp, &
      & +7.91999226960846E-01_wp, +8.10605045203656E-01_wp, +4.38499629754698E+00_wp, &
      & +1.18154096161350E+00_wp, +3.50661982359796E+00_wp, +3.79803204777630E+00_wp, &
      & +1.71950724069027E+00_wp, +7.52262747522523E-01_wp, +1.30981487576456E+00_wp, &
      & +1.93023548195301E+00_wp]

      call get_structure(mol, "mindless02")

      allocate(rcov(mol%nid))
      rcov(:) = get_covalent_rad(mol%num)

      call new_erf_ncoord(erf_ncoord, mol, kcn=kcn, cutoff=cutoff, rcov=rcov)
      call test_cn_gen(error, mol, erf_ncoord, cutoff, ref)

   end subroutine test_cn_mb02_erf


   subroutine test_cn_mb03_erf(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      type(erf_ncoord_type) :: erf_ncoord
      real(wp), allocatable :: rcov(:)

      real(wp), parameter :: cutoff = 30.0_wp
      real(wp), parameter :: kcn = 2.60_wp
      real(wp), parameter :: ref(16) = [&
      & +3.65604858453391E+00_wp, 2.47444042434446E+00_wp, 1.04729945244460E+00_wp, &
      & +4.69028664318902E+00_wp, 6.13331280895969E+00_wp, 3.96706985488549E+00_wp, &
      & +4.19093957903296E+00_wp, 1.24758706681051E+00_wp, 9.56572882868544E-01_wp, &
      & +9.15465255206712E-01_wp, 1.65382740011051E+00_wp, 2.71619299069121E+00_wp, &
      & +4.13366036611672E+00_wp, 1.25908852786395E+00_wp, 3.12414929301946E+00_wp, &
      & +1.24550167348772E+00_wp]

      call get_structure(mol, "mindless03")

      allocate(rcov(mol%nid))
      rcov(:) = get_covalent_rad(mol%num)

      call new_erf_ncoord(erf_ncoord, mol, kcn=kcn, cutoff=cutoff, rcov=rcov)
      call test_cn_gen(error, mol, erf_ncoord, cutoff, ref)

   end subroutine test_cn_mb03_erf


   subroutine test_cn_acetic_erf(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      type(erf_ncoord_type) :: erf_ncoord
      real(wp), allocatable :: rcov(:)

      real(wp), parameter :: cutoff = 30.0_wp
      real(wp), parameter :: kcn = 2.60_wp
      real(wp), parameter :: ref(32) = [&
      & 1.32759422973205E+00_wp, 1.94146232311076E+00_wp, 1.32785842732676E+00_wp, &
      & 1.94145796402605E+00_wp, 1.94144168404587E+00_wp, 1.32846648409150E+00_wp, &
      & 1.32817586952618E+00_wp, 1.94146973878954E+00_wp, 2.77909305549586E+00_wp, &
      & 3.54216537574710E+00_wp, 2.77910670281296E+00_wp, 3.54220807172730E+00_wp, &
      & 3.54223833711160E+00_wp, 2.77910422212159E+00_wp, 2.77925865658396E+00_wp, &
      & 3.54214599507453E+00_wp, 8.43683239278435E-01_wp, 1.03961906362830E+00_wp, &
      & 8.45225661192366E-01_wp, 8.42052731344644E-01_wp, 8.42069624420118E-01_wp, &
      & 8.45292837568377E-01_wp, 8.43651700598541E-01_wp, 1.03980036426124E+00_wp, &
      & 8.45340647438754E-01_wp, 8.43551963743648E-01_wp, 1.03938961293610E+00_wp, &
      & 8.42194599506036E-01_wp, 8.45292255082151E-01_wp, 8.43583848813426E-01_wp, &
      & 8.42107560458205E-01_wp, 1.03929415502029E+00_wp]

      call get_structure(mol, "x02")

      allocate(rcov(mol%nid))
      rcov(:) = get_covalent_rad(mol%num)

      call new_erf_ncoord(erf_ncoord, mol, kcn=kcn, cutoff=cutoff, rcov=rcov)
      call test_cn_gen(error, mol, erf_ncoord, cutoff, ref)

   end subroutine test_cn_acetic_erf


   subroutine test_dcndr_mb04_erf(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      type(erf_ncoord_type) :: erf_ncoord
      real(wp), allocatable :: rcov(:)

      real(wp), parameter :: cutoff = 30.0_wp

      call get_structure(mol, "mindless04")

      allocate(rcov(mol%nid))
      rcov(:) = get_covalent_rad(mol%num)

      call new_erf_ncoord(erf_ncoord, mol, cutoff=cutoff, rcov=rcov)
      call test_numgrad(error, mol, erf_ncoord, cutoff)

   end subroutine test_dcndr_mb04_erf


   subroutine test_dcndr_mb05_erf(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      type(erf_ncoord_type) :: erf_ncoord
      real(wp), allocatable :: rcov(:)

      real(wp), parameter :: cutoff = 30.0_wp

      call get_structure(mol, "mindless05")

      allocate(rcov(mol%nid))
      rcov(:) = get_covalent_rad(mol%num)

      call new_erf_ncoord(erf_ncoord, mol, cutoff=cutoff, rcov=rcov)
      call test_numgrad(error, mol, erf_ncoord, cutoff)

   end subroutine test_dcndr_mb05_erf


   subroutine test_dcndr_ammonia_erf(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      type(erf_ncoord_type) :: erf_ncoord
      real(wp), allocatable :: rcov(:)

      real(wp), parameter :: cutoff = 30.0_wp

      call get_structure(mol, "x04")

      allocate(rcov(mol%nid))
      rcov(:) = get_covalent_rad(mol%num)

      call new_erf_ncoord(erf_ncoord, mol, cutoff=cutoff, rcov=rcov)
      call test_numgrad(error, mol, erf_ncoord, cutoff)

   end subroutine test_dcndr_ammonia_erf


   subroutine test_dcndL_mb06_erf(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      type(erf_ncoord_type) :: erf_ncoord
      real(wp), allocatable :: rcov(:)

      real(wp), parameter :: cutoff = 30.0_wp

      call get_structure(mol, "mindless06")

      allocate(rcov(mol%nid))
      rcov(:) = get_covalent_rad(mol%num)

      call new_erf_ncoord(erf_ncoord, mol, cutoff=cutoff, rcov=rcov)
      call test_numsigma(error, mol, erf_ncoord, cutoff)

   end subroutine test_dcndL_mb06_erf


   subroutine test_dcndL_mb07_erf(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      type(erf_ncoord_type) :: erf_ncoord
      real(wp), allocatable :: rcov(:)

      real(wp), parameter :: cutoff = 30.0_wp

      call get_structure(mol, "mindless07")

      allocate(rcov(mol%nid))
      rcov(:) = get_covalent_rad(mol%num)

      call new_erf_ncoord(erf_ncoord, mol, cutoff=cutoff, rcov=rcov)
      call test_numsigma(error, mol, erf_ncoord, cutoff)

   end subroutine test_dcndL_mb07_erf


   subroutine test_dcndL_anthracene_erf(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      type(erf_ncoord_type) :: erf_ncoord
      real(wp), allocatable :: rcov(:)

      real(wp), parameter :: cutoff = 30.0_wp

      call get_structure(mol, "x05")

      allocate(rcov(mol%nid))
      rcov(:) = get_covalent_rad(mol%num)

      call new_erf_ncoord(erf_ncoord, mol, cutoff=cutoff, rcov=rcov)
      call test_numsigma(error, mol, erf_ncoord, cutoff)

   end subroutine test_dcndL_anthracene_erf


   !> ----------------------------------------------------
   !> Tests for electronegativity-weighted
   !> error-function-based CEH/GP3 coordination number
   !> using the Pyykko covalent radii and Pauling EN
   !> ----------------------------------------------------
   subroutine test_cn_mb01_erf_en(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      type(erf_en_ncoord_type) :: erf_en_ncoord
      real(wp), allocatable :: rcov(:)
      real(wp), allocatable :: en(:)

      real(wp), parameter :: cutoff = 30.0_wp
      real(wp), parameter :: kcn = 2.60_wp
      real(wp), parameter :: ref(16) = [&
      & +6.49835771218084E+00_wp, -9.24780426380943E-02_wp, -3.19404023806046E+00_wp, &
      & -7.26463405313327E-01_wp, -1.92171552979195E+00_wp, +6.71703342153316E-01_wp, &
      & -6.47861832477202E-01_wp, -2.56263447944906E+00_wp, -3.28660571935921E+00_wp, &
      & +8.86404306801211E-01_wp, +6.65084753274741E-01_wp, -2.51172025807030E+00_wp, &
      & +4.24336914659746E-01_wp, +2.98914644571775E+00_wp, -2.72992153136905E+00_wp, &
      & +5.53840756174106E+00_wp]

      call get_structure(mol, "mindless01")

      allocate(rcov(mol%nid), en(mol%nid))
      rcov(:) = get_covalent_rad(mol%num)
      en(:) = get_pauling_en(mol%num)

      call new_erf_en_ncoord(erf_en_ncoord, mol, kcn=kcn, cutoff=cutoff, rcov=rcov, en=en)
      call test_cn_gen(error, mol, erf_en_ncoord, cutoff, ref)

   end subroutine test_cn_mb01_erf_en


   subroutine test_cn_mb02_erf_en(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      type(erf_en_ncoord_type) :: erf_en_ncoord
      real(wp), allocatable :: rcov(:)
      real(wp), allocatable :: en(:)

      real(wp), parameter :: cutoff = 30.0_wp
      real(wp), parameter :: kcn = 2.60_wp
      real(wp), parameter :: ref(16) = [&
      & -1.12234584286121E-01_wp, -2.94004224176729E+00_wp, -5.58562445598307E-02_wp, &
      & -4.14055422698917E+00_wp, +4.94118711261405E+00_wp, +6.99329422648258E-01_wp, &
      & -1.03826235691724E-01_wp, -1.37913918152233E-01_wp, +9.80518929048175E-01_wp, &
      & +6.69155623786794E-01_wp, +5.48588370791672E-01_wp, +6.70886303840263E+00_wp, &
      & -4.81696150596437E+00_wp, -2.21594284437332E-01_wp, -2.79322359462227E-01_wp, &
      & -1.73933689598129E+00_wp]

      call get_structure(mol, "mindless02")

      allocate(rcov(mol%nid), en(mol%nid))
      rcov(:) = get_covalent_rad(mol%num)
      en(:) = get_pauling_en(mol%num)

      call new_erf_en_ncoord(erf_en_ncoord, mol, kcn=kcn, cutoff=cutoff, rcov=rcov, en=en)
      call test_cn_gen(error, mol,  erf_en_ncoord, cutoff, ref)

   end subroutine test_cn_mb02_erf_en


   subroutine test_cn_mb03_erf_en(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      type(erf_en_ncoord_type) :: erf_en_ncoord
      real(wp), allocatable :: rcov(:)
      real(wp), allocatable :: en(:)

      real(wp), parameter :: cutoff = 30.0_wp
      real(wp), parameter :: kcn = 2.60_wp
      real(wp), parameter :: ref(16) = [&
      & -1.57129310680311E+00_wp, -5.12538648600132E+00_wp, +3.44404010675507E-02_wp, &
      & +5.53888057043349E+00_wp, +6.08403921871331E+00_wp, +1.70484401191915E+00_wp, &
      & -3.05797643039751E+00_wp, -1.77339255176255E-01_wp, +1.77101353984179E-01_wp, &
      & +3.11611982445210E-01_wp, -4.72001776406229E+00_wp, -1.75545403486359E+00_wp, &
      & -2.87572032143436E+00_wp, +4.45164095433560E-01_wp, +5.12260227693958E+00_wp, &
      & -1.35496512197608E-01_wp]

      call get_structure(mol, "mindless03")

      allocate(rcov(mol%nid), en(mol%nid))
      rcov(:) = get_covalent_rad(mol%num)
      en(:) = get_pauling_en(mol%num)

      call new_erf_en_ncoord(erf_en_ncoord, mol, kcn=kcn, cutoff=cutoff, rcov=rcov, en=en)
      call test_cn_gen(error, mol, erf_en_ncoord, cutoff, ref)

   end subroutine test_cn_mb03_erf_en


   subroutine test_cn_acetic_erf_en(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      type(erf_en_ncoord_type) :: erf_en_ncoord
      real(wp), allocatable :: rcov(:)
      real(wp), allocatable :: en(:)

      real(wp), parameter :: cutoff = 30.0_wp
      real(wp), parameter :: kcn = 2.60_wp
      real(wp), parameter :: ref(32) = [&
      & -1.10960425359405E+00_wp, -1.87307032436238E+00_wp,-1.10988726298238E+00_wp, &
      & -1.87300941691613E+00_wp, -1.87307851037536E+00_wp,-1.11055533892111E+00_wp, &
      & -1.11022640841129E+00_wp, -1.87314182780441E+00_wp, 1.48844491507249E+00_wp, &
      & -5.92197262250447E-01_wp,  1.48846528050850E+00_wp,-5.92187160672111E-01_wp, &
      & -5.92267911690038E-01_wp,  1.48840420948914E+00_wp, 1.48839664548042E+00_wp, &
      & -5.92201115944998E-01_wp,  2.95283098472335E-01_wp, 1.20097019987756E+00_wp, &
      &  2.96054614475136E-01_wp,  2.94737814418179E-01_wp, 2.94743337206434E-01_wp, &
      &  2.96081399455567E-01_wp,  2.95272376357621E-01_wp, 1.20121563992199E+00_wp, &
      &  2.96099722915932E-01_wp,  2.95237799928904E-01_wp, 1.20068087168607E+00_wp, &
      &  2.94786022309938E-01_wp,  2.96081562273217E-01_wp, 2.95248938198809E-01_wp, &
      &  2.94755972287658E-01_wp,  1.20046637358882E+00_wp]

      call get_structure(mol, "x02")

      allocate(rcov(mol%nid), en(mol%nid))
      rcov(:) = get_covalent_rad(mol%num)
      en(:) = get_pauling_en(mol%num)

      call new_erf_en_ncoord(erf_en_ncoord, mol, kcn=kcn, cutoff=cutoff, rcov=rcov, en=en)
      call test_cn_gen(error, mol, erf_en_ncoord, cutoff, ref)

   end subroutine test_cn_acetic_erf_en


   subroutine test_dcndr_mb04_erf_en(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      type(erf_en_ncoord_type) :: erf_en_ncoord
      real(wp), allocatable :: rcov(:)
      real(wp), allocatable :: en(:)

      real(wp), parameter :: cutoff = 30.0_wp

      call get_structure(mol, "mindless04")

      allocate(rcov(mol%nid), en(mol%nid))
      rcov(:) = get_covalent_rad(mol%num)
      en(:) = get_pauling_en(mol%num)

      call new_erf_en_ncoord(erf_en_ncoord, mol, cutoff=cutoff, rcov=rcov, en=en)
      call test_numgrad(error, mol, erf_en_ncoord, cutoff)

   end subroutine test_dcndr_mb04_erf_en


   subroutine test_dcndr_mb05_erf_en(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      type(erf_en_ncoord_type) :: erf_en_ncoord
      real(wp), allocatable :: rcov(:)
      real(wp), allocatable :: en(:)

      real(wp), parameter :: cutoff = 30.0_wp

      call get_structure(mol, "mindless05")

      allocate(rcov(mol%nid), en(mol%nid))
      rcov(:) = get_covalent_rad(mol%num)
      en(:) = get_pauling_en(mol%num)

      call new_erf_en_ncoord(erf_en_ncoord, mol, cutoff=cutoff, rcov=rcov, en=en)
      call test_numgrad(error, mol, erf_en_ncoord, cutoff)

   end subroutine test_dcndr_mb05_erf_en


   subroutine test_dcndr_ammonia_erf_en(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      type(erf_en_ncoord_type) :: erf_en_ncoord
      real(wp), allocatable :: rcov(:)
      real(wp), allocatable :: en(:)

      real(wp), parameter :: cutoff = 30.0_wp

      call get_structure(mol, "x04")

      allocate(rcov(mol%nid), en(mol%nid))
      rcov(:) = get_covalent_rad(mol%num)
      en(:) = get_pauling_en(mol%num)

      call new_erf_en_ncoord(erf_en_ncoord, mol, cutoff=cutoff, rcov=rcov, en=en)
      call test_numgrad(error, mol, erf_en_ncoord, cutoff)

   end subroutine test_dcndr_ammonia_erf_en


   subroutine test_dcndL_mb06_erf_en(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      type(erf_en_ncoord_type) :: erf_en_ncoord
      real(wp), allocatable :: rcov(:)
      real(wp), allocatable :: en(:)

      real(wp), parameter :: cutoff = 30.0_wp

      call get_structure(mol, "mindless06")

      allocate(rcov(mol%nid), en(mol%nid))
      rcov(:) = get_covalent_rad(mol%num)
      en(:) = get_pauling_en(mol%num)

      call new_erf_en_ncoord(erf_en_ncoord, mol, cutoff=cutoff, rcov=rcov, en=en)
      call test_numsigma(error, mol, erf_en_ncoord, cutoff)

   end subroutine test_dcndL_mb06_erf_en


   subroutine test_dcndL_mb07_erf_en(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      type(erf_en_ncoord_type) :: erf_en_ncoord
      real(wp), allocatable :: rcov(:)
      real(wp), allocatable :: en(:)

      real(wp), parameter :: cutoff = 30.0_wp

      call get_structure(mol, "mindless07")

      allocate(rcov(mol%nid), en(mol%nid))
      rcov(:) = get_covalent_rad(mol%num)
      en(:) = get_pauling_en(mol%num)

      call new_erf_en_ncoord(erf_en_ncoord, mol, cutoff=cutoff, rcov=rcov, en=en)
      call test_numsigma(error, mol, erf_en_ncoord, cutoff)

   end subroutine test_dcndL_mb07_erf_en


   subroutine test_dcndL_anthracene_erf_en(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      type(erf_en_ncoord_type) :: erf_en_ncoord
      real(wp), allocatable :: rcov(:)
      real(wp), allocatable :: en(:)

      real(wp), parameter :: cutoff = 30.0_wp

      call get_structure(mol, "x05")

      allocate(rcov(mol%nid), en(mol%nid))
      rcov(:) = get_covalent_rad(mol%num)
      en(:) = get_pauling_en(mol%num)

      call new_erf_en_ncoord(erf_en_ncoord, mol, cutoff=cutoff, rcov=rcov, en=en)
      call test_numsigma(error, mol, erf_en_ncoord, cutoff)

   end subroutine test_dcndL_anthracene_erf_en


end module test_ncoord
