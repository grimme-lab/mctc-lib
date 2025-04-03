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

module test_ncoord
   use mctc_env, only : wp
   use mctc_env_testing, only : new_unittest, unittest_type, error_type, &
   & test_failed, check
   use mctc_io_structure, only : structure_type
   use testsuite_structure, only : get_structure
   use mctc_cutoff, only : get_lattice_points
   use mctc_data_covrad, only : get_covalent_rad
   use mctc_data_paulingen, only : get_pauling_en
   use mctc_ncoord_dexp, only : dexp_ncoord_type, new_dexp_ncoord
   use mctc_ncoord_exp, only : exp_ncoord_type, new_exp_ncoord
   use mctc_ncoord_erf, only : erf_ncoord_type, new_erf_ncoord
   use mctc_ncoord_erf_en, only : erf_en_ncoord_type, new_erf_en_ncoord
   use mctc_ncoord_erf_dftd4, only : erf_dftd4_ncoord_type, new_erf_dftd4_ncoord
   use mctc_ncoord_type, only : ncoord_type
   use mctc_ncoord, only : new_ncoord, cn_count, get_cn_count_id, get_cn_count_string
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
      & new_unittest("cn-mb01_dexp", test_cn_mb01_dexp), &
      & new_unittest("cn-mb01_dexp-defaults", test_cn_mb01_dexp_defaults), &
      & new_unittest("cn-mb02_dexp", test_cn_mb02_dexp), &
      & new_unittest("cn-mb03_dexp", test_cn_mb03_dexp), &
      & new_unittest("cn-acetic_dexp", test_cn_acetic_dexp), &
      & new_unittest("dcndr-mb04_dexp", test_dcndr_mb04_dexp), &
      & new_unittest("dcndr-mb05_dexp", test_dcndr_mb05_dexp), &
      & new_unittest("dcndr-ammonia_dexp", test_dcndr_ammonia_dexp), &
      & new_unittest("dcndL-mb06_dexp", test_dcndL_mb06_dexp), &
      & new_unittest("dcndL-mb07_dexp", test_dcndL_mb07_dexp), &
      & new_unittest("dcndL-antracene_dexp", test_dcndL_anthracene_dexp), &
      & new_unittest("cn-mb01_exp", test_cn_mb01_exp), &
      & new_unittest("cn-mb01_exp-defaults", test_cn_mb01_exp_defaults), &
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
      & new_unittest("cn-mb01_erf-defaults", test_cn_mb01_erf_defaults), &
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
      & new_unittest("cn-mb01_erf_en-defaults", test_cn_mb01_erf_en_defaults), &
      & new_unittest("cn-mb02_erf_en", test_cn_mb02_erf_en), &
      & new_unittest("cn-mb03_erf_en", test_cn_mb03_erf_en), &
      & new_unittest("cn-acetic_erf_en", test_cn_acetic_erf_en), &
      & new_unittest("dcndr-mb04_erf_en", test_dcndr_mb04_erf_en), &
      & new_unittest("dcndr-mb05_erf_en", test_dcndr_mb05_erf_en), &
      & new_unittest("dcndr-ammonia_erf_en", test_dcndr_ammonia_erf_en), &
      & new_unittest("dcndL-mb06_erf_en", test_dcndL_mb06_erf_en), &
      & new_unittest("dcndL-mb07_erf_en", test_dcndL_mb07_erf_en), &
      & new_unittest("dcndL-antracene_erf_en", test_dcndL_anthracene_erf_en), &
      & new_unittest("cn-mb01_erf_dftd4", test_cn_mb01_erf_dftd4), &
      & new_unittest("cn-mb01_erf_dftd4-defaults", test_cn_mb01_erf_dftd4_defaults), &
      & new_unittest("dfdcn-mb01_erf_dftd4", test_dfdcn_mb01_erf_dftd4), &
      & new_unittest("cn-mb02_erf_dftd4", test_cn_mb02_erf_dftd4), &
      & new_unittest("dfdcn-mb02_erf_dftd4", test_dfdcn_mb02_erf_dftd4), &
      & new_unittest("cn-mb03_erf_dftd4", test_cn_mb03_erf_dftd4), &
      & new_unittest("cn-acetic_erf_dftd4", test_cn_acetic_erf_dftd4), &
      & new_unittest("dcndr-mb04_erf_dftd4", test_dcndr_mb04_erf_dftd4), &
      & new_unittest("dcndr-mb05_erf_dftd4", test_dcndr_mb05_erf_dftd4), &
      & new_unittest("dcndr-ammonia_erf_dftd4", test_dcndr_ammonia_erf_dftd4), &
      & new_unittest("dcndL-mb06_erf_dftd4", test_dcndL_mb06_erf_dftd4), &
      & new_unittest("dcndL-mb07_erf_dftd4", test_dcndL_mb07_erf_dftd4), &
      & new_unittest("dcndL-antracene_erf_dftd4", test_dcndL_anthracene_erf_dftd4), &
      & new_unittest("cn_unknown", test_cn_unknown, should_fail=.true.), &
      & new_unittest("cn_count_string_to_id", test_cn_count_string_to_id), &
      & new_unittest("cn_count_id_to_string", test_cn_count_id_to_string) &
      & ]

   end subroutine collect_ncoord


   subroutine test_cn_gen(error, mol, ncoord, ref)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      !> Molecular structure data
      type(structure_type) :: mol

      !> Coordination number type
      class(ncoord_type)   :: ncoord

      !> Reference CNs
      real(wp), intent(in) :: ref(:)
      real(wp), allocatable :: cn(:)
      real(wp), allocatable :: lattr(:, :)

      allocate(cn(mol%nat))

      call get_lattice_points(mol%periodic, mol%lattice, ncoord%cutoff, lattr)
      call ncoord%get_coordination_number(mol, lattr, cn)

      if (any(abs(cn - ref) > thr)) then
         call test_failed(error, "Coordination numbers do not match")
         print'(3es21.14)', cn
      end if

   end subroutine test_cn_gen


   subroutine test_numgrad(error, mol, ncoord)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      !> Molecular structure data
      type(structure_type), intent(inout) :: mol

      !> Coordination number type
      class(ncoord_type)   :: ncoord

      integer :: iat, ic
      real(wp), allocatable :: cn(:), cnr(:), cnl(:)
      real(wp), allocatable :: dcndr(:, :, :), dcndL(:, :, :)
      real(wp), allocatable :: numdr(:, :, :)
      real(wp), allocatable :: lattr(:, :)
      real(wp), parameter :: step = 1.0e-6_wp

      allocate(cn(mol%nat), cnr(mol%nat), cnl(mol%nat), &
      & dcndr(3, mol%nat, mol%nat), dcndL(3, 3, mol%nat), &
      & numdr(3, mol%nat, mol%nat))

      call get_lattice_points(mol%periodic, mol%lattice, ncoord%cutoff, lattr)

      do iat = 1, mol%nat
         do ic = 1, 3
            mol%xyz(ic, iat) = mol%xyz(ic, iat) + step
            call ncoord%get_coordination_number(mol, lattr, cnr)
            mol%xyz(ic, iat) = mol%xyz(ic, iat) - 2*step
            call ncoord%get_coordination_number(mol, lattr, cnl)
            mol%xyz(ic, iat) = mol%xyz(ic, iat) + step
            numdr(ic, iat, :) = 0.5_wp*(cnr - cnl)/step
         end do
      end do

      call ncoord%get_coordination_number(mol, lattr, cn, dcndr, dcndL)

      if (any(abs(dcndr - numdr) > thr2)) then
         call test_failed(error, "Derivative of coordination number does not match")
      end if

   end subroutine test_numgrad


   subroutine test_numsigma(error, mol, ncoord)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      !> Molecular structure data
      type(structure_type), intent(inout) :: mol

      !> Coordination number type
      class(ncoord_type)   :: ncoord

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
      call get_lattice_points(mol%periodic, mol%lattice, ncoord%cutoff, lattr)

      eps(:, :) = unity
      xyz(:, :) = mol%xyz
      trans = lattr
      do ic = 1, 3
         do jc = 1, 3
            eps(jc, ic) = eps(jc, ic) + step
            mol%xyz(:, :) = matmul(eps, xyz)
            lattr(:, :) = matmul(eps, trans)
            call ncoord%get_coordination_number(mol, lattr, cnr)
            eps(jc, ic) = eps(jc, ic) - 2*step
            mol%xyz(:, :) = matmul(eps, xyz)
            lattr(:, :) = matmul(eps, trans)
            call ncoord%get_coordination_number(mol, lattr, cnl)
            eps(jc, ic) = eps(jc, ic) + step
            mol%xyz(:, :) = xyz
            lattr(:, :) = trans
            numdL(jc, ic, :) = 0.5_wp*(cnr - cnl)/step
         end do
      end do

      call ncoord%get_coordination_number(mol, lattr, cn, dcndr, dcndL)

      if (any(abs(dcndL - numdL) > 10.0_wp * thr2)) then
         call test_failed(error, "Derivative of coordination number does not match")
      end if

   end subroutine test_numsigma


   !> ----------------------------------------------------
   !> Tests for double-exponential coordination number
   !> ----------------------------------------------------
   subroutine test_cn_mb01_dexp(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      class(ncoord_type), allocatable :: dexp_ncoord
      real(wp), allocatable :: rcov(:)
      real(wp), allocatable :: cn(:)

      real(wp), parameter :: cutoff = 30.0_wp
      real(wp), parameter :: ref(16) = [&
      & 4.11453659059991E+0_wp, 9.32058998762811E-1_wp, 2.03554597140311E+0_wp, &
      & 1.42227835389358E+0_wp, 1.12812426574031E+0_wp, 1.05491602558828E+0_wp, &
      & 1.52709064704269E+0_wp, 1.95070367247232E+0_wp, 3.83759889196540E+0_wp, &
      & 1.09388314007182E+0_wp, 1.07090773695340E+0_wp, 2.00285254082830E+0_wp, &
      & 4.36400837813955E+0_wp, 3.83469860546080E+0_wp, 3.91542517673963E+0_wp, &
      & 5.58571682419960E+0_wp]

      call get_structure(mol, "mindless01")

      allocate(rcov(mol%nid), cn(mol%nat))
      rcov(:) = get_covalent_rad(mol%num)
      ! Test also the external interface
      call new_ncoord(dexp_ncoord, mol, cn_count%dexp, error, &
         & cutoff=cutoff, rcov=rcov)
      if(allocated(error)) return
      call dexp_ncoord%get_cn(mol, cn)

      if (any(abs(cn - ref) > thr)) then
         call test_failed(error, "Coordination numbers do not match")
         print'(3es21.14)', cn
      end if

   end subroutine test_cn_mb01_dexp

   subroutine test_cn_mb01_dexp_defaults(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      class(ncoord_type), allocatable :: dexp_ncoord
      real(wp), allocatable :: cn(:)

      real(wp), parameter :: cut = 8.0_wp
      real(wp), parameter :: ref(16) = [&
      & 4.09454175505598E+0_wp, 9.31542782327779E-1_wp, 2.03331622407469E+0_wp, &
      & 1.42122371191997E+0_wp, 1.12742367795718E+0_wp, 1.05428853626261E+0_wp, &
      & 1.52588252023738E+0_wp, 1.94868233639268E+0_wp, 3.82248414162617E+0_wp, &
      & 1.09321740798910E+0_wp, 1.07026473308676E+0_wp, 2.00070519876048E+0_wp, &
      & 4.33832738074586E+0_wp, 3.81962825718532E+0_wp, 3.89907034954333E+0_wp, &
      & 5.50039630110828E+0_wp]

      call get_structure(mol, "mindless01")

      allocate(cn(mol%nat))
      ! Test also the external interface
      call new_ncoord(dexp_ncoord, mol, cn_count%dexp, error, cut=cut)
      if(allocated(error)) return
      call dexp_ncoord%get_cn(mol, cn)

      if (any(abs(cn - ref) > thr)) then
         call test_failed(error, "Coordination numbers do not match")
         print'(3es21.14)', cn
      end if

   end subroutine test_cn_mb01_dexp_defaults


   subroutine test_cn_mb02_dexp(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      type(dexp_ncoord_type) :: dexp_ncoord
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

      call new_dexp_ncoord(dexp_ncoord, mol, cutoff=cutoff, rcov=rcov)
      call test_cn_gen(error, mol, dexp_ncoord, ref)

   end subroutine test_cn_mb02_dexp


   subroutine test_cn_mb03_dexp(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      type(dexp_ncoord_type) :: dexp_ncoord
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

      call new_dexp_ncoord(dexp_ncoord, mol, cutoff=cutoff, rcov=rcov)
      call test_cn_gen(error, mol, dexp_ncoord, ref)

   end subroutine test_cn_mb03_dexp


   subroutine test_cn_acetic_dexp(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      type(dexp_ncoord_type) :: dexp_ncoord
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

      call new_dexp_ncoord(dexp_ncoord, mol, cutoff=cutoff, rcov=rcov)
      call test_cn_gen(error, mol, dexp_ncoord, ref)

   end subroutine test_cn_acetic_dexp


   subroutine test_dcndr_mb04_dexp(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      type(dexp_ncoord_type) :: dexp_ncoord
      real(wp), allocatable :: rcov(:)

      real(wp), parameter :: cutoff = 30.0_wp

      call get_structure(mol, "mindless04")

      allocate(rcov(mol%nid))
      rcov(:) = get_covalent_rad(mol%num)

      call new_dexp_ncoord(dexp_ncoord, mol, cutoff=cutoff, rcov=rcov)
      call test_numgrad(error, mol, dexp_ncoord)

   end subroutine test_dcndr_mb04_dexp


   subroutine test_dcndr_mb05_dexp(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      type(dexp_ncoord_type) :: dexp_ncoord
      real(wp), allocatable :: rcov(:)

      real(wp), parameter :: cutoff = 30.0_wp

      call get_structure(mol, "mindless05")

      allocate(rcov(mol%nid))
      rcov(:) = get_covalent_rad(mol%num)

      call new_dexp_ncoord(dexp_ncoord, mol, cutoff=cutoff, rcov=rcov)
      call test_numgrad(error, mol, dexp_ncoord)

   end subroutine test_dcndr_mb05_dexp


   subroutine test_dcndr_ammonia_dexp(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      type(dexp_ncoord_type) :: dexp_ncoord
      real(wp), allocatable :: rcov(:)

      real(wp), parameter :: cutoff = 30.0_wp

      call get_structure(mol, "x04")

      allocate(rcov(mol%nid))
      rcov(:) = get_covalent_rad(mol%num)

      call new_dexp_ncoord(dexp_ncoord, mol, cutoff=cutoff, rcov=rcov)
      call test_numgrad(error, mol, dexp_ncoord)

   end subroutine test_dcndr_ammonia_dexp


   subroutine test_dcndL_mb06_dexp(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      type(dexp_ncoord_type) :: dexp_ncoord
      real(wp), allocatable :: rcov(:)

      real(wp), parameter :: cutoff = 30.0_wp

      call get_structure(mol, "mindless06")

      allocate(rcov(mol%nid))
      rcov(:) = get_covalent_rad(mol%num)

      call new_dexp_ncoord(dexp_ncoord, mol, cutoff=cutoff, rcov=rcov)
      call test_numsigma(error, mol, dexp_ncoord)

   end subroutine test_dcndL_mb06_dexp


   subroutine test_dcndL_mb07_dexp(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      type(dexp_ncoord_type) :: dexp_ncoord
      real(wp), allocatable :: rcov(:)

      real(wp), parameter :: cutoff = 30.0_wp

      call get_structure(mol, "mindless07")

      allocate(rcov(mol%nid))
      rcov(:) = get_covalent_rad(mol%num)

      call new_dexp_ncoord(dexp_ncoord, mol, cutoff=cutoff, rcov=rcov)
      call test_numsigma(error, mol, dexp_ncoord)

   end subroutine test_dcndL_mb07_dexp


   subroutine test_dcndL_anthracene_dexp(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      type(dexp_ncoord_type) :: dexp_ncoord
      real(wp), allocatable :: rcov(:)

      real(wp), parameter :: cutoff = 30.0_wp

      call get_structure(mol, "x05")

      allocate(rcov(mol%nid))
      rcov(:) = get_covalent_rad(mol%num)

      call new_dexp_ncoord(dexp_ncoord, mol, cutoff=cutoff, rcov=rcov)
      call test_numsigma(error, mol, dexp_ncoord)

   end subroutine test_dcndL_anthracene_dexp


   !> ----------------------------------------------------
   !> Tests for mono-exponential coordination number
   !> ----------------------------------------------------
   subroutine test_cn_mb01_exp(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      class(ncoord_type), allocatable :: exp_ncoord
      real(wp), allocatable :: rcov(:)
      real(wp), allocatable :: cn(:)

      real(wp), parameter :: cutoff = 30.0_wp
      real(wp), parameter :: kcn = 8.0_wp
      real(wp), parameter :: ref(16) = [&
      & 4.37656744263860E+0_wp, 1.01980731865293E+0_wp, 2.15301102461509E+0_wp, &
      & 1.48974161904353E+0_wp, 1.37740361220178E+0_wp, 1.19571408761236E+0_wp, &
      & 1.64256955610752E+0_wp, 2.10050172533832E+0_wp, 3.97810521360522E+0_wp, &
      & 1.17477637418781E+0_wp, 1.21863960255555E+0_wp, 2.31794359805965E+0_wp, &
      & 4.30010159253583E+0_wp, 3.91597563150413E+0_wp, 4.05566413625170E+0_wp, &
      & 5.80055689112793E+0_wp]

      call get_structure(mol, "mindless01")

      allocate(rcov(mol%nid), cn(mol%nat))
      rcov(:) = get_covalent_rad(mol%num)

      ! Test also the external interface
      call new_ncoord(exp_ncoord, mol, cn_count%exp, error, &
         & kcn=kcn, cutoff=cutoff, rcov=rcov)
      if(allocated(error)) return
      call exp_ncoord%get_cn(mol, cn)

      if (any(abs(cn - ref) > thr)) then
         call test_failed(error, "Coordination numbers do not match")
         print'(3es21.14)', cn
      end if
   
   end subroutine test_cn_mb01_exp


   subroutine test_cn_mb01_exp_defaults(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      class(ncoord_type), allocatable :: exp_ncoord
      real(wp), allocatable :: cn(:)

      real(wp), parameter :: cut = 8.0_wp
      real(wp), parameter :: ref(16) = [&
      & 4.12992877807345E+0_wp, 9.78311016805857E-1_wp, 2.00864270424507E+0_wp, &
      & 1.47752177526616E+0_wp, 1.03516897897016E+0_wp, 1.01148282016162E+0_wp, &
      & 1.50212590877184E+0_wp, 1.99644790103832E+0_wp, 3.87585066556086E+0_wp, &
      & 1.04261742233948E+0_wp, 1.01467576985712E+0_wp, 1.99102872800478E+0_wp, &
      & 4.60161397650915E+0_wp, 3.85745359167474E+0_wp, 3.97547595558861E+0_wp, &
      & 5.37578501884834E+0_wp]

      call get_structure(mol, "mindless01")

      allocate(cn(mol%nat))
      ! Test also the external interface
      call new_ncoord(exp_ncoord, mol, cn_count%exp, error, cut=cut)
      if(allocated(error)) return
      call exp_ncoord%get_cn(mol, cn)

      if (any(abs(cn - ref) > thr)) then
         call test_failed(error, "Coordination numbers do not match")
         print'(3es21.14)', cn
      end if

   end subroutine test_cn_mb01_exp_defaults


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
      call test_cn_gen(error, mol,  exp_ncoord, ref)

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
      call test_cn_gen(error, mol, exp_ncoord, ref)

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
      call test_cn_gen(error, mol, exp_ncoord, ref)

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
      call test_numgrad(error, mol, exp_ncoord)

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
      call test_numgrad(error, mol, exp_ncoord)

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
      call test_numgrad(error, mol, exp_ncoord)

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
      call test_numsigma(error, mol, exp_ncoord)

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
      call test_numsigma(error, mol, exp_ncoord)

   end subroutine test_dcndL_mb07_exp


   subroutine test_dcndL_anthracene_exp(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      type(exp_ncoord_type) :: exp_ncoord
      real(wp), allocatable :: rcov(:)

      real(wp), parameter :: cutoff = 30.0_wp

      call get_structure(mol, "x05")

      allocate(rcov(mol%nid))
      rcov(:) = get_covalent_rad(mol%num)

      call new_exp_ncoord(exp_ncoord, mol, cutoff=cutoff, rcov=rcov)
      call test_numsigma(error, mol, exp_ncoord)

   end subroutine test_dcndL_anthracene_exp


   !> ----------------------------------------------------
   !> Tests for error-function based coordination number 
   !> using the Pyykko covalent radii and Pauling EN
   !> ----------------------------------------------------
   subroutine test_cn_mb01_erf(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      class(ncoord_type), allocatable :: erf_ncoord
      real(wp), allocatable :: rcov(:)
      real(wp), allocatable :: cn(:)

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

      allocate(rcov(mol%nid), cn(mol%nat))
      rcov(:) = get_covalent_rad(mol%num)

      ! Test also the external interface
      call new_ncoord(erf_ncoord, mol, cn_count%erf, error, &
         & kcn=kcn, cutoff=cutoff, rcov=rcov)
      if(allocated(error)) return
      call erf_ncoord%get_cn(mol, cn)

      if (any(abs(cn - ref) > thr)) then
         call test_failed(error, "Coordination numbers do not match")
         print'(3es21.14)', cn
      end if

   end subroutine test_cn_mb01_erf


   subroutine test_cn_mb01_erf_defaults(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      class(ncoord_type), allocatable :: erf_ncoord
      real(wp), allocatable :: cn(:)

      real(wp), parameter :: cut = 8.0_wp
      real(wp), parameter :: ref(16) = [&
      & 3.94865526262727E+0_wp, 8.03534979384260E-1_wp, 1.82363418867863E+0_wp, &
      & 1.29194109497855E+0_wp, 1.00631892909636E+0_wp, 9.20734547485533E-1_wp, &
      & 1.43055872450306E+0_wp, 1.75329680336097E+0_wp, 3.38202218304395E+0_wp, &
      & 1.01209895835635E+0_wp, 9.31636038289878E-1_wp, 1.77838311316717E+0_wp, &
      & 3.94732423741765E+0_wp, 3.49939502605498E+0_wp, 3.49015552628822E+0_wp, &
      & 5.16876186109389E+0_wp]

      call get_structure(mol, "mindless01")

      allocate(cn(mol%nat))
      ! Test also the external interface
      call new_ncoord(erf_ncoord, mol, cn_count%erf, error, cut=cut)
      if(allocated(error)) return
      call erf_ncoord%get_cn(mol, cn)

      if (any(abs(cn - ref) > thr)) then
         call test_failed(error, "Coordination numbers do not match")
         print'(3es21.14)', cn
      end if

   end subroutine test_cn_mb01_erf_defaults


   subroutine test_cn_mb02_erf(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      type(erf_ncoord_type) :: erf_ncoord
      real(wp), allocatable :: rcov(:)

      real(wp), parameter :: cutoff = 30.0_wp
      real(wp), parameter :: kcn = 2.60_wp
      real(wp), parameter :: norm_exp = 0.8_wp
      real(wp), parameter :: ref(16) = [&
      & 7.82883150684095E-01_wp, 3.47414568086695E+00_wp, 3.21490904495080E+00_wp, &
      & 2.57204039419665E+00_wp, 4.70139897809430E+00_wp, 1.01600663641776E+00_wp, &
      & 8.02988129457653E-01_wp, 8.05575211220716E-01_wp, 4.53312550965630E+00_wp, &
      & 1.07341286735542E+00_wp, 3.48131954350286E+00_wp, 3.75872922171681E+00_wp, &
      & 1.83133763195600E+00_wp, 8.02241219305798E-01_wp, 1.33431699912730E+00_wp, &
      & 1.85405528497185E+00_wp]

      call get_structure(mol, "mindless02")

      allocate(rcov(mol%nid))
      rcov(:) = get_covalent_rad(mol%num)

      call new_erf_ncoord(erf_ncoord, mol, kcn=kcn, cutoff=cutoff, &
         & rcov=rcov, norm_exp=norm_exp)
      call test_cn_gen(error, mol, erf_ncoord, ref)

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
      call test_cn_gen(error, mol, erf_ncoord, ref)

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
      call test_cn_gen(error, mol, erf_ncoord, ref)

   end subroutine test_cn_acetic_erf


   subroutine test_dcndr_mb04_erf(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      type(erf_ncoord_type) :: erf_ncoord
      real(wp), allocatable :: rcov(:)

      real(wp), parameter :: cutoff = 30.0_wp
      real(wp), parameter :: cut = 8.0_wp

      call get_structure(mol, "mindless04")

      allocate(rcov(mol%nid))
      rcov(:) = get_covalent_rad(mol%num)

      call new_erf_ncoord(erf_ncoord, mol, cutoff=cutoff, rcov=rcov, cut=cut)
      call test_numgrad(error, mol, erf_ncoord)

   end subroutine test_dcndr_mb04_erf


   subroutine test_dcndr_mb05_erf(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      type(erf_ncoord_type) :: erf_ncoord
      real(wp), allocatable :: rcov(:)

      real(wp), parameter :: norm_exp = 0.8_wp
      real(wp), parameter :: cutoff = 30.0_wp

      call get_structure(mol, "mindless05")

      allocate(rcov(mol%nid))
      rcov(:) = get_covalent_rad(mol%num)

      call new_erf_ncoord(erf_ncoord, mol, cutoff=cutoff, &
         & rcov=rcov, norm_exp=norm_exp)
      call test_numgrad(error, mol, erf_ncoord)

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
      call test_numgrad(error, mol, erf_ncoord)

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
      call test_numsigma(error, mol, erf_ncoord)

   end subroutine test_dcndL_mb06_erf


   subroutine test_dcndL_mb07_erf(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      type(erf_ncoord_type) :: erf_ncoord
      real(wp), allocatable :: rcov(:)

      real(wp), parameter :: cutoff = 30.0_wp
      real(wp), parameter :: cut = 8.0_wp

      call get_structure(mol, "mindless07")

      allocate(rcov(mol%nid))
      rcov(:) = get_covalent_rad(mol%num)

      call new_erf_ncoord(erf_ncoord, mol, cutoff=cutoff, rcov=rcov, cut=cut)
      call test_numsigma(error, mol, erf_ncoord)

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
      call test_numsigma(error, mol, erf_ncoord)

   end subroutine test_dcndL_anthracene_erf


   !> ----------------------------------------------------
   !> Tests for electronegativity-weighted
   !> error-function-based coordination number
   !> using the Pyykko covalent radii and Pauling EN
   !> ----------------------------------------------------
   subroutine test_cn_mb01_erf_en(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      class(ncoord_type), allocatable :: erf_en_ncoord
      real(wp), allocatable :: rcov(:)
      real(wp), allocatable :: en(:)
      real(wp), allocatable :: cn(:)

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

      allocate(rcov(mol%nid), en(mol%nid), cn(mol%nat))
      rcov(:) = get_covalent_rad(mol%num)
      en(:) = get_pauling_en(mol%num)

      ! Test also the external interface
      call new_ncoord(erf_en_ncoord, mol, cn_count%erf_en, error, &
         & kcn=kcn, cutoff=cutoff, rcov=rcov, en=en)
      if(allocated(error)) return
      call erf_en_ncoord%get_cn(mol, cn)

      if (any(abs(cn - ref) > thr)) then
         call test_failed(error, "Coordination numbers do not match")
         print'(3es21.14)', cn
      end if

   end subroutine test_cn_mb01_erf_en


   subroutine test_cn_mb01_erf_en_defaults(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      class(ncoord_type), allocatable :: erf_en_ncoord
      real(wp), allocatable :: cn(:)

      real(wp), parameter :: cut = 8.0_wp
      real(wp), parameter :: ref(16) = [&
      & 6.28512318865646E+0_wp, -9.48165569926278E-2_wp, -3.20974798259955E+0_wp, &
      &-7.29074735683836E-1_wp, -1.91124739345982E+0_wp,  6.75956934105788E-1_wp, &
      &-6.45376614102426E-1_wp, -2.57466431918300E+0_wp, -3.30563352773637E+0_wp, &
      & 8.94205686629223E-1_wp,  6.68995462547443E-1_wp, -2.50415951195522E+0_wp, &
      & 4.30396885503051E-1_wp,  2.99748612833491E+0_wp, -2.73382879558600E+0_wp, &
      & 5.47020661174583E+0_wp]

      call get_structure(mol, "mindless01")

      allocate(cn(mol%nat))
      ! Test also the external interface
      call new_ncoord(erf_en_ncoord, mol, cn_count%erf_en, error, cut=cut)
      if(allocated(error)) return
      call erf_en_ncoord%get_cn(mol, cn)

      if (any(abs(cn - ref) > thr)) then
         call test_failed(error, "Coordination numbers do not match")
         print'(3es21.14)', cn
      end if

   end subroutine test_cn_mb01_erf_en_defaults


   subroutine test_cn_mb02_erf_en(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      type(erf_en_ncoord_type) :: erf_en_ncoord
      real(wp), allocatable :: rcov(:)
      real(wp), allocatable :: en(:)

      real(wp), parameter :: cutoff = 30.0_wp
      real(wp), parameter :: kcn = 2.60_wp
      real(wp), parameter :: norm_exp = 0.8_wp
      real(wp), parameter :: ref(16) = [&
      & -1.20766490409683E-01_wp, -3.16431597148754E+00_wp, -5.05004208597497E-02_wp, &
      & -4.25983254262925E+00_wp,  5.12905009209427E+00_wp,  8.28195323146111E-01_wp, &
      & -1.20642619114040E-01_wp, -1.30015328757524E-01_wp,  1.04018711784936E+00_wp, &
      &  8.14999806238091E-01_wp,  6.97386143793777E-01_wp,  6.83247561402713E+00_wp, &
      & -5.18157773756499E+00_wp, -2.40202426638317E-01_wp, -3.02851981491491E-01_wp, &
      & -1.77158857819614E+00_wp]

      call get_structure(mol, "mindless02")

      allocate(rcov(mol%nid), en(mol%nid))
      rcov(:) = get_covalent_rad(mol%num)
      en(:) = get_pauling_en(mol%num)

      call new_erf_en_ncoord(erf_en_ncoord, mol, kcn=kcn, cutoff=cutoff, &
         & rcov=rcov, en=en, norm_exp=norm_exp)
      call test_cn_gen(error, mol,  erf_en_ncoord, ref)

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
      call test_cn_gen(error, mol, erf_en_ncoord, ref)

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
      call test_cn_gen(error, mol, erf_en_ncoord, ref)

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
      call test_numgrad(error, mol, erf_en_ncoord)

   end subroutine test_dcndr_mb04_erf_en


   subroutine test_dcndr_mb05_erf_en(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      type(erf_en_ncoord_type) :: erf_en_ncoord
      real(wp), allocatable :: rcov(:)
      real(wp), allocatable :: en(:)

      real(wp), parameter :: norm_exp = 0.8_wp
      real(wp), parameter :: cutoff = 30.0_wp

      call get_structure(mol, "mindless05")

      allocate(rcov(mol%nid), en(mol%nid))
      rcov(:) = get_covalent_rad(mol%num)
      en(:) = get_pauling_en(mol%num)

      call new_erf_en_ncoord(erf_en_ncoord, mol, cutoff=cutoff, &
         & rcov=rcov, en=en, norm_exp=norm_exp)
      call test_numgrad(error, mol, erf_en_ncoord)

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
      call test_numgrad(error, mol, erf_en_ncoord)

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
      call test_numsigma(error, mol, erf_en_ncoord)

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
      call test_numsigma(error, mol, erf_en_ncoord)

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
      call test_numsigma(error, mol, erf_en_ncoord)

   end subroutine test_dcndL_anthracene_erf_en


   !> ----------------------------------------------------
   !> Tests for DFT-D4 coordination number
   !> using the Pyykko covalent radii and Pauling EN
   !> ----------------------------------------------------
   subroutine test_cn_mb01_erf_dftd4(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      class(ncoord_type), allocatable :: erf_dftd4_ncoord
      real(wp), allocatable :: rcov(:)
      real(wp), allocatable :: en(:)
      real(wp), allocatable :: cn(:)

      real(wp), parameter :: cutoff = 30.0_wp
      real(wp), parameter :: ref(16) = [&
      & 3.07349677110402E+0_wp, 9.31461605116103E-1_wp, 1.43709439375839E+0_wp, &
      & 1.33309431581960E+0_wp, 7.20743527030337E-1_wp, 8.59659004770982E-1_wp, &
      & 1.35782158177921E+0_wp, 1.53940006996025E+0_wp, 3.19400368195259E+0_wp, &
      & 8.12162111631342E-1_wp, 8.59533443784854E-1_wp, 1.53347108155587E+0_wp, &
      & 4.23314989525721E+0_wp, 3.03048504567396E+0_wp, 3.45229319488306E+0_wp, &
      & 4.28478289652264E+0_wp]

      call get_structure(mol, "mindless01")

      allocate(rcov(mol%nid), en(mol%nid), cn(mol%nat))
      rcov(:) = get_covalent_rad(mol%num)
      en(:) = get_pauling_en(mol%num)

      ! Test also the external interface
      call new_ncoord(erf_dftd4_ncoord, mol, cn_count%dftd4, error, &
         & cutoff=cutoff, rcov=rcov, en=en)
      if(allocated(error)) return
      call erf_dftd4_ncoord%get_cn(mol, cn)

      if (any(abs(cn - ref) > thr)) then
         call test_failed(error, "Coordination numbers do not match")
         print'(3es21.14)', cn
      end if

   end subroutine test_cn_mb01_erf_dftd4


   subroutine test_cn_mb01_erf_dftd4_defaults(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      class(ncoord_type), allocatable :: erf_dftd4_ncoord
      real(wp), allocatable :: cn(:)

      real(wp), parameter :: cut = 8.0_wp
      real(wp), parameter :: ref(16) = [&
      & 3.06660652852575E+0_wp, 9.30945897066814E-1_wp, 1.43601901807739E+0_wp, &
      & 1.33215820126205E+0_wp, 7.20389472953384E-1_wp, 8.59202243173923E-1_wp, &
      & 1.35685365476930E+0_wp, 1.53817284066048E+0_wp, 3.18619182109750E+0_wp, &
      & 8.11742082108597E-1_wp, 8.59076781607564E-1_wp, 1.53225308252248E+0_wp, &
      & 4.21062383837406E+0_wp, 3.02389795134399E+0_wp, 3.44209283304307E+0_wp, &
      & 4.26105988742614E+0_wp]

      call get_structure(mol, "mindless01")

      allocate(cn(mol%nat))
      ! Test also the external interface
      call new_ncoord(erf_dftd4_ncoord, mol, cn_count%dftd4, error, cut=cut)
      if(allocated(error)) return
      call erf_dftd4_ncoord%get_cn(mol, cn)

      if (any(abs(cn - ref) > thr)) then
         call test_failed(error, "Coordination numbers do not match")
         print'(3es21.14)', cn
      end if

   end subroutine test_cn_mb01_erf_dftd4_defaults


   subroutine test_dfdcn_mb01_erf_dftd4(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      class(ncoord_type), allocatable :: erf_dftd4_ncoord
      real(wp), allocatable :: rcov(:)
      real(wp), allocatable :: en(:)
      real(wp) :: gradient(3, 16), sigma(3,3), dEdcn(16)
      real(wp), allocatable :: cn(:)
      real(wp), allocatable :: lattr(:, :)

      real(wp), parameter :: cutoff = 30.0_wp
      real(wp), parameter :: ref_gradient(3, 16) = reshape([ &
      &  2.3515337861584880_wp, -3.0453827536691351_wp,  0.5392536733971742_wp, & 
      & -0.4086227335489838_wp,  0.0501074408829545_wp,  0.2137313744331271_wp, & 
      &  0.0249303991608369_wp, -0.0373339882416463_wp, -0.0756057413224991_wp, & 
      & -0.5436172455452489_wp,  0.4933244472902760_wp,  1.2499429094951524_wp, & 
      &  0.0187062301843386_wp,  0.0079863446319139_wp, -0.0342050060044000_wp, & 
      &  0.0541178064781164_wp,  0.0314514945594538_wp,  0.0256701683856457_wp, & 
      & -1.1131746078453495_wp, -1.6288054931512879_wp,  0.4146928754871603_wp, & 
      &  0.0887505649437715_wp,  0.0350320577127468_wp, -0.0407677557149725_wp, & 
      & -0.0499761354250785_wp,  0.1548143128637227_wp, -0.2724294261044626_wp, & 
      & -0.2140027394072616_wp, -0.0253209254791824_wp, -0.0023845891460968_wp, & 
      &  0.0091192286107324_wp, -0.0054889861418698_wp, -0.0708601381531837_wp, & 
      &  0.1710608463902656_wp,  0.5486016273258228_wp,  0.0456750463319099_wp, & 
      & -1.4496220571725840_wp,  0.2848532829040934_wp,  2.1631929338961609_wp, & 
      &  0.2437819446177886_wp,  0.0864993947414499_wp, -0.8074962768633803_wp, & 
      &  0.0760143426254694_wp,  0.2932453158431059_wp, -0.1384011976765554_wp, & 
      &  0.7410003697746987_wp,  2.7564164279275825_wp, -3.2100088504407793_wp], &
      & shape(ref_gradient))
      real(wp), parameter :: ref_sigma(3, 3) = reshape([ &
      &-14.102412014163017_wp,  5.058593018200457_wp,  7.2112237247416280_wp, &      
      &  5.058593018200459_wp,-18.111874478203490_wp,  6.8763096498221916_wp, &        
      &  7.211223724741628_wp,  6.876309649822192_wp, -22.636906090720050_wp], &
      & shape(ref_sigma))

      dEdcn = 1.0_wp
      gradient = 0.0_wp
      sigma = 0.0_wp

      call get_structure(mol, "mindless01")

      allocate(rcov(mol%nid), en(mol%nid), cn(mol%nat))
      rcov(:) = get_covalent_rad(mol%num)
      en(:) = get_pauling_en(mol%num)

      ! Test also the external interface
      call new_ncoord(erf_dftd4_ncoord, mol, cn_count%dftd4, error, &
         & cutoff=cutoff, rcov=rcov, en=en)
      if(allocated(error)) return
      call get_lattice_points(mol%periodic, mol%lattice, cutoff, lattr)
      call erf_dftd4_ncoord%add_coordination_number_derivs(mol, lattr, &
         & dEdcn, gradient, sigma)

      if (any(abs(gradient - ref_gradient) > thr)) then
         call test_failed(error, "Coordination number gradient does not match")
         print'(3es21.14)', gradient - ref_gradient
      end if

      if (any(abs(sigma - ref_sigma) > thr)) then
         call test_failed(error, "Coordination numbers sigma does not match")
         print'(3es21.14)', sigma - ref_sigma
      end if

   end subroutine test_dfdcn_mb01_erf_dftd4


   subroutine test_cn_mb02_erf_dftd4(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      type(erf_dftd4_ncoord_type) :: erf_dftd4_ncoord
      real(wp), allocatable :: rcov(:)
      real(wp), allocatable :: en(:)

      real(wp), parameter :: cutoff = 30.0_wp
      real(wp), parameter :: norm_exp = 0.8_wp
      real(wp), parameter :: ref(16) = [&
      & 9.42718287075170E-1_wp, 3.36848356833576E+0_wp, 3.63463906109694E+0_wp, &
      & 2.27670250096768E+0_wp, 4.71481260163654E+0_wp, 8.10661654309892E-1_wp, &
      & 9.47358382643435E-1_wp, 9.48316042740831E-1_wp, 4.64606177656574E+0_wp, &
      & 8.10384366749581E-1_wp, 3.74392916782297E+0_wp, 2.93620819522544E+0_wp, &
      & 1.24273507745740E+0_wp, 9.30562514461567E-1_wp, 1.67284106029936E+0_wp, &
      & 1.68882066428832E+0_wp]

      call get_structure(mol, "mindless02")

      allocate(rcov(mol%nid), en(mol%nid))
      rcov(:) = get_covalent_rad(mol%num)
      en(:) = get_pauling_en(mol%num)

      call new_erf_dftd4_ncoord(erf_dftd4_ncoord, mol, cutoff=cutoff, &
         & rcov=rcov, en=en, norm_exp=norm_exp)
      call test_cn_gen(error, mol, erf_dftd4_ncoord, ref)

   end subroutine test_cn_mb02_erf_dftd4


   subroutine test_dfdcn_mb02_erf_dftd4(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      class(ncoord_type), allocatable :: erf_dftd4_ncoord
      real(wp), allocatable :: rcov(:)
      real(wp), allocatable :: en(:)
      real(wp) :: gradient(3, 16), sigma(3,3), dEdcn(16)
      real(wp), allocatable :: cn(:)
      real(wp), allocatable :: lattr(:, :)

      real(wp), parameter :: cutoff = 30.0_wp
      real(wp), parameter :: ref_gradient_sum(3, 16) = reshape([ &
      &  2.79749285166706E-1_wp,  3.23279014603977E-1_wp,  4.74749890756763E-1_wp, &
      &  1.02437156867081E+0_wp,  4.37155089119230E-1_wp, -1.13169437751526E-1_wp, &
      &  2.54696077961347E-1_wp,  4.09066066212646E-1_wp, -1.12269299047491E-2_wp, &
      &  2.51614630638352E-1_wp,  1.87222747939519E-1_wp,  2.76829286072773E-1_wp, &
      &  4.40284680801481E-2_wp, -7.56166386161761E-1_wp,  7.01904086405679E-1_wp, &
      &  1.58721720143057E-1_wp,  3.63440057577123E-1_wp,  1.83518080621616E-1_wp, &
      &  2.13914184282644E-1_wp,  3.52649741628978E-1_wp,  1.68047797901627E-3_wp, &
      &  2.29179262974525E-1_wp,  1.02874773836429E-1_wp, -1.37475215145587E-2_wp, &
      & -1.34756354987157E+0_wp, -1.85608742158071E-1_wp, -2.84338045516046E-1_wp, &
      &  1.76244408740674E-2_wp,  2.21601609485103E-1_wp,  2.15429160786252E-1_wp, &
      &  1.59652700020135E+0_wp,  4.66835609245963E-1_wp,  1.13197638015800E-1_wp, &
      &  5.07486065271068E-1_wp, -1.00280866066602E-2_wp,  4.34684154881412E-1_wp, &
      &  2.03877223927068E-1_wp,  1.89357746763728E-1_wp,  2.05312698749555E-1_wp, &
      &  6.50398980865652E-2_wp,  3.05967305503566E-1_wp,  1.26394855283175E-1_wp, &
      & -4.47718726768925E-1_wp,  5.82778267363327E-1_wp,  6.43702044153264E-1_wp, &
      &  1.48452450362789E-1_wp,  2.09575185646903E-1_wp,  2.45079560981574E-1_wp], &
      & shape(ref_gradient_sum))
      real(wp), parameter :: ref_sigma_sum(3, 3) = reshape([ &
      & -1.11429548887998E+1_wp, -1.15390001770966E+0_wp, -5.03186565579172E-1_wp, &
      & -1.15390001770966E+0_wp, -7.18355603112573E+0_wp,  1.24148995646507E+0_wp, &
      & -5.03186565579172E-1_wp,  1.24148995646507E+0_wp, -5.13022598538117E+0_wp], &
      & shape(ref_sigma_sum))

      ! Include pervious gradient and sigma
      dEdcn = 0.5_wp
      gradient = 0.2_wp
      sigma = 0.3_wp

      call get_structure(mol, "mindless02")

      allocate(rcov(mol%nid), en(mol%nid), cn(mol%nat))
      rcov(:) = get_covalent_rad(mol%num)
      en(:) = get_pauling_en(mol%num)

      ! Test also the external interface
      call new_ncoord(erf_dftd4_ncoord, mol, cn_count%dftd4, error, &
         & cutoff=cutoff, rcov=rcov, en=en)
      if(allocated(error)) return
      call get_lattice_points(mol%periodic, mol%lattice, cutoff, lattr)
      call erf_dftd4_ncoord%add_coordination_number_derivs(mol, lattr, &
         & dEdcn, gradient, sigma)

      if (any(abs(gradient - ref_gradient_sum) > thr)) then
         call test_failed(error, "Coordination number gradient does not match")
         print'(3es21.14)', gradient - ref_gradient_sum
      end if

      if (any(abs(sigma - ref_sigma_sum) > thr)) then
         call test_failed(error, "Coordination numbers sigma does not match")
         print'(3es21.14)', sigma - ref_sigma_sum
      end if

   end subroutine test_dfdcn_mb02_erf_dftd4


   subroutine test_cn_mb03_erf_dftd4(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      type(erf_dftd4_ncoord_type) :: erf_dftd4_ncoord
      real(wp), allocatable :: rcov(:)
      real(wp), allocatable :: en(:)

      real(wp), parameter :: cutoff = 30.0_wp
      real(wp), parameter :: kcn = 8.0_wp
      real(wp), parameter :: ref(16) = [&
      & 3.70724324529119E+0_wp, 2.11973219426028E+0_wp, 9.26017486406989E-1_wp, &
      & 3.90899183247175E+0_wp, 6.23852155281573E+0_wp, 4.15468179554002E+0_wp, &
      & 4.24624109603910E+0_wp, 1.03290088178372E+0_wp, 9.26063450718642E-1_wp, &
      & 9.26690202951857E-1_wp, 1.23222500932634E+0_wp, 2.60466433049767E+0_wp, &
      & 4.32156749864851E+0_wp, 8.23391482357522E-1_wp, 2.68029791972147E+0_wp, &
      & 1.19256701474199E+0_wp]

      call get_structure(mol, "mindless03")

      allocate(rcov(mol%nid), en(mol%nid))
      rcov(:) = get_covalent_rad(mol%num)
      en(:) = get_pauling_en(mol%num)

      call new_erf_dftd4_ncoord(erf_dftd4_ncoord, mol, kcn=kcn, &
         & cutoff=cutoff, rcov=rcov, en=en)
      call test_cn_gen(error, mol, erf_dftd4_ncoord, ref)

   end subroutine test_cn_mb03_erf_dftd4


   subroutine test_cn_acetic_erf_dftd4(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      type(erf_dftd4_ncoord_type) :: erf_dftd4_ncoord
      real(wp), allocatable :: rcov(:)
      real(wp), allocatable :: en(:)

      real(wp), parameter :: cutoff = 30.0_wp
      real(wp), parameter :: ref(32) = [&
      & 8.57927937964499E-1_wp, 1.65578811302979E+0_wp, 8.57942519815764E-01_wp, &
      & 1.65577032315393E+0_wp, 1.65579750428472E+0_wp, 8.57976292566367E-01_wp, &
      & 8.57959806712906E-1_wp, 1.65580595790859E+0_wp, 2.68757316963446E+00_wp, &
      & 3.75387937835847E+0_wp, 2.68757320525778E+0_wp, 3.75388279785624E+00_wp, &
      & 3.75389467927099E+0_wp, 2.68757350196962E+0_wp, 2.68757458902025E+00_wp, &
      & 3.75387626586114E+0_wp, 9.24588078703746E-1_wp, 8.00786883779774E-01_wp, &
      & 9.25187896946826E-1_wp, 9.24714572053233E-1_wp, 9.24714022896347E-01_wp, &
      & 9.25191607736594E-1_wp, 9.24588475035834E-1_wp, 8.00785878187927E-01_wp, &
      & 9.25198590606433E-1_wp, 9.24591610342463E-1_wp, 8.00781165120561E-01_wp, &
      & 9.24717032271544E-1_wp, 9.25190368617526E-1_wp, 9.24586232339746E-01_wp, &
      & 9.24711233829591E-1_wp, 8.00775373869118E-1_wp]

      call get_structure(mol, "x02")

      allocate(rcov(mol%nid), en(mol%nid))
      rcov(:) = get_covalent_rad(mol%num)
      en(:) = get_pauling_en(mol%num)

      call new_erf_dftd4_ncoord(erf_dftd4_ncoord, mol, cutoff=cutoff, rcov=rcov, en=en)
      call test_cn_gen(error, mol, erf_dftd4_ncoord, ref)

   end subroutine test_cn_acetic_erf_dftd4


   subroutine test_dcndr_mb04_erf_dftd4(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      type(erf_dftd4_ncoord_type) :: erf_dftd4_ncoord
      real(wp), allocatable :: rcov(:)
      real(wp), allocatable :: en(:)

      real(wp), parameter :: cutoff = 30.0_wp

      call get_structure(mol, "mindless04")

      allocate(rcov(mol%nid), en(mol%nid))
      rcov(:) = get_covalent_rad(mol%num)
      en(:) = get_pauling_en(mol%num)

      call new_erf_dftd4_ncoord(erf_dftd4_ncoord, mol, cutoff=cutoff, rcov=rcov, en=en)
      call test_numgrad(error, mol, erf_dftd4_ncoord)

   end subroutine test_dcndr_mb04_erf_dftd4


   subroutine test_dcndr_mb05_erf_dftd4(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      type(erf_dftd4_ncoord_type) :: erf_dftd4_ncoord
      real(wp), allocatable :: rcov(:)
      real(wp), allocatable :: en(:)

      real(wp), parameter :: cutoff = 30.0_wp

      call get_structure(mol, "mindless05")

      allocate(rcov(mol%nid), en(mol%nid))
      rcov(:) = get_covalent_rad(mol%num)
      en(:) = get_pauling_en(mol%num)

      call new_erf_dftd4_ncoord(erf_dftd4_ncoord, mol, cutoff=cutoff, rcov=rcov, en=en)
      call test_numgrad(error, mol, erf_dftd4_ncoord)

   end subroutine test_dcndr_mb05_erf_dftd4


   subroutine test_dcndr_ammonia_erf_dftd4(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      type(erf_dftd4_ncoord_type) :: erf_dftd4_ncoord
      real(wp), allocatable :: rcov(:)
      real(wp), allocatable :: en(:)

      real(wp), parameter :: cutoff = 30.0_wp

      call get_structure(mol, "x04")

      allocate(rcov(mol%nid), en(mol%nid))
      rcov(:) = get_covalent_rad(mol%num)
      en(:) = get_pauling_en(mol%num)

      call new_erf_dftd4_ncoord(erf_dftd4_ncoord, mol, cutoff=cutoff, rcov=rcov, en=en)
      call test_numgrad(error, mol, erf_dftd4_ncoord)

   end subroutine test_dcndr_ammonia_erf_dftd4


   subroutine test_dcndL_mb06_erf_dftd4(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      type(erf_dftd4_ncoord_type) :: erf_dftd4_ncoord
      real(wp), allocatable :: rcov(:)
      real(wp), allocatable :: en(:)

      real(wp), parameter :: cutoff = 30.0_wp

      call get_structure(mol, "mindless06")

      allocate(rcov(mol%nid), en(mol%nid))
      rcov(:) = get_covalent_rad(mol%num)
      en(:) = get_pauling_en(mol%num)

      call new_erf_dftd4_ncoord(erf_dftd4_ncoord, mol, cutoff=cutoff, rcov=rcov, en=en)
      call test_numsigma(error, mol, erf_dftd4_ncoord)

   end subroutine test_dcndL_mb06_erf_dftd4


   subroutine test_dcndL_mb07_erf_dftd4(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      type(erf_dftd4_ncoord_type) :: erf_dftd4_ncoord
      real(wp), allocatable :: rcov(:)
      real(wp), allocatable :: en(:)

      real(wp), parameter :: cutoff = 30.0_wp

      call get_structure(mol, "mindless07")

      allocate(rcov(mol%nid), en(mol%nid))
      rcov(:) = get_covalent_rad(mol%num)
      en(:) = get_pauling_en(mol%num)

      call new_erf_dftd4_ncoord(erf_dftd4_ncoord, mol, cutoff=cutoff, rcov=rcov, en=en)
      call test_numsigma(error, mol, erf_dftd4_ncoord)

   end subroutine test_dcndL_mb07_erf_dftd4


   subroutine test_dcndL_anthracene_erf_dftd4(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      type(erf_dftd4_ncoord_type) :: erf_dftd4_ncoord
      real(wp), allocatable :: rcov(:)
      real(wp), allocatable :: en(:)

      real(wp), parameter :: cutoff = 30.0_wp

      call get_structure(mol, "x05")

      allocate(rcov(mol%nid), en(mol%nid))
      rcov(:) = get_covalent_rad(mol%num)
      en(:) = get_pauling_en(mol%num)

      call new_erf_dftd4_ncoord(erf_dftd4_ncoord, mol, cutoff=cutoff, rcov=rcov, en=en)
      call test_numsigma(error, mol, erf_dftd4_ncoord)

   end subroutine test_dcndL_anthracene_erf_dftd4


   subroutine test_cn_unknown(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      type(structure_type) :: mol
      class(ncoord_type), allocatable :: ncoord

      call get_structure(mol, "mindless01")

      ! Test also the external interface
      call new_ncoord(ncoord, mol, get_cn_count_id("unknown"), error)
      if(allocated(error)) return

   end subroutine test_cn_unknown


   subroutine test_cn_count_string_to_id(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error
   
      call check(error, get_cn_count_id("exp"), cn_count%exp)
      if (allocated(error)) return

      call check(error, get_cn_count_id("dexp"), cn_count%dexp)
      if (allocated(error)) return

      call check(error, get_cn_count_id("erf"), cn_count%erf)
      if (allocated(error)) return

      call check(error, get_cn_count_id("erf_en"), cn_count%erf_en)
      if (allocated(error)) return

      call check(error, get_cn_count_id("dftd4"), cn_count%dftd4)
      if (allocated(error)) return

      call check(error, get_cn_count_id("derf"), -1)
      if (allocated(error)) return


   end subroutine test_cn_count_string_to_id 

   
   subroutine test_cn_count_id_to_string(error)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error
   
      call check(error, get_cn_count_string(cn_count%exp), "exp")
      if (allocated(error)) return

      call check(error, get_cn_count_string(cn_count%dexp), "dexp")
      if (allocated(error)) return

      call check(error, get_cn_count_string(cn_count%erf), "erf")
      if (allocated(error)) return

      call check(error, get_cn_count_string(cn_count%erf_en), "erf_en")
      if (allocated(error)) return

      call check(error, get_cn_count_string(cn_count%dftd4), "dftd4")
      if (allocated(error)) return

      call check(error, get_cn_count_string(-1), "")
      if (allocated(error)) return

   end subroutine test_cn_count_id_to_string 


end module test_ncoord
