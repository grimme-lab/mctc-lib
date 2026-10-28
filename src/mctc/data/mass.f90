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

!> @file mctc/data/mass.f90
!> Provides atomic masses for all elements

!> Atomic masses of the elements, one value per element.
!>
!> Values are relative atomic masses (in Da) selected as follows:
!>
!> - Elements with a single-valued standard atomic weight use the
!>   CIAAW Standard Atomic Weights 2024 [1, 2], which include the 2024
!>   revision of Gd, Lu and Zr [3].
!> - The 14 elements whose standard atomic weight is given as an interval
!>   (H, Li, B, C, N, O, Mg, Si, S, Cl, Ar, Br, Tl, Pb) use the CIAAW
!>   abridged standard atomic weight 2024 [1, 4], which replaces the former
!>   conventional atomic weights [1].
!> - The 34 elements without a standard atomic weight (Tc, Pm, Po-Ac, Np-Og)
!>   use the atomic mass of their longest-lived isotope, taken to the full
!>   precision given in the AME2020 electronic data file [5].
!>   The isotope is the one with the longest half-life among the most stable
!>   known isotopes of radioactive elements compiled by CIAAW [6] from
!>   NUBASE2020 [7]. The isotope and its NUBASE2020 half-life are noted
!>   next to each value. Masses marked "mass est." are estimated from
!>   systematics (marked with # in [5]) rather than measured.
!>
!> Cases where the choice of the longest-lived isotope is not unambiguous:
!>
!> - Tc: 97Tc (4.21(16) Ma) and 98Tc (4.2(3) Ma) are equally stable within
!>   their uncertainties [6]; 97Tc has the longer recommended half-life and
!>   is the isotope given in the IUPAC Periodic Table [8].
!> - Mt: 277Mt (9(6) s) and 278Mt (6(3) s) are indistinguishable [6];
!>   277Mt has the longer recommended half-life and is given in [8].
!> - Lr: CIAAW [6] lists 262Lr (approx. 4 h) and 266Lr (22(14) h); the
!>   longer-lived 266Lr is used, while [8] and Table 2 of [1] give 262Lr.
!> - Ds: NUBASE2020 [7] lists 282Ds with 4.2(3.3) min, which is not included
!>   in the CIAAW compilation [6] or [8]; 281Ds (14(3) s) is used.
!> - Nuclear isomers (e.g. 277mHs, 293mLv in [7]) are not considered.
!>
!> Masses are returned in atomic units (electron masses) using the CODATA
!> 2018 electron mass in u.
!>
!> [1] T. Prohaska, J. Irrgeher, J. Benefield, J. K. Böhlke, L. A. Chesson,
!>     T. B. Coplen, T. Ding, P. J. H. Dunn, M. Gröning, N. E. Holden,
!>     H. A. J. Meijer, H. Moossen, A. Possolo, Y. Takahashi, J. Vogl,
!>     T. Walczyk, J. Wang, M. E. Wieser, S. Yoneda, X.-K. Zhu, J. Meija,
!>     Standard atomic weights of the elements 2021 (IUPAC Technical Report),
!>     Pure Appl. Chem. 94, 573-600 (2022), DOI: 10.1515/pac-2019-0603
!> [2] CIAAW, Standard Atomic Weights 2024,
!>     https://www.ciaaw.org/atomic-weights.htm
!> [3] Standard atomic weights of three technology critical elements revised,
!>     Chem. Int. 47, 20 (2025), DOI: 10.1515/ci-2025-0105
!> [4] CIAAW, Abridged Standard Atomic Weights 2024,
!>     https://www.ciaaw.org/abridged-atomic-weights.htm
!> [5] AME2020 atomic mass table, electronic file mass_1.mas20.txt
!>     (column "ATOMIC MASS" in micro-u), Atomic Mass Data Center,
!>     https://www-nds.iaea.org/amdc/ame2020/mass_1.mas20.txt;
!>     this file contains the unrounded version of the values published in
!>     M. Wang, W. J. Huang, F. G. Kondev, G. Audi, S. Naimi,
!>     The AME 2020 atomic mass evaluation (II). Tables, graphs and references,
!>     Chin. Phys. C 45, 030003 (2021), DOI: 10.1088/1674-1137/abddaf
!> [6] CIAAW, Radioactive Elements (last updated 29 Mar 2022),
!>     https://www.ciaaw.org/radioactive-elements.htm
!> [7] F. G. Kondev, M. Wang, W. J. Huang, S. Naimi, G. Audi,
!>     The NUBASE2020 evaluation of nuclear physics properties,
!>     Chin. Phys. C 45, 030001 (2021), DOI: 10.1088/1674-1137/abddae
!> [8] IUPAC Periodic Table of the Elements, version dated 4 May 2022,
!>     https://iupac.org/what-we-do/periodic-table-of-elements/
module mctc_data_mass
   use mctc_env, only : wp
   use mctc_io_codata2018, only : electron_mass_in_u
   use mctc_io_symbols, only : to_number
   implicit none
   private

   public :: get_atomic_mass


   !> Get atomic mass for a species
   interface get_atomic_mass
      module procedure :: get_atomic_mass_symbol
      module procedure :: get_atomic_mass_number
   end interface get_atomic_mass


   integer, parameter :: max_elem = 118

   !> Conversion from Dalton to atomic units (electron mass)
   real(wp), parameter :: datoau = 1.0_wp / electron_mass_in_u

   !> Atomic masses in atomic units, converted from Dalton
   real(wp), parameter :: atomic_masses(max_elem) = datoau * [ &
      & 1.0080_wp,          & ! H  interval [1.00784, 1.00811]
      & 4.002602_wp,        & ! He
      & 6.94_wp,            & ! Li interval [6.938, 6.997]
      & 9.0121831_wp,       & ! Be
      & 10.81_wp,           & ! B  interval [10.806, 10.821]
      & 12.011_wp,          & ! C  interval [12.0096, 12.0116]
      & 14.007_wp,          & ! N  interval [14.00643, 14.00728]
      & 15.999_wp,          & ! O  interval [15.99903, 15.99977]
      & 18.998403162_wp,    & ! F
      & 20.1797_wp,         & ! Ne
      & 22.98976928_wp,     & ! Na
      & 24.305_wp,          & ! Mg interval [24.304, 24.307]
      & 26.9815384_wp,      & ! Al
      & 28.085_wp,          & ! Si interval [28.084, 28.086]
      & 30.973761998_wp,    & ! P
      & 32.06_wp,           & ! S  interval [32.059, 32.076]
      & 35.45_wp,           & ! Cl interval [35.446, 35.457]
      & 39.95_wp,           & ! Ar interval [39.792, 39.963]
      & 39.0983_wp,         & ! K
      & 40.078_wp,          & ! Ca
      & 44.955907_wp,       & ! Sc
      & 47.867_wp,          & ! Ti
      & 50.9415_wp,         & ! V
      & 51.9961_wp,         & ! Cr
      & 54.938043_wp,       & ! Mn
      & 55.845_wp,          & ! Fe
      & 58.933194_wp,       & ! Co
      & 58.6934_wp,         & ! Ni
      & 63.546_wp,          & ! Cu
      & 65.38_wp,           & ! Zn
      & 69.723_wp,          & ! Ga
      & 72.630_wp,          & ! Ge
      & 74.921595_wp,       & ! As
      & 78.971_wp,          & ! Se
      & 79.904_wp,          & ! Br interval [79.901, 79.907]
      & 83.798_wp,          & ! Kr
      & 85.4678_wp,         & ! Rb
      & 87.62_wp,           & ! Sr
      & 88.905838_wp,       & ! Y
      & 91.222_wp,          & ! Zr
      & 92.90637_wp,        & ! Nb
      & 95.95_wp,           & ! Mo
      & 96.906360720_wp,    & ! Tc 97Tc, t1/2 = 4.21(16) Ma
      & 101.07_wp,          & ! Ru
      & 102.90549_wp,       & ! Rh
      & 106.42_wp,          & ! Pd
      & 107.8682_wp,        & ! Ag
      & 112.414_wp,         & ! Cd
      & 114.818_wp,         & ! In
      & 118.710_wp,         & ! Sn
      & 121.760_wp,         & ! Sb
      & 127.60_wp,          & ! Te
      & 126.90447_wp,       & ! I
      & 131.293_wp,         & ! Xe
      & 132.90545196_wp,    & ! Cs
      & 137.327_wp,         & ! Ba
      & 138.90547_wp,       & ! La
      & 140.116_wp,         & ! Ce
      & 140.90766_wp,       & ! Pr
      & 144.242_wp,         & ! Nd
      & 144.912755748_wp,   & ! Pm 145Pm, t1/2 = 17.7(4) a
      & 150.36_wp,          & ! Sm
      & 151.964_wp,         & ! Eu
      & 157.249_wp,         & ! Gd
      & 158.925354_wp,      & ! Tb
      & 162.500_wp,         & ! Dy
      & 164.930329_wp,      & ! Ho
      & 167.259_wp,         & ! Er
      & 168.934219_wp,      & ! Tm
      & 173.045_wp,         & ! Yb
      & 174.96669_wp,       & ! Lu
      & 178.486_wp,         & ! Hf
      & 180.94788_wp,       & ! Ta
      & 183.84_wp,          & ! W
      & 186.207_wp,         & ! Re
      & 190.23_wp,          & ! Os
      & 192.217_wp,         & ! Ir
      & 195.084_wp,         & ! Pt
      & 196.966570_wp,      & ! Au
      & 200.592_wp,         & ! Hg
      & 204.38_wp,          & ! Tl interval [204.382, 204.385]
      & 207.2_wp,           & ! Pb interval [206.14, 207.94]
      & 208.98040_wp,       & ! Bi
      & 208.982430361_wp,   & ! Po 209Po, t1/2 = 124(3) a
      & 209.987147423_wp,   & ! At 210At, t1/2 = 8.1(4) h
      & 222.017576017_wp,   & ! Rn 222Rn, t1/2 = 3.8215(2) d
      & 223.019734241_wp,   & ! Fr 223Fr, t1/2 = 22.00(7) min
      & 226.025408186_wp,   & ! Ra 226Ra, t1/2 = 1600(7) a
      & 227.027750594_wp,   & ! Ac 227Ac, t1/2 = 21.772(3) a
      & 232.0377_wp,        & ! Th
      & 231.03588_wp,       & ! Pa
      & 238.02891_wp,       & ! U
      & 237.048171640_wp,   & ! Np 237Np, t1/2 = 2144(7) ka
      & 244.064204401_wp,   & ! Pu 244Pu, t1/2 = 81.3(3) Ma
      & 243.061379889_wp,   & ! Am 243Am, t1/2 = 7.350(9) ka
      & 247.070352678_wp,   & ! Cm 247Cm, t1/2 = 15.6(5) Ma
      & 247.070305889_wp,   & ! Bk 247Bk, t1/2 = 1.38(25) ka
      & 251.079587171_wp,   & ! Cf 251Cf, t1/2 = 898(44) a
      & 252.082979173_wp,   & ! Es 252Es, t1/2 = 471.7(1.9) d
      & 257.095105419_wp,   & ! Fm 257Fm, t1/2 = 100.5(2) d
      & 258.098433634_wp,   & ! Md 258Md, t1/2 = 51.6(3) d
      & 259.100998364_wp,   & ! No 259No, t1/2 = 58(5) min
      & 266.119874_wp,      & ! Lr 266Lr, t1/2 = 22(14) h, mass est.
      & 267.121787_wp,      & ! Rf 267Rf, t1/2 = 2.5(1.5) h, mass est.
      & 268.125669_wp,      & ! Db 268Db, t1/2 = 29(3) h, mass est.
      & 269.128495_wp,      & ! Sg 269Sg, t1/2 = 5(2) min, mass est.
      & 270.133366_wp,      & ! Bh 270Bh, t1/2 = 3.8(3.0) min, mass est.
      & 269.133649_wp,      & ! Hs 269Hs, t1/2 = 15(7) s, mass est.
      & 277.153525_wp,      & ! Mt 277Mt, t1/2 = 9(6) s, mass est.
      & 281.164545_wp,      & ! Ds 281Ds, t1/2 = 14(3) s, mass est.
      & 282.169343_wp,      & ! Rg 282Rg, t1/2 = 130(50) s, mass est.
      & 285.177227_wp,      & ! Cn 285Cn, t1/2 = 30(8) s, mass est.
      & 286.182456_wp,      & ! Nh 286Nh, t1/2 = 12(5) s, mass est.
      & 290.191875_wp,      & ! Fl 290Fl, t1/2 = 80(60) s, mass est.
      & 290.196235_wp,      & ! Mc 290Mc, t1/2 = 840(360) ms, mass est.
      & 293.204583_wp,      & ! Lv 293Lv, t1/2 = 70(30) ms, mass est.
      & 294.210840_wp,      & ! Ts 294Ts, t1/2 = 70(30) ms, mass est.
      & 294.213979_wp       & ! Og 294Og, t1/2 = 0.7(3) ms, mass est.
      & ]

contains


!> Get atomic mass for species with a given symbol
elemental function get_atomic_mass_symbol(symbol) result(mass)

   !> Element symbol
   character(len=*), intent(in) :: symbol

   !> Atomic mass in atomic units
   real(wp) :: mass

   mass = get_atomic_mass(to_number(symbol))

end function get_atomic_mass_symbol


!> Get atomic mass for species with a given atomic number
elemental function get_atomic_mass_number(number) result(mass)

   !> Atomic number
   integer, intent(in) :: number

   !> Atomic mass in atomic units
   real(wp) :: mass

   if (number > 0 .and. number <= size(atomic_masses)) then
      mass = atomic_masses(number)
   else
      mass = 0.0_wp
   end if

end function get_atomic_mass_number


end module mctc_data_mass
