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

module test_read_pdb
   use mctc_env_accuracy, only : wp
   use mctc_env_testing, only : new_unittest, unittest_type, error_type, check
   use mctc_io_read_pdb
   use mctc_io_structure, only : structure_type
   implicit none
   private

   public :: collect_read_pdb


contains


!> Collect all exported unit tests
subroutine collect_read_pdb(testsuite)

   !> Collection of tests
   type(unittest_type), allocatable, intent(out) :: testsuite(:)

   testsuite = [ &
      & new_unittest("valid1-pdb", test_valid1_pdb), &
      & new_unittest("valid2-pdb", test_valid2_pdb), &
      & new_unittest("valid3-pdb", test_valid3_pdb), &
      & new_unittest("valid4-pdb", test_valid4_pdb), &
      & new_unittest("invalid1-pdb", test_invalid1_pdb, should_fail=.true.), &
      & new_unittest("invalid2-pdb", test_invalid2_pdb, should_fail=.true.), &
      & new_unittest("invalid3-pdb", test_invalid3_pdb, should_fail=.true.) &
      & ]

end subroutine collect_read_pdb


subroutine test_valid1_pdb(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "ATOM      1  N   GLY Z   1      -0.821  -2.072  16.609  1.00  9.93           N1+", &
      "ATOM      2  CA  GLY Z   1      -1.705  -2.345  15.487  1.00  7.38           C", &
      "ATOM      3  C   GLY Z   1      -0.968  -3.008  14.344  1.00  4.89           C", &
      "ATOM      4  O   GLY Z   1       0.258  -2.982  14.292  1.00  5.05           O", &
      "ATOM      5  HA2 GLY Z   1      -2.130  -1.405  15.135  1.00  0.00           H", &
      "ATOM      6  HA3 GLY Z   1      -2.511  -2.999  15.819  1.00  0.00           H", &
      "ATOM      7  H1  GLY Z   1      -1.364  -1.742  17.394  1.00  0.00           H", &
      "ATOM      8  H2  GLY Z   1      -0.150  -1.365  16.344  1.00  0.00           H", &
      "ATOM      9  H3  GLY Z   1      -0.334  -2.918  16.868  1.00  0.00           H", &
      "ATOM     10  N   ASN Z   2      -1.721  -3.603  13.425  1.00  3.53           N", &
      "ATOM     11  CA  ASN Z   2      -1.141  -4.323  12.291  1.00  1.85           C", &
      "ATOM     12  C   ASN Z   2      -1.748  -3.900  10.968  1.00  3.00           C", &
      "ATOM     13  O   ASN Z   2      -2.955  -3.683  10.873  1.00  3.99           O", &
      "ATOM     14  CB  ASN Z   2      -1.353  -5.827  12.446  1.00  5.03           C", &
      "ATOM     15  CG  ASN Z   2      -0.679  -6.391  13.683  1.00  5.08           C", &
      "ATOM     16  OD1 ASN Z   2       0.519  -6.202  13.896  1.00  6.10           O", &
      "ATOM     17  ND2 ASN Z   2      -1.448  -7.087  14.506  1.00  8.41           N", &
      "ATOM     18  H   ASN Z   2      -2.726  -3.557  13.512  1.00  0.00           H", &
      "ATOM     19  HA  ASN Z   2      -0.070  -4.123  12.263  1.00  0.00           H", &
      "ATOM     20  HB2 ASN Z   2      -0.945  -6.328  11.568  1.00  0.00           H", &
      "ATOM     21  HB3 ASN Z   2      -2.423  -6.029  12.503  1.00  0.00           H", &
      "ATOM     22 HD21 ASN Z   2      -2.427  -7.218  14.293  1.00  0.00           H", &
      "ATOM     23 HD22 ASN Z   2      -1.056  -7.487  15.346  1.00  0.00           H", &
      "ATOM     24  N   LEU Z   3      -0.907  -3.803   9.944  1.00  3.47           N", &
      "ATOM     25  CA  LEU Z   3      -1.388  -3.576   8.586  1.00  3.48           C", &
      "ATOM     26  C   LEU Z   3      -0.783  -4.660   7.709  1.00  3.29           C", &
      "ATOM     27  O   LEU Z   3       0.437  -4.788   7.643  1.00  3.80           O", &
      "ATOM     28  CB  LEU Z   3      -0.977  -2.185   8.081  1.00  3.88           C", &
      "ATOM     29  CG  LEU Z   3      -1.524  -1.669   6.736  1.00  8.66           C", &
      "ATOM     30  CD1 LEU Z   3      -1.225  -0.191   6.570  1.00  9.89           C", &
      "ATOM     31  CD2 LEU Z   3      -0.962  -2.409   5.541  1.00 13.56           C", &
      "ATOM     32  H   LEU Z   3       0.086  -3.888  10.109  1.00  0.00           H", &
      "ATOM     33  HA  LEU Z   3      -2.475  -3.661   8.568  1.00  0.00           H", &
      "ATOM     34  HB2 LEU Z   3      -1.284  -1.469   8.843  1.00  0.00           H", &
      "ATOM     35  HB3 LEU Z   3       0.111  -2.162   8.026  1.00  0.00           H", &
      "ATOM     36  HG  LEU Z   3      -2.606  -1.798   6.737  1.00  0.00           H", &
      "ATOM     37 HD11 LEU Z   3      -1.623   0.359   7.423  1.00  0.00           H", &
      "ATOM     38 HD12 LEU Z   3      -1.691   0.173   5.654  1.00  0.00           H", &
      "ATOM     39 HD13 LEU Z   3      -0.147  -0.043   6.513  1.00  0.00           H", &
      "ATOM     40 HD21 LEU Z   3      -1.168  -3.475   5.643  1.00  0.00           H", &
      "ATOM     41 HD22 LEU Z   3      -1.429  -2.035   4.630  1.00  0.00           H", &
      "ATOM     42 HD23 LEU Z   3       0.115  -2.250   5.489  1.00  0.00           H", &
      "ATOM     43  N   VAL Z   4      -1.635  -5.424   7.029  1.00  3.17           N", &
      "ATOM     44  CA  VAL Z   4      -1.165  -6.460   6.119  1.00  3.61           C", &
      "ATOM     45  C   VAL Z   4      -1.791  -6.230   4.755  1.00  5.31           C", &
      "ATOM     46  O   VAL Z   4      -3.014  -6.209   4.620  1.00  7.31           O", &
      "ATOM     47  CB  VAL Z   4      -1.567  -7.872   6.593  1.00  5.31           C", &
      "ATOM     48  CG1 VAL Z   4      -1.012  -8.934   5.633  1.00  6.73           C", &
      "ATOM     49  CG2 VAL Z   4      -1.083  -8.120   8.018  1.00  5.48           C", &
      "ATOM     50  H   VAL Z   4      -2.628  -5.282   7.146  1.00  0.00           H", &
      "ATOM     51  HA  VAL Z   4      -0.080  -6.402   6.034  1.00  0.00           H", &
      "ATOM     52  HB  VAL Z   4      -2.655  -7.939   6.585  1.00  0.00           H", &
      "ATOM     53 HG11 VAL Z   4      -1.303  -9.926   5.980  1.00  0.00           H", &
      "ATOM     54 HG12 VAL Z   4      -1.414  -8.766   4.634  1.00  0.00           H", &
      "ATOM     55 HG13 VAL Z   4       0.075  -8.864   5.603  1.00  0.00           H", &
      "ATOM     56 HG21 VAL Z   4      -1.377  -9.121   8.333  1.00  0.00           H", &
      "ATOM     57 HG22 VAL Z   4       0.003  -8.032   8.053  1.00  0.00           H", &
      "ATOM     58 HG23 VAL Z   4      -1.529  -7.383   8.686  1.00  0.00           H", &
      "ATOM     59  N   SER Z   5      -0.966  -6.052   3.736  1.00  7.53           N", &
      "ATOM     60  CA  SER Z   5      -1.526  -5.888   2.407  1.00 11.48           C", &
      "ATOM     61  C   SER Z   5      -1.207  -7.085   1.529  1.00 16.35           C", &
      "ATOM     62  O   SER Z   5      -0.437  -7.976   1.902  1.00 14.00           O", &
      "ATOM     63  CB  SER Z   5      -1.031  -4.596   1.767  1.00 13.36           C", &
      "ATOM     64  OG  SER Z   5       0.361  -4.652   1.540  1.00 15.80           O", &
      "ATOM     65  OXT SER Z   5      -1.737  -7.178   0.429  1.00 17.09           O1-", &
      "ATOM     66  H   SER Z   5       0.033  -6.031   3.880  1.00  0.00           H", &
      "ATOM     67  HA  SER Z   5      -2.610  -5.822   2.504  1.00  0.00           H", &
      "ATOM     68  HB2 SER Z   5      -1.543  -4.449   0.816  1.00  0.00           H", &
      "ATOM     69  HB3 SER Z   5      -1.254  -3.759   2.428  1.00  0.00           H", &
      "ATOM     70  HG  SER Z   5       0.653  -3.831   1.137  1.00  0.00           H", &
      "TER      71      SER Z   5", &
      "HETATM   72  O   HOH Z 101       0.935  -5.175  16.502  1.00 18.83           O", &
      "HETATM   73  H1  HOH Z 101       0.794  -5.522  15.621  1.00  0.00           H", &
      "HETATM   74  H2  HOH Z 101       1.669  -4.561  16.489  1.00  0.00           H", &
      "HETATM   75  O   HOH Z 102       0.691  -8.408  17.879  0.91 56.55           O", &
      "HETATM   76  H1  HOH Z 102       1.392  -8.125  18.466  0.91  0.00           H", &
      "HETATM   77  H2  HOH Z 102       0.993  -8.356  16.972  0.91  0.00           H", &
      "END"
   rewind(unit)

   call read_pdb(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, struc%nat, 76, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, 4, "Number of species does not match")
   if (allocated(error)) return
   call check(error, struc%charge, 0.0_wp, "Total charge is not correct")
   if (allocated(error)) return

end subroutine test_valid1_pdb


subroutine test_valid2_pdb(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "HETATM 2463  CHA HEM A 154       9.596 -13.100  10.368  1.00  0.00           C", &
      "HETATM 2464  CHB HEM A 154      11.541 -10.200   7.336  1.00  0.00           C", &
      "HETATM 2465  CHC HEM A 154       9.504  -6.500   9.390  1.00  0.00           C", &
      "HETATM 2466  CHD HEM A 154       7.260  -9.300  12.422  1.00  0.00           C", &
      "HETATM 2467  C1A HEM A 154      10.383 -12.600   9.488  1.00  0.00           C", &
      "HETATM 2468  C2A HEM A 154      10.970 -13.500   8.607  1.00  0.00           C", &
      "HETATM 2469  C3A HEM A 154      11.537 -12.600   7.825  1.00  0.00           C", &
      "HETATM 2470  C4A HEM A 154      11.295 -11.300   8.020  1.00  0.00           C", &
      "HETATM 2471  CMA HEM A 154      12.628 -13.100   6.455  1.00  0.00           C", &
      "HETATM 2472  CAA HEM A 154      11.250 -15.000   8.705  1.00  0.00           C", &
      "HETATM 2473  CBA HEM A 154       9.870 -15.600   8.607  1.00  0.00           C", &
      "HETATM 2474  CGA HEM A 154       8.899 -14.700   7.531  1.00  0.00           C", &
      "HETATM 2475  O1A HEM A 154       8.337 -14.400   7.825  1.00  0.00           O", &
      "HETATM 2476  O2A HEM A 154       9.062 -14.700   7.238  1.00  0.00           O", &
      "HETATM 2477  C1B HEM A 154      11.178  -8.900   7.629  1.00  0.00           C", &
      "HETATM 2478  C2B HEM A 154      11.745  -7.800   6.847  1.00  0.00           C", &
      "HETATM 2479  C3B HEM A 154      11.020  -6.800   7.434  1.00  0.00           C", &
      "HETATM 2480  C4B HEM A 154      10.370  -7.200   8.607  1.00  0.00           C", &
      "HETATM 2481  CMB HEM A 154      12.615  -7.800   5.575  1.00  0.00           C", &
      "HETATM 2482  CAB HEM A 154      11.203  -5.300   7.042  1.00  0.00           C", &
      "HETATM 2483  CBB HEM A 154      11.911  -4.800   6.064  1.00  0.00           C", &
      "HETATM 2484  C1C HEM A 154       8.817  -6.900  10.270  1.00  0.00           C", &
      "HETATM 2485  C2C HEM A 154       8.130  -6.100  11.150  1.00  0.00           C", &
      "HETATM 2486  C3C HEM A 154       7.543  -6.900  12.031  1.00  0.00           C", &
      "HETATM 2487  C4C HEM A 154       7.805  -8.200  11.737  1.00  0.00           C", &
      "HETATM 2488  CMC HEM A 154       8.051  -4.600  11.053  1.00  0.00           C", &
      "HETATM 2489  CAC HEM A 154       6.414  -6.500  13.107  1.00  0.00           C", &
      "HETATM 2490  CBC HEM A 154       6.193  -5.100  13.204  1.00  0.00           C", &
      "HETATM 2491  C1D HEM A 154       7.843 -10.600  12.031  1.00  0.00           C", &
      "HETATM 2492  C2D HEM A 154       7.256 -11.700  12.911  1.00  0.00           C", &
      "HETATM 2493  C3D HEM A 154       8.101 -12.800  12.226  1.00  0.00           C", &
      "HETATM 2494  C4D HEM A 154       8.809 -12.300  11.248  1.00  0.00           C", &
      "HETATM 2495  CMD HEM A 154       6.427 -11.800  13.987  1.00  0.00           C", &
      "HETATM 2496  CAD HEM A 154       7.897 -14.200  12.715  1.00  0.00           C", &
      "HETATM 2497  CBD HEM A 154       8.085 -14.200  14.182  1.00  0.00           C", &
      "HETATM 2498  CGD HEM A 154       9.023 -15.500  14.476  1.00  0.00           C", &
      "HETATM 2499  O1D HEM A 154       8.898 -15.800  15.063  1.00  0.00           O", &
      "HETATM 2500  O2D HEM A 154       9.527 -15.600  13.987  1.00  0.00           O", &
      "HETATM 2501  NA  HEM A 154      10.487 -11.300   8.999  1.00  0.00           N", &
      "HETATM 2502  NB  HEM A 154      10.570  -8.600   8.607  1.00  0.00           N", &
      "HETATM 2503  NC  HEM A 154       8.613  -8.200  10.759  1.00  0.00           N", &
      "HETATM 2504  ND  HEM A 154       8.709 -10.900  11.248  1.00  0.00           N", &
      "HETATM 2505 FE   HEM A 154       9.621  -9.800   9.781  1.00  0.00          Fe", &
      "HETATM 2506  HHA HEM A 154       9.526 -14.175  10.446  1.00  0.00           H", &
      "HETATM 2507  HHB HEM A 154      12.102 -10.334   6.423  1.00  0.00           H", &
      "HETATM 2508  HHC HEM A 154       9.433  -5.442   9.183  1.00  0.00           H", &
      "HETATM 2509  HHD HEM A 154       6.484  -9.203  13.167  1.00  0.00           H", &
      "HETATM 2510 HAA2 HEM A 154      11.721 -15.251   9.655  1.00  0.00           H", &
      "HETATM 2511 HAA3 HEM A 154      11.871 -15.329   7.871  1.00  0.00           H", &
      "HETATM 2512 HBA2 HEM A 154       9.950 -16.625   8.245  1.00  0.00           H", &
      "HETATM 2513 HBA3 HEM A 154       9.407 -15.602   9.594  1.00  0.00           H", &
      "HETATM 2514  HAB HEM A 154      10.678  -4.585   7.657  1.00  0.00           H", &
      "HETATM 2515 HAC2 HEM A 154       5.478  -6.978  12.818  1.00  0.00           H", &
      "HETATM 2516 HAC3 HEM A 154       6.713  -6.877  14.085  1.00  0.00           H", &
      "HETATM 2517 HAD2 HEM A 154       6.889 -14.534  12.471  1.00  0.00           H", &
      "HETATM 2518 HAD3 HEM A 154       8.627 -14.862  12.250  1.00  0.00           H", &
      "HETATM 2519 HBD2 HEM A 154       8.582 -13.286  14.506  1.00  0.00           H", &
      "HETATM 2520 HBD3 HEM A 154       7.124 -14.306  14.686  1.00  0.00           H", &
      "HETATM 2521 HBB1 HEM A 154      12.463  -5.456   5.407  1.00  0.00           H", &
      "HETATM 2522 HBB2 HEM A 154      11.944  -3.731   5.913  1.00  0.00           H", &
      "HETATM 2523 HMD1 HEM A 154       5.952 -10.917  14.388  1.00  0.00           H", &
      "HETATM 2524 HMD2 HEM A 154       6.244 -12.763  14.439  1.00  0.00           H", &
      "HETATM 2525 HMA1 HEM A 154      12.066 -13.093   5.521  1.00  0.00           H", &
      "HETATM 2526 HMA2 HEM A 154      13.462 -12.402   6.382  1.00  0.00           H", &
      "HETATM 2527 HMA3 HEM A 154      13.009 -14.104   6.642  1.00  0.00           H", &
      "HETATM 2528 HMB1 HEM A 154      13.413  -7.065   5.680  1.00  0.00           H", &
      "HETATM 2529 HMB2 HEM A 154      11.998  -7.545   4.713  1.00  0.00           H", &
      "HETATM 2530 HMB3 HEM A 154      13.048  -8.790   5.431  1.00  0.00           H", &
      "HETATM 2531 HMC1 HEM A 154       8.716  -4.251  10.263  1.00  0.00           H", &
      "HETATM 2532 HMC2 HEM A 154       8.353  -4.158  12.003  1.00  0.00           H", &
      "HETATM 2533 HMC3 HEM A 154       7.027  -4.304  10.824  1.00  0.00           H", &
      "HETATM 2534 HBC1 HEM A 154       5.472  -4.900  13.996  1.00  0.00           H", &
      "HETATM 2535 HBC2 HEM A 154       5.804  -4.727  12.256  1.00  0.00           H", &
      "HETATM 2536 HBC3 HEM A 154       7.133  -4.599  13.434  1.00  0.00           H", &
      "END"
   rewind(unit)

   call read_pdb(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, struc%nat, 74, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, 5, "Number of species does not match")
   if (allocated(error)) return
   call check(error, struc%charge, 0.0_wp, "Total charge is not correct")
   if (allocated(error)) return

end subroutine test_valid2_pdb


subroutine test_valid3_pdb(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "ATOM      1  N   PCA A   1      -0.169 -16.525  -1.918  1.00 39.42      9WGA 334", &
      "ATOM      2  CA  PCA A   1      -1.347 -16.464  -2.767  1.00 38.75      9WGA 335", &
      "ATOM      3  C   PCA A   1      -2.629 -16.172  -2.059  1.00 36.17      9WGA 336", &
      "ATOM      4  O   PCA A   1      -3.696 -16.128  -2.783  1.00 37.04      9WGA 337", &
      "ATOM      5  CB  PCA A   1      -1.231 -17.791  -3.531  1.00 39.80      9WGA 338", &
      "ATOM      6  CG  PCA A   1      -0.711 -18.699  -2.396  1.00 40.00      9WGA 339", &
      "ATOM      7  CD  PCA A   1       0.259 -17.781  -1.710  1.00 40.78      9WGA 340", &
      "ATOM      8  OE  PCA A   1       1.275 -18.105  -1.083  1.00 41.14      9WGA 341", &
      "ATOM      9 2H   PCA A   1       0.636 -16.724  -2.477  1.00 39.42      9WGA H + new", &
      "ATOM     10  HA  PCA A   1      -1.381 -15.601  -3.448  1.00 38.75      9WGA H   new", &
      "ATOM     11 1HB  PCA A   1      -2.196 -18.131  -3.935  1.00 39.80      9WGA H   new", &
      "ATOM     12 2HB  PCA A   1      -0.531 -17.731  -4.378  1.00 39.80      9WGA H   new", &
      "ATOM     13 1HG  PCA A   1      -1.518 -19.028  -1.724  1.00 40.00      9WGA H   new", &
      "ATOM     14 2HG  PCA A   1      -0.223 -19.607  -2.781  1.00 40.00      9WGA H   new", &
      "TER      15      PCA A   1                                              9WGA1493", &
      "END                                                                     9WGA3299"
   rewind(unit)

   call read_pdb(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, struc%nat, 14, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, 4, "Number of species does not match")
   if (allocated(error)) return
   call check(error, struc%charge, 0.0_wp, "Total charge is not correct")
   if (allocated(error)) return

end subroutine test_valid3_pdb


subroutine test_valid4_pdb(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "ATOM      1  N   GLY Z   1      -0.821  -2.072  16.609  1.00  9.93           N1+", &
      "ANISOU    1  N   GLY Z   1     1184   1952    638    314   -191   -326       N", &
      "ATOM      2  CA  GLY Z   1      -1.705  -2.345  15.487  1.00  7.38           C", &
      "ANISOU    2  CA  GLY Z   1      957   1374    472    279   -124   -261       C", &
      "ATOM      3  C   GLY Z   1      -0.968  -3.008  14.344  1.00  4.89           C", &
      "ANISOU    3  C   GLY Z   1      899    614    343    211    112   -106       C", &
      "ATOM      4  O   GLY Z   1       0.258  -2.982  14.292  1.00  5.05           O", &
      "ANISOU    4  O   GLY Z   1      839    595    485    -11     -7   -180       O", &
      "ATOM      5  HA2 GLY Z   1      -2.130  -1.405  15.135  1.00  0.00           H", &
      "ATOM      6  HA3 GLY Z   1      -2.511  -2.999  15.819  1.00  0.00           H", &
      "ATOM      7  H1  GLY Z   1      -1.364  -1.742  17.394  1.00  0.00           H", &
      "ATOM      8  H2  GLY Z   1      -0.150  -1.365  16.344  1.00  0.00           H", &
      "ATOM      9  H3  GLY Z   1      -0.334  -2.918  16.868  1.00  0.00           H", &
      "ATOM     10  H   GLY Z   1      -1.141  -4.323  12.291  1.00  1.85           H", &
      "TER      11      GLY Z   1", &
      "END"
   rewind(unit)

   call read_pdb(struc, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, struc%nat, 10, "Number of atoms does not match")
   if (allocated(error)) return
   call check(error, struc%nid, 4, "Number of species does not match")
   if (allocated(error)) return
   call check(error, struc%charge, 1.0_wp, "Total charge is not correct")
   if (allocated(error)) return

end subroutine test_valid4_pdb


subroutine test_invalid1_pdb(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "ATOM      1  N   GLY Z   1      -0.821  -2.072  16.609  1.00  9.93           N1+", &
      "ATOM      2  CA  GLY Z   1      -1.705  -2.345  15.487  1.00  7.38           C", &
      "ATOM      3  C   GLY Z   1      -0.968  -3.008  14.344  1.00  4.89           C", &
      "ATOM      4  O   GLY Z   1       0.258  -2.982  14.292  1.00  5.05           O", &
      "ATOM      5  HA2 GLY Z   1      -2.130  -1.405  15.135  1.00  0.00           H", &
      "ATOM      6  HA3 GLY Z   1      -2.511  -2.999  15.819  1.00  0.00           H", &
      "ATOM      7  H1  GLY Z   1      -1.364  -1.742  17.394  1.00  0.00           H", &
      "ATOM      8  H2  GLY Z   1      -0.150  -1.365  16.344  1.00  0.00           H", &
      "ATOM      9  H3  GLY Z   1      -0.334  -2.918  16.868  1.00  0.00           H", &
      "ATOM     10  N   ASN Z   2      -1.721  -3.603  13.425  1.00  3.53           N", &
      "ATOM     11  CA  ASN Z   2      -1.141  -4.323  12.291  1.00  1.85           C", &
      "ATOM     12  C   ASN Z   2      -1.748  -3.900  10.968  1.00  3.00           C", &
      "ATOM     13  O   ASN Z   2      -2.955  -3.683  10.873  1.00  3.99           O", &
      "ATOM     14  CB  ASN Z   2      -1.353  -5.827  12.446  1.00  5.03           C", &
      "ATOM     15  CG  ASN Z   2      -0.679  -6.391  13.683  1.00  5.08           C", &
      "ATOM     16  OD1 ASN Z   2       0.519  -6.202  13.896  1.00  6.10           O", &
      "ATOM     17  ND2 ASN Z   2      -1.448  -7.087  14.506  1.00  8.41           N", &
      "ATOM     18  H   ASN Z   2      -2.726  -3.557  13.512  1.00  0.00           H", &
      "ATOM     19  HA  ASN Z   2      -0.070  -4.123  12.263  1.00  0.00           H", &
      "ATOM     20  HB2 ASN Z   2      -0.945  -6.328  11.568  1.00  0.00           H", &
      "ATOM     21  HB3 ASN Z   2       a.bcd  -6.029  12.503  1.00  0.00           H", &
      "ATOM     22 HD21 ASN Z   2      -2.427  -7.218  14.293  1.00  0.00           H", &
      "ATOM     23 HD22 ASN Z   2      -1.056  -7.487  15.346  1.00  0.00           H", &
      "ATOM     24  N   LEU Z   3      -0.907  -3.803   9.944  1.00  3.47           N", &
      "ATOM     25  CA  LEU Z   3      -1.388  -3.576   8.586  1.00  3.48           C", &
      "ATOM     26  C   LEU Z   3      -0.783  -4.660   7.709  1.00  3.29           C", &
      "ATOM     27  O   LEU Z   3       0.437  -4.788   7.643  1.00  3.80           O", &
      "ATOM     28  CB  LEU Z   3      -0.977  -2.185   8.081  1.00  3.88           C", &
      "ATOM     29  CG  LEU Z   3      -1.524  -1.669   6.736  1.00  8.66           C", &
      "ATOM     30  CD1 LEU Z   3      -1.225  -0.191   6.570  1.00  9.89           C", &
      "ATOM     31  CD2 LEU Z   3      -0.962  -2.409   5.541  1.00 13.56           C", &
      "ATOM     32  H   LEU Z   3       0.086  -3.888  10.109  1.00  0.00           H", &
      "ATOM     33  HA  LEU Z   3      -2.475  -3.661   8.568  1.00  0.00           H", &
      "ATOM     34  HB2 LEU Z   3      -1.284  -1.469   8.843  1.00  0.00           H", &
      "ATOM     35  HB3 LEU Z   3       0.111  -2.162   8.026  1.00  0.00           H", &
      "ATOM     36  HG  LEU Z   3      -2.606  -1.798   6.737  1.00  0.00           H", &
      "ATOM     37 HD11 LEU Z   3      -1.623   0.359   7.423  1.00  0.00           H", &
      "ATOM     38 HD12 LEU Z   3      -1.691   0.173   5.654  1.00  0.00           H", &
      "ATOM     39 HD13 LEU Z   3      -0.147  -0.043   6.513  1.00  0.00           H", &
      "ATOM     40 HD21 LEU Z   3      -1.168  -3.475   5.643  1.00  0.00           H", &
      "ATOM     41 HD22 LEU Z   3      -1.429  -2.035   4.630  1.00  0.00           H", &
      "ATOM     42 HD23 LEU Z   3       0.115  -2.250   5.489  1.00  0.00           H", &
      "ATOM     43  N   VAL Z   4      -1.635  -5.424   7.029  1.00  3.17           N", &
      "ATOM     44  CA  VAL Z   4      -1.165  -6.460   6.119  1.00  3.61           C", &
      "ATOM     45  C   VAL Z   4      -1.791  -6.230   4.755  1.00  5.31           C", &
      "ATOM     46  O   VAL Z   4      -3.014  -6.209   4.620  1.00  7.31           O", &
      "ATOM     47  CB  VAL Z   4      -1.567  -7.872   6.593  1.00  5.31           C", &
      "ATOM     48  CG1 VAL Z   4      -1.012  -8.934   5.633  1.00  6.73           C", &
      "ATOM     49  CG2 VAL Z   4      -1.083  -8.120   8.018  1.00  5.48           C", &
      "ATOM     50  H   VAL Z   4      -2.628  -5.282   7.146  1.00  0.00           H", &
      "ATOM     51  HA  VAL Z   4      -0.080  -6.402   6.034  1.00  0.00           H", &
      "ATOM     52  HB  VAL Z   4      -2.655  -7.939   6.585  1.00  0.00           H", &
      "ATOM     53 HG11 VAL Z   4      -1.303  -9.926   5.980  1.00  0.00           H", &
      "ATOM     54 HG12 VAL Z   4      -1.414  -8.766   4.634  1.00  0.00           H", &
      "ATOM     55 HG13 VAL Z   4       0.075  -8.864   5.603  1.00  0.00           H", &
      "ATOM     56 HG21 VAL Z   4      -1.377  -9.121   8.333  1.00  0.00           H", &
      "ATOM     57 HG22 VAL Z   4       0.003  -8.032   8.053  1.00  0.00           H", &
      "ATOM     58 HG23 VAL Z   4      -1.529  -7.383   8.686  1.00  0.00           H", &
      "ATOM     59  N   SER Z   5      -0.966  -6.052   3.736  1.00  7.53           N", &
      "ATOM     60  CA  SER Z   5      -1.526  -5.888   2.407  1.00 11.48           C", &
      "ATOM     61  C   SER Z   5      -1.207  -7.085   1.529  1.00 16.35           C", &
      "ATOM     62  O   SER Z   5      -0.437  -7.976   1.902  1.00 14.00           O", &
      "ATOM     63  CB  SER Z   5      -1.031  -4.596   1.767  1.00 13.36           C", &
      "ATOM     64  OG  SER Z   5       0.361  -4.652   1.540  1.00 15.80           O", &
      "ATOM     65  OXT SER Z   5      -1.737  -7.178   0.429  1.00 17.09           O1-", &
      "ATOM     66  H   SER Z   5       0.033  -6.031   3.880  1.00  0.00           H", &
      "ATOM     67  HA  SER Z   5      -2.610  -5.822   2.504  1.00  0.00           H", &
      "ATOM     68  HB2 SER Z   5      -1.543  -4.449   0.816  1.00  0.00           H", &
      "ATOM     69  HB3 SER Z   5      -1.254  -3.759   2.428  1.00  0.00           H", &
      "ATOM     70  HG  SER Z   5       0.653  -3.831   1.137  1.00  0.00           H", &
      "TER      71      SER Z   5", &
      "HETATM   72  O   HOH Z 101       0.935  -5.175  16.502  1.00 18.83           O", &
      "HETATM   73  H1  HOH Z 101       0.794  -5.522  15.621  1.00  0.00           H", &
      "HETATM   74  H2  HOH Z 101       1.669  -4.561  16.489  1.00  0.00           H", &
      "HETATM   75  O   HOH Z 102       0.691  -8.408  17.879  0.91 56.55           O", &
      "HETATM   76  H1  HOH Z 102       1.392  -8.125  18.466  0.91  0.00           H", &
      "HETATM   77  H2  HOH Z 102       0.993  -8.356  16.972  0.91  0.00           H", &
      "END"
   rewind(unit)

   call read_pdb(struc, unit, error)
   close(unit)

end subroutine test_invalid1_pdb


subroutine test_invalid2_pdb(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "HETATM 2463  CHA HEM A 154       9.596 -13.100  10.368  1.00  0.00           C", &
      "HETATM 2464  CHB HEM A 154      11.541 -10.200   7.336  1.00  0.00           C", &
      "HETATM 2465  CHC HEM A 154       9.504  -6.500   9.390  1.00  0.00           C", &
      "HETATM 2466  CHD HEM A 154       7.260  -9.300  12.422  1.00  0.00           C", &
      "HETATM 2467  C1A HEM A 154      10.383 -12.600   9.488  1.00  0.00           C", &
      "HETATM 2468  C2A HEM A 154      10.970 -13.500   8.607  1.00  0.00           C", &
      "HETATM 2469  C3A HEM A 154      11.537 -12.600   7.825  1.00  0.00           C", &
      "HETATM 2470  C4A HEM A 154      11.295 -11.300   8.020  1.00  0.00           C", &
      "HETATM 2471  CMA HEM A 154      12.628 -13.100   6.455  1.00  0.00           C", &
      "HETATM 2472  CAA HEM A 154      11.250 -15.000   8.705  1.00  0.00           C", &
      "HETATM 2473  CBA HEM A 154       9.870 -15.600   8.607  1.00  0.00           C", &
      "HETATM 2474  CGA HEM A 154       8.899 -14.700   7.531  1.00  0.00           C", &
      "HETATM 2475  O1A HEM A 154       8.337 -14.400   7.825  1.00  0.00           O", &
      "HETATM 2476  O2A HEM A 154       9.062 -14.700   7.238  1.00  0.00           O", &
      "HETATM 2477  C1B HEM A 154      11.178  -8.900   7.629  1.00  0.00           C", &
      "HETATM 2478  C2B HEM A 154      11.745  -7.800   6.847  1.00  0.00           C", &
      "HETATM 2479  C3B HEM A 154      11.020  -6.800   7.434  1.00  0.00           C", &
      "HETATM 2480  C4B HEM A 154      10.370  -7.200   8.607  1.00  0.00           C", &
      "HETATM 2481  CMB HEM A 154      12.615  -7.800   5.575  1.00  0.00           C", &
      "HETATM 2482  CAB HEM A 154      11.203  -5.300   7.042  1.00  0.00           C", &
      "HETATM 2483  CBB HEM A 154      11.911  -4.800   6.064  1.00  0.00           C", &
      "HETATM 2484  C1C HEM A 154       8.817  -6.900  10.270  1.00  0.00           C", &
      "HETATM 2485  C2C HEM A 154       8.130  -6.100  11.150  1.00  0.00           C", &
      "HETATM 2486  C3C HEM A 154       7.543  -6.900  12.031  1.00  0.00           C", &
      "HETATM 2487  C4C HEM A 154       7.805  -8.200  11.737  1.00  0.00           C", &
      "HETATM 2488  CMC HEM A 154       8.051  -4.600  11.053  1.00  0.00           C", &
      "HETATM 2489  CAC HEM A 154       6.414  -6.500  13.107  1.00  0.00           C", &
      "HETATM 2490  CBC HEM A 154       6.193  -5.100  13.204  1.00  0.00           C", &
      "HETATM 2491  C1D HEM A 154       7.843 -10.600  12.031  1.00  0.00           C", &
      "HETATM 2492  C2D HEM A 154       7.256 -11.700  12.911  1.00  0.00           C", &
      "HETATM 2493  C3D HEM A 154       8.101 -12.800  12.226  1.00  0.00           C", &
      "HETATM 2494  C4D HEM A 154       8.809 -12.300  11.248  1.00  0.00           C", &
      "HETATM 2495  CMD HEM A 154       6.427 -11.800  13.987  1.00  0.00           C", &
      "HETATM 2496  CAD HEM A 154       7.897 -14.200  12.715  1.00  0.00           C", &
      "HETATM 2497  CBD HEM A 154       8.085 -14.200  14.182  1.00  0.00           C", &
      "HETATM 2498  CGD HEM A 154       9.023 -15.500  14.476  1.00  0.00           C", &
      "HETATM 2499  O1D HEM A 154       8.898 -15.800  15.063  1.00  0.00           O", &
      "HETATM 2500  O2D HEM A 154       9.527 -15.600  13.987  1.00  0.00           O", &
      "HETATM 2501  NA  HEM A 154      10.487 -11.300   8.999  1.00  0.00           N", &
      "HETATM 2502  NB  HEM A 154      10.570  -8.600   8.607  1.00  0.00           N", &
      "HETATM 2503  NC  HEM A 154       8.613  -8.200  10.759  1.00  0.00           N", &
      "HETATM 2504  ND  HEM A 154       8.709 -10.900  11.248  1.00  0.00           N", &
      "HETATM 2505 FE   HEM A 154       9.621  -9.800   9.781  1.00  0.00 Fe", &
      "HETATM 2506  HHA HEM A 154       9.526 -14.175  10.446  1.00  0.00           H", &
      "HETATM 2507  HHB HEM A 154      12.102 -10.334   6.423  1.00  0.00           H", &
      "HETATM 2508  HHC HEM A 154       9.433  -5.442   9.183  1.00  0.00           H", &
      "HETATM 2509  HHD HEM A 154       6.484  -9.203  13.167  1.00  0.00           H", &
      "HETATM 2510 HAA2 HEM A 154      11.721 -15.251   9.655  1.00  0.00           H", &
      "HETATM 2511 HAA3 HEM A 154      11.871 -15.329   7.871  1.00  0.00           H", &
      "HETATM 2512 HBA2 HEM A 154       9.950 -16.625   8.245  1.00  0.00           H", &
      "HETATM 2513 HBA3 HEM A 154       9.407 -15.602   9.594  1.00  0.00           H", &
      "HETATM 2514  HAB HEM A 154      10.678  -4.585   7.657  1.00  0.00           H", &
      "HETATM 2515 HAC2 HEM A 154       5.478  -6.978  12.818  1.00  0.00           H", &
      "HETATM 2516 HAC3 HEM A 154       6.713  -6.877  14.085  1.00  0.00           H", &
      "HETATM 2517 HAD2 HEM A 154       6.889 -14.534  12.471  1.00  0.00           H", &
      "HETATM 2518 HAD3 HEM A 154       8.627 -14.862  12.250  1.00  0.00           H", &
      "HETATM 2519 HBD2 HEM A 154       8.582 -13.286  14.506  1.00  0.00           H", &
      "HETATM 2520 HBD3 HEM A 154       7.124 -14.306  14.686  1.00  0.00           H", &
      "HETATM 2521 HBB1 HEM A 154      12.463  -5.456   5.407  1.00  0.00           H", &
      "HETATM 2522 HBB2 HEM A 154      11.944  -3.731   5.913  1.00  0.00           H", &
      "HETATM 2523 HMD1 HEM A 154       5.952 -10.917  14.388  1.00  0.00           H", &
      "HETATM 2524 HMD2 HEM A 154       6.244 -12.763  14.439  1.00  0.00           H", &
      "HETATM 2525 HMA1 HEM A 154      12.066 -13.093   5.521  1.00  0.00           H", &
      "HETATM 2526 HMA2 HEM A 154      13.462 -12.402   6.382  1.00  0.00           H", &
      "HETATM 2527 HMA3 HEM A 154      13.009 -14.104   6.642  1.00  0.00           H", &
      "HETATM 2528 HMB1 HEM A 154      13.413  -7.065   5.680  1.00  0.00           H", &
      "HETATM 2529 HMB2 HEM A 154      11.998  -7.545   4.713  1.00  0.00           H", &
      "HETATM 2530 HMB3 HEM A 154      13.048  -8.790   5.431  1.00  0.00           H", &
      "HETATM 2531 HMC1 HEM A 154       8.716  -4.251  10.263  1.00  0.00           H", &
      "HETATM 2532 HMC2 HEM A 154       8.353  -4.158  12.003  1.00  0.00           H", &
      "HETATM 2533 HMC3 HEM A 154       7.027  -4.304  10.824  1.00  0.00           H", &
      "HETATM 2534 HBC1 HEM A 154       5.472  -4.900  13.996  1.00  0.00           H", &
      "HETATM 2535 HBC2 HEM A 154       5.804  -4.727  12.256  1.00  0.00           H", &
      "HETATM 2536 HBC3 HEM A 154       7.133  -4.599  13.434  1.00  0.00           H", &
      "END"
   rewind(unit)

   call read_pdb(struc, unit, error)
   close(unit)

end subroutine test_invalid2_pdb


subroutine test_invalid3_pdb(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: struc
   integer :: unit

   open(status='scratch', newunit=unit)
   write(unit, '(a)') &
      "ATOM      1  N   GLY Z   1      -0.821  -2.072  16.609  1.00  9.93           N1+", &
      "ANISOU    1  N   GLY Z   1     1184   1952    638    314   -191   -326       N", &
      "ATOM      2  CA  GLY Z   1      -1.705  -2.345  15.487  1.00  7.38           C", &
      "ANISOU    2  CA  GLY Z   1      957   1374    472    279   -124   -261       C", &
      "ATOM      3  C   GLY Z   1      -0.968  -3.008  14.344  1.00  4.89           C", &
      "ANISOU    3  C   GLY Z   1      899    614    343    211    112   -106       C", &
      "ATOM      4  O   GLY Z   1       0.258  -2.982  14.292  1.00  5.05           O", &
      "ANISOU    4  O   GLY Z   1      839    595    485    -11     -7   -180       O", &
      "ATOM      5  HA2 GLY Z   1      -2.130  -1.405  15.135  1.00  0.00           H", &
      "ATOM      6  HA3 GLY Z   1      -2.511  -2.999  15.819  1.00  0.00           H", &
      "ATOM      7  H1  GLY Z   1      -1.364  -1.742  17.394  1.00  0.00           H", &
      "ATOM      8  H2  GLY Z   1      -0.150  -1.365  16.344  1.00  0.00           H", &
      "ATOM      9  H3  GLY Z   1      -0.334  -2.918  16.868  1.00  0.00           H", &
      "ATOM     10  X   GLY Z   1      -1.141  -4.323  12.291  1.00  1.85           X", &
      "TER      11      GLY Z   1", &
      "END"
   rewind(unit)

   call read_pdb(struc, unit, error)
   close(unit)

end subroutine test_invalid3_pdb


end module test_read_pdb
