---
title: Protein data bank (PDB) format
---

## Specification

@Note [Reference](http://www.wwpdb.org/documentation/file-format-content/format33/v3.3.html)

The extension identifying this format is ``pdb``.

## Example

4QXX protein with explicit hydrogen:

```text
HEADER    PROTEIN FIBRIL                          22-JUL-14   4QXX              
TITLE     STRUCTURE OF THE AMYLOID FORMING PEPTIDE GNLVS (RESIDUES 26-30) FROM  
TITLE    2 THE EOSINOPHIL MAJOR BASIC PROTEIN (EMBP)                            
DBREF  4QXX Z    1     5  UNP    P13727   PRG2_HUMAN     131    135             
SEQRES   1 Z    5  GLY ASN LEU VAL SER                                          
FORMUL   2  HOH   *2(H2 O)                                                      
CRYST1    4.755   16.816   35.759  90.00  90.00  90.00 P 2 21 21     4          
ORIGX1      1.000000  0.000000  0.000000        0.00000                         
ORIGX2      0.000000  1.000000  0.000000        0.00000                         
ORIGX3      0.000000  0.000000  1.000000        0.00000                         
SCALE1      0.210305  0.000000  0.000000        0.00000                         
SCALE2      0.000000  0.059467  0.000000        0.00000                         
SCALE3      0.000000  0.000000  0.027965        0.00000                         
ATOM      1  N   GLY Z   1      -0.821  -2.072  16.609  1.00  9.93           N
ANISOU    1  N   GLY Z   1     1184   1952    638    314   -191   -326       N
ATOM      2  CA  GLY Z   1      -1.705  -2.345  15.487  1.00  7.38           C
ANISOU    2  CA  GLY Z   1      957   1374    472    279   -124   -261       C
ATOM      3  C   GLY Z   1      -0.968  -3.008  14.344  1.00  4.89           C
ANISOU    3  C   GLY Z   1      899    614    343    211    112   -106       C
ATOM      4  O   GLY Z   1       0.258  -2.982  14.292  1.00  5.05           O
ANISOU    4  O   GLY Z   1      839    595    485    -11     -7   -180       O
ATOM      5  HA2 GLY Z   1      -2.130  -1.405  15.135  1.00  0.00           H
ATOM      6  HA3 GLY Z   1      -2.511  -2.999  15.819  1.00  0.00           H
ATOM      7  H1  GLY Z   1      -1.364  -1.742  17.394  1.00  0.00           H
ATOM      8  H2  GLY Z   1      -0.150  -1.365  16.344  1.00  0.00           H
ATOM      9  H3  GLY Z   1      -0.334  -2.918  16.868  1.00  0.00           H
ATOM     10  N   ASN Z   2      -1.721  -3.603  13.425  1.00  3.53           N
ANISOU   10  N   ASN Z   2      747    329    264   -226    117    -67       N
ATOM     11  CA  ASN Z   2      -1.141  -4.323  12.291  1.00  1.85           C
ANISOU   11  CA  ASN Z   2      313    164    225     76    -23     77       C
ATOM     12  C   ASN Z   2      -1.748  -3.900  10.968  1.00  3.00           C
ANISOU   12  C   ASN Z   2      610    293    238    197     -2    -42       C
ATOM     13  O   ASN Z   2      -2.955  -3.683  10.873  1.00  3.99           O
ANISOU   13  O   ASN Z   2      599    514    402    199    191    -60       O
ATOM     14  CB  ASN Z   2      -1.353  -5.827  12.446  1.00  5.03           C
ANISOU   14  CB  ASN Z   2     1173    368    369    170    -47     37       C
ATOM     15  CG  ASN Z   2      -0.679  -6.391  13.683  1.00  5.08           C
ANISOU   15  CG  ASN Z   2      727    718    484    228   -243     90       C
ATOM     16  OD1 ASN Z   2       0.519  -6.202  13.896  1.00  6.10           O
ANISOU   16  OD1 ASN Z   2      828    960    531    477    -61    100       O
ATOM     17  ND2 ASN Z   2      -1.448  -7.087  14.506  1.00  8.41           N
ANISOU   17  ND2 ASN Z   2     1513   1193    488     40    102    279       N
ATOM     18  H   ASN Z   2      -2.726  -3.557  13.512  1.00  0.00           H
ATOM     19  HA  ASN Z   2      -0.070  -4.123  12.263  1.00  0.00           H
ATOM     20  HB2 ASN Z   2      -0.945  -6.328  11.568  1.00  0.00           H
ATOM     21  HB3 ASN Z   2      -2.423  -6.029  12.503  1.00  0.00           H
ATOM     22 HD21 ASN Z   2      -2.427  -7.218  14.293  1.00  0.00           H
ATOM     23 HD22 ASN Z   2      -1.056  -7.487  15.346  1.00  0.00           H
ATOM     24  N   LEU Z   3      -0.907  -3.803   9.944  1.00  3.47           N
ANISOU   24  N   LEU Z   3      701    405    213   -242     25    -26       N
ATOM     25  CA  LEU Z   3      -1.388  -3.576   8.586  1.00  3.48           C
ANISOU   25  CA  LEU Z   3      728    324    271     79    180     -5       C
ATOM     26  C   LEU Z   3      -0.783  -4.660   7.709  1.00  3.29           C
ANISOU   26  C   LEU Z   3      684    261    306    -17    150    -80       C
ATOM     27  O   LEU Z   3       0.437  -4.788   7.643  1.00  3.80           O
ANISOU   27  O   LEU Z   3      590    437    415    141    178   -122       O
ATOM     28  CB  LEU Z   3      -0.977  -2.185   8.081  1.00  3.88           C
ANISOU   28  CB  LEU Z   3      899    293    282    171    125     42       C
ATOM     29  CG  LEU Z   3      -1.524  -1.669   6.736  1.00  8.66           C
ANISOU   29  CG  LEU Z   3     2091    598    600     63    -94     66       C
ATOM     30  CD1 LEU Z   3      -1.225  -0.191   6.570  1.00  9.89           C
ANISOU   30  CD1 LEU Z   3     2263    792    703    132   -163    143       C
ATOM     31  CD2 LEU Z   3      -0.962  -2.409   5.541  1.00 13.56           C
ANISOU   31  CD2 LEU Z   3     3203   1048    901   -640   -305     94       C
ATOM     32  H   LEU Z   3       0.086  -3.888  10.109  1.00  0.00           H
ATOM     33  HA  LEU Z   3      -2.475  -3.661   8.568  1.00  0.00           H
ATOM     34  HB2 LEU Z   3      -1.284  -1.469   8.843  1.00  0.00           H
ATOM     35  HB3 LEU Z   3       0.111  -2.162   8.026  1.00  0.00           H
ATOM     36  HG  LEU Z   3      -2.606  -1.798   6.737  1.00  0.00           H
ATOM     37 HD11 LEU Z   3      -1.623   0.359   7.423  1.00  0.00           H
ATOM     38 HD12 LEU Z   3      -1.691   0.173   5.654  1.00  0.00           H
ATOM     39 HD13 LEU Z   3      -0.147  -0.043   6.513  1.00  0.00           H
ATOM     40 HD21 LEU Z   3      -1.168  -3.475   5.643  1.00  0.00           H
ATOM     41 HD22 LEU Z   3      -1.429  -2.035   4.630  1.00  0.00           H
ATOM     42 HD23 LEU Z   3       0.115  -2.250   5.489  1.00  0.00           H
ATOM     43  N   VAL Z   4      -1.635  -5.424   7.029  1.00  3.17           N
ANISOU   43  N   VAL Z   4      604    266    333   -100    104   -123       N
ATOM     44  CA  VAL Z   4      -1.165  -6.460   6.119  1.00  3.61           C
ANISOU   44  CA  VAL Z   4      607    353    411    205   -241   -157       C
ATOM     45  C   VAL Z   4      -1.791  -6.230   4.755  1.00  5.31           C
ANISOU   45  C   VAL Z   4      543    915    562    395    -15    -39       C
ATOM     46  O   VAL Z   4      -3.014  -6.209   4.620  1.00  7.31           O
ANISOU   46  O   VAL Z   4      577   1569    630     45     -5   -227       O
ATOM     47  CB  VAL Z   4      -1.567  -7.872   6.593  1.00  5.31           C
ANISOU   47  CB  VAL Z   4     1024    336    657     64     39   -167       C
ATOM     48  CG1 VAL Z   4      -1.012  -8.934   5.633  1.00  6.73           C
ANISOU   48  CG1 VAL Z   4     1131    549    879    220    104   -300       C
ATOM     49  CG2 VAL Z   4      -1.083  -8.120   8.018  1.00  5.48           C
ANISOU   49  CG2 VAL Z   4      819    632    630     15     15     42       C
ATOM     50  H   VAL Z   4      -2.628  -5.282   7.146  1.00  0.00           H
ATOM     51  HA  VAL Z   4      -0.080  -6.402   6.034  1.00  0.00           H
ATOM     52  HB  VAL Z   4      -2.655  -7.939   6.585  1.00  0.00           H
ATOM     53 HG11 VAL Z   4      -1.303  -9.926   5.980  1.00  0.00           H
ATOM     54 HG12 VAL Z   4      -1.414  -8.766   4.634  1.00  0.00           H
ATOM     55 HG13 VAL Z   4       0.075  -8.864   5.603  1.00  0.00           H
ATOM     56 HG21 VAL Z   4      -1.377  -9.121   8.333  1.00  0.00           H
ATOM     57 HG22 VAL Z   4       0.003  -8.032   8.053  1.00  0.00           H
ATOM     58 HG23 VAL Z   4      -1.529  -7.383   8.686  1.00  0.00           H
ATOM     59  N   SER Z   5      -0.966  -6.052   3.736  1.00  7.53           N
ANISOU   59  N   SER Z   5      810   1357    693    337     48    302       N
ATOM     60  CA  SER Z   5      -1.526  -5.888   2.407  1.00 11.48           C
ANISOU   60  CA  SER Z   5     1654   1766    943    560   -145    241       C
ATOM     61  C   SER Z   5      -1.207  -7.085   1.529  1.00 16.35           C
ANISOU   61  C   SER Z   5     3066   2118   1029    758   -523   -208       C
ATOM     62  O   SER Z   5      -0.437  -7.976   1.902  1.00 14.00           O
ANISOU   62  O   SER Z   5     2584   1676   1060    878   -402   -452       O
ATOM     63  CB  SER Z   5      -1.031  -4.596   1.767  1.00 13.36           C
ANISOU   63  CB  SER Z   5     1565   2151   1361    818    -30    608       C
ATOM     64  OG  SER Z   5       0.361  -4.652   1.540  1.00 15.80           O
ANISOU   64  OG  SER Z   5     1604   2812   1587    822     25    763       O
ATOM     65  OXT SER Z   5      -1.737  -7.178   0.429  1.00 17.09           O
ANISOU   65  OXT SER Z   5     2807   2495   1192    648   -712   -430       O
ATOM     66  H   SER Z   5       0.033  -6.031   3.880  1.00  0.00           H
ATOM     67  HA  SER Z   5      -2.610  -5.822   2.504  1.00  0.00           H
ATOM     68  HB2 SER Z   5      -1.543  -4.449   0.816  1.00  0.00           H
ATOM     69  HB3 SER Z   5      -1.254  -3.759   2.428  1.00  0.00           H
ATOM     70  HG  SER Z   5       0.653  -3.831   1.137  1.00  0.00           H
TER      71      SER Z   5
HETATM   72  O   HOH Z 101       0.935  -5.175  16.502  1.00 18.83           O
ANISOU   72  O   HOH Z 101     3066   2772   1315  -1227   -232    339       O
HETATM   73  H1  HOH Z 101       0.794  -5.522  15.621  1.00  0.00           H
HETATM   74  H2  HOH Z 101       1.669  -4.561  16.489  1.00  0.00           H
HETATM   75  O  AHOH Z 102       0.691  -8.408  17.879  0.91 56.55           O
ANISOU   75  O  AHOH Z 102     9673   9234   2579      0      5   1219       O
HETATM   76  O  BHOH Z 102      -0.788  -9.006  16.641  0.09 38.95           O
ANISOU   76  O  BHOH Z 102     6801   4266   3734   2095  -1531    549       O
HETATM   77  H1 AHOH Z 102       1.392  -8.125  18.466  0.91  0.00           H
HETATM   78  H1 BHOH Z 102      -1.351  -9.776  16.563  0.09  0.00           H
HETATM   79  H2 AHOH Z 102       0.993  -8.356  16.972  0.91  0.00           H
HETATM   80  H2 BHOH Z 102      -0.927  -8.594  17.494  0.09  0.00           H
CONECT   73   72
CONECT   74   72
CONECT   72   73   74
CONECT   78   76
CONECT   77   75
CONECT   80   76
CONECT   79   75
CONECT   75   77   79
CONECT   76   78   80
END
```

## Extensions

No extension implemented to the original format.

## Missing Features

The following features are currently not supported:

- Support for multiple file PDB input is not available
- Fractional side occupation is currently not supported
  all optional sides count as full atoms
- Cell information is not preserved, PDB input is always handled molecular

@Note Feel free to contribute support for missing features
      or bring missing features to our attention by opening an issue.
