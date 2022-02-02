---
title: DFTB+ general format
---

## Specification

@Note [Reference](https://dftbplus.org/fileadmin/DFTBPLUS/public/dftbplus/latest/manual.pdf)

The general (gen) format is used for DFTB+ as geometry input format.
It is based on the [xyz format](./format-xyz.html).

The first line contains the number of atoms and the specific kind of provided
geometry.
Available types are cluster (``C``), supercell (``S``), fractional (``F``),
and helical (``H``), the letter defining the format is case-insensitive.

The second line gives the element symbols for each group of atoms separated by
spaces, the groups are indexed starting from 1 and references in the specification
of the atomic coordinates by this index rather than their element symbol.

The following lines are specified as two integers and three reals separated by
spaces. The first integer is currently ignored. The second integer references
the the element symbol in the second line.
The atomic coordinates are given in Ångström for cluster, supercell and helical,
while they are given as fraction of the lattice vector for fractional input types.

For supercell and fractional input the next lines contains three reals containing
the origin of the stucture, followed by three lines of each three reals for the
lattice vectors.

Lines starting with the ``#`` are comments and are ignored while parsing.

The format is identified by the extension ``gen``.

## Example

Caffeine molecule in genFormat:

```text
24 C
 C N O H
    1    1    1.07317000000000E+00    4.88500000000000E-02   -7.57300000000000E-02
    2    2    2.51365000000000E+00    1.25600000000000E-02   -7.58000000000000E-02
    3    1    3.35199000000000E+00    1.09592000000000E+00   -7.53300000000000E-02
    4    2    4.61898000000000E+00    7.30280000000000E-01   -7.54900000000000E-02
    5    1    4.57907000000000E+00   -6.31440000000000E-01   -7.53100000000000E-02
    6    1    3.30131000000000E+00   -1.10256000000000E+00   -7.52400000000000E-02
    7    1    2.98068000000000E+00   -2.48687000000000E+00   -7.37700000000000E-02
    8    3    1.82530000000000E+00   -2.90038000000000E+00   -7.57700000000000E-02
    9    2    4.11440000000000E+00   -3.30433000000000E+00   -6.93600000000000E-02
   10    1    5.45174000000000E+00   -2.85618000000000E+00   -7.23500000000000E-02
   11    3    6.38934000000000E+00   -3.65965000000000E+00   -7.23200000000000E-02
   12    2    5.66240000000000E+00   -1.47682000000000E+00   -7.48700000000000E-02
   13    1    7.00947000000000E+00   -9.36480000000000E-01   -7.52400000000000E-02
   14    1    3.92063000000000E+00   -4.74093000000000E+00   -6.15800000000000E-02
   15    4    7.33980000000000E-01    1.08786000000000E+00   -7.50300000000000E-02
   16    4    7.12390000000000E-01   -4.56980000000000E-01    8.23350000000000E-01
   17    4    7.12400000000000E-01   -4.55800000000000E-01   -9.75490000000000E-01
   18    4    2.99301000000000E+00    2.11762000000000E+00   -7.47800000000000E-02
   19    4    7.76531000000000E+00   -1.72634000000000E+00   -7.59100000000000E-02
   20    4    7.14864000000000E+00   -3.21820000000000E-01    8.19690000000000E-01
   21    4    7.14802000000000E+00   -3.20760000000000E-01   -9.69530000000000E-01
   22    4    2.86501000000000E+00   -5.02316000000000E+00   -5.83300000000000E-02
   23    4    4.40233000000000E+00   -5.15920000000000E+00    8.28370000000000E-01
   24    4    4.40017000000000E+00   -5.16929000000000E+00   -9.47800000000000E-01
```

Ammonia molecular crystal:

```text
16 S
 H N
    1    1    2.19855889440000E+00    1.76390058240000E+00    8.80145481600000E-01
    2    1    1.76390058240000E+00    8.80145481600000E-01    2.19855889440000E+00
    3    1    8.80145481600000E-01    2.19855889440000E+00    1.76390058240000E+00
    4    1    4.84115108400000E+00    1.61941554720000E+00    4.93981400880000E+00
    5    1    4.35630903840000E+00    2.49981169680000E+00    3.63248012160000E+00
    6    1    3.51957925440000E+00    1.15357413600000E+00    4.08403345680000E+00
    7    1    4.08403345680000E+00    3.51957925440000E+00    1.15357413600000E+00
    8    1    4.93981400880000E+00    4.84115108400000E+00    1.61941554720000E+00
    9    1    3.63248012160000E+00    4.35630903840000E+00    2.49981169680000E+00
   10    1    2.49981169680000E+00    3.63248012160000E+00    4.35630903840000E+00
   11    1    1.15357413600000E+00    4.08403345680000E+00    3.51957925440000E+00
   12    1    1.61941554720000E+00    4.93981400880000E+00    4.84115108400000E+00
   13    2    1.37461317840000E+00    1.37461317840000E+00    1.37461317840000E+00
   14    2    3.99815460000000E+00    1.99105592400000E+00    4.46364507600000E+00
   15    2    4.46364507600000E+00    3.99815460000000E+00    1.99105592400000E+00
   16    2    1.99105592400000E+00    4.46364507600000E+00    3.99815460000000E+00
    0.00000000000000    0.00000000000000    0.00000000000000
    5.01336000000000    0.00000000000000    0.00000000000000
    0.00000000000000    5.01336000000000    0.00000000000000
    0.00000000000000    0.00000000000000    5.01336000000000
```

## Extensions

No extension implemented to the original format.

## Missing Features

The implementation of this format is (to our knowledge) feature-complete.

@Note Feel free to contribute support for missing features
      or bring missing features to our attention by opening an issue.
