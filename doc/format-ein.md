---
title: Gaussian external format
---

## Specification

@Note [Reference](https://gaussian.com/external/)

The first line of the input is read as four integers of width 10, ``(4i10)``,
containing the number of atoms in the first integer.
A run mode specific integer is given in the second entry.
The third integer contains the total charge and the fourth integer the spin as
number of unpaired electrons.
The total charge and the systems spin are stored in the [[structure_type]].

The structure is specified by atomic numbers, cartesian coordinates in atomic units
(Bohr) and a scalar quantity, usually partial charges using the fixed format
``(i10,4f20.12)``.
The element is identified by its atomic number,
which is converted to its capitalized element symbol internally.
Only positive, non-zero integers are allowed as atomic numbers.

The expected file extension is ``ein``.

## Examples

Caffeine molecule in Gaussian external format:

```text
        24         1         0         0
         6      2.027996941030      0.092313100971     -0.143108928077      0.000000000000
         7      4.750109032883      0.023734954927     -0.143241208877      0.000000000000
         6      6.334341685252      2.070988200950     -0.142353037792      0.000000000000
         7      8.728605263543      1.380028892063     -0.142655393906      0.000000000000
         6      8.653186310426     -1.193248402810     -0.142315243278      0.000000000000
         6      6.238570386230     -2.083535979669     -0.142182962479      0.000000000000
         6      5.632667631585     -4.699502178348     -0.139405065684      0.000000000000
         8      3.449316339873     -5.480922657010     -0.143184517105      0.000000000000
         7      7.775087464402     -6.244277357876     -0.131071375299      0.000000000000
         6     10.302293246446     -5.397396780594     -0.136721655174      0.000000000000
         8     12.074100072866     -6.915734697428     -0.136664963403      0.000000000000
         7     10.700382864677     -2.790784724183     -0.141483763966      0.000000000000
         6     13.245975677887     -1.769690333624     -0.142182962479      0.000000000000
         6      7.408915313425     -8.959057313972     -0.116369309269      0.000000000000
         1      1.387020877193      2.055757011721     -0.141786120079      0.000000000000
         1      1.346221699097     -0.863566855309      1.555905663964      0.000000000000
         1      1.346240596354     -0.861336978970     -1.843408533601      0.000000000000
         1      5.655967949599      4.001720959646     -0.141313688652      0.000000000000
         1     14.674305959118     -3.262309083535     -0.143449078705      0.000000000000
         1     13.508968805056     -0.608151528241      1.548989267863      0.000000000000
         1     13.507797175115     -0.606148418987     -1.832145768365      0.000000000000
         1      5.414083058620     -9.492394601323     -0.110227700709      0.000000000000
         1      8.319196188304     -9.749472887017      1.565392087032      0.000000000000
         1      8.315114380769     -9.768540219438     -1.791082028670      0.000000000000
```

## Extensions

No extension implemented to the original format.

## Missing Features

The following features are currently not supported:

- the requested run-mode is dropped while reading.
- scalar atomic quantities are not preserved and dropped.

@Note Feel free to contribute support for missing features
      or bring missing features to our attention by opening an issue.
