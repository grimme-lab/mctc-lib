---
title: XYZ Format
---

## Overview

| Property | Value |
|----------|-------|
| File extensions | `.xyz`, `.log` |
| Coordinate units | Ångström |
| Supports periodicity | No |
| Supports bonds | No |
| Format hint | `xyz` |

## Specification

@Note [Reference](http://www.ccl.net/chemistry/resources/messages/1996/10/21.005-dir/index.html)

The xyz format is a simple ASCII format for storing Cartesian coordinates and element symbols.

**File structure:**

1. **Line 1**: Number of atoms (integer)
2. **Line 2**: Comment line (can be empty, often contains title or energy)
3. **Lines 3+**: Element symbol followed by three coordinates (x, y, z)

Coordinates are given in Ångström and are internally converted to Bohr.

## Example

Caffeine molecule in xyz format:

```text
24

C            1.07317000000000        0.04885000000000       -0.07573000000000
N            2.51365000000000        0.01256000000000       -0.07580000000000
C            3.35199000000000        1.09592000000000       -0.07533000000000
N            4.61898000000000        0.73028000000000       -0.07549000000000
C            4.57907000000000       -0.63144000000000       -0.07531000000000
C            3.30131000000000       -1.10256000000000       -0.07524000000000
C            2.98068000000000       -2.48687000000000       -0.07377000000000
O            1.82530000000000       -2.90038000000000       -0.07577000000000
N            4.11440000000000       -3.30433000000000       -0.06936000000000
C            5.45174000000000       -2.85618000000000       -0.07235000000000
O            6.38934000000000       -3.65965000000000       -0.07232000000000
N            5.66240000000000       -1.47682000000000       -0.07487000000000
C            7.00947000000000       -0.93648000000000       -0.07524000000000
C            3.92063000000000       -4.74093000000000       -0.06158000000000
H            0.73398000000000        1.08786000000000       -0.07503000000000
H            0.71239000000000       -0.45698000000000        0.82335000000000
H            0.71240000000000       -0.45580000000000       -0.97549000000000
H            2.99301000000000        2.11762000000000       -0.07478000000000
H            7.76531000000000       -1.72634000000000       -0.07591000000000
H            7.14864000000000       -0.32182000000000        0.81969000000000
H            7.14802000000000       -0.32076000000000       -0.96953000000000
H            2.86501000000000       -5.02316000000000       -0.05833000000000
H            4.40233000000000       -5.15920000000000        0.82837000000000
H            4.40017000000000       -5.16929000000000       -0.94780000000000
```

## Extensions

The reader supports the following extensions beyond the standard format:

- **Atomic numbers**: Integer atomic numbers are accepted instead of element symbols
  and are automatically converted to capitalized element symbols

## Limitations

The following features are currently not supported:

- Scalar atomic quantities (4th column) are not preserved and dropped
- Vector atomic quantities (columns 5-7) are not preserved and dropped

@Note Feel free to contribute support for missing features
      or bring missing features to our attention by opening an issue.

