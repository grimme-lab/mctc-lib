---
title: Crystallographic Information File
---

## Overview

| Property | Value |
|----------|-------|
| File extension | `.cif` |
| Coordinate units | Fractional, cell lengths in Ångström, angles in degrees |
| Supports periodicity | Yes (always 3D periodic) |
| Format hint | `cif` |
| Read / write | Read only |

## Specification

The Crystallographic Information File (CIF) is the IUCr standard exchange format for crystal structures.
It consists of data blocks (`data_<name>`) containing tag–value pairs and `loop_` tables.

### Format Detection

The format is identified by:
- File extension: `.cif`
- Format specifier: `cif`

### Supported Data Items

| Data name | Description |
|-----------|-------------|
| `_cell_length_a`, `_cell_length_b`, `_cell_length_c` | Cell lengths in Ångström (required) |
| `_cell_angle_alpha`, `_cell_angle_beta`, `_cell_angle_gamma` | Cell angles in degrees (default 90) |
| `_space_group_symop_operation_xyz`, `_symmetry_equiv_pos_as_xyz` | Symmetry operations like `-x, 1/2+y, -z` |
| `_symmetry_space_group_name_H-M`, `_space_group_name_H-M_alt` | Space group name, only checked for P1 |
| `_space_group_IT_number`, `_symmetry_Int_Tables_number` | Space group number, only checked for P1 |
| `_atom_site_fract_x`, `_atom_site_fract_y`, `_atom_site_fract_z` | Fractional coordinates (required) |
| `_atom_site_type_symbol` | Element, charges like `Hg2+` are ignored |
| `_atom_site_label` | Element from leading letters (`C1A`, `Cl2`), used without type symbol |

Data names are case-insensitive. Standard uncertainties like `4.1749(3)` are ignored.
Comments, quoted strings and semicolon text fields are skipped.
All other data items and loops are ignored.

### Structure Construction

- The first data block containing an atom site loop is read; its name is stored as comment.
- Every atom site is expanded by all symmetry operations; images coinciding with an
  earlier image of the same site (within 10⁻³ in fractional coordinates) are dropped.
- Positions are not wrapped into the unit cell, P1 inputs keep their coordinates exactly.
- The lattice is constructed with *a* along *x* and *b* in the *xy* plane.
- Structures in a left-handed lattice are written with inverted fractional coordinates,
  which keeps their handedness in the right-handed cell described by the cell parameters.

## Example

Rock salt with explicit symmetry operations:

```text
data_rocksalt
_cell_length_a 4.1749(3)
_cell_length_b 4.1749
_cell_length_c 4.1749
_symmetry_space_group_name_H-M 'F m 3 m'
loop_
_symmetry_equiv_pos_as_xyz
x,y,z
x,1/2+y,1/2+z
1/2+x,y,1/2+z
1/2+x,1/2+y,z
-x,-y,-z
loop_
_atom_site_label
_atom_site_fract_x
_atom_site_fract_y
_atom_site_fract_z
Mg 0.00000 0.00000 0.00000
O 0.50000 0.50000 0.50000
```

## Limitations

- Written CIF files contain a single data block in space group P1 with cell parameters
  and fractional atom site coordinates, named after the structure comment.
- Files must list the compound's symmetry operations explicitly unless the space group is P1.
- Cartesian atom site coordinates (`_atom_site_Cartn_x`) are not supported.
- Partial occupancies (`_atom_site_occupancy` other than 1) and disordered sites
  (`_atom_site_disorder_group` other than `.` or `?`) are rejected with an error.
- Cell parameters that do not describe a unit cell with positive volume are rejected with an error.
- CIF 2.0 list and table values are not supported.

@Note Feel free to bring missing features to our attention by opening an issue.
