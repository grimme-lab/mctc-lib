---
title: Pymatgen JSON
---

## Overview

| Property | Value |
|----------|-------|
| File extensions | `.pmgjson`, `.json` |
| Coordinate units | Ångström |
| Supports periodicity | Yes (`Structure` class) |
| Supports charge/spin | Yes |
| Format hint | `pymatgen` |

@Note Requires JSON support (jonquil dependency)

## Specification

@Note [Reference](https://pymatgen.org)

Pymatgen JSON format represents molecular and periodic structures using the Python Materials Genomics library schema.

### Supported Classes

| Class | Description | Periodicity |
|-------|-------------|-------------|
| `Molecule` | Molecular structure | Non-periodic |
| `Structure` | Periodic structure with lattice | 3D periodic |

### Key Fields

| Field | Description |
|-------|-------------|
| `@class` | `Molecule` or `Structure` |
| `charge` | Total charge |
| `spin_multiplicity` | Spin multiplicity (Molecule only) |
| `lattice` | Lattice parameters (Structure only) |
| `sites` | Array of atomic sites |
| `sites[].species` | Element and occupancy |
| `sites[].xyz` | Cartesian coordinates (Ångström) |
| `sites[].abc` | Fractional coordinates (Structure) |

## Examples

### Molecular System

Water molecules using `Molecule` schema:

```json
{
  "@module": "pymatgen.core.structure",
  "@class": "Molecule",
  "charge": 0,
  "spin_multiplicity": 1,
  "sites": [
    {
      "name": "O",
      "species": [{"element": "O", "occu": 1}],
      "xyz": [1.1847029, 1.1150792, -0.0344641],
      "properties": {},
      "label": "O"
    },
    {
      "name": "H",
      "species": [{"element": "H", "occu": 1}],
      "xyz": [0.4939088, 0.9563767, 0.6340089],
      "properties": {},
      "label": "H"
    },
    {
      "name": "H",
      "species": [{"element": "H", "occu": 1}],
      "xyz": [2.0242676, 1.0811246, 0.4301417],
      "properties": {},
      "label": "H"
    },
    {
      "name": "O",
      "species": [{"element": "O", "occu": 1}],
      "xyz": [-1.1469443, 0.0697649, 1.1470196],
      "properties": {},
      "label": "O"
    },
    {
      "name": "H",
      "species": [{"element": "H", "occu": 1}],
      "xyz": [-1.2798308, -0.5232169, 1.8902833],
      "properties": {},
      "label": "H"
    },
    {
      "name": "H",
      "species": [{"element": "H", "occu": 1}],
      "xyz": [-1.0641398, -0.4956693, 0.356925],
      "properties": {},
      "label": "H"
    },
    {
      "name": "O",
      "species": [{"element": "O", "occu": 1}],
      "xyz": [-0.1633508, -1.0289346, -1.2401808],
      "properties": {},
      "label": "O"
    },
    {
      "name": "H",
      "species": [{"element": "H", "occu": 1}],
      "xyz": [0.4914771, -0.3248733, -1.0784838],
      "properties": {},
      "label": "H"
    },
    {
      "name": "H",
      "species": [{"element": "H", "occu": 1}],
      "xyz": [-0.5400907, -0.8496512, -2.1052499],
      "properties": {},
      "label": "H"
    }
  ],
  "properties": {}
}
```

### Periodic System

Rutile TiO₂ using `Structure` schema:

```json
{
  "@module": "pymatgen.core.structure",
  "@class": "Structure",
  "charge": 0.0,
  "lattice": {
    "matrix": [
      [5.59003664376222, 0.0, 0.0],
      [0.0, 8.68089159045265, 0.0],
      [0.0, 0.0, 8.68089159045265]
    ],
    "pbc": [true, true, true],
    "a": 5.59003664376222,
    "b": 8.68089159045265,
    "c": 8.68089159045265,
    "alpha": 90.0,
    "beta": 90.0,
    "gamma": 90.0,
    "volume": 421.253303917213
  },
  "properties": {},
  "sites": [
    {
      "species": [{"element": "Ti", "occu": 1}],
      "abc": [0.0, 0.0, 0.0],
      "properties": {},
      "label": "Ti",
      "xyz": [0.0, 0.0, 0.0]
    },
    {
      "species": [{"element": "Ti", "occu": 1}],
      "abc": [0.5, 0.5000000000000007, 0.5000000000000007],
      "properties": {},
      "label": "Ti",
      "xyz": [2.79501832188111, 4.340445795226331, 4.340445795226331]
    },
    {
      "species": [{"element": "O", "occu": 1}], 
      "abc": [0.0, 0.30530000000000074, 0.30530000000000074],
      "properties": {},
      "label": "O",
      "xyz": [0.0, 2.6502762025652005, 2.6502762025652005]
    },
    {
      "species": [{"element": "O", "occu": 1}], 
      "abc": [0.0, 0.6947000000000005, 0.6947000000000005],
      "properties": {},
      "label": "O",
      "xyz": [0.0, 6.03061538788746, 6.03061538788746]
    },
    {
      "species": [{"element": "O", "occu": 1}], 
      "abc": [0.5, 0.1946999999999999, 0.8053000000000002],
      "properties": {},
      "label": "O",
      "xyz": [2.79501832188111, 1.69016959266113, 6.99072199779152]
    },
    {
      "species": [{"element": "O", "occu": 1}], 
      "abc": [0.5, 0.8053000000000002, 0.1946999999999999],
      "properties": {},
      "label": "O",
      "xyz": [2.79501832188111, 6.99072199779152, 1.69016959266113]
    }
  ]
}
```

## Limitations

- Schema completeness is not verified during reading
- Not all pymatgen fields are preserved in the structure type
- Site occupancies other than 1.0 are not fully supported

@Note Feel free to contribute support for missing features
      or bring missing features to our attention by opening an issue.

