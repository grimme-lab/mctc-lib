---
title: Pymatgen JSON
---

## Specification

@Note [Reference](https://pymatgen.org)

Pymatgen formatted JSON files are identified by the extension ``pmgjson`` or ``json`` and parsed following the ``Molecule`` or ``Structure`` format.


## Example

Water molecules using ``Molecule`` schema.

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

Rutile using ``Structure`` schema:

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

## Missing features

The schema is not verified on completeness and not all data is stored in the final structure type.

@Note Feel free to contribute support for missing features
      or bring missing features to our attention by opening an issue.
