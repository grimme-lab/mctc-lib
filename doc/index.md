---
title: Supported Geometry Formats
---

This library supports reading and writing molecular structures in the following formats.
All formats can be auto-detected by file extension or explicitly specified using the `filetype` enumerator.

## General ASCII Formats

- [xyz format](./format-xyz.html) - Simple Cartesian coordinate format (`.xyz`, `.log`)
- [Connection table files](./format-ctfile.html) - MDL molfile and SDF formats (`.mol`, `.sdf`)
- [Protein Data Bank](./format-pdb.html) - PDB format for biomolecules (`.pdb`)

## JSON-based Formats

- [QCSchema JSON](./format-qcschema.html) - MolSSI QCSchema format (`.qcjson`, `.json`)
- [Chemical JSON](./format-cjson.html) - Avogadro Chemical JSON (`.cjson`, `.json`)
- [Pymatgen JSON](./format-pymatgen.html) - Pymatgen Molecule/Structure (`.pmgjson`, `.json`)

## Program-specific Formats

- [Turbomole coord](./format-tmol.html) - Turbomole/riper coordinate format (`.tmol`, `.coord`)
- [VASP POSCAR](./format-vasp.html) - VASP geometry input (`.vasp`, `.poscar`, `.contcar`)
- [DFTB+ gen format](./format-gen.html) - DFTB+ general format (`.gen`)
- [Gaussian external](./format-ein.html) - Gaussian external program input (`.ein`)
- [FHI-aims geometry.in](./format-aims.html) - FHI-aims input format (`geometry.in`)
- [Q-Chem molecule](./format-qchem.html) - Q-Chem molecule block (`.qchem`)

## Format Detection

File formats are detected automatically based on:

1. File extension (e.g., `.xyz`, `.mol`, `.vasp`)
2. Basename (e.g., `coord`, `POSCAR`, `CONTCAR`, `geometry.in`)

When automatic detection is not possible (e.g., reading from stdin), use the format hint options in `mctc-convert` or the `filetype` enumerator in the library API.
