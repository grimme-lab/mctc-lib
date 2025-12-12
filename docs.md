---
project: MCTC-library
summary: Modular computation tool chain library
project_github: https://github.com/grimme-lab/mctc-lib
project_download: https://github.com/grimme-lab/mctc-lib/releases
author: Grimme group, Bonn
github: https://github.com/grimme-lab
src_dir: ./src
         ./app
output_dir: ./_docs
exclude_dir: ./test
page_dir: ./doc
docmark: <
predocmark: >
source: true
graph: false
sort: alpha
print_creation_date: true
extra_mods: iso_fortran_env:https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fFORTRAN_005fENV.html
creation_date: %Y-%m-%d %H:%M %z
md_extensions: markdown.extensions.toc
               markdown.extensions.smarty
---

API documentation for the modular computation tool chain library.
For installation instructions and getting started, see the [README](https://github.com/grimme-lab/mctc-lib).

[TOC]

## Module Overview

The library is organized into four main modules:

| Module | Purpose |
|--------|---------|
| [[mctc_io]] | Structure I/O and representation |
| [[mctc_env]] | Error handling and precision constants |
| [[mctc_data]] | Element data (radii, electronegativities) |
| [[mctc_ncoord]] | Coordination number evaluation |

### Units Convention

All quantities in mctc-lib use **atomic units**:

- Coordinates and radii are in **Bohr** (1 Bohr = 0.529177 Å)
- Energies are in **Hartree** (where applicable)

The [[mctc_io_convert]] module provides conversion factors derived from CODATA constants:

```f90
use mctc_io_convert, only : aatoau, autoaa

! Convert Ångström to Bohr (atomic units)
xyz_bohr = xyz_ang * aatoau

! Convert Bohr to Ångström
xyz_ang = xyz_bohr * autoaa
```

Available conversion factors:

| Factor | Description |
|--------|-------------|
| `aatoau` | Ångström → Bohr |
| `autoaa` | Bohr → Ångström |
| `autoeV` | Hartree → electron volts |
| `evtoau` | electron volts → Hartree |
| `autokj` | Hartree → kJ/mol |
| `kjtoau` | kJ/mol → Hartree |
| `autokcal` | Hartree → kcal/mol |
| `kcaltoau` | kcal/mol → Hartree |
| `autorcm` | Hartree → cm⁻¹ |
| `rcmtoau` | cm⁻¹ → Hartree |
| `autonm` | Hartree → nm (wavelength) |
| `nmtoau` | nm (wavelength) → Hartree |


## Structure Representation

The [[structure_type]] is the central data structure for molecular and periodic systems.

### Key Components

| Component | Type | Description |
|-----------|------|-------------|
| `nat` | integer | Number of atoms |
| `nid` | integer | Number of unique species |
| `xyz(3, nat)` | real(wp) | Cartesian coordinates (Bohr) |
| `num(nid)` | integer | Atomic numbers for each species |
| `id(nat)` | integer | Species index for each atom |
| `sym(nid)` | character | Element symbols for each species |
| `charge` | real(wp) | Total molecular charge |
| `uhf` | integer | Number of unpaired electrons |
| `lattice(3, 3)` | real(wp) | Lattice vectors (Bohr), optional |
| `periodic(3)` | logical | Periodic directions, optional |
| `bond(2, nbd)` | integer | Bond connectivity, optional |

### Creating Structures

Use [[new]] for programmatic construction:

```f90
use mctc_io
type(structure_type) :: mol

! From atomic numbers and coordinates (in Bohr)
call new(mol, [8, 1, 1], xyz, charge=0.0_wp, uhf=0)

! From element symbols
call new(mol, ["O", "H", "H"], xyz)

! With periodicity
call new(mol, num, xyz, lattice=lattice, periodic=[.true., .true., .true.])
```


## Input and Output

The [[mctc_io]] module provides format-agnostic structure I/O.

### Reading Structures

Use [[read_structure]] to read from files:

```f90
use mctc_io
use mctc_env

type(structure_type) :: mol
type(error_type), allocatable :: error

! Auto-detect format from extension
call read_structure(mol, "input.xyz", error)

! Explicit format specification
call read_structure(mol, "input.dat", error, filetype%xyz)
```

### Writing Structures

Use [[write_structure]] to write to files:

```f90
! Auto-detect format
call write_structure(mol, "output.mol", error)

! Explicit format
call write_structure(mol, "output.dat", error, filetype%gen)
```

### Format Detection

The [[filetype]] enumerator identifies supported formats:

```f90
use mctc_io, only : filetype, get_filetype

integer :: ftype
ftype = get_filetype("molecule.xyz")  ! Returns filetype%xyz
```

See the [format documentation](./page/index.html) for details on each format.


## Error Handling

The [[mctc_env]] module provides the [[error_type]] for error propagation.

### Error Pattern

```f90
use mctc_env
type(error_type), allocatable :: error

call library_routine(result, error)
if (allocated(error)) then
   write(*, '(a)') error%message
   error stop 1
end if
```

### Creating Errors

Use [[fatal_error]] in your own routines:

```f90
use mctc_env, only : error_type, fatal_error

subroutine my_routine(input, output, error)
   type(error_type), allocatable, intent(out) :: error

   if (invalid_condition) then
      call fatal_error(error, "Descriptive error message")
      return
   end if
end subroutine
```


## Element Data

The [[mctc_data]] module provides element-specific data.
All radii are returned in **Bohr**.

### Available Functions

| Function | Description |
|----------|-------------|
| [[get_covalent_rad]] | Covalent radii |
| [[get_vdw_rad]] | van der Waals radii |
| [[get_atomic_rad]] | Atomic radii |
| [[get_pauling_en]] | Pauling electronegativities |

### Usage

```f90
use mctc_data
use mctc_env, only : wp

real(wp) :: r_cov, r_vdw, en

r_cov = get_covalent_rad(6)   ! Carbon
r_vdw = get_vdw_rad(6)
en = get_pauling_en(6)
```


## Coordination Numbers

The [[mctc_ncoord]] module provides coordination number evaluators.

### Counting Functions

The [[cn_count]] enumerator selects the counting function:

| Function | Description | Typical Use |
|----------|-------------|-------------|
| `cn_count%exp` | Exponential | General purpose |
| `cn_count%dexp` | Double-exponential | Sharper cutoff |
| `cn_count%erf` | Error function | GFN methods |
| `cn_count%erf_en` | EN-weighted error function | Electronegativity corrections |
| `cn_count%dftd4` | DFT-D4 error function | Dispersion corrections |

### Usage

```f90
use mctc_ncoord
use mctc_io, only : structure_type
use mctc_env, only : wp, error_type

class(ncoord_type), allocatable :: ncoord
type(error_type), allocatable :: error
real(wp), allocatable :: cn(:)

call new_ncoord(ncoord, mol, cn_count%exp, error)
if (allocated(error)) stop error%message

allocate(cn(mol%nat))
call ncoord%get_cn(mol, cn)
```


## Symbol Conversion

Convert between element symbols and atomic numbers:

```f90
use mctc_io, only : to_symbol, to_number

character(len=2) :: sym
integer :: num

sym = to_symbol(6)     ! Returns "C"
num = to_number("C")   ! Returns 6
num = to_number("c")   ! Case-insensitive, returns 6
```


## Integration Guide

### As Meson Subproject

Add to `subprojects/mctc-lib.wrap`:

```ini
[wrap-git]
directory = mctc-lib
url = https://github.com/grimme-lab/mctc-lib
revision = head
```

In `meson.build`:

```python
mctc_dep = dependency('mctc-lib', fallback: ['mctc-lib', 'mctc_dep'])
```

### As fpm Dependency

In `fpm.toml`:

```toml
[dependencies.mctc-lib]
git = "https://github.com/grimme-lab/mctc-lib"
```

