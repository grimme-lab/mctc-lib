# Modular computation tool chain library

[![Apache-2.0](https://img.shields.io/github/license/grimme-lab/mctc-lib)](LICENSE)
[![Release](https://img.shields.io/github/v/release/grimme-lab/mctc-lib)](https://github.com/grimme-lab/mctc-lib/releases/latest)
[![CI](https://github.com/grimme-lab/mctc-lib/workflows/CI/badge.svg)](https://github.com/grimme-lab/mctc-lib/actions)
[![docs](https://github.com/grimme-lab/mctc-lib/workflows/docs/badge.svg)](https://grimme-lab.github.io/mctc-lib)
[![codecov](https://codecov.io/gh/grimme-lab/mctc-lib/branch/main/graph/badge.svg)](https://codecov.io/gh/grimme-lab/mctc-lib)

A Fortran library providing unified molecular structure data handling and geometry file format I/O for computational chemistry applications.
The library supports reading and writing of molecular structures in more than twelve different geometry formats and provides element data and coordination number utilities.


## Features

- **Unified structure representation**: A common [``structure_type``](https://grimme-lab.github.io/mctc-lib/type/structure_type.html) for handling molecular and periodic systems
- **Multi-format I/O**: Read and write structures in 12+ geometry formats
- **Element data**: Access to atomic/covalent/vdW radii and Pauling electronegativities
- **Coordination numbers**: Multiple counting functions (exponential, error function, electronegativity-weighted)
- **Helpful error messages**: Detailed error reporting with source location information
- **Multiple build systems**: Support for meson, CMake, and fpm


## Quick Start

```f90
program example
   use mctc_io
   use mctc_env
   implicit none
   type(structure_type) :: mol
   type(error_type), allocatable :: error

   ! Read a structure (format auto-detected from extension)
   call read_structure(mol, "molecule.xyz", error)
   if (allocated(error)) stop error%message

   ! Access structure data
   print '(a,i0)', "Number of atoms: ", mol%nat
   print '(a,f12.6)', "Total charge: ", mol%charge

   ! Write to different format
   call write_structure(mol, "molecule.mol", error)
   if (allocated(error)) stop error%message
end program
```


## Supported formats

This library supports reading and writing in more than ten different geometry formats, including general ASCII formats, like xyz, JSON based formats, like QCSchema, and program specific formats, for example compatible with Turbomole or Vasp.

*General geometry formats*

- [xyz][xyz] file format with ``xyz`` extension
- [Protein data base][pdb] file format with ``pdb`` extension
- [mol][ctfile] and [structure data][ctfile] connection table file formats with ``mol`` and ``sdf`` extension, respectively

[xyz]: http://www.ccl.net/chemistry/resources/messages/1996/10/21.005-dir/index.html
[pdb]: http://www.wwpdb.org/documentation/file-format-content/format33/v3.3.html
[ctfile]: https://www.daylight.com/meetings/mug05/Kappler/ctfile.pdf

*JSON based formats*

- [Pymatgen JSON][pmg] with ``pmgjson`` or ``json`` extension
- [QCSchema JSON][qcsk] with ``qcjson`` or ``json`` extension
- [Chemical JSON][cjson] with ``cjson`` or ``json`` extension

[pmg]: https://pymatgen.org
[qcsk]: https://molssi-qc-schema.readthedocs.io
[cjson]: https://github.com/OpenChemistry/avogadrolibs/blob/master/avogadro/io/cjsonformat.cpp

*Program specific formats*

- [Q-Chem molecule][qchem] file format with ``qchem`` extension
- [Turbomole coord][tmol] file format with ``tmol`` or ``coord`` extension
- [VASP POSCAR and CONTCAR][vasp] files with ``vasp``, ``poscar``, or ``contcar`` extension
- [DFTB+ gen][gen] format with ``gen`` extension
- [Gaussian external][ein] format with ``ein`` extension
- [FHI-aims][aims] ``geometry.in`` input files

[aims]: https://fhi-aims.org
[qchem]: https://manual.q-chem.com
[tmol]: https://www.turbomole.org
[gen]: https://dftbplus.org
[ein]: https://gaussian.com/external/
[vasp]: https://www.vasp.at/wiki/index.php


## Installation

To build this project from the source code in this repository you need to have
a Fortran compiler supporting Fortran 2008 and one of the supported build systems:
- [meson](https://mesonbuild.com) version 0.55 or newer (except 1.8.0), with
  a build-system backend, *i.e.* [ninja](https://ninja-build.org) version 1.7 or newer
- [cmake](https://cmake.org) version 3.14 or newer, with
  a build-system backend, *i.e.* [ninja](https://ninja-build.org) version 1.10 or newer
- [fpm](https://github.com/fortran-lang/fpm) version 0.3.0 or newer

Currently this project supports GCC, Intel and PGI/NVHPC compilers.


### Building with meson

Setup a build with

```
meson setup _build
```

You can select the Fortran compiler by the `FC` environment variable.
To compile the project run

```
meson compile -C _build
```

You can run the projects testsuite with

```
meson test -C _build --print-errorlogs
```

To include ``mctc-lib`` in your project add the following wrap file to your subprojects directory:

```ini
[wrap-git]
directory = mctc-lib
url = https://github.com/grimme-lab/mctc-lib
revision = head
```

You can retrieve the dependency from the wrap fallback with

```meson
mctc_dep = dependency('mctc-lib', fallback: ['mctc-lib', 'mctc_dep'])
```

and add it as dependency to your targets.


### Building with CMake

Alternatively, this project can be build with CMake (in this case ninja 1.10 or newer is required):

```
cmake -B _build -G Ninja
```

To compile the project with CMake run

```
cmake --build _build
```

You can run the project testsuite with

```
pushd _build && ctest && popd
```

To include ``mctc-lib`` in your CMake project retrieve it using the ``FetchContent`` module:

```cmake
if(NOT TARGET mctc-lib)
  set("mctc-lib-url" "https://github.com/grimme-lab/mctc-lib")
  message(STATUS "Retrieving mctc-lib from ${mctc-lib-url}")
  include(FetchContent)
  FetchContent_Declare(
    "mctc-lib"
    GIT_REPOSITORY "${mctc-lib-url}"
    GIT_TAG "HEAD"
  )
  FetchContent_MakeAvailable("mctc-lib")
endif()
```

And link against the ``"mctc-lib"`` interface library.

```cmake
target_link_libraries("${PROJECT_NAME}-lib" PUBLIC "mctc-lib")
```


### Building with fpm

Invoke fpm in the project root with

```
fpm build
```

To run the testsuite use

```
fpm test
```

You can access the ``mctc-convert`` program using the run subcommand

```
fpm run -- --help
```

To use ``mctc-lib`` for testing include it as dependency in your package manifest

```toml
[dependencies]
mctc-lib.git = "https://github.com/grimme-lab/mctc-lib"
```


## Example

An example application is provided with the [``mctc-convert``](man/mctc-convert.1.adoc) program to convert between different supported input formats.


### Using mctc-convert

After building, the ``mctc-convert`` tool can convert between any supported formats:

```bash
# Convert xyz to Turbomole coord
mctc-convert molecule.xyz molecule.coord

# Convert VASP POSCAR to xyz
mctc-convert POSCAR structure.xyz

# Pipe from stdin to stdout
cat input.xyz | mctc-convert -i xyz -o mol - -

# Preserve bond information from SDF when converting
mctc-convert optimized.xyz final.sdf --template original.sdf
```


### Library Usage

To read an input file using the IO library use the ``read_structure`` routine.
The final geometry data is stored in a ``structure_type``:

```f90
use mctc_io
use mctc_env
type(structure_type) :: mol
type(error_type), allocatable :: error

call read_structure(mol, "input.xyz", error)
if (allocated(error)) then
   print '(a)', error%message
   error stop
end if
```

The environment library provides a basic error back-propagation mechanism using an allocatable ``error_type``, which is passed to the library routines.
Usually the reader can detect the file type from the suffix of file names.
Alternatively, the ``filetype`` enumerator provides the identifiers of all supported file types, which can be passed as optional argument to the ``read_structure`` routine.

In a similar way the ``write_structure`` routine allows to write a ``structure_type`` to a file or unit:

```f90
use mctc_io
use mctc_env
type(structure_type) :: mol
type(error_type), allocatable :: error

call write_structure(mol, "output.xyz", error)
if (allocated(error)) then
   print '(a)', error%message
   error stop
end if
```

The [``mctc-convert``](man/mctc-convert.1.adoc) program provides a chained reader and writer call to act as a geometry file converter.
Checkout the implementation in [``app/main.f90``](app/main.f90).


## Working with the Structure Type

The [``structure_type``](https://grimme-lab.github.io/mctc-lib/type/structure_type.html) is the central data structure for representing molecular systems:

```f90
type(structure_type) :: mol

! Basic properties
mol%nat           ! Number of atoms
mol%nid           ! Number of unique species
mol%charge        ! Total molecular charge
mol%uhf           ! Number of unpaired electrons

! Atomic data (arrays)
mol%xyz(:, :)     ! Cartesian coordinates (3, nat) in Bohr
mol%id(:)         ! Species index for each atom (nat)
mol%num(:)        ! Atomic numbers for each species (nid)
mol%sym(:)        ! Element symbols for each species (nid)

! Periodic systems
mol%lattice(:, :) ! Lattice vectors (3, 3) in Bohr
mol%periodic(:)   ! Periodic directions (3)

! Optional data
mol%bond(:, :)    ! Bond connectivity
mol%comment       ! Structure title/comment
```

### Creating Structures Programmatically

All inputs use atomic units. Coordinates must be provided in Bohr (1 Bohr ≈ 0.529 Å).

```f90
use mctc_io
use mctc_env, only : wp
implicit none
type(structure_type) :: mol
integer :: num(3)
real(wp) :: xyz(3, 3)

! Water molecule (coordinates in Bohr)
num = [8, 1, 1]  ! O, H, H
xyz = reshape([ &
   & 0.0_wp, 0.0_wp, 0.2372_wp, &
   & 0.0_wp, 1.4939_wp, -0.9487_wp, &
   & 0.0_wp, -1.4939_wp, -0.9487_wp], [3, 3])

call new(mol, num, xyz, charge=0.0_wp, uhf=0)
```


## Using Element Data

Access element-specific properties from the [``mctc_data``](https://grimme-lab.github.io/mctc-lib/module/mctc_data.html) module:

```f90
use mctc_data
use mctc_env, only : wp
implicit none
real(wp) :: radius

! Get covalent radius for carbon (atomic number 6)
radius = get_covalent_rad(6)

! Available functions:
! get_covalent_rad(num) - Covalent radii in Bohr
! get_vdw_rad(num)      - van der Waals radii in Bohr
! get_atomic_rad(num)   - Atomic radii in Bohr
! get_pauling_en(num)   - Pauling electronegativities
```


## Element Symbol Conversion

Convert between element symbols and atomic numbers:

```f90
use mctc_io, only : to_number, to_symbol

integer :: num
character(len=2) :: sym

num = to_number("C")     ! Returns 6
sym = to_symbol(6)       ! Returns "C"
```


## Specifying File Formats

When the file extension is non-standard, use the ``filetype`` enumerator:

```f90
use mctc_io

call read_structure(mol, "geometry.in", error, filetype%aims)
call write_structure(mol, "output.dat", error, filetype%xyz)
```

Available format identifiers:
- ``filetype%xyz`` - xyz format
- ``filetype%tmol`` - Turbomole coord
- ``filetype%molfile`` - mol file
- ``filetype%sdf`` - SDF format
- ``filetype%vasp`` - VASP POSCAR
- ``filetype%pdb`` - PDB format
- ``filetype%gen`` - DFTB+ genFormat
- ``filetype%gaussian`` - Gaussian external
- ``filetype%qcschema`` - QCSchema JSON
- ``filetype%cjson`` - Chemical JSON
- ``filetype%pymatgen`` - Pymatgen JSON
- ``filetype%aims`` - FHI-aims
- ``filetype%qchem`` - Q-Chem


## Error reporting

The geometry input readers try to be provide helpful error messages, no user should be left alone with an error message like *invalid input*.
Unclear error messages are considered a bug in *mctc-lib*, if you struggle to make sense of a reported error, file us an issue and we will make the report better.

**How can helpful error messages look like?**
Here are some examples.

1. negative number of atoms declared in xyz file

```
Error: Impossible number of atoms provided
 --> struc.xyz:1:1-2
  |
1 | -3
  | ^^ expected positive integer value
  |
```

2. total charge is not specified as integer

```
Error: Cannot read eht entry
  --> struc.coord:18:13-15
   |
18 | $eht charge=one unpaired=0
   |             ^^^ expected integer value
   |
```

3. a fixed width entry contains an incorrect value

```
Error: Cannot read charges
  --> struc.mol:29:23-25
   |
29 | M  CHG  3   1   1   3   b   2  -1
   |                       ^^^ expected integer value
   |
```

4. Turbomole input with conflicting data groups

```
Error: Conflicting lattice and cell groups
  --> struc.coord:37:1-5
   |
35 | $lattice angs
   | -------- lattice first defined here
   :
37 | $cell angs
   | ^^^^^ conflicting cell group
   |
```

We try to retain as much information as possible when displaying the error message to make it easy to fix the offending part in the input.


## API Documentation

Full API documentation is available at [grimme-lab.github.io/mctc-lib](https://grimme-lab.github.io/mctc-lib).

Key modules:
- [``mctc_io``](https://grimme-lab.github.io/mctc-lib/module/mctc_io.html) - Structure I/O (``read_structure``, ``write_structure``, ``structure_type``)
- [``mctc_env``](https://grimme-lab.github.io/mctc-lib/module/mctc_env.html) - Environment utilities (``error_type``, ``wp`` working precision)
- [``mctc_data``](https://grimme-lab.github.io/mctc-lib/module/mctc_data.html) - Element data (radii, electronegativities)
- [``mctc_ncoord``](https://grimme-lab.github.io/mctc-lib/module/mctc_ncoord.html) - Coordination number evaluation


## License

Licensed under the Apache License, Version 2.0 (the “License”);
you may not use this file except in compliance with the License.
You may obtain a copy of the License at
http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an *“as is” basis*,
*without warranties or conditions of any kind*, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

Unless you explicitly state otherwise, any contribution intentionally
submitted for inclusion in this project by you, as defined in the
Apache-2.0 license, shall be licensed as above, without any additional
terms or conditions.
