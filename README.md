# Modular computation tool chain library

[![Apache-2.0](https://img.shields.io/github/license/grimme-lab/mctc-lib)](LICENSE)
[![Release](https://img.shields.io/github/v/release/grimme-lab/mctc-lib)](https://github.com/grimme-lab/mctc-lib/releases/latest)
[![CI](https://github.com/grimme-lab/mctc-lib/workflows/CI/badge.svg)](https://github.com/grimme-lab/mctc-lib/actions)
[![docs](https://github.com/grimme-lab/mctc-lib/workflows/docs/badge.svg)](https://grimme-lab.github.io/mctc-lib)


## Installation

To build this project from the source code in this repository you need to have
- a Fortran compiler supporting Fortran 2008
- [meson](https://mesonbuild.com) version 0.53 or newer
- a build-system backend, *i.e.* [ninja](https://ninja-build.org) version 1.7 or newer

Setup a build with

```
meson setup _build
```

You can select the Fortran compiler by the `FC` environment variable, currently this project supports GCC, Intel and PGI/NVHPC compilers.
To compile the project run

```
meson compile -C _build
```

You can run the projects testsuite with

```
meson test -C _build --print-errorlogs
```

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


## Example

An example application is provided with the [``mctc-convert``](man/mctc-convert.1.adoc) program to convert between different supported input formats.

To read an input file using the IO library use the ``read_structure`` routine.
The final geometry data is stored in a ``structure_type``:

```fortran
use mctc_io
use mctc_env
type(structure_type) :: mol
type(error_type), allocatable :: error

call read_structure(mol, "input.xyz", error)
if (allocated(error)) then
   print '("[Error]", 1x, a)', error%message
   error stop
end if
```

The environment library provides a basic error back-propagation mechanism using an allocatable ``error_type``, which is passed to the library routines.
Usually the reader can detect the file type from the suffix of file names.
Alternatively, the ``filetype`` enumerator provides the identifiers of all supported file types, which can be passed as optional argument to the ``read_structure`` routine.

In a similar way the ``write_structure`` routine allows to write a ``structure_type`` to a file or unit:

``` fortran
use mctc_io
use mctc_env
type(structure_type) :: mol
type(error_type), allocatable :: error

call write_structure(mol, "output.xyz", error)
if (allocated(error)) then
   print '("[Error]", 1x, a)', error%message
   error stop
end if
```

The [``mctc-convert``](man/mctc-convert.1.adoc) program provides a chained reader and writer call to act as a geometry file converter.
Checkout the implementation in [``app/main.f90``](app/main.f90).


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
