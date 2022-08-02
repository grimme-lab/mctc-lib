# Modular computation tool chain library

[![Apache-2.0](https://img.shields.io/github/license/grimme-lab/mctc-lib)](LICENSE)
[![Release](https://img.shields.io/github/v/release/grimme-lab/mctc-lib)](https://github.com/grimme-lab/mctc-lib/releases/latest)
[![CI](https://github.com/grimme-lab/mctc-lib/workflows/CI/badge.svg)](https://github.com/grimme-lab/mctc-lib/actions)
[![docs](https://github.com/grimme-lab/mctc-lib/workflows/docs/badge.svg)](https://grimme-lab.github.io/mctc-lib)
[![codecov](https://codecov.io/gh/grimme-lab/mctc-lib/branch/main/graph/badge.svg)](https://codecov.io/gh/grimme-lab/mctc-lib)


## Installation

To build this project from the source code in this repository you need to have
a Fortran compiler supporting Fortran 2008 and one of the supported build systems:
- [meson](https://mesonbuild.com) version 0.55 or newer, with
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

To read an input file using the IO library use the ``read_structure`` routine.
The final geometry data is stored in a ``structure_type``:

```fortran
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

``` fortran
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
