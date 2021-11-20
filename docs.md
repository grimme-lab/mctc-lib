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

Common tool chain for working with molecular structure data in various applications.
This library provides a unified way to perform operations on molecular structure data, like reading and writing to common geometry file formats.

[TOC]

## Input and Output

The IO module ([[mctc_io]]) provides access to a common type to declare molecular structure data ([[structure_type]]).
Also, reader routines ([[mctc_io_read]]) to obtain [[structure_type]] objects from input files are available.
To write a [[structure_type]] object a set of writer routines are available as well ([[mctc_io_write]]).


## Standard environment

The tool chain library provides an environment module ([[mctc_env]]) to allow the usage of common constants across different users.
For a minimal error handling the [[error_type]] is available and should be passed as allocatable type to the library procedures.
The allocation status of the [[error_type]] is used to determine failed executions and the respective error message is stored transparently in the [[error_type]].


## Light testing framework

Additionally, the environment module provides a testsuite implementation to setup a slim and light testing framework in dependent applications.
The test framework can be easily setup by the [[mctc_env_testing]] module.


## Getting Started

### Meson

Create a new meson project and include `mctc-lib` either as git-submodule in your subprojects directory or create a wrap file to fetch it from upstream:

```ini
[wrap-git]
directory = mctc-lib
url = https://github.com/grimme-lab/mctc-lib
revision = head
```

To load the project the necessary boilerplate code for subprojects is just

<!--pygments doesn't know about meson, python highlighting looks okayish-->
```python
mctc_prj = subproject(
  'mctc-lib',
  version: '>=0.1',
  default_options: [
    'default_library=static',
  ],
)
mctc_dep = mctc_prj.get_variable('mctc_dep')
```

Now you can add `mctc_dep` to your dependencies and access the public API by the `mctc` module.

We recommend to set the default library type of `mctc-lib` to static when linking your applications or library against it.
Note for library type both and shared `mctc-lib` will install itself along with your project.

For more fine-tuned control you can access:

- the library target with `mctc_lib`
- the private include dir of this target, containing the Fortran module files, with `mctc_inc`
- the license files of `mctc-lib` with `mctc_lic`

If you are linking your application statically against `mctc-lib` and still want to distribute the license files of `mctc-lib` (thank you), just use

```python
install_data(
  mctc_prj.get_variable('mctc_lic'),
  install_dir: get_option('datadir')/'licenses'/meson.project_name()/'mctc-lib',
)
```


### Fortran Package Manager (fpm)

This project supports [fpm](https://github.com/fortran-lang/fpm) as build system as well.
Just add it to the dependencies in your `fpm.toml` file:

```toml
[dependencies]
[dependencies.mctc-lib]
git = "https://github.com/grimme-lab/mctc-lib"
```
