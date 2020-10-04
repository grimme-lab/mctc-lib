---
project: MCTC-library
summary: Modular computation tool chain library
project_github: https://github.com/grimme-lab/mctc-lib
project_download: https://github.com/grimme-lab/mctc-lib/releases
author: Grimme group, Bonn
github: https://github.com/grimme-lab
src_dir: ./src
         ./app
output_dir: ./docs
exclude_dir: ./test
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

Common tool chain to molecular structure data in various applications developed in our group.


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
