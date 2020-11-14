# Modular computation tool chain library

[![Apache-2.0](https://img.shields.io/github/license/grimme-lab/mctc-lib)](LICENSE)
[![CI](https://github.com/grimme-lab/mctc-lib/workflows/CI/badge.svg)](https://github.com/grimme-lab/mctc-lib/actions)
[![docs](https://github.com/grimme-lab/mctc-lib/workflows/docs/badge.svg)](https://grimme-lab.github.io/mctc-lib)
[![codecov](https://codecov.io/gh/grimme-lab/mctc-lib/branch/main/graph/badge.svg)](https://codecov.io/gh/grimme-lab/mctc-lib)


## Installation

To build this project from the source code in this repository you need to have
- a Fortran compiler supporting Fortran 2008
- [meson](https://mesonbuild.com) version 0.53 or newer
- a build-system backend, *i.e.* [ninja](https://ninja-build.org) version 1.7 or newer

Setup a build with

```
meson setup _build
```

You can select the Fortran compiler by the `FC` environment variable, currently this project supports GCC and Intel compilers.
To compile the project run

```
meson compile -C _build
```

You can run the projects testsuite with

```
meson test -C _build --print-errorlogs
```


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
