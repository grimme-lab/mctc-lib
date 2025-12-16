# AGENTS.md - Development Guide for mctc-lib

This document provides guidance for AI agents and contributors working with the mctc-lib codebase.

## Project Overview

**mctc-lib** (Modular Computation Tool Chain Library) is a Fortran library providing unified molecular structure data handling and geometry file format I/O for computational chemistry applications.

### Supported Geometry Formats

The library supports reading and writing 12+ geometry formats:

*General ASCII formats:*
- **xyz** - Xmol/xyz files (`.xyz`, `.log`)
- **pdb** - Protein Data Bank format (`.pdb`)
- **mol/sdf** - MDL connection table files (`.mol`, `.sdf`)

*JSON-based formats:*
- **QCSchema** - MolSSI QCSchema JSON (`.qcjson`, `.json`)
- **Chemical JSON** - Avogadro Chemical JSON (`.cjson`, `.json`)
- **Pymatgen JSON** - Pymatgen Molecule/Structure (`.pmgjson`, `.json`)

*Program-specific formats:*
- **Turbomole coord** - Turbomole/riper coordinates (`.tmol`, `.coord`)
- **VASP POSCAR** - VASP geometry files (`.vasp`, `.poscar`, `.contcar`)
- **DFTB+ gen** - DFTB+ genFormat (`.gen`)
- **Gaussian external** - Gaussian external program input (`.ein`)
- **FHI-aims** - FHI-aims geometry input (`geometry.in`)
- **Q-Chem** - Q-Chem molecule block (`.qchem`)

## Repository Structure

```
mctc-lib/
├── src/mctc/           # Main library source code
│   ├── io/             # I/O module (readers/writers for all formats)
│   │   ├── read/       # Format-specific readers
│   │   └── write/      # Format-specific writers
│   ├── env/            # Environment module (error handling, testing)
│   ├── data/           # Element data (radii, electronegativities)
│   └── ncoord/         # Coordination number utilities
├── app/                # Application code (mctc-convert tool)
├── test/               # Unit test suite
├── doc/                # FORD documentation pages (format descriptions)
├── man/                # Manual pages (asciidoc format)
├── include/            # Public include files
├── config/             # Build configuration scripts
└── subprojects/        # Meson wrap dependencies (jonquil, toml-f, test-drive)
```

## Build Systems

This project supports three build systems. Choose based on your workflow:

### Meson (Recommended)

```bash
# Configure
meson setup _build

# Build
meson compile -C _build

# Test
meson test -C _build --print-errorlogs

# Install
meson install -C _build
```

**Note**: Meson version 1.8.0 has a known bug and is explicitly unsupported. Use any other version ≥ 0.55.

### CMake

```bash
# Configure
cmake -B _build -G Ninja

# Build
cmake --build _build

# Test
cd _build && ctest && cd ..
```

### fpm (Fortran Package Manager)

```bash
# Build
fpm build

# Test
fpm test

# Run application
fpm run -- --help
```

## Dependencies

- **jonquil** (v0.3.0 or later) - JSON parsing (optional, enables JSON format support)
- **toml-f** (v0.4.3 or later) - TOML parsing (dependency of jonquil)
- **test-drive** - Testing framework (test dependency only)

Dependencies are managed via Meson subprojects (wrap files), CMake find modules, or fpm.

## Testing

### Running Tests

Tests are organized by functionality in `test/`:

- `test_read_*.f90` - Reader tests for each format
- `test_write_*.f90` - Writer tests for each format
- `test_math.f90` - Mathematical utility tests
- `test_ncoord.f90` - Coordination number tests
- `test_symbols.f90` - Element symbol conversion tests

```bash
# Run all tests with meson
meson test -C _build --print-errorlogs --suite mctc-lib

# Run specific test
meson test -C _build test_read_xyz --print-errorlogs
```

### Writing Tests

Tests use the `mctc_env_testing` module:

```fortran
use mctc_env_testing, only : new_unittest, unittest_type, error_type, check

subroutine collect_my_tests(testsuite)
   type(unittest_type), allocatable, intent(out) :: testsuite(:)
   testsuite = [ &
      & new_unittest("test-name", test_procedure), &
      & new_unittest("expected-fail", test_fail, should_fail=.true.) &
   ]
end subroutine
```

## Code Style and Conventions

### Fortran Style

- Use Fortran 2008 standard features
- Free-form source format (`.f90`, `.F90` for preprocessed)
- Module names: `mctc_<subsystem>_<component>` (e.g., `mctc_io_read`)
- Private by default, explicitly export public entities
- Use `implicit none` in all program units

### File Header

All source files must include the Apache-2.0 license header:

```fortran
! This file is part of mctc-lib.
!
! Licensed under the Apache License, Version 2.0 (the "License");
! ...
```

### Error Handling

Use the `error_type` for error propagation:

```fortran
use mctc_env, only : error_type, fatal_error

subroutine my_routine(result, error)
   type(error_type), allocatable, intent(out) :: error
   
   if (some_error_condition) then
      call fatal_error(error, "Descriptive error message")
      return
   end if
end subroutine
```

### Documentation

- Use FORD-compatible docstrings (`!>` for preceding, `!<` for trailing)
- Document all public interfaces
- Run `ford docs.md` to generate documentation

## CI/CD Workflow

### GitHub Actions

The CI pipeline (`.github/workflows/build.yml`) runs on push and pull requests:

- **Platforms**: Ubuntu, macOS
- **Compilers**: GCC (10, 11, 12, 14), Intel oneAPI (2021)
- **Build systems**: Meson, CMake, fpm
- **Coverage**: Collected with GCC 11 and uploaded to Codecov

### DCO (Developer Certificate of Origin)

Contributions require sign-off. The DCO bot checks all commits. Sign your commits:

```bash
git commit -s -m "Your commit message"
```

### Documentation Deployment

Documentation is built with FORD and deployed to GitHub Pages on:
- Pushes to `main` branch
- Tagged releases

## Adding New Features

### Adding a New File Format

1. Create reader in `src/mctc/io/read/` (e.g., `format.f90`)
2. Create writer in `src/mctc/io/write/` (e.g., `format.f90`)
3. Add filetype enum in `src/mctc/io/filetype.f90`
4. Register in `src/mctc/io/read.f90` and `src/mctc/io/write.f90`
5. Update meson.build and CMakeLists.txt in relevant directories
6. Add tests in `test/test_read_format.f90` and `test/test_write_format.f90`
7. Document in `doc/format-<name>.md`

### Core Data Types

- `structure_type` (`mctc_io_structure`) - Main molecular structure container
- `error_type` (`mctc_env`) - Error handling type

## Common Tasks

### Convert Geometry Files

```bash
# Using the mctc-convert tool
meson compile -C _build
./_build/app/mctc-convert input.xyz output.mol
```

### Read Structure in Your Code

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

## Build Options

### Meson Options (`meson_options.txt`)

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `openmp` | boolean | false | Enable OpenMP parallelization |
| `json` | feature | auto | Enable JSON format support |

```bash
meson setup _build -Djson=enabled -Dopenmp=true
```

### CMake Options

| Option | Description |
|--------|-------------|
| `WITH_JSON` | Enable JSON support |
| `WITH_OpenMP` | Enable OpenMP |

## Troubleshooting

### Common Issues

1. **Meson 1.8.0 error**: Upgrade or downgrade meson (`pip install meson!=1.8.0`)

2. **JSON tests fail**: Ensure jonquil dependency is available or disable with `-Djson=disabled`

3. **Missing Fortran compiler**: Set `FC` environment variable:
   ```bash
   FC=gfortran meson setup _build
   ```

## External Resources

- [GitHub Repository](https://github.com/grimme-lab/mctc-lib)
- [API Documentation](https://grimme-lab.github.io/mctc-lib)
- [Issue Tracker](https://github.com/grimme-lab/mctc-lib/issues)

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make changes following the code style guidelines
4. Add tests for new functionality
5. Ensure all tests pass
6. Sign off commits (DCO requirement)
7. Submit a pull request

Error messages in mctc-lib are designed to be helpful with source location information.
If you encounter unclear error messages, please report them as bugs.
