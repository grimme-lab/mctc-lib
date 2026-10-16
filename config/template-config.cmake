@PACKAGE_INIT@

set(MCTCLIB_WITH_OpenMP @MCTCLIB_WITH_OpenMP@)
set(MCTCLIB_WITH_JSON @MCTCLIB_WITH_JSON@)
set(MCTCLIB_USE_JONQUIL @MCTCLIB_USE_JONQUIL@)
set(MCTCLIB_USE_TOMLF @MCTCLIB_USE_TOMLF@)

enable_language("Fortran")

if(NOT TARGET "@PROJECT_NAME@::@PROJECT_NAME@")
  include(CMakeFindDependencyMacro)

  if(MCTCLIB_WITH_OpenMP)
    find_dependency("OpenMP" COMPONENTS Fortran)
  endif()

  if(@MCTCLIB_USE_TOMLF@)
    find_dependency("toml-f")
  endif()

  if(@MCTCLIB_USE_JONQUIL@)
    find_dependency("jonquil")
  endif()

  include("${CMAKE_CURRENT_LIST_DIR}/@PROJECT_NAME@-targets.cmake")
endif()
