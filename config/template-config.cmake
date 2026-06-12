@PACKAGE_INIT@

set(MCTCLIB_WITH_OpenMP @MCTCLIB_WITH_OpenMP@)
set(MCTCLIB_WITH_JSON @MCTCLIB_WITH_JSON@)
set(MCTCLIB_USE_JONQUIL @MCTCLIB_USE_JONQUIL@)
set(MCTCLIB_USE_TOMLF @MCTCLIB_USE_TOMLF@)

enable_language("Fortran")

if(NOT TARGET "@PROJECT_NAME@::@PROJECT_NAME@")
  include("${CMAKE_CURRENT_LIST_DIR}/@PROJECT_NAME@-targets.cmake")
  include(CMakeFindDependencyMacro)

  if(NOT TARGET "OpenMP::OpenMP_Fortran" AND MCTCLIB_WITH_OpenMP)
    find_dependency("OpenMP")
  endif()

  if(NOT TARGET "tomlf::tomlf" AND MCTCLIB_USE_TOMLF)
    find_dependency("tomlf")
  endif()

  if(NOT TARGET "jonquil::jonquil" AND MCTCLIB_USE_JONQUIL)
    find_dependency("jonquil")
  endif()
endif()
