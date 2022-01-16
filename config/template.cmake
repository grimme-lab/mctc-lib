@PACKAGE_INIT@

set("@PROJECT_NAME@_WITH_OpenMP" @WITH_OpenMP@)
set("@PROJECT_NAME@_WITH_JSON" @WITH_JSON@)

if(NOT TARGET "@PROJECT_NAME@::@PROJECT_NAME@")
  include("${CMAKE_CURRENT_LIST_DIR}/@PROJECT_NAME@-targets.cmake")

  include(CMakeFindDependencyMacro)

  if(NOT TARGET "OpenMP::OpenMP_Fortran" AND "@PROJECT_NAME@_WITH_OpenMP")
    find_dependency("OpenMP")
  endif()

  if(NOT TARGET "jsonfortran::jsonfortran" AND "@PROJECT_NAME@_WITH_JSON")
    find_dependency("jsonfortran")
  endif()
endif()
