# This file is part of mctc-lib.
#
# Licensed under the Apache License, Version 2.0 (the "License"); you may not
# use this file except in compliance with the License. You may obtain a copy of
# the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations under
# the License.

cmake_minimum_required(VERSION 3.24)

project(
  "mctc-lib"
  LANGUAGES "Fortran"
  VERSION "0.6.1"
  DESCRIPTION "Modular computation tool chain")

# Follow GNU conventions for installing directories
include(GNUInstallDirs)

# Build options
option(BUILD_SHARED_LIBS "Whether the libraries built should be shared" ON)
option(CMAKE_POSITION_INDEPENDENT_CODE
       "Enable position independent code support" ON)
# Unprefixed options are kept for compatibility and only provide the defaults
option(
  WITH_OpenMP
  "Enable support for shared memory parallelisation with OpenMP (deprecated, use MCTCLIB_WITH_OpenMP)"
  ON)
option(
  WITH_JSON
  "Enable support for JSON parsing via jonquil (deprecated, use MCTCLIB_WITH_JSON)"
  OFF)
option(MCTCLIB_WITH_OpenMP
       "Enable support for shared memory parallelisation with OpenMP"
       ${WITH_OpenMP})
option(MCTCLIB_WITH_JSON "Enable support for JSON parsing via jonquil"
       ${WITH_JSON})

set(MCTCLIB_USE_JONQUIL ${MCTCLIB_WITH_JSON})
set(MCTCLIB_USE_TOMLF ${MCTCLIB_WITH_JSON})

# Set build type as CMake does not provide defaults
if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  set(CMAKE_BUILD_TYPE
      "RelWithDebInfo"
      CACHE STRING "Build type to be used." FORCE)
  message(
    STATUS "Setting build type to '${CMAKE_BUILD_TYPE}' as none was specified.")
endif()

# Compiler-specific configurations
if("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "PGI"
   OR "${CMAKE_Fortran_COMPILER_ID}" STREQUAL "NVHPC"
   OR "${CMAKE_Fortran_COMPILER_ID}" STREQUAL "Flang")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -Mbackslash -Mallocatable=03")
endif()

# Install location of the generated Fortran module files
set("${PROJECT_NAME}-module-dir"
    "${PROJECT_NAME}/modules"
    CACHE STRING "Subdirectory to install generated module files to")
set(module-dir "${${PROJECT_NAME}-module-dir}")

# Dependencies

if(MCTCLIB_WITH_OpenMP)
  find_package(OpenMP REQUIRED COMPONENTS Fortran)
endif()

if(MCTCLIB_WITH_JSON)
  include(FetchContent)
  function(declare_dep name url tag)
    if(EXISTS "${PROJECT_SOURCE_DIR}/subprojects/${name}/CMakeLists.txt")
      FetchContent_Declare(
        "${name}" SOURCE_DIR "${PROJECT_SOURCE_DIR}/subprojects/${name}"
                  FIND_PACKAGE_ARGS CONFIG)
    else()
      FetchContent_Declare(
        "${name}"
        GIT_REPOSITORY "${url}"
        GIT_TAG "${tag}"
        GIT_SHALLOW TRUE
        FIND_PACKAGE_ARGS CONFIG)
    endif()
  endfunction()

  declare_dep("toml-f" "https://github.com/toml-f/toml-f" "v0.5.1")
  declare_dep("jonquil" "https://github.com/toml-f/jonquil" "v0.3.1")

  # toml-f first, so jonquil reuses the same toml-f::toml-f target
  FetchContent_MakeAvailable("toml-f" "jonquil")
endif()

# MCTC library target
add_subdirectory("src")

# Add example application
add_subdirectory("app")

# Export targets for other projects
install(TARGETS "${PROJECT_NAME}" "${PROJECT_NAME}-lib"
        EXPORT "${PROJECT_NAME}-targets")
install(
  EXPORT "${PROJECT_NAME}-targets"
  NAMESPACE "${PROJECT_NAME}::"
  DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME}")
install(DIRECTORY "${PROJECT_BINARY_DIR}/include/"
        DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}/${module-dir}")
# Package license files
install(FILES "LICENSE"
        DESTINATION "${CMAKE_INSTALL_DATADIR}/licenses/${PROJECT_NAME}")

# CMake package configuration
include(CMakePackageConfigHelpers)
configure_package_config_file(
  "${PROJECT_SOURCE_DIR}/config/template-config.cmake"
  "${PROJECT_BINARY_DIR}/${PROJECT_NAME}-config.cmake"
  INSTALL_DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME}")
write_basic_package_version_file(
  "${PROJECT_BINARY_DIR}/${PROJECT_NAME}-config-version.cmake"
  VERSION "${PROJECT_VERSION}"
  COMPATIBILITY SameMinorVersion)
install(FILES "${PROJECT_BINARY_DIR}/${PROJECT_NAME}-config.cmake"
              "${PROJECT_BINARY_DIR}/${PROJECT_NAME}-config-version.cmake"
        DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME}")

# pkg-config package configuration
if(BUILD_SHARED_LIBS)
  set(PKG_CONFIG_REQUIRES "Requires.private")
else()
  set(PKG_CONFIG_REQUIRES "Requires")
endif()

if(MCTCLIB_WITH_JSON)
  set(PKG_CONFIG_REQUIREMENTS "jonquil toml-f")
else()
  set(PKG_CONFIG_REQUIREMENTS)
endif()

configure_file("${PROJECT_SOURCE_DIR}/config/template.pc"
               "${PROJECT_BINARY_DIR}/${PROJECT_NAME}.pc" @ONLY)
install(FILES "${PROJECT_BINARY_DIR}/${PROJECT_NAME}.pc"
        DESTINATION "${CMAKE_INSTALL_LIBDIR}/pkgconfig")

# Add the testsuite
if(PROJECT_IS_TOP_LEVEL)
  enable_testing()
  add_subdirectory("test")
endif()
