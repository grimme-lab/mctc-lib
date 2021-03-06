# This file is part of mctc-lib.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

cmake_minimum_required(VERSION 3.9)
get_directory_property(is-subproject PARENT_DIRECTORY)

project(
  "mctc-lib"
  LANGUAGES "Fortran"
  VERSION "0.2.1"
  DESCRIPTION "Modular computation tool chain"
)

# Follow GNU conventions for installing directories
include(GNUInstallDirs)

# General configuration information
add_subdirectory("config")

# Collect source of the project
set(srcs)
add_subdirectory("src")

# MCTC library target
add_library(
  "${PROJECT_NAME}-lib"
  "${srcs}"
)
set_target_properties(
  "${PROJECT_NAME}-lib"
  PROPERTIES
  POSITION_INDEPENDENT_CODE TRUE
  OUTPUT_NAME "${PROJECT_NAME}"
  VERSION "${PROJECT_VERSION}"
  SOVERSION "${PROJECT_VERSION_MAJOR}"
  Fortran_MODULE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/include"
)
target_include_directories(
  "${PROJECT_NAME}-lib"
  INTERFACE
  $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/include>
  $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>
)

# Add example application
add_subdirectory("app")

if(NOT is-subproject OR BUILD_SHARED_LIBS)
  # Export targets for other projects
  add_library("${PROJECT_NAME}" INTERFACE)
  target_link_libraries("${PROJECT_NAME}" INTERFACE "${PROJECT_NAME}-lib")
  install(
    TARGETS
    "${PROJECT_NAME}"
    "${PROJECT_NAME}-lib"
    EXPORT
    "${PROJECT_NAME}-targets"
    LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}"
    ARCHIVE DESTINATION "${CMAKE_INSTALL_LIBDIR}"
  )
  install(
    EXPORT
    "${PROJECT_NAME}-targets"
    NAMESPACE
    "${PROJECT_NAME}::"
    DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME}"
  )
  install(
    DIRECTORY
    "${CMAKE_CURRENT_BINARY_DIR}/include/"
    DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}"
  )
  # Package license files
  install(
    FILES
    "LICENSE"
    DESTINATION "${CMAKE_INSTALL_DATADIR}/licenses/${PROJECT_NAME}"
  )
else()
  # Expose license files to parent project
  set(
    "${PROJECT_NAME}-lic"
    "${CMAKE_CURRENT_SOURCE_DIR}/LICENSE-Apache"
    "${CMAKE_CURRENT_SOURCE_DIR}/LICENSE-MIT"
    PARENT_SCOPE
  )
endif()

# add the testsuite
enable_testing()
add_subdirectory("test")
