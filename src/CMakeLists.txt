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

# Collect source of the project
set(srcs)
add_subdirectory("mctc")

add_library(
  "${PROJECT_NAME}-lib"
  "${srcs}"
)
add_library("${PROJECT_NAME}::${PROJECT_NAME}-lib" ALIAS "${PROJECT_NAME}-lib")

# Sources test the value with #if, therefore it must always be defined as 0 or 1
target_compile_definitions(
  "${PROJECT_NAME}-lib"
  PRIVATE
  "WITH_JSON=$<BOOL:${MCTCLIB_WITH_JSON}>"
)
target_link_libraries(
  "${PROJECT_NAME}-lib"
  PRIVATE
  "$<$<BOOL:${MCTCLIB_WITH_JSON}>:jonquil::jonquil>"
  "$<$<BOOL:${MCTCLIB_WITH_JSON}>:toml-f::toml-f>"
  "$<$<BOOL:${MCTCLIB_WITH_OpenMP}>:OpenMP::OpenMP_Fortran>"
)
set_target_properties(
  "${PROJECT_NAME}-lib"
  PROPERTIES
  OUTPUT_NAME "${PROJECT_NAME}"
  VERSION "${PROJECT_VERSION}"
  SOVERSION "${PROJECT_VERSION_MAJOR}"
  Fortran_MODULE_DIRECTORY "${PROJECT_BINARY_DIR}/include"
)
target_include_directories(
  "${PROJECT_NAME}-lib"
  PUBLIC
  $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/include>
  $<BUILD_INTERFACE:${PROJECT_BINARY_DIR}/include>
  $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>
  $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}/${module-dir}>
)
if(NOT EXISTS "${PROJECT_BINARY_DIR}/include")
  file(MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/include")
endif()

# Public target name used by consumers
add_library("${PROJECT_NAME}" INTERFACE)
add_library("${PROJECT_NAME}::${PROJECT_NAME}" ALIAS "${PROJECT_NAME}")
target_link_libraries("${PROJECT_NAME}" INTERFACE "${PROJECT_NAME}-lib")
