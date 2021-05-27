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

cmake_minimum_required(VERSION 3.14)

project(
  "mctc-convert"
  LANGUAGES "Fortran"
  VERSION "0.2.3"
  DESCRIPTION "Example project for converting geometry inputs"
)

if(NOT TARGET "mctc-lib::mctc-lib")
  find_package("mctc-lib")
endif()

add_executable(
  "${PROJECT_NAME}"
  "../app/main.f90"
)
target_link_libraries(
  "${PROJECT_NAME}"
  PRIVATE
  "mctc-lib::mctc-lib"
)

install(
  TARGETS
  "${PROJECT_NAME}"
  DESTINATION
  "${CMAKE_INSTALL_BINDIR}"
)
