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

add_subdirectory("read")
add_subdirectory("structure")
add_subdirectory("write")

set(dir "${CMAKE_CURRENT_SOURCE_DIR}")

list(
  APPEND srcs
  "${dir}/codata2018.f90"
  "${dir}/constants.f90"
  "${dir}/convert.f90"
  "${dir}/filetype.f90"
  "${dir}/math.f90"
  "${dir}/read.f90"
  "${dir}/resize.f90"
  "${dir}/structure.f90"
  "${dir}/symbols.f90"
  "${dir}/utils.f90"
  "${dir}/write.f90"
)

set(srcs "${srcs}" PARENT_SCOPE)
