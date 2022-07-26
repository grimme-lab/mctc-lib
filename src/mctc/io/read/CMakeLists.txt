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

set(dir "${CMAKE_CURRENT_SOURCE_DIR}")

list(
  APPEND srcs
  "${dir}/aims.f90"
  "${dir}/cjson.F90"
  "${dir}/ctfile.f90"
  "${dir}/gaussian.f90"
  "${dir}/genformat.f90"
  "${dir}/qchem.f90"
  "${dir}/qcschema.F90"
  "${dir}/pdb.f90"
  "${dir}/turbomole.f90"
  "${dir}/vasp.f90"
  "${dir}/xyz.f90"
)

set(srcs "${srcs}" PARENT_SCOPE)
