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

# Unit testing
set(
  tests
  "cutoff"
  "data"
  "math"
  "ncoord"
  "read"
  "read-aims"
  "read-cjson"
  "read-ctfile"
  "read-gaussian"
  "read-genformat"
  "read-pdb"
  "read-qchem"
  "read-qcschema"
  "read-turbomole"
  "read-vasp"
  "read-xyz"
  "symbols"
  "write"
  "write-aims"
  "write-cjson"
  "write-ctfile"
  "write-gaussian"
  "write-genformat"
  "write-pdb"
  "write-qchem"
  "write-qcschema"
  "write-turbomole"
  "write-vasp"
  "write-xyz"
)
set(
  test-srcs
  "main.f90"
  "testsuite_structure.f90"
)
foreach(t IN LISTS tests)
  string(MAKE_C_IDENTIFIER ${t} t) 
  list(APPEND test-srcs "test_${t}.f90")
endforeach()

add_executable(
  "${PROJECT_NAME}-tester"
  "${test-srcs}"
)
target_link_libraries(
  "${PROJECT_NAME}-tester"
  PRIVATE
  "${PROJECT_NAME}-lib"
)

foreach(t IN LISTS tests)
  add_test("${PROJECT_NAME}/${t}" "${PROJECT_NAME}-tester" "${t}")
endforeach()
