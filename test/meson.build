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

tests = [
  'math',
  'read',
  'read-aims',
  'read-cjson',
  'read-ctfile',
  'read-gaussian',
  'read-genformat',
  'read-pdb',
  'read-qchem',
  'read-qcschema',
  'read-turbomole',
  'read-vasp',
  'read-xyz',
  'symbols',
  'write',
  'write-aims',
  'write-cjson',
  'write-ctfile',
  'write-gaussian',
  'write-genformat',
  'write-pdb',
  'write-qchem',
  'write-turbomole',
  'write-vasp',
  'write-xyz',
]

test_srcs = files(
  'main.f90',
  'testsuite_structure.f90',
)
foreach t : tests
  test_srcs += files('test_@0@.f90'.format(t.underscorify()))
endforeach

tester = executable(
  'tester',
  sources: test_srcs,
  dependencies: mctc_dep,
)

foreach t : tests
  test(t, tester, args: t)
endforeach
