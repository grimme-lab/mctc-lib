project(
  'jsonfortran',
  'Fortran',
  version: files('.VERSION'),
)

fc = meson.get_compiler('fortran')
if fc.get_id() == 'intel-cl'
  add_project_arguments(
    '-fpp',
    language: 'fortran',
  )
endif

jsonfortran_lib = library(
  meson.project_name(),
  sources: files(
    'src/json_kinds.F90',
    'src/json_parameters.F90',
    'src/json_string_utilities.F90',
    'src/json_value_module.F90',
    'src/json_file_module.F90',
    'src/json_module.F90',
  ),
  include_directories: include_directories('src'),
)

jsonfortran_dep = declare_dependency(
  link_with: jsonfortran_lib,
  include_directories: jsonfortran_lib.private_dir_include(),
)

install_data(
  'LICENSE',
  install_dir: get_option('datadir')/'licenses'/'mctc-lib'/meson.project_name()
)
