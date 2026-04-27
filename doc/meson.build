ver_parts = meson.project_version().split('.')

doc_conf = configuration_data()
doc_conf.merge_from(teaconf)
doc_conf.set('PATCHLEVEL_STR', meson.project_version())
doc_conf.set('VERSION', ver_parts[0] + '.' + ver_parts[1])

configure_file(
  input:          'tclreadline.n.in',
  output:         'tclreadline.n',
  configuration:  doc_conf,
  install:        true,
  install_dir:    get_option('mandir') / 'mann',
)
