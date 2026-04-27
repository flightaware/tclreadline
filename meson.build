project('tclreadline', 'c',
  version:          '2.4.1',
  default_options:  ['c_std=c23,c2x,c17,c11', 'warning_level=2'],
  meson_version:    '>=1.3.0',
)

cc = meson.get_compiler('c')
fs = import('fs')
add_project_arguments('-D_DEFAULT_SOURCE', language: 'c')

if get_option('debug')
  add_project_arguments(cc.get_supported_arguments(['-ggdb3']), language: 'c')
endif

add_project_arguments('-Wno-missing-field-initializers', language: 'c')

conf = configuration_data()
add_project_arguments('-DHAVE_CONFIG_H', language: 'c')

deps = []
inc = [include_directories('generic')]

pkg_sources = files(
  'generic/tclreadline.c',
  'generic/tclreadlineBuildInfo.c',
)

# --- Teabase init (provides tcl_dep, pkg_dir, tclsh, teaconf) ---
subdir('teabase/init')

# --- Readline dependency ---
# static: true so pkg-config consults Libs.private / Requires.private (e.g.
# ncursesw, tinfo).  Without it, a static-only libreadline.a packaging links
# without its termcap deps and the resulting .so fails to dlopen with
# "undefined symbol: UP".  When a shared libreadline.so is available the
# linker still picks it; the only effect is a couple of redundant DT_NEEDED
# entries that libreadline.so itself would have pulled in anyway.
readline_dep = dependency('readline', required: true, static: true)
deps += readline_dep

# --- Version string ---
ver_parts = meson.project_version().split('.')
conf.set_quoted('TCLRL_VERSION_STR', ver_parts[0] + '.' + ver_parts[1])

# --- Feature detection ---
conf.set10('HAVE_STRING_H', cc.has_header('string.h'))
conf.set10('HAVE_SIGACTION', cc.has_function('sigaction'))

rl_check_deps = [readline_dep, tcl_dep]

# Check for rl_executing_macro
if cc.links('''
    #include <stdio.h>
    #include <readline/readline.h>
    extern char* rl_executing_macro;
    int main(void) { rl_executing_macro = (char*) 0; return 0; }
''', dependencies: rl_check_deps, name: 'rl_executing_macro')
  conf.set('EXECUTING_MACRO_NAME', 'rl_executing_macro')
elif cc.links('''
    #include <stdio.h>
    #include <readline/readline.h>
    extern char* _rl_executing_macro;
    int main(void) { _rl_executing_macro = (char*) 0; return 0; }
''', dependencies: rl_check_deps, name: '_rl_executing_macro')
  conf.set10('EXECUTING_MACRO_HACK', true)
  conf.set('EXECUTING_MACRO_NAME', '_rl_executing_macro')
endif

conf.set10('CLEANUP_AFTER_SIGNAL', cc.links('''
    #include <stdio.h>
    #include <readline/readline.h>
    extern void rl_cleanup_after_signal(void);
    int main(void) { rl_cleanup_after_signal(); return 0; }
''', dependencies: rl_check_deps, name: 'rl_cleanup_after_signal'))

conf.set10('EXTEND_LINE_BUFFER', cc.links('''
    #include <stdio.h>
    #include <readline/readline.h>
    extern void rl_extend_line_buffer(int len);
    int main(void) { rl_extend_line_buffer(1); return 0; }
''', dependencies: rl_check_deps, name: 'rl_extend_line_buffer'))

conf.set10('HAVE_RL_LIBRARY_VERSION', cc.links('''
    #include <stdio.h>
    #include <readline/readline.h>
    extern const char *rl_library_version;
    int main(void) { const char *v = rl_library_version; (void)v; return 0; }
''', dependencies: rl_check_deps, name: 'rl_library_version'))

# --- Generate tclreadlineUuid.h from git ---
git = find_program('git', native: true, required: false)

uuid_val = 'unknown'
if git.found()
  git_hash = run_command(git, '-C', meson.project_source_root(),
    'rev-parse', 'HEAD', check: false)
  if git_hash.returncode() == 0
    uuid_val = 'git-' + git_hash.stdout().strip()
  endif
endif

uuid_conf = configuration_data()
uuid_conf.set('TCLREADLINE_VERSION_UUID', uuid_val)

configure_file(
  output: 'tclreadlineUuid.h',
  configuration: uuid_conf,
  input: 'tclreadlineUuid.h.in',
)

# Build root has generated tclreadlineUuid.h and config.h
inc += include_directories('.')

# =====================================================================
# Standalone binaries (tclshrl / wishrl)
#
# Detection must happen before subdir('teabase') which freezes conf
# by generating config.h.  Build targets are created afterwards.
# =====================================================================

build_tclshrl = get_option('tclshrl')
build_wishrl  = get_option('wishrl')
embed_method  = 'none'

if build_tclshrl or build_wishrl
  # Embedding method IDs (must match shrl.c)
  METHOD_C23_EMBED  = 10
  METHOD_ZIPFS      = 11
  METHOD_OBJCOPY    = 12
  METHOD_CHAR_ARRAY = 13
  METHOD_FINDLIB    = 14

  conf.set('EMBED_METHOD_C23_EMBED',   METHOD_C23_EMBED)
  conf.set('EMBED_METHOD_ZIPFS',       METHOD_ZIPFS)
  conf.set('EMBED_METHOD_OBJCOPY',     METHOD_OBJCOPY)
  conf.set('EMBED_METHOD_CHAR_ARRAY',  METHOD_CHAR_ARRAY)
  conf.set('EMBED_METHOD_FINDLIBRARY', METHOD_FINDLIB)

  # --- Detect available embedding methods ---

  # C23 #embed
  have_c23_embed = cc.compiles('''
      const char embedded_data[] = {
      #embed "''' + meson.current_source_dir() / 'library/tclreadlineSetup.tcl' + '''"
          ,0
      };
      int main(void) { return sizeof(embedded_data) < 2; }
  ''', dependencies: rl_check_deps, name: 'C23 #embed')

  # Tcl_StaticLibrary vs Tcl_StaticPackage shim
  if not cc.links('''
      #undef USE_TCL_STUBS
      #include <tcl.h>
      int main(void) { Tcl_StaticLibrary(NULL, NULL, NULL, NULL); return 0; }
  ''', dependencies: [tcl_dep], name: 'Tcl_StaticLibrary')
    conf.set('Tcl_StaticLibrary', 'Tcl_StaticPackage')
  endif

  # Zipfs support in Tcl library
  have_zipfs = cc.links('''
      #undef USE_TCL_STUBS
      #include <tcl.h>
      int main(void) { TclZipfs_Mount(NULL, NULL, NULL, NULL); return 0; }
  ''', dependencies: [tcl_dep], name: 'zipfs support')
  conf.set10('HAVE_ZIPFS', have_zipfs)

  # Check if build tclsh supports zipfs
  have_build_tclsh_zipfs = false
  if have_zipfs and is_variable('build_tclsh') and build_tclsh.found()
    r = run_command(build_tclsh, '-e', 'zipfs root', check: false)
    have_build_tclsh_zipfs = r.returncode() == 0
  endif

  # Objcopy embedding
  objcopy = find_program('objcopy', required: false, native: true)
  have_objcopy = objcopy.found()

  # char_array needs a build tclsh
  have_build_tclsh = is_variable('build_tclsh') and build_tclsh.found()

  # --- Select embedding method ---
  initscripts_method = get_option('initscripts_method')

  if initscripts_method == 'auto'
    if have_c23_embed
      embed_method = 'c23_embed'
    elif have_zipfs and have_build_tclsh_zipfs
      embed_method = 'zipfs'
    elif have_objcopy
      embed_method = 'objcopy'
    elif have_build_tclsh
      embed_method = 'char_array'
    else
      embed_method = 'tcl_findLibrary'
    endif
  else
    embed_method = initscripts_method
  endif

  embed_method_ids = {
    'c23_embed':       METHOD_C23_EMBED,
    'zipfs':           METHOD_ZIPFS,
    'objcopy':         METHOD_OBJCOPY,
    'char_array':      METHOD_CHAR_ARRAY,
    'tcl_findLibrary': METHOD_FINDLIB,
  }
  conf.set('USE_EMBED_METHOD', embed_method_ids[embed_method])
  message('Init script embedding method: ' + embed_method)
endif

# --- Tcl library scripts ---
teaconf_files = files(
  'library/tclreadlineSetup.tcl',
  'library/tclreadlineCompleter.tcl',
)

# --- Teabase main (builds shared module, tests, config.h) ---
subdir('teabase')

# =====================================================================
# Build standalone binary targets (after teabase, using frozen config)
# =====================================================================

if build_tclshrl or build_wishrl
  nostubs_args = ['-DNOSTUBS']

  # Full Tcl library dep (not stubs) for standalone linking
  tcl_full_dep = dependency('tcl')
  shrl_deps = [tcl_full_dep, readline_dep]

  # NOSTUBS-compiled package objects as a static convenience library
  nostubs_lib = static_library('tclreadline_nostubs',
    pkg_sources,
    c_args: nostubs_args,
    include_directories: inc,
    dependencies: shrl_deps,
  )

  # Embedded script artifacts (method-dependent)
  embed_objs = []   # .o files (objcopy)
  embed_srcs = []   # .c files (char_array)

  if embed_method == 'objcopy'
    python3 = find_program('python3')
    embed_objcopy = files('tools/embed_objcopy.py')
    foreach script : ['tclreadlineSetup.tcl', 'tclreadlineCompleter.tcl']
      sym = '_TclReadlineInitScript_' + script.replace('.', '_')
      embed_objs += custom_target('embed_' + script,
        output: '_embed_' + script + '.o',
        input: 'library' / script,
        command: [python3, embed_objcopy,
          cc.cmd_array()[0], objcopy, '@INPUT@', '@OUTPUT@', sym],
      )
    endforeach
  elif embed_method == 'char_array'
    foreach script : ['tclreadlineSetup.tcl', 'tclreadlineCompleter.tcl']
      embed_srcs += custom_target('embed_c_' + script,
        output: '_embed_char_array_' + script + '.c',
        input: 'library' / script,
        command: [
          build_tclsh,
          meson.current_source_dir() / 'tools/to_char_array_literal.tcl',
          script,
        ],
        feed: true,
        capture: true,
      )
    endforeach
  endif

  tcl_script_files = files(
    'library/tclreadlineSetup.tcl',
    'library/tclreadlineCompleter.tcl',
  )

  if build_tclshrl
    tclshrl = executable('tclshrl',
      'generic/shrl.c', embed_srcs,
      link_with: nostubs_lib,
      objects: embed_objs,
      c_args: nostubs_args,
      include_directories: inc,
      dependencies: shrl_deps,
      install: true,
    )
    if embed_method == 'zipfs'
      custom_target('tclshrl_zipfs',
        output: 'tclshrl_zipfs.stamp',
        input: tcl_script_files,
        depends: tclshrl,
        command: [
          build_tclsh,
          meson.current_source_dir() / 'tools/append_init_scripts.tcl',
          tclshrl.full_path(),
          tclshrl.full_path(),
          'library/tclreadlineSetup.tcl library/tclreadlineCompleter.tcl',
        ],
        build_by_default: true,
      )
    endif
  endif

  if build_wishrl
    tk_dep = dependency('tk', required: true)
    wishrl = executable('wishrl',
      'generic/shrl.c', embed_srcs,
      link_with: nostubs_lib,
      objects: embed_objs,
      c_args: nostubs_args + ['-DIS_WISHRL=1'],
      include_directories: inc,
      dependencies: shrl_deps + [tk_dep],
      install: true,
    )
    if embed_method == 'zipfs'
      custom_target('wishrl_zipfs',
        output: 'wishrl_zipfs.stamp',
        input: tcl_script_files,
        depends: wishrl,
        command: [
          build_tclsh,
          meson.current_source_dir() / 'tools/append_init_scripts.tcl',
          wishrl.full_path(),
          wishrl.full_path(),
          'library/tclreadlineSetup.tcl library/tclreadlineCompleter.tcl',
        ],
        build_by_default: true,
      )
    endif
  endif
endif

# --- Documentation ---
subdir('doc')
