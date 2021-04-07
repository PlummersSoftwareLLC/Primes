project('primes', 'c', default_options : ['buildtype=release', 'warning_level=2'])
cc = meson.get_compiler('c')

threads = dependency('threads')
m = cc.find_library('m')
deps = [threads, m]

executable(meson.project_name(), 'src/primes.c', dependencies : deps)