(lang dune 3.18)

(name ochat)

(generate_opam_files true)

(using menhir 3.0)

(pin
 (url "git+https://github.com/anmonteiro/piaf.git")
 (package
  (name piaf)))

(package
 (name ochat)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
  (ocaml
   (>= 5.1.0))
  eio
  dune
  omd
  lambdasoup
  markup
  core
  core_unix
  ppx_jane
  ppxlib
  pcre
  re
  owl
  eio
  eio_main
  cohttp-eio
  ezgzip
  lwt_eio
  base64
  jsonaf
  tls-eio
  notty
  mirage-crypto-rng-eio
  ppx_jsonaf_conv
  ppx_expect
  cohttp
  expect_test_helpers_core
  re2
  path_glob
  bimage
  bimage-unix
  piaf)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
