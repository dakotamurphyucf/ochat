(lang dune 3.21)

(name ochat)

(generate_opam_files true)

(using menhir 3.0)

(pin
 (url "git+https://github.com/anmonteiro/piaf.git")
 (package
  (name piaf)))


(pin
  (url "git+https://github.com/dakotamurphyucf/ocaml-textmate-language.git")
  (package
  (name textmate-language)))




(package
 (name ochat)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
  (ocaml
   (>= 5.1.0))
  (eio (>= 1.3))
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
  digestif
  optint
  textmate-language
  owl
  eio_main
  cohttp-eio
  (ca-certs :with-test)
  ezgzip
  lwt_eio
  base64
  jsonaf
  uutf
  uuseg
  uri
  tls-eio
  notty
  mirage-crypto-rng
  mirage-crypto-rng-eio
  ppx_jsonaf_conv
  ppx_expect
  cohttp
  irmin
  irmin-git
  irmin-watcher
  expect_test_helpers_core
  re2
  path_glob
  bimage
  bimage-unix
  (nottui (< 0.5))
  ppx_blob
  menhir
  menhirLib
  (menhirSdk :build)
  piaf)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
