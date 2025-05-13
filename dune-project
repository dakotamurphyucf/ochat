(lang dune 3.14)
(name chatgpt)



(generate_opam_files true)

(using menhir 3.0)

(package
 (name chatgpt)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends 
  (ocaml
   (>= 5.1.0))
  eio 
  dune 
  omd 
  lambdasoup 
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
  mirage-crypto-rng-eio
  ppx_jsonaf_conv
  ppx_expect
  cohttp
  expect_test_helpers_core
  re2)
  
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
