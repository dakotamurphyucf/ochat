(* controller_register.ml *)
open Core

let unnamed : string ref = ref ""
let set s = unnamed := s
let get () = !unnamed
let clear () = unnamed := ""
let is_empty () = String.is_empty !unnamed
