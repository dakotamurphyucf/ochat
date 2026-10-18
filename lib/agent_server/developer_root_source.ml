open! Core
module Capture = Chatml_developer_capture
module Selection = Developer_capture_host.Source_selection
module Prepared = Chatml_prepared_program

type t =
  | Command of Capture.t
  | Repl of Selection.t

let command capture = Command capture
let repl selection = Repl selection

let prepared = function
  | Command capture -> Capture.prepared capture
  | Repl selection -> Selection.prepared selection
;;

let fingerprint = function
  | Command capture -> Capture.fingerprint capture
  | Repl selection -> Selection.repl_root_fingerprint selection
;;

let project_source source =
  let prepared = prepared source in
  match Prepared.provenance prepared with
  | Project { declaration; lock = _ } -> declaration
  | Implicit -> Prepared.entry prepared
;;

let command_capture = function
  | Command capture -> Some capture
  | Repl _ -> None
;;
