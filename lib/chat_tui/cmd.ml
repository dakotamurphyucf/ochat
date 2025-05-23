open Types
open Core

let run (c : cmd) : unit =
  match c with
  | Persist_session thunk -> thunk ()
  | Start_streaming thunk -> thunk ()
  | Cancel_streaming thunk -> thunk ()
;;

let run_all (cs : cmd list) : unit = List.iter cs ~f:run
