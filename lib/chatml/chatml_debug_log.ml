open Core

type sink = string -> unit

let sink_ref : sink option ref = ref None

let set_sink sink = sink_ref := Some sink
let clear_sink () = sink_ref := None

let emit_line line =
  match !sink_ref with
  | Some sink -> sink line
  | None -> ()
;;

let emitf fmt = Printf.ksprintf emit_line fmt
