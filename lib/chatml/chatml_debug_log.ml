open Core

type sink = string -> unit

let sink_ref : sink option ref = ref None
let set_sink sink = sink_ref := Some sink
let clear_sink () = sink_ref := None

let emit render =
  match !sink_ref with
  | None -> ()
  | Some sink -> sink (render ())
;;

let emit_line line = emit (fun () -> line)
let emitf fmt = Printf.ksprintf emit_line fmt
