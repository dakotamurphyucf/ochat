open Core
module JT = Mcp_types.Jsonrpc

let run env mode =
  let reader = Eio.Buf_read.of_flow (Eio.Stdenv.stdin env) ~max_size:1_000_000 in
  let read () = Eio.Buf_read.line reader |> Jsonaf.of_string in
  let initialize = read () |> JT.request_of_jsonaf in
  let response = JT.ok ~id:initialize.id (`Object []) |> JT.jsonaf_of_response in
  Eio.Flow.copy_string (Jsonaf.to_string response ^ "\n") (Eio.Stdenv.stdout env);
  ignore (read () : Jsonaf.t);
  ignore (read () : Jsonaf.t);
  ignore (read () : Jsonaf.t);
  if String.equal mode "invalid"
  then Eio.Flow.copy_string "not-json\n" (Eio.Stdenv.stdout env)
;;
