open Core

let run env =
  let input = Eio.Flow.read_all (Eio.Stdenv.stdin env) in
  let request_id =
    Jsonaf.of_string input |> Jsonaf.member_exn "request_id" |> Jsonaf.string_exn
  in
  let response =
    `Object
      [ "version", `Number "1"
      ; "request_id", `String request_id
      ; "decision", `String "approve_once"
      ]
  in
  Eio.Flow.copy_string (Jsonaf.to_string response) (Eio.Stdenv.stdout env)
;;

let () = Eio_main.run run
