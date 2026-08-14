open! Core

let request_id source =
  match Jsonaf.parse source with
  | Ok (`Object fields) ->
    (match List.Assoc.find fields "request_id" ~equal:String.equal with
     | Some (`String value) -> value
     | _ -> "missing")
  | Ok _ | Error _ -> "malformed"
;;

let response id body =
  sprintf {|{"version":1,"request_id":%s,%s}|} (Jsonaf.to_string (`String id)) body
;;

let run env =
  let input = Eio.Flow.read_all (Eio.Stdenv.stdin env) in
  let id = request_id input in
  let stdout = Eio.Stdenv.stdout env in
  let stderr = Eio.Stdenv.stderr env in
  match Sys.getenv "HOOK_MODE" |> Option.value ~default:"continue" with
  | "continue" -> Eio.Flow.copy_string (response id {|"action":"continue"|}) stdout
  | "wrong_id" -> Eio.Flow.copy_string (response "wrong" {|"action":"continue"|}) stdout
  | "malformed" -> Eio.Flow.copy_string "{" stdout
  | "extra" -> Eio.Flow.copy_string (response id {|"action":"continue"|} ^ "junk") stdout
  | "duplicate" ->
    Eio.Flow.copy_string
      (sprintf {|{"version":1,"version":1,"request_id":"%s","action":"continue"}|} id)
      stdout
  | "invalid_utf8" -> Eio.Flow.copy_string "\255" stdout
  | "stderr" ->
    Eio.Flow.copy_string "private-key-value" stderr;
    Eio.Flow.copy_string (response id {|"action":"continue"|}) stdout
  | "secret_echo" ->
    Eio.Flow.copy_string "private-key-value" stderr;
    Eio.Flow.copy_string "{" stdout
  | "overflow" -> Eio.Flow.copy_string (String.make 4096 'x') stdout
  | "timeout" ->
    Eio.Time.sleep (Eio.Stdenv.clock env) 1.;
    Eio.Flow.copy_string (response id {|"action":"continue"|}) stdout
  | "nonzero" -> exit 7
  | _ -> exit 9
;;

let () = Eio_main.run run
