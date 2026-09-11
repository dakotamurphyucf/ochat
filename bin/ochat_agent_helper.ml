open Core

let fail message =
  Out_channel.output_string stderr ("ochat-agent-helper: " ^ message ^ "\n");
  exit 1
;;

let read_request maximum =
  let chunk = Bytes.create 8192 in
  let buffer = Buffer.create 8192 in
  let rec loop () =
    let count =
      Stdlib.input
        Stdlib.stdin
        chunk
        0
        (Int.min 8192 (maximum - Buffer.length buffer + 1))
    in
    match count with
    | 0 -> Buffer.contents buffer
    | count when Buffer.length buffer + count <= maximum ->
      Stdlib.Buffer.add_subbytes buffer chunk 0 count;
      loop ()
    | _ -> fail "request exceeds the input limit"
  in
  loop ()
;;

let () =
  match Array.length (Sys.get_argv ()) with
  | 1 ->
    let limits = Shell_access.Request_channel.default_limits in
    let request = read_request limits.max_request_bytes in
    let request =
      try Jsonaf.of_string request |> Jsonaf.to_string with
      | _ -> fail "request must be valid JSON"
    in
    (match Shell_access.Request_channel.Client.exchange ~limits request with
     | Error error -> fail error
     | Ok response ->
       let response =
         try Jsonaf.of_string response |> Jsonaf.to_string with
         | _ -> fail "host response was not valid JSON"
       in
       Out_channel.output_string stdout (response ^ "\n");
       Out_channel.flush stdout)
  | _ -> fail "accepts one JSON request on stdin and no command-line options"
;;
