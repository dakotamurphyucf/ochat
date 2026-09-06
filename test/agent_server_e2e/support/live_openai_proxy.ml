open Core

type t =
  { env : Eio_unix.Stdenv.base
  ; key : string
  ; ledger : string
  ; mutex : Eio.Mutex.t
  ; client : Piaf.Client.t
  ; mutable forwarded : int
  ; mutable streamed : int
  ; mutable failed : int
  ; mutable upstream_status : int option
  ; mutable guard_error : string option
  ; probe : Stream_probe.t
  }

let authorized_key () =
  if not (Option.equal String.equal (Sys.getenv "OCHAT_E2E_LIVE_OPENAI") (Some "1"))
  then failwith "live OpenAI tests require OCHAT_E2E_LIVE_OPENAI=1";
  match Sys.getenv "OPENAI_API_KEY" with
  | Some key when not (String.is_empty key) -> key
  | _ -> failwith "live OpenAI tests require OPENAI_API_KEY"
;;

let ledger_path () =
  match Sys.getenv "OCHAT_E2E_LIVE_BUDGET_FILE" with
  | Some path when Filename.is_absolute path -> path
  | _ -> failwith "an absolute persistent OCHAT_E2E_LIVE_BUDGET_FILE is required"
;;

let diagnostics_enabled () =
  Option.equal String.equal (Sys.getenv "OCHAT_E2E_STREAM_DIAGNOSTICS") (Some "1")
;;

let reserve t =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    let path = Eio.Path.(Eio.Stdenv.fs t.env / t.ledger) in
    let used =
      if Eio.Path.is_file path
      then Int.of_string (String.strip (Eio.Path.load path))
      else 0
    in
    if used < 0 || used >= 14 then failwith "live test request budget exhausted";
    Agent_store.Durable_file.replace
      ~env:t.env
      ~durability:Flush_file_and_directory
      ~path:t.ledger
      (Int.to_string (used + 1) ^ "\n")
    |> function
    | Ok () -> t.forwarded <- t.forwarded + 1
    | Error _ -> failwith "could not reserve live test budget")
;;

let bounded_request encoded =
  if String.length encoded > 32_768
  then failwith "live fixture request exceeds byte budget";
  let json = Jsonaf.of_string encoded in
  if not (Poly.equal (Jsonaf.member "model" json) (Some (`String "gpt-5.6-sol")))
  then failwith "live fixture requested an unauthorized model";
  List.iter [ "previous_response_id"; "conversation" ] ~f:(fun name ->
    match Jsonaf.member name json with
    | None | Some `Null -> ()
    | _ -> failwith "live fixture must send all context explicitly");
  let tools =
    match Jsonaf.member "tools" json with
    | Some (`Array values) -> values
    | _ -> []
  in
  List.iter tools ~f:(fun tool ->
    match Jsonaf.member "type" tool with
    | Some (`String ("function" | "custom")) -> ()
    | _ -> failwith "paid hosted tools are forbidden in the live fixture");
  List.iter [ "input_image"; "input_audio"; "input_file" ] ~f:(fun tag ->
    if String.is_substring encoded ~substring:tag
    then failwith "live fixture is text-only");
  let fields =
    match json with
    | `Object fields -> fields
    | _ -> failwith "expected request object"
  in
  let fields =
    List.filter fields ~f:(fun (key, _) ->
      not
        (List.mem
           [ "max_output_tokens"; "service_tier"; "store" ]
           key
           ~equal:String.equal))
  in
  `Object
    ([ "max_output_tokens", `Number "4096"
     ; "service_tier", `String "default"
     ; "store", `False
     ]
     @ fields)
;;

let response_for_http1 response =
  match Piaf.Body.length response.Piaf.Response.body with
  | `Unknown ->
    let headers =
      Piaf.Headers.to_list response.headers
      |> List.filter ~f:(fun (name, _) ->
        not
          (List.exists
             [ "content-length"; "transfer-encoding"; "connection" ]
             ~f:(String.Caseless.equal name)))
      |> fun headers -> Piaf.Headers.of_list (("transfer-encoding", "chunked") :: headers)
    in
    Piaf.Response.with_ response ~headers
  | `Fixed _ | `Chunked | `Close_delimited | `Error _ -> response
;;

let forward t request =
  let encoded =
    Piaf.Body.to_string (Piaf.Request.body request)
    |> Result.map_error ~f:(fun _ -> "could not read live request")
    |> Result.ok_or_failwith
  in
  let body = bounded_request encoded in
  reserve t;
  if Poly.equal (Jsonaf.member "stream" body) (Some `True)
  then t.streamed <- t.streamed + 1;
  let headers =
    [ "authorization", "Bearer " ^ t.key; "content-type", "application/json" ]
  in
  match
    Piaf.Client.post
      t.client
      ~headers
      ~body:(Piaf.Body.of_string (Jsonaf.to_string body))
      "/v1/responses"
  with
  | Error _ ->
    t.failed <- t.failed + 1;
    Piaf.Response.of_string ~body:"upstream transport failed" `Bad_gateway
  | Ok response ->
    t.upstream_status <- Some (Piaf.Status.to_code response.status);
    if Piaf.Status.to_code response.status <> 200 then t.failed <- t.failed + 1;
    let response = response_for_http1 response in
    if diagnostics_enabled () then Stream_probe.wrap t.probe response else response
;;

let verify_model_access t =
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock t.env) 15. (fun () ->
    let response =
      Piaf.Client.get
        t.client
        ~headers:[ "authorization", "Bearer " ^ t.key ]
        "/v1/models/gpt-5.6-sol"
      |> Result.map_error ~f:(fun _ -> "live model-access preflight transport failed")
      |> Result.ok_or_failwith
    in
    ignore (Piaf.Body.drain response.body : (unit, Piaf.Error.t) result);
    let status = Piaf.Status.to_code response.status in
    if status <> 200
    then failwith (sprintf "live model-access preflight returned HTTP %d" status))
;;

let handler t ({ Piaf.Server.request; _ } : _ Piaf.Server.ctx) =
  match Piaf.Request.meth request, Piaf.Request.target request with
  | `POST, "/v1/responses" ->
    (try forward t request with
     | Eio.Cancel.Cancelled _ as exn -> raise exn
     | Failure message ->
       t.guard_error <- Some message;
       t.failed <- t.failed + 1;
       Piaf.Response.of_string ~body:"live fixture guard refused request" `Bad_request
     | _ ->
       t.failed <- t.failed + 1;
       Piaf.Response.of_string ~body:"live fixture guard refused request" `Bad_request)
  | _ -> Piaf.Server.Handler.not_found ()
;;

let start ~sw ~env ~environment ~port =
  let key = authorized_key () in
  Temporary_environment.register_secret environment key;
  let client =
    Piaf.Client.create ~sw env (Uri.of_string "https://api.openai.com")
    |> Result.map_error ~f:(fun _ -> "could not create TLS OpenAI client")
    |> Result.ok_or_failwith
  in
  let t =
    { env
    ; key
    ; ledger = ledger_path ()
    ; mutex = Eio.Mutex.create ()
    ; client
    ; forwarded = 0
    ; streamed = 0
    ; failed = 0
    ; upstream_status = None
    ; guard_error = None
    ; probe = Stream_probe.create ()
    }
  in
  verify_model_access t;
  let config = Piaf.Server.Config.create (`Tcp (Eio.Net.Ipaddr.V4.loopback, port)) in
  let server = Piaf.Server.create ~config (handler t) in
  ignore (Piaf.Server.Command.start ~sw env server : Piaf.Server.Command.t);
  t
;;

let metrics t =
  [ ("stream_diagnostics_enabled", if diagnostics_enabled () then `True else `False)
  ; "forwarded_requests", `Number (Int.to_string t.forwarded)
  ; "streaming_requests", `Number (Int.to_string t.streamed)
  ; "upstream_or_guard_failures", `Number (Int.to_string t.failed)
  ; ( "last_upstream_status"
    , Option.value_map t.upstream_status ~default:`Null ~f:(fun status ->
        `Number (Int.to_string status)) )
  ; "reserved_usd_upper_bound", `Number (Int.to_string t.forwarded)
  ; ( "guard_error"
    , Option.value_map t.guard_error ~default:`Null ~f:(fun value -> `String value) )
  ]
  @ Stream_probe.metrics t.probe
;;

let forwarded t = t.forwarded
