open Core

(*------------------------------------------------------------------*)
(* Convenience aliases                                               *)
(*------------------------------------------------------------------*)

module Headers = Piaf.Headers
module Body = Piaf.Body

exception Connection_closed

(*------------------------------------------------------------------*)
(* State record                                                      *)
(*------------------------------------------------------------------*)

type t =
  { client : Piaf.Client.t
  ; endpoint_path : string
  ; incoming : Jsonaf.t Eio.Stream.t
  ; sw : Eio.Switch.t
  ; env : Eio_unix.Stdenv.base
  ; mutable session_id : string option
  ; mutable auth_token : string option
  ; creds_opt : Oauth2_manager.creds option
  ; issuer : string
  ; mutable closed : bool
  ; stopped : unit Eio.Promise.t
  ; stop : unit Eio.Promise.u
  }

let close t =
  if not t.closed
  then (
    t.closed <- true;
    Eio.Promise.resolve t.stop ();
    Piaf.Client.shutdown t.client)
;;

let fail_transport t = function
  | `Exn (Eio.Cancel.Cancelled _ as exn) ->
    close t;
    raise exn
  | _ -> close t
;;

(*------------------------------------------------------------------*)
(* Helpers                                                           *)
(*------------------------------------------------------------------*)

let parse_json str =
  try Some (Jsonaf.of_string str) with
  | _ -> None
;;

let push_json_queue t json = if not t.closed then Eio.Stream.add t.incoming json
let extract_session_id headers = Headers.get headers "Mcp-Session-Id"

let update_session_id t headers =
  match extract_session_id headers with
  | None -> ()
  | Some id -> t.session_id <- Some id
;;

(*------------------------------------------------------------------*)
(* SSE streaming helpers                                             *)
(*------------------------------------------------------------------*)

let json_id = function
  | `Object fields -> List.Assoc.find fields ~equal:String.equal "id"
  | _ -> None
;;

let sse_data event =
  String.split_lines event
  |> List.filter_map ~f:(fun line ->
    Option.bind (String.chop_prefix line ~prefix:"data:") ~f:(fun data ->
      let data = String.lstrip data in
      if String.equal data "[DONE]" then None else Some data))
  |> String.concat ~sep:"\n"
;;

let sse_event =
  let open Eio.Buf_read in
  let rec lines acc =
    let open Syntax in
    let* line = line in
    if String.is_empty line
    then return (String.concat ~sep:"\n" (List.rev acc))
    else
      let* next = peek_char in
      if Option.is_none next
      then return (String.concat ~sep:"\n" (List.rev (line :: acc)))
      else lines (line :: acc)
  in
  lines []
;;

let pump_body t body flow =
  Fun.protect
    (fun () ->
       match
         Body.iter body ~f:(fun { buffer; off; len } ->
           Eio.Flow.write flow [ Cstruct.of_bigarray ~off ~len buffer ])
       with
       | Ok () -> ()
       | Error error -> fail_transport t error)
    ~finally:(fun () -> Eio.Flow.close flow)
;;

let consume_sse t ~request_id reader =
  let replied = ref false in
  let events = Eio.Buf_read.seq sse_event ~stop:Eio.Buf_read.at_end_of_input reader in
  Seq.iter
    (fun event ->
       let data = sse_data event in
       if not (String.is_empty data)
       then (
         match parse_json data with
         | None -> close t
         | Some json ->
           if Option.is_some request_id && Poly.equal (json_id json) request_id
           then replied := true;
           push_json_queue t json))
    events;
  if Option.is_some request_id && not !replied then close t
;;

let parse_sse_stream t ~request_id body =
  Eio.Fiber.fork ~sw:t.sw (fun () ->
    try
      let r, w = Eio_unix.pipe t.sw in
      Eio.Fiber.fork ~sw:t.sw (fun () ->
        try pump_body t body w with
        | Eio.Cancel.Cancelled _ as exn ->
          close t;
          raise exn
        | _ -> close t);
      Fun.protect
        (fun () ->
           let reader = Eio.Buf_read.of_flow r ~max_size:Core.Int.max_value in
           consume_sse t ~request_id reader)
        ~finally:(fun () -> Eio.Flow.close r)
    with
    | Eio.Cancel.Cancelled _ as exn ->
      close t;
      raise exn
    | _ -> close t)
;;

let with_discarded_body t body f =
  match Body.drain body with
  | Ok () -> f ()
  | Error error -> fail_transport t error
;;

let rec perform_post ?(retry = false) (t : t) (payload : string) : unit =
  let headers_base =
    [ "content-type", "application/json"
    ; "accept", "application/json, text/event-stream"
    ]
  in
  let headers_list =
    headers_base
    @ (match t.session_id with
       | None -> []
       | Some sid -> [ "Mcp-Session-Id", sid ])
    @
    match t.auth_token with
    | None -> []
    | Some tok -> [ "Authorization", "Bearer " ^ tok ]
  in
  let body = Body.of_string payload in
  match Piaf.Client.post t.client ~headers:headers_list ~body t.endpoint_path with
  | Error err -> fail_transport t err
  | Ok response ->
    (* 401 handling *)
    if Piaf.Status.to_code response.status = 401 && not retry
    then
      with_discarded_body t response.body (fun () ->
        match t.creds_opt with
        | None -> close t
        | Some creds ->
          (match t.auth_token with
           | Some _ when not retry -> ()
           | None when retry -> ()
           | _ ->
             (match Oauth2_manager.get ~env:t.env ~sw:t.sw ~issuer:t.issuer creds with
              | Ok tok -> t.auth_token <- Some tok.access_token
              | Error e -> Printf.eprintf "(mcp-http) OAuth flow failed: %s\n" e));
          (* retry once *)
          perform_post ~retry:true t payload)
    else if Piaf.Status.to_code response.status >= 400
    then with_discarded_body t response.body (fun () -> close t)
    else (
      (* capture session id if provided *)
      update_session_id t response.headers;
      let content_type =
        match Headers.get response.headers "content-type" with
        | None -> "application/json"
        | Some v -> v
      in
      if String.is_prefix ~prefix:"text/event-stream" content_type
      then (
        let request_id = Option.bind (parse_json payload) ~f:json_id in
        parse_sse_stream t ~request_id response.body)
      else (
        match Piaf.Body.to_string response.body with
        | Error e -> fail_transport t e
        | Ok body_str ->
          (match parse_json body_str with
           | None ->
             if not (String.is_empty body_str && Piaf.Status.to_code response.status = 202)
             then close t
           | Some (`Array arr) -> List.iter arr ~f:(push_json_queue t)
           | Some json -> push_json_queue t json)))
;;

let set_up_auth ~env ~sw ~issuer uri =
  (*--------------------------------------------------------------*)
  (* Credentials selection – precedence                           *)
  (* 1. explicit URI query parameters (?client_id=…&client_secret=…)  *)
  (* 2. environment variables (global fallback)                    *)
  (*--------------------------------------------------------------*)
  let creds_from_uri () : Oauth2_manager.creds option =
    match
      Uri.get_query_param uri "client_id", Uri.get_query_param uri "client_secret"
    with
    | Some id, Some secret when (not (String.is_empty id)) && not (String.is_empty secret)
      -> Some (Oauth2_manager.Client_secret { id; secret; scope = None })
    | _ -> None
  in
  let creds_from_env () : Oauth2_manager.creds option =
    match Sys.getenv "MCP_CLIENT_ID", Sys.getenv "MCP_CLIENT_SECRET" with
    | Some id, Some secret ->
      Some (Oauth2_manager.Client_secret { id; secret; scope = None })
    | _ -> None
  in
  let creds_from_store () : Oauth2_manager.creds option =
    match Oauth2_client_store.lookup ~env ~issuer with
    | None -> None
    | Some cred ->
      (match cred.client_secret with
       | Some secret ->
         Some (Oauth2_manager.Client_secret { id = cred.client_id; secret; scope = None })
       | None -> Some (Oauth2_manager.Pkce { client_id = cred.client_id }))
  in
  (* Attempt dynamic registration when allowed by server metadata and we have
     no pre-existing credentials. *)
  let creds_from_registration () : Oauth2_manager.creds option =
    match creds_from_uri () with
    | Some _ -> None (* explicit creds – no registration *)
    | None ->
      (match creds_from_env () with
       | Some _ -> None
       | None ->
         (match creds_from_store () with
          | Some _ ->
            (* We have stored credentials – no need to register again. *)
            None
          | None ->
            (* Fetch metadata; fall back to default paths on failure *)
            let meta_res =
              let path = issuer ^ "/.well-known/oauth-authorization-server" in
              match Oauth2_http.get_json ~env ~sw path with
              | Ok json ->
                (try Some (Oauth2_types.Metadata.t_of_jsonaf json) with
                 | _ -> None)
              | Error _ -> None
            in
            let registration_endpoint =
              match meta_res with
              | Some meta ->
                (match meta.registration_endpoint with
                 | Some url -> url
                 | None -> issuer ^ "/register")
              | None -> issuer ^ "/register"
            in
            (* Build minimal registration payload (anonymous public client) *)
            let payload = `Object [] in
            let registration_result : Oauth2_manager.creds option =
              match Oauth2_http.post_json ~env ~sw registration_endpoint payload with
              | Ok json ->
                (try
                   let reg = Oauth2_types.Client_registration.t_of_jsonaf json in
                   let cred : Oauth2_client_store.Credential.t =
                     { client_id = reg.client_id; client_secret = reg.client_secret }
                   in
                   (* Persist credentials so future runs skip registration. *)
                   Oauth2_client_store.store ~env ~issuer cred;
                   Some
                     (match reg.client_secret with
                      | Some secret ->
                        Oauth2_manager.Client_secret
                          { id = reg.client_id; secret; scope = None }
                      | None -> Oauth2_manager.Pkce { client_id = reg.client_id })
                 with
                 | Eio.Cancel.Cancelled _ as exn -> raise exn
                 | _ -> None)
              | Error _ -> None
            in
            (* ------------------------------------------------------------------ *)
            (* C-4: Fallback when server doesn’t support registration                *)
            (* ------------------------------------------------------------------ *)
            let fallback_pkce_creds () : Oauth2_manager.creds =
              (* Ensure RNG is initialised before generating random bytes. *)
              (try Mirage_crypto_rng_unix.use_default () with
               | _ -> ());
              (* Generate a short, URL-safe identifier. *)
              let rand_str = Mirage_crypto_rng.generate 6 in
              let b64 =
                Base64.encode_string
                  ~pad:false
                  ~alphabet:Base64.uri_safe_alphabet
                  rand_str
              in
              let client_id = "ocamlochat-" ^ b64 in
              let cred_rec : Oauth2_client_store.Credential.t =
                { client_id; client_secret = None }
              in
              (* Persist so subsequent sessions reuse the same ID. *)
              Oauth2_client_store.store ~env ~issuer cred_rec;
              Oauth2_manager.Pkce { client_id }
            in
            (match registration_result with
             | Some c -> Some c
             | None -> Some (fallback_pkce_creds ()))))
  in
  let creds_opt : Oauth2_manager.creds option =
    match creds_from_uri () with
    | Some _ as c -> c
    | None ->
      (match creds_from_env () with
       | Some _ as c -> c
       | None ->
         (match creds_from_store () with
          | Some _ as c -> c
          | None -> creds_from_registration ()))
  in
  let auth_token_result : string option =
    match creds_opt with
    | None -> None
    | Some creds ->
      (match Oauth2_manager.get ~env ~sw ~issuer creds with
       | Ok tok -> Some tok.access_token
       | Error e ->
         (try Logs.warn (fun f -> f "OAuth token fetch failed: %s" e) with
          | _ -> ());
         None)
  in
  creds_opt, auth_token_result
;;

(*------------------------------------------------------------------*)
(* TRANSPORT implementation                                          *)
(*------------------------------------------------------------------*)

let connect ?(auth = true) ~(sw : Eio.Switch.t) ~env (uri_str : string) : t =
  (*------------------------------------------------------------------*)
  (* Parse URI and create persistent Piaf client                      *)
  (*------------------------------------------------------------------*)
  let uri = Uri.of_string uri_str in
  let scheme = Uri.scheme uri |> Option.value_exn in
  (match scheme with
   | "http" | "https" | "mcp+http" | "mcp+https" -> ()
   | _ -> invalid_arg "Mcp_transport_http.connect: unsupported URI scheme");
  (* Create a base URI without the path component for the Piaf client
     (Piaf connects to authority – path is given per request).        *)
  let base_uri = Uri.with_path uri "" in
  (* Attempt OAuth token retrieval – we look at env vars as documented in
     [oauth.md].  This is best-effort: if anything fails we fall back to
     anonymous access. *)
  let issuer =
    (* issuer = scheme://authority (no path) *)
    let base_no_path = Uri.with_path uri "" in
    Uri.to_string base_no_path
  in
  let creds_opt, auth_token_result =
    match auth with
    | true ->
      (match Oauth2_http.protect (fun () -> Ok (set_up_auth ~env ~sw ~issuer uri)) with
       | Ok credentials -> credentials
       | Error _ -> None, None)
    | false ->
      (* No auth – we do not attempt to fetch credentials or tokens *)
      None, None
  in
  match Piaf.Client.create ~sw env base_uri with
  | Error err ->
    invalid_arg
      (Printf.sprintf
         "Mcp_transport_http.connect: unable to connect – %s"
         (Piaf.Error.to_string err))
  | Ok client ->
    let incoming = Eio.Stream.create 64 in
    let stopped, stop = Eio.Promise.create () in
    { client
    ; endpoint_path = Uri.path uri
    ; incoming
    ; sw
    ; env
    ; session_id = None
    ; auth_token = auth_token_result
    ; creds_opt
    ; issuer
    ; closed = false
    ; stopped
    ; stop
    }
;;

let send (t : t) (json : Jsonaf.t) : unit =
  if t.closed then raise Connection_closed;
  let payload = Jsonaf.to_string json in
  (* Spawn a fibre so that [send] is non-blocking wrt the caller,
     matching the behaviour of the stdio transport (which writes
     quickly to a pipe and returns). *)
  Eio.Fiber.fork ~sw:t.sw (fun () ->
    try perform_post t payload with
    | Eio.Cancel.Cancelled _ as exn ->
      close t;
      raise exn
    | _ -> close t)
;;

let recv (t : t) : Jsonaf.t =
  if t.closed then raise Connection_closed;
  Eio.Fiber.first
    (fun () -> Eio.Stream.take t.incoming)
    (fun () ->
       Eio.Promise.await t.stopped;
       raise Connection_closed)
;;

let is_closed (t : t) = t.closed

(*------------------------------------------------------------------*)
(* Register exception in the interface namespace                     *)
(*------------------------------------------------------------------*)
