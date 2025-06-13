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
  }

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

let parse_sse_stream t (body : Body.t) : unit =
  (* We follow the approach from the [ocaml_piaf_example] snippet in the
     project README: copy the [Body.t] into a pipe-backed flow and then use
     [Eio.Buf_read] for robust, line-oriented parsing. *)
  let module B = Eio.Buf_read in
  Eio.Fiber.fork ~sw:t.sw (fun () ->
    let r, w = Eio_unix.pipe t.sw in
    Eio.Fiber.fork ~sw:t.sw (fun () ->
      let res =
        Body.iter
          ~f:(fun { buffer; off; len } ->
            Eio.Flow.write w [ Cstruct.of_bigarray ~off ~len buffer ])
          body
      in
      (match res with
       | Ok () -> ()
       | Error error -> Format.eprintf "error: %a@." Piaf.Error.pp_hum error);
      Eio.Flow.close w);
    let reader = Eio.Buf_read.of_flow r ~max_size:Core.Int.max_value in
    (* we want to get all the lines until we hit a double newline *)
    (* Parse one SSE "event" – terminated by a blank line (i.e. two
       consecutive newlines).
       1. If the last event in the stream is not followed by a trailing
          blank line we still want to emit it when the input ends.
       2. Accept both "data:" and "data: " prefixes as allowed by the
          SSE spec. *)
    let parse_event =
      let rec run acc =
        let open B.Syntax in
        let* line = B.line in
        let* next = B.peek_char in
        match next with
        | Some '\n' ->
          let* () = B.skip 1 in
          B.return (String.concat (List.rev (line :: acc)))
        | None ->
          (* end-of-input – yield whatever we collected so far *)
          B.return (String.concat (List.rev (line :: acc)))
        | _ -> run ("\n" :: line :: acc)
      in
      run []
    in
    let events = B.seq parse_event ~stop:B.at_end_of_input reader in
    let on_event event =
      let data =
        event
        |> String.split_lines
        |> List.filter_map ~f:(fun line ->
          match
            ( String.chop_prefix line ~prefix:"data: "
            , String.chop_prefix line ~prefix:"data:" )
          with
          | Some rest, _ ->
            if String.is_prefix ~prefix:"[DONE]" rest then None else Some rest
          | None, Some rest ->
            let rest = String.lstrip rest in
            if String.is_prefix ~prefix:"[DONE]" rest then None else Some rest
          | _ -> None)
        |> String.concat
      in
      let choice =
        match String.is_empty data with
        | true -> None
        | false ->
          (match Jsonaf.parse data |> Result.bind ~f:(fun json -> Ok json) with
           | Ok json -> Some json
           | Error _ -> None)
      in
      match choice with
      | None -> ()
      | Some choice -> push_json_queue t choice
    in
    Seq.iter on_event events;
    (* Close reader *)
    Eio.Flow.close r)
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
  | Error err ->
    t.closed <- true;
    Printf.eprintf "(mcp-http) POST error: %s\n" (Piaf.Error.to_string err)
  | Ok response ->
    (* 401 handling *)
    if Piaf.Status.to_code response.status = 401 && not retry
    then (
      match t.creds_opt with
      | None -> () (* no creds – we cannot retry *)
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
    else (
      (* capture session id if provided *)
      update_session_id t response.headers;
      let content_type =
        match Headers.get response.headers "content-type" with
        | None -> "application/json"
        | Some v -> v
      in
      if String.is_prefix ~prefix:"text/event-stream" content_type
      then parse_sse_stream t response.body
      else (
        match Piaf.Body.to_string response.body with
        | Error e ->
          t.closed <- true;
          Printf.eprintf
            "(mcp-http) failed to read response body: %s\n"
            (Piaf.Error.to_string e)
        | Ok body_str ->
          (match parse_json body_str with
           | None -> Printf.eprintf "(mcp-http) ignoring non-JSON response body\n"
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
    (* We do not support credentials in the URI query parameters – use
       environment variables or store credentials in the file system. *)
    match
      Uri.get_query_param uri "client_id", Uri.get_query_param uri "client_secret"
    with
    | Some id, Some secret when (not (String.is_empty id)) && not (String.is_empty secret)
      -> Some (Oauth2_manager.Client_secret { id; secret; scope = None })
    | _ -> None
  in
  let creds_from_env () : Oauth2_manager.creds option =
    (* We do not support environment variables for credentials – use
       explicit query parameters or store credentials in the file system. *)
    match Sys.getenv "MCP_CLIENT_ID", Sys.getenv "MCP_CLIENT_SECRET" with
    | Some id, Some secret ->
      Some (Oauth2_manager.Client_secret { id; secret; scope = None })
    | _ -> None
  in
  let creds_from_store () : Oauth2_manager.creds option =
    (* We do not support credentials from the store – use explicit query
       parameters or environment variables. *)
    (* Look up issuer in the credential store. *)

    (* We do not support credentials from the store – use explicit query
       parameters or environment variables. *)
    (* Look up issuer in the credential store. *)
    match Oauth2_client_store.lookup ~env ~issuer with
    | None -> None
    | Some cred ->
      (match cred.client_secret with
       | Some secret ->
         if true then failwith "Mcp_transport_http.connect: secret";
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
       | Some _ ->
         (* We do not support environment variables for registration – use
            explicit query parameters or store credentials in the file system. *)
         None
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
              let client_id = "ocamlgpt-" ^ b64 in
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
    | Some _ as c ->
      print_endline "Using credentials from URI query parameters";
      c
    | None ->
      (match creds_from_env () with
       | Some _ as c ->
         prerr_endline "Using credentials from environment variables";
         c
       | None ->
         (match creds_from_store () with
          | Some _ as c ->
            print_endline "Using credentials from client store";
            c
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
    | true -> set_up_auth ~env ~sw ~issuer uri
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
    }
;;

let send (t : t) (json : Jsonaf.t) : unit =
  if t.closed then raise Connection_closed;
  let payload = Jsonaf.to_string json in
  (* Spawn a fibre so that [send] is non-blocking wrt the caller,
     matching the behaviour of the stdio transport (which writes
     quickly to a pipe and returns). *)
  Eio.Fiber.fork ~sw:t.sw (fun () -> perform_post t payload)
;;

let recv (t : t) : Jsonaf.t =
  if t.closed then raise Connection_closed;
  try Eio.Stream.take t.incoming with
  | End_of_file ->
    t.closed <- true;
    raise Connection_closed
;;

let is_closed (t : t) = t.closed

let close (t : t) : unit =
  if not t.closed
  then (
    t.closed <- true;
    Piaf.Client.shutdown t.client)
;;

(*------------------------------------------------------------------*)
(* Register exception in the interface namespace                     *)
(*------------------------------------------------------------------*)
