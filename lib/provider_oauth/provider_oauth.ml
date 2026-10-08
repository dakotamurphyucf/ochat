module Native_unix = Unix
open! Core
module P = Provider_oauth_protocol

module Error = struct
  type stage =
    | Configure
    | Listen
    | Callback
    | Device_challenge
    | Device_poll
    | Exchange
    | Identity
  [@@deriving sexp_of]

  type code =
    | Invalid_configuration
    | Unsupported_route
    | Closed
    | Timed_out
    | Invalid_callback
    | State_mismatch
    | Authorization_denied
    | Protocol_error
    | Response_too_large
    | Transport_unavailable
    | Identity_mismatch
    | Identity_unverifiable
    | Submission_uncertain
  [@@deriving equal, sexp_of]

  type t =
    { stage : stage
    ; code : code
    }
  [@@deriving sexp_of]

  let stage t = t.stage
  let code t = t.code
  let make stage code = { stage; code }
end

let issuer = "https://auth.openai.com"
let client = "app_EMoamEEZ73f0CkXaXp7hrann"
let browser_scopes = [ "openid"; "profile"; "email"; "offline_access" ]

module Policy = struct
  type t =
    { expected_account : string option
    ; callback_port : int
    ; access_scope_claim : bool
    }

  let valid text =
    (not (String.is_empty text))
    && String.length text <= 512
    && String.for_all text ~f:(fun c -> Char.to_int c >= 0x21 && Char.to_int c < 0x7f)
  ;;

  let issuer _ = issuer
  let client_registration _ = client
  let resource _ = "https://chatgpt.com/backend-api/codex"

  let direct_codex ?(access_scope_claim = false) ~expected_account ~callback_port () =
    if
      Option.exists expected_account ~f:(fun a -> not (valid a))
      || (callback_port <> 1455 && callback_port <> 1457)
    then Error (Error.make Configure Invalid_configuration)
    else Ok { expected_account; callback_port; access_scope_claim }
  ;;
end

module Transport = struct
  type t = Oauth_transport.t

  let create ~net ~clock =
    Oauth_transport.create ~net ~clock
    |> Result.map_error ~f:(fun _ -> Error.make Configure Transport_unavailable)
  ;;

  let close = Oauth_transport.close
end

module Challenge = struct
  type t =
    | Browser of Uri.t
    | Device of
        { uri : Uri.t
        ; user_code : string
        }

  let with_browser_uri t ~f =
    match t with
    | Browser uri -> Ok (f uri)
    | Device _ -> Error (Error.make Configure Invalid_configuration)
  ;;

  let with_device_prompt t ~f =
    match t with
    | Device { uri; user_code } -> Ok (f ~verification_uri:uri ~user_code)
    | Browser _ -> Error (Error.make Configure Invalid_configuration)
  ;;
end

module Verified = struct
  type scope_source =
    | Token_response
    | Browser_request
    | Prior_exact
    | Qualified_access_token_claim
  [@@deriving equal, sexp_of]

  type t =
    { identity : Oauth_identity.t
    ; access : Provider_secret_store.Secret.t
    ; refresh : Provider_secret_store.Secret.t P.Presence.t
    ; scope_presence : string list P.Presence.t
    ; scope_source : scope_source
    ; continuity : Provider_secret_store.Secret.t
    }

  let account t = Oauth_identity.account t.identity
  let subject t = Oauth_identity.subject t.identity
  let scopes t = Oauth_identity.scopes t.identity
  let expires_at t = Oauth_identity.expires_at t.identity
  let scope_presence t = t.scope_presence
  let scope_source t = t.scope_source
  let response_expires_in t = Oauth_identity.response_expires_in t.identity
  let with_continuity t ~f = f t.continuity
  let with_material t ~f = f ~access:t.access ~refresh:t.refresh
end

let protocol stage = function
  | P.Error.Response_too_large -> Error.make stage Response_too_large
  | State_mismatch -> Error.make stage State_mismatch
  | Authorization_denied -> Error.make stage Authorization_denied
  | Invalid_callback -> Error.make stage Invalid_callback
  | Invalid_input | Invalid_json | Invalid_pkce | Invalid_grant ->
    Error.make stage Protocol_error
;;

let exchange
      transport
      policy
      ~code
      ~pkce
      ~redirect
      ~nonce
      ~requested_scopes
      ~wall_clock
      ~on_validate
  =
  let open Result.Let_syntax in
  let submitted = ref false in
  let body =
    P.form
      [ "grant_type", "authorization_code"
      ; "client_id", client
      ; "redirect_uri", redirect
      ; "code", code
      ; "code_verifier", P.Pkce.verifier pkce
      ]
  in
  let%bind status, response =
    Oauth_transport.post
      transport
      Token
      ~content_type:"application/x-www-form-urlencoded"
      ~body
      ~on_possible_submission:(fun () -> submitted := true)
    |> Result.map_error ~f:(fun _ ->
      Error.make
        Exchange
        (if !submitted then Submission_uncertain else Transport_unavailable))
  in
  if status <> 200
  then Error (Error.make Exchange Protocol_error)
  else (
    let%bind token = P.Token.decode response |> Result.map_error ~f:(protocol Exchange) in
    on_validate ();
    let%bind identity =
      Oauth_identity.validate
        token
        ~now:(Eio.Time.now wall_clock)
        ~client
        ~expected_account:policy.Policy.expected_account
        ~expected_subject:None
        ~nonce
        ~requested_scopes
        ~access_scope_claim:policy.Policy.access_scope_claim
        ~prior:None
      |> Result.map_error ~f:(function
        | Oauth_identity.Error.Account | Subject | Issuer | Audience | Nonce ->
          Error.make Identity Identity_mismatch
        | Invalid_token | Expiry | Scopes -> Error.make Identity Identity_unverifiable)
    in
    let secret value =
      Provider_secret_store.Secret.of_bytes (Bytes.of_string value)
      |> Result.map_error ~f:(fun _ -> Error.make Identity Protocol_error)
    in
    let%bind access = secret (P.Token.access token) in
    let encoded_continuity = Oauth_identity.continuity identity in
    let%bind () =
      if String.length encoded_continuity <= 8192
      then Ok ()
      else Error (Error.make Identity Protocol_error)
    in
    let%bind continuity = secret encoded_continuity in
    let%map refresh =
      match P.Token.refresh token with
      | Absent -> Ok P.Presence.Absent
      | Null -> Ok P.Presence.Null
      | Value value -> Result.map (secret value) ~f:(fun value -> P.Presence.Value value)
    in
    { Verified.identity
    ; access
    ; refresh
    ; scope_presence = P.Token.scopes token
    ; scope_source =
        (match P.Token.scopes token, requested_scopes with
         | Value _, _ -> Verified.Token_response
         | (Absent | Null), Some _ -> Browser_request
         | (Absent | Null), None -> Qualified_access_token_claim)
    ; continuity
    })
;;

exception Explicit_close

module Login = struct
  type phase =
    | Starting
    | Awaiting_callback
    | Polling
    | Exchanging
    | Validating
    | Ready
    | Failed
    | Cancelled
  [@@deriving equal, sexp_of]

  type completion =
    | Finished of (Verified.t, Error.t) result
    | Raised of exn * Stdlib.Printexc.raw_backtrace

  type t =
    { mutable phase : phase
    ; mutable context : Eio.Cancel.t option
    ; done_ : completion Eio.Promise.t
    ; mutable consumed : bool
    ; mutable close_completed : bool
    }

  let phase t = t.phase

  let completion = function
    | Finished result -> result
    | Raised (exn, backtrace) -> Stdlib.Printexc.raise_with_backtrace exn backtrace
  ;;

  let await t =
    if t.consumed
    then Error (Error.make Exchange Closed)
    else (
      t.consumed <- true;
      Eio.Promise.await t.done_ |> completion)
  ;;

  let close t =
    Eio.Cancel.protect (fun () ->
      (match t.context with
       | None -> ()
       | Some context -> Eio.Cancel.cancel context Explicit_close);
      let outcome = Eio.Promise.await t.done_ in
      if not t.close_completed
      then (
        t.close_completed <- true;
        match outcome with
        | Finished _ | Raised (Eio.Cancel.Cancelled _, _) -> ()
        | Raised _ -> ignore (completion outcome : (Verified.t, Error.t) result)))
  ;;

  let start ~sw ~clock ~maximum_wait run =
    let seconds = Time_ns.Span.to_sec maximum_wait in
    if (not (Float.is_finite seconds)) || Float.(seconds <= 0. || seconds > 900.)
    then Error (Error.make Configure Invalid_configuration)
    else (
      let done_, resolved = Eio.Promise.create () in
      let challenge, ready = Eio.Promise.create () in
      let t =
        { phase = Starting
        ; context = None
        ; done_
        ; consumed = false
        ; close_completed = false
        }
      in
      let exposed = ref false in
      let expose c =
        exposed := true;
        Eio.Promise.resolve ready (Ok c)
      in
      Eio.Fiber.fork ~sw (fun () ->
        let outcome =
          try
            let started = Eio.Time.Mono.now clock in
            let result =
              Eio.Cancel.sub (fun context ->
                t.context <- Some context;
                Fun.protect
                  ~finally:(fun () -> t.context <- None)
                  (fun () ->
                     Eio.Time.Timeout.run_exn
                       (Eio.Time.Timeout.seconds clock seconds)
                       (fun () -> Eio.Switch.run (fun owned_sw -> run t owned_sw expose))))
            in
            if
              Float.(
                Mtime.Span.to_float_ns (Mtime.span started (Eio.Time.Mono.now clock))
                >= seconds *. 1e9)
            then raise Eio.Time.Timeout;
            t.phase
            <- (match result with
                | Ok _ -> Ready
                | Error _ -> Failed);
            Finished result
          with
          | Eio.Cancel.Cancelled Explicit_close ->
            t.phase <- Cancelled;
            Finished (Error (Error.make Exchange Closed))
          | Eio.Time.Timeout ->
            t.phase <- Failed;
            Finished (Error (Error.make Exchange Timed_out))
          | exn ->
            t.phase <- Cancelled;
            Raised (exn, Stdlib.Printexc.get_raw_backtrace ())
        in
        if not !exposed
        then (
          let result =
            match outcome with
            | Finished (Error error) -> Error error
            | Finished (Ok _) -> Error (Error.make Configure Protocol_error)
            | Raised _ -> Error (Error.make Configure Closed)
          in
          Eio.Promise.resolve ready result);
        Eio.Promise.resolve resolved outcome);
      Eio.Switch.on_release sw (fun () -> close t);
      match Eio.Promise.await challenge with
      | Ok challenge -> Ok (t, challenge)
      | Error error ->
        (* Do not lose unexpected setup exceptions behind a typed projection. *)
        (match Eio.Promise.await done_ with
         | Raised _ as outcome ->
           ignore (completion outcome : (Verified.t, Error.t) result)
         | Finished _ -> ());
        Error error)
  ;;

  let random secure_random =
    let bytes = Cstruct.create 32 in
    Eio.Flow.read_exact secure_random bytes;
    Base64.encode_exn
      ~pad:false
      ~alphabet:Base64.uri_safe_alphabet
      (Cstruct.to_string bytes)
  ;;

  let callback_request reader expected_host =
    let first = Oauth_transport.line reader in
    let fields = Oauth_transport.headers reader in
    let valid_host =
      Option.equal String.equal (Map.find fields "host") (Some expected_host)
    in
    let valid_origin =
      match Map.find fields "origin" with
      | None -> true
      | Some origin -> String.equal origin issuer
    in
    if
      (not valid_host)
      || (not valid_origin)
      || Map.mem fields "transfer-encoding"
      || Option.exists (Map.find fields "content-length") ~f:(fun n ->
        not (String.equal n "0"))
    then Error (Error.make Callback Invalid_callback)
    else (
      match String.split first ~on:' ' with
      | [ "GET"; target; "HTTP/1.1" ] -> Ok target
      | _ -> Error (Error.make Callback Invalid_callback))
  ;;

  let start_browser
        ~transport
        ~policy
        ~sw
        ~net
        ~secure_random
        ~clock
        ~wall_clock
        ~maximum_wait
    =
    start ~sw ~clock ~maximum_wait (fun t owned_sw expose ->
      let open Result.Let_syntax in
      let state = random secure_random in
      let nonce = random secure_random in
      let%bind pkce =
        P.Pkce.of_verifier (random secure_random)
        |> Result.map_error ~f:(protocol Configure)
      in
      let port = policy.Policy.callback_port in
      let%bind socket =
        try
          Ok
            (Eio.Net.listen
               ~sw:owned_sw
               ~reuse_addr:false
               ~backlog:4
               net
               (`Tcp (Eio.Net.Ipaddr.V4.loopback, port)))
        with
        | Eio.Io _ | Native_unix.Unix_error _ ->
          Error (Error.make Listen Transport_unavailable)
      in
      let host = sprintf "127.0.0.1:%d" port in
      let redirect = "http://" ^ host ^ "/auth/callback" in
      let auth =
        Uri.add_query_params'
          (Uri.of_string (issuer ^ "/oauth/authorize"))
          ([ "response_type", "code"
           ; "client_id", client
           ; "redirect_uri", redirect
           ; "scope", String.concat ~sep:" " browser_scopes
           ; "state", state
           ; "nonce", nonce
           ; "code_challenge", P.Pkce.challenge pkce
           ; "code_challenge_method", "S256"
           ; "id_token_add_organizations", "true"
           ; "codex_cli_simplified_flow", "true"
           ; "originator", "ochat"
           ]
           @ Option.to_list
               (Option.map policy.expected_account ~f:(fun account ->
                  "allowed_workspace_id", account)))
      in
      t.phase <- Awaiting_callback;
      expose (Challenge.Browser auth);
      let rec accept () =
        let result =
          Eio.Switch.run (fun request_sw ->
            let flow, _ = Eio.Net.accept ~sw:request_sw socket in
            let result =
              try
                Eio.Time.Timeout.run_exn (Eio.Time.Timeout.seconds clock 2.) (fun () ->
                  let reader = Eio.Buf_read.of_flow flow ~max_size:16384 in
                  let%bind target = callback_request reader host in
                  P.Callback.parse ~expected_state:state ~target
                  |> Result.map_error ~f:(protocol Callback))
              with
              | Eio.Time.Timeout
              | End_of_file
              | Eio.Buf_read.Buffer_limit_exceeded
              | Oauth_transport.Transport_error _
              | Failure _ -> Error (Error.make Callback Invalid_callback)
            in
            let accepted =
              match result with
              | Ok _ -> true
              | Error _ -> false
            in
            (* Fixed response; never reflect code/state/provider text into HTML. *)
            (try
               Eio.Time.Timeout.run_exn (Eio.Time.Timeout.seconds clock 2.) (fun () ->
                 Eio.Flow.copy_string
                   (if accepted
                    then
                      "HTTP/1.1 200 OK\r\nContent-Length: 0\r\nConnection: close\r\n\r\n"
                    else
                      "HTTP/1.1 400 Bad Request\r\n\
                       Content-Length: 0\r\n\
                       Connection: close\r\n\
                       \r\n")
                   flow)
             with
             | Eio.Io _ | Eio.Time.Timeout -> ());
            result)
        in
        match result with
        | Error _ -> accept ()
        | Ok P.Callback.Denied -> Error (Error.make Callback Authorization_denied)
        | Ok (P.Callback.Code code) ->
          t.phase <- Exchanging;
          exchange
            transport
            policy
            ~code
            ~pkce
            ~redirect
            ~nonce:(Some nonce)
            ~requested_scopes:(Some browser_scopes)
            ~wall_clock
            ~on_validate:(fun () -> t.phase <- Validating)
      in
      accept ())
  ;;

  let start_device ~transport ~policy ~sw ~clock ~wall_clock ~maximum_wait =
    start ~sw ~clock ~maximum_wait (fun t _owned_sw expose ->
      let open Result.Let_syntax in
      let%bind status, response =
        Oauth_transport.post
          transport
          User_code
          ~content_type:"application/json"
          ~body:(Jsonaf.to_string (`Object [ "client_id", `String client ]))
          ~on_possible_submission:(fun () -> ())
        |> Result.map_error ~f:(fun _ ->
          Error.make Device_challenge Transport_unavailable)
      in
      if status = 404
      then Error (Error.make Device_challenge Unsupported_route)
      else if status <> 200
      then Error (Error.make Device_challenge Protocol_error)
      else (
        let%bind challenge =
          P.Device.decode_challenge response
          |> Result.map_error ~f:(protocol Device_challenge)
        in
        t.phase <- Polling;
        expose
          (Challenge.Device
             { uri = Uri.of_string (issuer ^ "/codex/device")
             ; user_code = P.Device.user_code challenge
             });
        let rec poll () =
          let%bind status, response =
            Oauth_transport.post
              transport
              Device_poll
              ~content_type:"application/json"
              ~body:
                (Jsonaf.to_string
                   (`Object
                       [ "device_auth_id", `String (P.Device.id challenge)
                       ; "user_code", `String (P.Device.user_code challenge)
                       ]))
              ~on_possible_submission:(fun () -> ())
            |> Result.map_error ~f:(fun _ -> Error.make Device_poll Transport_unavailable)
          in
          if status = 403 || status = 404
          then (
            Eio.Time.Mono.sleep clock (Float.of_int (P.Device.interval_seconds challenge));
            poll ())
          else if status <> 200
          then Error (Error.make Device_poll Protocol_error)
          else (
            let%bind grant =
              P.Device.decode_grant response |> Result.map_error ~f:(protocol Device_poll)
            in
            t.phase <- Exchanging;
            exchange
              transport
              policy
              ~code:(P.Device.authorization_code grant)
              ~pkce:(P.Device.pkce grant)
              ~redirect:(issuer ^ "/deviceauth/callback")
              ~nonce:None
              ~requested_scopes:None
              ~wall_clock
              ~on_validate:(fun () -> t.phase <- Validating))
        in
        poll ()))
  ;;
end

module Existing = struct
  let of_registry
        ~issuer:expected_issuer
        ~client_registration
        ~resource
        ~account
        ~subject
        ~scopes
        ~expires_at
        ~access
        ~refresh
        ~continuity
    =
    if
      (not (String.equal expected_issuer issuer))
      || (not (String.equal client_registration client))
      || (not (String.equal resource "https://chatgpt.com/backend-api/codex"))
      || (not (Policy.valid account && Policy.valid subject))
      || List.is_empty scopes
      || List.length scopes > 128
      || (not (List.for_all scopes ~f:Policy.valid))
      || List.contains_dup scopes ~compare:String.compare
    then Error (Error.make Identity Identity_mismatch)
    else
      Provider_secret_store.Secret.with_string continuity ~f:(fun encoded ->
        Oauth_identity.restore_continuity encoded ~account ~subject ~scopes ~expires_at)
      |> Result.map_error ~f:(fun _ -> Error.make Identity Identity_unverifiable)
      |> Result.map ~f:(fun identity ->
        { Verified.identity
        ; access
        ; refresh
        ; scope_presence = P.Presence.Absent
        ; scope_source = Prior_exact
        ; continuity
        })
  ;;
end

module Refresh = struct
  type policy =
    | Preserve_omitted
    | Require_rotated
  [@@deriving equal, sexp_of]

  type outcome =
    | Verified of Verified.t
    | Definitely_not_submitted
    | Authoritative_rejection
    | Possibly_consumed

  let invalid_grant body =
    match Jsonaf.parse body with
    | Ok (`Object fields) ->
      (match String.Map.of_alist fields with
       | `Ok fields ->
         (match Map.find fields "error" with
          | Some (`String "invalid_grant") -> true
          | _ -> false)
       | `Duplicate_key _ -> false)
    | Ok _ | Error _ -> false
  ;;

  let exchange ~transport ~policy ~refresh_policy ~wall_clock prior =
    match prior.Verified.refresh with
    | Absent | Null -> Definitely_not_submitted
    | Value refresh ->
      let submitted = ref false in
      let response =
        Provider_secret_store.Secret.with_string refresh ~f:(fun refresh ->
          Oauth_transport.post
            transport
            Token
            ~content_type:"application/json"
            ~body:
              (Jsonaf.to_string
                 (`Object
                     [ "grant_type", `String "refresh_token"
                     ; "client_id", `String client
                     ; "refresh_token", `String refresh
                     ]))
            ~on_possible_submission:(fun () -> submitted := true))
      in
      (match response with
       | Error _ -> if !submitted then Possibly_consumed else Definitely_not_submitted
       | Ok (400, body) when invalid_grant body -> Authoritative_rejection
       | Ok (status, _) when status <> 200 -> Possibly_consumed
       | Ok (_, body) ->
         let verified =
           let open Result.Let_syntax in
           let%bind token = P.Token.decode body |> Result.map_error ~f:(fun _ -> ()) in
           let%bind identity =
             Oauth_identity.validate
               token
               ~now:(Eio.Time.now wall_clock)
               ~client
               ~expected_account:(Some (Verified.account prior))
               ~expected_subject:(Some (Verified.subject prior))
               ~nonce:None
               ~requested_scopes:None
               ~access_scope_claim:policy.Policy.access_scope_claim
               ~prior:(Some prior.identity)
             |> Result.map_error ~f:(fun _ -> ())
           in
           let%bind access =
             Provider_secret_store.Secret.of_bytes
               (Bytes.of_string (P.Token.access token))
             |> Result.map_error ~f:(fun _ -> ())
           in
           let%bind refresh =
             match P.Token.refresh token, refresh_policy with
             | Null, _ | Absent, Require_rotated -> Error ()
             | Absent, Preserve_omitted -> Ok P.Presence.Absent
             | Value value, _ ->
               Provider_secret_store.Secret.of_bytes (Bytes.of_string value)
               |> Result.map ~f:(fun secret -> P.Presence.Value secret)
               |> Result.map_error ~f:(fun _ -> ())
           in
           let encoded_continuity = Oauth_identity.continuity identity in
           let%bind () =
             if String.length encoded_continuity <= 8192 then Ok () else Error ()
           in
           let%bind continuity =
             Provider_secret_store.Secret.of_bytes (Bytes.of_string encoded_continuity)
             |> Result.map_error ~f:(fun _ -> ())
           in
           Ok
             { Verified.identity
             ; access
             ; refresh
             ; scope_presence = P.Token.scopes token
             ; scope_source =
                 (match P.Token.scopes token with
                  | Value _ -> Verified.Token_response
                  | Absent | Null -> Prior_exact)
             ; continuity
             }
         in
         (match verified with
          | Ok verified -> Verified verified
          | Error () -> Possibly_consumed))
  ;;
end

module Direct_headers = struct
  let endpoint = "https://chatgpt.com/backend-api/codex/responses"

  let with_headers verified ~endpoint:target ~f =
    if not (String.equal target endpoint)
    then Error (Error.make Configure Unsupported_route)
    else
      Ok
        (Provider_secret_store.Secret.with_string
           verified.Verified.access
           ~f:(fun access ->
             f
               [ "Authorization", "Bearer " ^ access
               ; "ChatGPT-Account-ID", Verified.account verified
               ; "originator", "ochat"
               ; "User-Agent", "ochat"
               ]))
  ;;
end

module For_testing = struct
  type endpoint = Oauth_transport.endpoint =
    | User_code
    | Device_poll
    | Token

  type transport_error = Oauth_transport.Error.t =
    | Closed
    | Connection
    | Tls
    | Invalid_http
    | Body_limit
    | Timeout

  let scripted_transport = Oauth_transport.scripted
  let parse_response = Oauth_transport.parse_response
end
