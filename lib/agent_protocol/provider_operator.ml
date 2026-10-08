open! Core

type protocol_error = Protocol_error.t

let invalid () = Protocol_error.invalid_request "invalid provider operator document"
let check condition = if condition then Ok () else Error (invalid ())

let fields json allowed =
  let open Result.Let_syntax in
  let%bind () = Json_codec.validate_limits ~max_depth:16 ~max_bytes:(256 * 1024) json in
  let%bind fields = Json_codec.fields json in
  let%map () =
    check
      (List.for_all (Json_codec.to_alist fields) ~f:(fun (key, _) ->
         List.mem allowed key ~equal:String.equal))
  in
  fields
;;

let req = Json_codec.required_as

let nullable fields name decode =
  match Json_codec.optional fields name with
  | None | Some `Null -> Ok None
  | Some json -> Result.map (decode json) ~f:Option.some
;;

let optional encode = function
  | None -> `Null
  | Some value -> encode value
;;

let number value = `Number (Int64.to_string value)
let epoch = Json_codec.bounded_int64 ~min:0L ~max:Int64.max_value

let label json =
  let open Result.Let_syntax in
  let%bind value = Json_codec.string json in
  let%map () =
    check
      ((not (String.is_empty value))
       && String.length value <= 512
       && not
            (String.exists value ~f:(fun c -> Char.to_int c < 32 || Char.to_int c = 127))
      )
  in
  value
;;

module Validated_id (Bound : sig
    val maximum : int
  end) =
struct
  type t = string [@@deriving compare, equal, sexp_of]

  let of_string value =
    Result.map
      (check
         ((not (String.is_empty value))
          && String.length value <= Bound.maximum
          && String.for_all value ~f:(fun c ->
            Char.is_alphanum c || List.mem [ '_'; '-'; '.'; ':' ] c ~equal:Char.equal)))
      ~f:(fun () -> value)
  ;;

  let t_of_sexp = function
    | Sexp.Atom value ->
      (match of_string value with
       | Ok value -> value
       | Error _ -> failwith "invalid provider identifier")
    | Sexp.List _ -> failwith "invalid provider identifier"
  ;;

  let to_string t = t
  let to_json t = `String t
  let of_json json = Result.bind (Json_codec.string json) ~f:of_string
end

module Profile_id = Validated_id (struct
    let maximum = 256
  end)

module Source_id = Validated_id (struct
    let maximum = 128
  end)

module Flow_id = Validated_id (struct
    let maximum = 48
  end)

module Revision = Validated_id (struct
    let maximum = 48
  end)

module Limits = struct
  type t =
    { max_flows : int
    ; max_profiles : int
    ; max_flow_seconds : int
    }

  let create ~max_flows ~max_profiles ~max_flow_seconds =
    Result.map
      (check
         (max_flows > 0
          && max_flows <= 128
          && max_profiles > 0
          && max_profiles <= 128
          && max_flow_seconds > 0
          && max_flow_seconds <= 3600))
      ~f:(fun () -> { max_flows; max_profiles; max_flow_seconds })
  ;;

  let default = { max_flows = 64; max_profiles = 128; max_flow_seconds = 900 }
  let max_flows t = t.max_flows
  let max_profiles t = t.max_profiles
  let max_flow_seconds t = t.max_flow_seconds
end

module Operation = struct
  type t =
    | Setup
    | Status
    | Login
    | Challenge
    | Cancel
    | Logout
    | Select
    | Configure_environment
  [@@deriving equal, sexp]
end

module Error = struct
  type t =
    | Denied
    | Missing_profile
    | Invalid_request
    | Busy
    | Closed
    | Flow_expired
    | Flow_interrupted
    | Challenge_unavailable
    | Submission_uncertain
    | Network
    | Account_denied
    | Model_denied
    | Store_unavailable
    | Unsupported
  [@@deriving equal, sexp]

  let values =
    [ "denied", Denied
    ; "missing_profile", Missing_profile
    ; "invalid_request", Invalid_request
    ; "busy", Busy
    ; "closed", Closed
    ; "flow_expired", Flow_expired
    ; "flow_interrupted", Flow_interrupted
    ; "challenge_unavailable", Challenge_unavailable
    ; "submission_uncertain", Submission_uncertain
    ; "network", Network
    ; "account_denied", Account_denied
    ; "model_denied", Model_denied
    ; "store_unavailable", Store_unavailable
    ; "unsupported", Unsupported
    ]
  ;;

  let to_json t =
    `String (List.find_exn values ~f:(fun (_, value) -> equal value t) |> fst)
  ;;

  let of_json = Json_codec.enum ~name:"provider failure" values
end

module Login_mode = struct
  type t =
    | Browser
    | Device
  [@@deriving equal, sexp]

  let to_json = function
    | Browser -> `String "browser"
    | Device -> `String "device"
  ;;

  let of_json =
    Json_codec.enum ~name:"provider login mode" [ "browser", Browser; "device", Device ]
  ;;
end

module Flow_ref = struct
  type t =
    { server_id : Id.Server.t
    ; profile : Profile_id.t
    ; flow_id : Flow_id.t
    ; expires_at : Timestamp.t
    }
  [@@deriving sexp]

  let to_json t =
    `Object
      [ "server_id", Id.Server.to_json t.server_id
      ; "profile", Profile_id.to_json t.profile
      ; "flow_id", Flow_id.to_json t.flow_id
      ; "expires_at", Timestamp.to_json t.expires_at
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind f = fields json [ "server_id"; "profile"; "flow_id"; "expires_at" ] in
    let%bind server_id = req f "server_id" Id.Server.of_json in
    let%bind profile = req f "profile" Profile_id.of_json in
    let%bind flow_id = req f "flow_id" Flow_id.of_json in
    let%map expires_at = req f "expires_at" Timestamp.of_json in
    { server_id; profile; flow_id; expires_at }
  ;;
end

module Flow_result = struct
  type phase =
    | Pending
    | Completed
    | Failed of Error.t
    | Cancelled
    | Interrupted
    | Expired
  [@@deriving equal, sexp]

  type t =
    { flow : Flow_ref.t
    ; phase : phase
    }
  [@@deriving sexp]

  let phase_json = function
    | Failed error -> `Object [ "kind", `String "failed"; "error", Error.to_json error ]
    | phase ->
      `Object
        [ ( "kind"
          , `String
              (match phase with
               | Pending -> "pending"
               | Completed -> "completed"
               | Cancelled -> "cancelled"
               | Interrupted -> "interrupted"
               | Expired -> "expired"
               | Failed _ -> assert false) )
        ]
  ;;

  let phase_of_json json =
    let open Result.Let_syntax in
    let%bind f = fields json [ "kind"; "error" ] in
    let%bind kind = req f "kind" Json_codec.string in
    if String.equal kind "failed"
    then Result.map (req f "error" Error.of_json) ~f:(fun error -> Failed error)
    else (
      let%bind () = check (Option.is_none (Json_codec.optional f "error")) in
      Json_codec.enum
        ~name:"provider flow phase"
        [ "pending", Pending
        ; "completed", Completed
        ; "cancelled", Cancelled
        ; "interrupted", Interrupted
        ; "expired", Expired
        ]
        (`String kind))
  ;;

  let to_json t = `Object [ "flow", Flow_ref.to_json t.flow; "phase", phase_json t.phase ]

  let of_json json =
    let open Result.Let_syntax in
    let%bind f = fields json [ "flow"; "phase" ] in
    let%bind flow = req f "flow" Flow_ref.of_json in
    let%map phase = req f "phase" phase_of_json in
    { flow; phase }
  ;;
end

module Private_challenge = struct
  type t =
    | Browser of Uri.t
    | Device of
        { verification_uri : Uri.t
        ; user_code : string
        }

  let sexp_of_t _ = Sexp.Atom "<redacted-provider-challenge>"
  let t_of_sexp _ = failwith "provider challenge sexp decoding is forbidden"

  let valid_uri uri =
    let encoded = Uri.to_string uri in
    String.length encoded <= 4096
    && Option.is_none (Uri.userinfo uri)
    && Option.is_none (Uri.fragment uri)
    && Option.exists (Uri.host uri) ~f:(fun host ->
      (not (String.is_empty host))
      && (Option.equal String.equal (Uri.scheme uri) (Some "https")
          || (Option.equal String.equal (Uri.scheme uri) (Some "http")
              && List.mem [ "localhost"; "127.0.0.1"; "::1" ] host ~equal:String.equal)))
    && not (String.exists encoded ~f:(fun c -> Char.to_int c < 32 || Char.to_int c = 127))
  ;;

  let browser ~authorization_uri =
    Result.map
      (check (valid_uri authorization_uri))
      ~f:(fun () -> Browser authorization_uri)
  ;;

  let device ~verification_uri ~user_code =
    Result.map
      (check
         (valid_uri verification_uri
          && (not (String.is_empty (String.strip user_code)))
          && String.length user_code <= 128
          && String.for_all user_code ~f:(fun c ->
            Char.to_int c >= 32 && Char.to_int c < 127)))
      ~f:(fun () -> Device { verification_uri; user_code })
  ;;

  let with_browser_uri t ~f =
    match t with
    | Browser uri -> Some (f uri)
    | Device _ -> None
  ;;

  let with_device_prompt t ~f =
    match t with
    | Device { verification_uri; user_code } -> Some (f ~verification_uri ~user_code)
    | Browser _ -> None
  ;;

  module Authorized_transport = struct
    let to_json = function
      | Browser uri ->
        `Object
          [ "kind", `String "browser"; "authorization_uri", `String (Uri.to_string uri) ]
      | Device { verification_uri; user_code } ->
        `Object
          [ "kind", `String "device"
          ; "verification_uri", `String (Uri.to_string verification_uri)
          ; "user_code", `String user_code
          ]
    ;;

    let uri json =
      let open Result.Let_syntax in
      let%bind encoded = Json_codec.string json in
      let%bind () = check (String.length encoded <= 4096) in
      try
        let uri = Uri.of_string encoded in
        if valid_uri uri then Ok uri else Error (invalid ())
      with
      | _ -> Error (invalid ())
    ;;

    let of_json json =
      let open Result.Let_syntax in
      let%bind f =
        fields json [ "kind"; "authorization_uri"; "verification_uri"; "user_code" ]
      in
      let%bind kind = req f "kind" Json_codec.string in
      match kind with
      | "browser" ->
        let%bind () =
          check
            (Option.is_none (Json_codec.optional f "verification_uri")
             && Option.is_none (Json_codec.optional f "user_code"))
        in
        let%bind authorization_uri = req f "authorization_uri" uri in
        browser ~authorization_uri
      | "device" ->
        let%bind () =
          check (Option.is_none (Json_codec.optional f "authorization_uri"))
        in
        let%bind verification_uri = req f "verification_uri" uri in
        let%bind user_code = req f "user_code" Json_codec.string in
        device ~verification_uri ~user_code
      | _ -> Error (invalid ())
    ;;
  end
end

module Setup_request = struct
  type t = { idempotency_key : Idempotency_key.t } [@@deriving sexp]

  let to_json t = `Object [ "idempotency_key", Idempotency_key.to_json t.idempotency_key ]

  let of_json json =
    let open Result.Let_syntax in
    let%bind f = fields json [ "idempotency_key" ] in
    let%map idempotency_key = req f "idempotency_key" Idempotency_key.of_json in
    { idempotency_key }
  ;;
end

module Setup_result = struct
  type t =
    { server_id : Id.Server.t
    ; revision : Revision.t
    }
  [@@deriving sexp]

  let to_json t =
    `Object
      [ "server_id", Id.Server.to_json t.server_id
      ; "revision", Revision.to_json t.revision
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind f = fields json [ "server_id"; "revision" ] in
    let%bind server_id = req f "server_id" Id.Server.of_json in
    let%map revision = req f "revision" Revision.of_json in
    { server_id; revision }
  ;;
end

module Status_request = struct
  type t = { profile : Profile_id.t option } [@@deriving sexp]

  let to_json t = `Object [ "profile", optional Profile_id.to_json t.profile ]

  let of_json json =
    let open Result.Let_syntax in
    let%bind f = fields json [ "profile" ] in
    let%map profile = nullable f "profile" Profile_id.of_json in
    { profile }
  ;;
end

module Selection_result = struct
  type t =
    { profile : Profile_id.t
    ; revision : Revision.t
    }
  [@@deriving sexp]

  let to_json t =
    `Object
      [ "profile", Profile_id.to_json t.profile; "revision", Revision.to_json t.revision ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind f = fields json [ "profile"; "revision" ] in
    let%bind profile = req f "profile" Profile_id.of_json in
    let%map revision = req f "revision" Revision.of_json in
    { profile; revision }
  ;;
end

module Login_request = struct
  type t =
    { profile : Profile_id.t
    ; mode : Login_mode.t
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  let to_json t =
    `Object
      [ "profile", Profile_id.to_json t.profile
      ; "mode", Login_mode.to_json t.mode
      ; "idempotency_key", Idempotency_key.to_json t.idempotency_key
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind f = fields json [ "profile"; "mode"; "idempotency_key" ] in
    let%bind profile = req f "profile" Profile_id.of_json in
    let%bind mode = req f "mode" Login_mode.of_json in
    let%map idempotency_key = req f "idempotency_key" Idempotency_key.of_json in
    { profile; mode; idempotency_key }
  ;;
end

module Challenge_request = struct
  type t = { flow : Flow_ref.t } [@@deriving sexp]

  let to_json t = `Object [ "flow", Flow_ref.to_json t.flow ]

  let of_json json =
    let open Result.Let_syntax in
    let%bind f = fields json [ "flow" ] in
    let%map flow = req f "flow" Flow_ref.of_json in
    { flow }
  ;;
end

module Cancel_request = struct
  type t =
    { flow : Flow_ref.t
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  let to_json t =
    `Object
      [ "flow", Flow_ref.to_json t.flow
      ; "idempotency_key", Idempotency_key.to_json t.idempotency_key
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind f = fields json [ "flow"; "idempotency_key" ] in
    let%bind flow = req f "flow" Flow_ref.of_json in
    let%map idempotency_key = req f "idempotency_key" Idempotency_key.of_json in
    { flow; idempotency_key }
  ;;
end

module Logout_request = struct
  type t =
    { profile : Profile_id.t
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  let to_json t =
    `Object
      [ "profile", Profile_id.to_json t.profile
      ; "idempotency_key", Idempotency_key.to_json t.idempotency_key
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind f = fields json [ "profile"; "idempotency_key" ] in
    let%bind profile = req f "profile" Profile_id.of_json in
    let%map idempotency_key = req f "idempotency_key" Idempotency_key.of_json in
    { profile; idempotency_key }
  ;;
end

module Select_request = struct
  type t =
    { profile : Profile_id.t
    ; expected_revision : Revision.t
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  let to_json t =
    `Object
      [ "profile", Profile_id.to_json t.profile
      ; "expected_revision", Revision.to_json t.expected_revision
      ; "idempotency_key", Idempotency_key.to_json t.idempotency_key
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind f = fields json [ "profile"; "expected_revision"; "idempotency_key" ] in
    let%bind profile = req f "profile" Profile_id.of_json in
    let%bind expected_revision = req f "expected_revision" Revision.of_json in
    let%map idempotency_key = req f "idempotency_key" Idempotency_key.of_json in
    { profile; expected_revision; idempotency_key }
  ;;
end

module Environment_request = struct
  type t =
    { profile : Profile_id.t
    ; source : Source_id.t
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  let to_json t =
    `Object
      [ "profile", Profile_id.to_json t.profile
      ; "source", Source_id.to_json t.source
      ; "idempotency_key", Idempotency_key.to_json t.idempotency_key
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind f = fields json [ "profile"; "source"; "idempotency_key" ] in
    let%bind profile = req f "profile" Profile_id.of_json in
    let%bind source = req f "source" Source_id.of_json in
    let%map idempotency_key = req f "idempotency_key" Idempotency_key.of_json in
    { profile; source; idempotency_key }
  ;;
end

module Configuration_result = struct
  type t =
    { profile : Profile_id.t
    ; auth_epoch : int64
    ; revision : Revision.t
    }
  [@@deriving sexp]

  let to_json t =
    `Object
      [ "profile", Profile_id.to_json t.profile
      ; "auth_epoch", number t.auth_epoch
      ; "revision", Revision.to_json t.revision
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind f = fields json [ "profile"; "auth_epoch"; "revision" ] in
    let%bind profile = req f "profile" Profile_id.of_json in
    let%bind auth_epoch = req f "auth_epoch" epoch in
    let%map revision = req f "revision" Revision.of_json in
    { profile; auth_epoch; revision }
  ;;
end

module Status_result = struct
  type availability =
    | Missing
    | Configured
    | Disabled
    | Renewal_required
    | Renewal_uncertain
    | Secret_unavailable
    | Store_unavailable
  [@@deriving equal, sexp]

  type failure =
    | Network
    | Account_denied
    | Model_denied
    | Submission_uncertain
  [@@deriving equal, sexp]

  type profile =
    { profile : Profile_id.t
    ; account : string option
    ; availability : availability
    ; last_failure : failure option
    ; auth_epoch : int64 option
    ; credential_revision : Revision.t option
    }
  [@@deriving sexp]

  type t =
    { server_id : Id.Server.t
    ; setup_required : bool
    ; profiles : profile list
    ; flows : Flow_result.t list
    ; selection : Selection_result.t option
    }
  [@@deriving sexp]

  let availability_values =
    [ "missing", Missing
    ; "configured", Configured
    ; "disabled", Disabled
    ; "renewal_required", Renewal_required
    ; "renewal_uncertain", Renewal_uncertain
    ; "secret_unavailable", Secret_unavailable
    ; "store_unavailable", Store_unavailable
    ]
  ;;

  let failure_values =
    [ "network", Network
    ; "account_denied", Account_denied
    ; "model_denied", Model_denied
    ; "submission_uncertain", Submission_uncertain
    ]
  ;;

  let availability_json t =
    `String
      (List.find_exn availability_values ~f:(fun (_, v) -> equal_availability v t) |> fst)
  ;;

  let failure_json t =
    `String (List.find_exn failure_values ~f:(fun (_, v) -> equal_failure v t) |> fst)
  ;;

  let profile_json (t : profile) =
    `Object
      [ "profile", Profile_id.to_json t.profile
      ; "account", optional (fun s -> `String s) t.account
      ; "availability", availability_json t.availability
      ; "last_failure", optional failure_json t.last_failure
      ; "auth_epoch", optional number t.auth_epoch
      ; "credential_revision", optional Revision.to_json t.credential_revision
      ]
  ;;

  let profile_of_json json =
    let open Result.Let_syntax in
    let%bind f =
      fields
        json
        [ "profile"
        ; "account"
        ; "availability"
        ; "last_failure"
        ; "auth_epoch"
        ; "credential_revision"
        ]
    in
    let%bind profile = req f "profile" Profile_id.of_json in
    let%bind account = nullable f "account" label in
    let%bind availability =
      req
        f
        "availability"
        (Json_codec.enum ~name:"provider availability" availability_values)
    in
    let%bind last_failure =
      nullable
        f
        "last_failure"
        (Json_codec.enum ~name:"provider last failure" failure_values)
    in
    let%bind auth_epoch = nullable f "auth_epoch" epoch in
    let%map credential_revision = nullable f "credential_revision" Revision.of_json in
    { profile; account; availability; last_failure; auth_epoch; credential_revision }
  ;;

  let to_json t =
    `Object
      [ "server_id", Id.Server.to_json t.server_id
      ; ("setup_required", if t.setup_required then `True else `False)
      ; "profiles", `Array (List.map t.profiles ~f:profile_json)
      ; "flows", `Array (List.map t.flows ~f:Flow_result.to_json)
      ; "selection", optional Selection_result.to_json t.selection
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind f =
      fields json [ "server_id"; "setup_required"; "profiles"; "flows"; "selection" ]
    in
    let%bind server_id = req f "server_id" Id.Server.of_json in
    let%bind setup_required = req f "setup_required" Json_codec.bool in
    let%bind profiles = req f "profiles" (Json_codec.list profile_of_json) in
    let%bind flows = req f "flows" (Json_codec.list Flow_result.of_json) in
    let%bind selection = nullable f "selection" Selection_result.of_json in
    let%bind () =
      check
        (List.length profiles <= 128
         && List.length flows <= 128
         && (not
               (List.contains_dup
                  (List.map profiles ~f:(fun (p : profile) -> p.profile))
                  ~compare:Profile_id.compare))
         && (not
               (List.contains_dup
                  (List.map flows ~f:(fun (flow : Flow_result.t) -> flow.flow.flow_id))
                  ~compare:Flow_id.compare))
         && List.for_all flows ~f:(fun (flow : Flow_result.t) ->
           Id.Server.equal server_id flow.flow.server_id))
    in
    let%map () =
      check
        ((not setup_required)
         || (List.is_empty profiles && List.is_empty flows && Option.is_none selection))
    in
    { server_id; setup_required; profiles; flows; selection }
  ;;
end

module Logout_result = struct
  type drain =
    | Drained
    | Pending
  [@@deriving equal, sexp]

  type t =
    { profile : Profile_id.t
    ; auth_epoch : int64
    ; drain : drain
    ; cleanup_pending : bool
    }
  [@@deriving sexp]

  let to_json t =
    `Object
      [ "profile", Profile_id.to_json t.profile
      ; "auth_epoch", number t.auth_epoch
      ; ( "drain"
        , `String
            (match t.drain with
             | Drained -> "drained"
             | Pending -> "pending") )
      ; ("cleanup_pending", if t.cleanup_pending then `True else `False)
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind f = fields json [ "profile"; "auth_epoch"; "drain"; "cleanup_pending" ] in
    let%bind profile = req f "profile" Profile_id.of_json in
    let%bind auth_epoch = req f "auth_epoch" epoch in
    let%bind drain =
      req
        f
        "drain"
        (Json_codec.enum
           ~name:"provider admission drain"
           [ "drained", Drained; "pending", Pending ])
    in
    let%map cleanup_pending = req f "cleanup_pending" Json_codec.bool in
    { profile; auth_epoch; drain; cleanup_pending }
  ;;
end
