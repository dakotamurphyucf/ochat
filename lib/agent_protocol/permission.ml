open Core

type state =
  | Pending
  | Approved
  | Denied
  | Expired
  | Cancelled
[@@deriving compare, equal, sexp]

type choice =
  | Approve_once
  | Approve_session
  | Approve_prefix
  | Durable_exact
  | Deny
[@@deriving compare, equal, sexp]

type owner =
  | Operation of Id.Operation.t
  | Invocation of Id.Invocation.t
[@@deriving equal, sexp]

type t =
  { id : Id.Permission.t
  ; session_id : Id.Session.t
  ; generation : int
  ; owner : owner
  ; call_id : string
  ; tool_name : string
  ; runtime_identity : string option
  ; invocation_display : string
  ; rationale : string option
  ; effects : string list
  ; choices : choice list
  ; created_at : Timestamp.t
  ; expires_at : Timestamp.t option
  ; state : state
  ; resolution : resolution option
  }
[@@deriving sexp]

and resolution =
  { choice : choice
  ; principal_id : Id.Principal.t option
  ; resolved_at : Timestamp.t
  ; reason : string option
  }
[@@deriving sexp]

let t_of_sexp sexp =
  let sexp =
    match sexp with
    | Sexp.List fields ->
      Sexp.List
        (List.map fields ~f:(function
           | Sexp.List [ Atom "operation_id"; id ] ->
             Sexp.List [ Atom "owner"; List [ Atom "Operation"; id ] ]
           | field -> field))
    | _ -> sexp
  in
  t_of_sexp sexp
;;

let optional_field name value encode =
  Option.map value ~f:(fun value -> name, encode value)
;;

let state_values =
  [ "pending", Pending
  ; "approved", Approved
  ; "denied", Denied
  ; "expired", Expired
  ; "cancelled", Cancelled
  ]
;;

let state_to_string state =
  List.Assoc.find_exn
    (List.map state_values ~f:(fun (name, state) -> state, name))
    state
    ~equal:equal_state
;;

let state_of_json = Json_codec.enum ~name:"permission state" state_values

let choice_values =
  [ "approve_once", Approve_once
  ; "approve_session", Approve_session
  ; "approve_prefix", Approve_prefix
  ; "durable_exact", Durable_exact
  ; "deny", Deny
  ]
;;

let choice_to_string choice =
  List.Assoc.find_exn
    (List.map choice_values ~f:(fun (name, choice) -> choice, name))
    choice
    ~equal:equal_choice
;;

let choice_of_json = Json_codec.enum ~name:"permission choice" choice_values

let validate_nonempty name value =
  if String.is_empty value
  then Error (Protocol_error.invalid_request (name ^ " must be nonempty"))
  else Ok value
;;

let validate_unique name values ~compare =
  if Option.is_some (List.find_a_dup values ~compare)
  then Error (Protocol_error.invalid_request (name ^ " contains duplicates"))
  else Ok values
;;

module Resolution = struct
  type t = resolution [@@deriving sexp]

  let to_json t =
    let fields =
      [ Some ("choice", `String (choice_to_string t.choice))
      ; optional_field "principal_id" t.principal_id Id.Principal.to_json
      ; Some ("resolved_at", Timestamp.to_json t.resolved_at)
      ; optional_field "reason" t.reason (fun value -> `String value)
      ]
      |> List.filter_opt
    in
    `Object fields
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind choice = Json_codec.required_as fields "choice" choice_of_json in
    let%bind principal_id =
      Json_codec.optional_as fields "principal_id" Id.Principal.of_json
    in
    let%bind resolved_at =
      Json_codec.required_as fields "resolved_at" Timestamp.of_json
    in
    let%map reason = Json_codec.optional_as fields "reason" Json_codec.string in
    { choice; principal_id; resolved_at; reason }
  ;;
end

let to_json t =
  let fields =
    [ Some ("id", Id.Permission.to_json t.id)
    ; Some ("session_id", Id.Session.to_json t.session_id)
    ; Some ("generation", `Number (Int.to_string t.generation))
    ; Some
        (match t.owner with
         | Operation id -> "operation_id", Id.Operation.to_json id
         | Invocation id -> "invocation_id", Id.Invocation.to_json id)
    ; Some ("call_id", `String t.call_id)
    ; Some ("tool_name", `String t.tool_name)
    ; optional_field "runtime_identity" t.runtime_identity (fun value -> `String value)
    ; Some ("invocation_display", `String t.invocation_display)
    ; optional_field "rationale" t.rationale (fun value -> `String value)
    ; Some ("effects", `Array (List.map t.effects ~f:(fun value -> `String value)))
    ; Some
        ( "choices"
        , `Array (List.map t.choices ~f:(fun choice -> `String (choice_to_string choice)))
        )
    ; Some ("created_at", Timestamp.to_json t.created_at)
    ; optional_field "expires_at" t.expires_at Timestamp.to_json
    ; Some ("state", `String (state_to_string t.state))
    ; optional_field "resolution" t.resolution Resolution.to_json
    ]
    |> List.filter_opt
  in
  `Object fields
;;

let decode_identity fields =
  let open Result.Let_syntax in
  let%bind id = Json_codec.required_as fields "id" Id.Permission.of_json in
  let%bind session_id = Json_codec.required_as fields "session_id" Id.Session.of_json in
  let%bind generation =
    Json_codec.required_as
      fields
      "generation"
      (Json_codec.bounded_int ~min:0 ~max:Int.max_value)
  in
  let%map owner =
    match
      ( List.Assoc.mem (Json_codec.to_alist fields) "operation_id" ~equal:String.equal
      , List.Assoc.mem (Json_codec.to_alist fields) "invocation_id" ~equal:String.equal )
    with
    | true, false ->
      Json_codec.required_as fields "operation_id" Id.Operation.of_json
      |> Result.map ~f:(fun id -> Operation id)
    | false, true ->
      Json_codec.required_as fields "invocation_id" Id.Invocation.of_json
      |> Result.map ~f:(fun id -> Invocation id)
    | _ -> Error (Protocol_error.invalid_request "permission requires exactly one owner")
  in
  id, session_id, generation, owner
;;

let decode_invocation fields =
  let open Result.Let_syntax in
  let%bind call_id = Json_codec.required_as fields "call_id" Json_codec.string in
  let%bind tool_name = Json_codec.required_as fields "tool_name" Json_codec.string in
  let%bind runtime_identity =
    Json_codec.optional_as fields "runtime_identity" Json_codec.string
  in
  let%bind invocation_display =
    Json_codec.required_as fields "invocation_display" Json_codec.string
  in
  let%map rationale = Json_codec.optional_as fields "rationale" Json_codec.string in
  call_id, tool_name, runtime_identity, invocation_display, rationale
;;

let decode_policy fields =
  let open Result.Let_syntax in
  let%bind effects =
    Json_codec.required_as fields "effects" (Json_codec.list Json_codec.string)
  in
  let%bind choices =
    Json_codec.required_as fields "choices" (Json_codec.list choice_of_json)
  in
  let%bind created_at = Json_codec.required_as fields "created_at" Timestamp.of_json in
  let%map expires_at = Json_codec.optional_as fields "expires_at" Timestamp.of_json in
  effects, choices, created_at, expires_at
;;

let validate t =
  let open Result.Let_syntax in
  let%bind (_ : string) = validate_nonempty "permission call ID" t.call_id in
  let%bind (_ : string) = validate_nonempty "permission tool name" t.tool_name in
  let%bind (_ : string) =
    validate_nonempty "permission invocation display" t.invocation_display
  in
  let%bind effects =
    validate_unique "permission effects" t.effects ~compare:String.compare
  in
  let%bind choices =
    validate_unique "permission choices" t.choices ~compare:compare_choice
  in
  if List.is_empty choices
  then Error (Protocol_error.invalid_request "permission choices must be nonempty")
  else if not (Bool.equal (equal_state t.state Pending) (Option.is_none t.resolution))
  then Error (Protocol_error.invalid_request "permission state and resolution disagree")
  else Ok { t with effects; choices }
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind fields = Json_codec.fields json in
  let%bind id, session_id, generation, owner = decode_identity fields in
  let%bind call_id, tool_name, runtime_identity, invocation_display, rationale =
    decode_invocation fields
  in
  let%bind effects, choices, created_at, expires_at = decode_policy fields in
  let%bind state = Json_codec.required_as fields "state" state_of_json in
  let%bind resolution = Json_codec.optional_as fields "resolution" Resolution.of_json in
  validate
    { id
    ; session_id
    ; generation
    ; owner
    ; call_id
    ; tool_name
    ; runtime_identity
    ; invocation_display
    ; rationale
    ; effects
    ; choices
    ; created_at
    ; expires_at
    ; state
    ; resolution
    }
;;

module List_request = struct
  type t =
    { session_id : Id.Session.t
    ; page : Page.Request.t
    ; state : state option
    }
  [@@deriving sexp]

  let to_json t =
    let fields =
      ("session_id", Id.Session.to_json t.session_id) :: Page.Request.to_fields t.page
    in
    match t.state with
    | None -> `Object fields
    | Some state -> `Object (fields @ [ "state", `String (state_to_string state) ])
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind session_id = Json_codec.required_as fields "session_id" Id.Session.of_json in
    let%bind page = Page.Request.of_fields fields in
    let%map state = Json_codec.optional_as fields "state" state_of_json in
    { session_id; page; state }
  ;;
end

module Respond_request = struct
  type t =
    { session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
    ; permission_id : Id.Permission.t
    ; permission_generation : int
    ; choice : choice
    ; reason : string option
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  let to_json t =
    let fields =
      [ Some ("session_id", Id.Session.to_json t.session_id)
      ; Some ("attachment_id", Id.Attachment.to_json t.attachment_id)
      ; Some ("permission_id", Id.Permission.to_json t.permission_id)
      ; Some ("permission_generation", `Number (Int.to_string t.permission_generation))
      ; Some ("choice", `String (choice_to_string t.choice))
      ; optional_field "reason" t.reason (fun value -> `String value)
      ; Some ("idempotency_key", Idempotency_key.to_json t.idempotency_key)
      ]
      |> List.filter_opt
    in
    `Object fields
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind session_id = Json_codec.required_as fields "session_id" Id.Session.of_json in
    let%bind attachment_id =
      Json_codec.required_as fields "attachment_id" Id.Attachment.of_json
    in
    let%bind permission_id =
      Json_codec.required_as fields "permission_id" Id.Permission.of_json
    in
    let%bind permission_generation =
      Json_codec.required_as
        fields
        "permission_generation"
        (Json_codec.bounded_int ~min:0 ~max:Int.max_value)
    in
    let%bind choice = Json_codec.required_as fields "choice" choice_of_json in
    let%bind reason = Json_codec.optional_as fields "reason" Json_codec.string in
    let%map idempotency_key =
      Json_codec.required_as fields "idempotency_key" Idempotency_key.of_json
    in
    { session_id
    ; attachment_id
    ; permission_id
    ; permission_generation
    ; choice
    ; reason
    ; idempotency_key
    }
  ;;
end

module Respond_result = struct
  type nonrec t =
    { permission : t
    ; mutation : Mutation_result.t
    }
  [@@deriving sexp]

  let to_json t =
    `Object (("permission", to_json t.permission) :: Mutation_result.to_fields t.mutation)
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind permission = Json_codec.required_as fields "permission" of_json in
    let%map mutation = Mutation_result.of_fields fields in
    { permission; mutation }
  ;;
end
