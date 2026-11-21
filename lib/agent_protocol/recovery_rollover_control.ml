open! Core
module J = Json_codec
module W = Recovery_wire
module Scope = Recovery_window.Scope

let feature = "recovery.rollover_control"
let max_envelope_bytes = 8192
let limits = W.limits ~max_bytes:max_envelope_bytes ~max_fields:100 ~max_nodes:200
let invalid message = Error (Protocol_error.invalid_request message)
let admit json = Result.map (W.measure ~limits json) ~f:ignore
let boolean value = if value then `True else `False
let optional name encode = Option.map ~f:(fun value -> name, encode value)

let session_of_json json =
  let open Result.Let_syntax in
  let%bind _ = W.fields json [ "server_id"; "session_id" ] in
  Session_ref.of_json json
;;

let label ~name ~equal entries value =
  match List.find entries ~f:(fun (_, candidate) -> equal candidate value) with
  | Some (label, _) -> `String label
  | None -> failwith ("missing " ^ name ^ " wire value")
;;

module Basis = struct
  type t =
    { scope : Scope.t
    ; revision : int64
    }
  [@@deriving equal, sexp_of]

  let create scope ~revision =
    if Int64.(revision >= 0L)
    then Ok { scope; revision }
    else invalid "invalid rollover revision"
  ;;

  let to_json t =
    `Object [ "scope", Scope.to_json t.scope; "revision", W.revision_to_json t.revision ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = W.fields json [ "scope"; "revision" ] in
    let%bind scope = J.required_as fields "scope" Scope.of_json in
    let%bind revision = J.required_as fields "revision" W.revision_of_json in
    create scope ~revision
  ;;
end

module Inspect = struct
  let method_name = "session.recovery.status"

  type t =
    { session : Session_ref.t
    ; attachment_id : Id.Attachment.t option
    ; key : Idempotency_key.t option
    }

  let to_json t =
    `Object
      (("session", Session_ref.to_json t.session)
       :: List.filter_opt
            [ optional "attachment_id" Id.Attachment.to_json t.attachment_id
            ; optional "idempotency_key" Idempotency_key.to_json t.key
            ])
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind () = admit json in
    let%bind fields = W.fields json [ "session"; "attachment_id"; "idempotency_key" ] in
    let%bind session = J.required_as fields "session" session_of_json in
    let%bind attachment_id = J.optional_as fields "attachment_id" Id.Attachment.of_json in
    let%map key = J.optional_as fields "idempotency_key" Idempotency_key.of_json in
    { session; attachment_id; key }
  ;;
end

module Request = struct
  let method_name = "session.recovery.rollover"

  type t =
    { expected : Basis.t
    ; attachment_id : Id.Attachment.t
    ; key : Idempotency_key.t
    ; force : bool
    }

  let session t = Scope.session t.expected.scope

  let to_json t =
    `Object
      [ "expected", Basis.to_json t.expected
      ; "attachment_id", Id.Attachment.to_json t.attachment_id
      ; "idempotency_key", Idempotency_key.to_json t.key
      ; "force", boolean t.force
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind () = admit json in
    let%bind fields =
      W.fields json [ "expected"; "attachment_id"; "idempotency_key"; "force" ]
    in
    let%bind expected = J.required_as fields "expected" Basis.of_json in
    let%bind attachment_id = J.required_as fields "attachment_id" Id.Attachment.of_json in
    let%bind key = J.required_as fields "idempotency_key" Idempotency_key.of_json in
    let%map force = J.required_as fields "force" J.bool in
    { expected; attachment_id; key; force }
  ;;
end

module Receipt = struct
  type failure =
    | Basis_changed
    | Cancelled
    | Checkpoint_required
    | Hook_vetoed
    | Hook_failed
  [@@deriving equal, sexp_of]

  let failures =
    [ "basis_changed", Basis_changed
    ; "cancelled", Cancelled
    ; "checkpoint_required", Checkpoint_required
    ; "hook_vetoed", Hook_vetoed
    ; "hook_failed", Hook_failed
    ]
  ;;

  type quality =
    | Checkpointed
    | Archive_only_missing_checkpoint
    | Archive_only_unavailable_checkpoint
  [@@deriving equal, sexp_of]

  let qualities =
    [ "checkpointed", Checkpointed
    ; "archive_only_missing_checkpoint", Archive_only_missing_checkpoint
    ; "archive_only_unavailable_checkpoint", Archive_only_unavailable_checkpoint
    ]
  ;;

  type outcome =
    | Pending
    | Committed of
        { successor : Scope.t
        ; cut_revision : int64
        ; quality : quality
        }
    | Failed of failure
  [@@deriving equal, sexp_of]

  type t =
    { expected : Basis.t
    ; outcome : outcome
    }
  [@@deriving equal, sexp_of]

  let create (expected : Basis.t) outcome =
    let valid =
      match outcome with
      | Pending | Failed _ -> true
      | Committed { successor; cut_revision; quality = _ } ->
        Session_ref.equal (Scope.session expected.scope) (Scope.session successor)
        && Int.equal (Scope.generation expected.scope) (Scope.generation successor)
        && (not
              (Id.Context_window.equal
                 (Scope.window expected.scope)
                 (Scope.window successor)))
        && Int64.(cut_revision > expected.revision)
    in
    if valid
    then Ok { expected; outcome }
    else invalid "invalid committed rollover receipt"
  ;;

  let to_json t =
    let status =
      match t.outcome with
      | Pending -> [ "state", `String "pending" ]
      | Failed failure ->
        [ "state", `String "failed"
        ; "reason", label ~name:"rollover failure" ~equal:equal_failure failures failure
        ]
      | Committed { successor; cut_revision; quality } ->
        [ "state", `String "committed"
        ; "successor", Scope.to_json successor
        ; "cut_revision", W.revision_to_json cut_revision
        ; "quality", label ~name:"rollover quality" ~equal:equal_quality qualities quality
        ]
    in
    `Object (("expected", Basis.to_json t.expected) :: status)
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind () = admit json in
    let%bind fields = J.fields json in
    let%bind expected = J.required_as fields "expected" Basis.of_json in
    let%bind state = J.required_as fields "state" J.string in
    let closed names = Extension_codec.closed fields ("expected" :: "state" :: names) in
    let%bind outcome =
      match state with
      | "pending" ->
        let%map () = closed [] in
        Pending
      | "failed" ->
        let%bind () = closed [ "reason" ] in
        let%map reason =
          J.required_as fields "reason" (J.enum ~name:"rollover failure" failures)
        in
        Failed reason
      | "committed" ->
        let%bind () = closed [ "successor"; "cut_revision"; "quality" ] in
        let%bind successor = J.required_as fields "successor" Scope.of_json in
        let%bind cut_revision = J.required_as fields "cut_revision" W.revision_of_json in
        let%map quality =
          J.required_as fields "quality" (J.enum ~name:"rollover quality" qualities)
        in
        Committed { successor; cut_revision; quality }
      | _ -> invalid "invalid rollover receipt state"
    in
    create expected outcome
  ;;

  let validate t ~(request : Request.t) =
    let forced =
      match t.outcome with
      | Committed
          { quality =
              Archive_only_missing_checkpoint | Archive_only_unavailable_checkpoint
          ; _
          } -> true
      | Pending | Failed _ | Committed { quality = Checkpointed; _ } -> false
    in
    if Basis.equal t.expected request.expected && (request.force || not forced)
    then Ok ()
    else invalid "rollover receipt differs from the request"
  ;;
end

module Capacity = struct
  type target =
    { profile : string
    ; model : string
    ; configuration_revision : int64
    }
  [@@deriving equal, sexp_of]

  let target ~profile ~model ~configuration_revision =
    let valid_label value =
      String.length value > 0
      && String.length value <= 256
      && Stdlib.String.is_valid_utf_8 value
    in
    if valid_label profile && valid_label model && Int64.(configuration_revision >= 0L)
    then Ok { profile; model; configuration_revision }
    else invalid "invalid recovery capacity target"
  ;;

  let target_to_json t =
    `Object
      [ "profile", `String t.profile
      ; "model", `String t.model
      ; "configuration_revision", W.revision_to_json t.configuration_revision
      ]
  ;;

  let target_of_json json =
    let open Result.Let_syntax in
    let%bind fields = W.fields json [ "profile"; "model"; "configuration_revision" ] in
    let%bind profile = J.required_as fields "profile" J.string in
    let%bind model = J.required_as fields "model" J.string in
    let%bind configuration_revision =
      J.required_as fields "configuration_revision" W.revision_of_json
    in
    target ~profile ~model ~configuration_revision
  ;;

  type unavailable =
    | Disabled
    | Awaiting_preparation
    | Stale_preparation
    | Evidence_unavailable
  [@@deriving equal, sexp_of]

  let unavailable =
    [ "disabled", Disabled
    ; "awaiting_preparation", Awaiting_preparation
    ; "stale_preparation", Stale_preparation
    ; "evidence_unavailable", Evidence_unavailable
    ]
  ;;

  type freshness = Session_configuration.phase =
    | Preparing
    | Effective
    | Retained
  [@@deriving equal, sexp_of]

  let freshness = [ "preparing", Preparing; "effective", Effective; "retained", Retained ]

  type evidence =
    | Measured
    | Estimated
  [@@deriving equal, sexp_of]

  let evidence = [ "measured", Measured; "estimated", Estimated ]

  type remaining =
    | Available of int64
    | Exceeded_by of int64
    | Excess_out_of_range
  [@@deriving equal, sexp_of]

  let remaining_to_json = function
    | Available n -> `Object [ "available", W.revision_to_json n ]
    | Exceeded_by n -> `Object [ "exceeded_by", W.revision_to_json n ]
    | Excess_out_of_range -> `Object [ "excess_out_of_range", `True ]
  ;;

  let remaining_of_json json =
    let open Result.Let_syntax in
    let%bind fields =
      W.fields json [ "available"; "exceeded_by"; "excess_out_of_range" ]
    in
    match J.to_alist fields with
    | [ ("available", n) ] -> Result.map (W.revision_of_json n) ~f:(fun n -> Available n)
    | [ ("exceeded_by", n) ] ->
      let%bind n = W.revision_of_json n in
      if Int64.(n > 0L) then Ok (Exceeded_by n) else invalid "invalid exceeded capacity"
    | [ ("excess_out_of_range", `True) ] -> Ok Excess_out_of_range
    | _ -> invalid "invalid recovery remaining capacity"
  ;;

  type t =
    | Unknown of
        { reason : unavailable
        ; target : target option
        }
    | Observed of
        { target : target
        ; freshness : freshness
        ; remaining : (evidence * remaining) option
        }
  [@@deriving equal, sexp_of]

  let valid = function
    | Unknown _ | Observed { remaining = None; _ } -> true
    | Observed { remaining = Some (_, Available n); _ } -> Int64.(n >= 0L)
    | Observed { remaining = Some (_, Exceeded_by n); _ } -> Int64.(n > 0L)
    | Observed { remaining = Some (_, Excess_out_of_range); _ } -> true
  ;;

  let to_json = function
    | Unknown { reason; target } ->
      `Object
        (( "unavailable"
         , label ~name:"capacity availability" ~equal:equal_unavailable unavailable reason
         )
         :: Option.to_list (optional "target" target_to_json target))
    | Observed { target; freshness = phase; remaining } ->
      `Object
        ([ "target", target_to_json target
         ; ( "freshness"
           , label ~name:"capacity freshness" ~equal:equal_freshness freshness phase )
         ]
         @ Option.value_map remaining ~default:[] ~f:(fun (kind, value) ->
           [ ( "evidence"
             , label ~name:"capacity evidence" ~equal:equal_evidence evidence kind )
           ; "remaining", remaining_to_json value
           ]))
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = J.fields json in
    match J.optional fields "unavailable" with
    | Some reason ->
      let%bind () = Extension_codec.closed fields [ "unavailable"; "target" ] in
      let%bind reason = J.enum ~name:"capacity availability" unavailable reason in
      let%map target = J.optional_as fields "target" target_of_json in
      Unknown { reason; target }
    | None ->
      let%bind () =
        Extension_codec.closed fields [ "target"; "freshness"; "evidence"; "remaining" ]
      in
      let%bind target = J.required_as fields "target" target_of_json in
      let%bind freshness =
        J.required_as fields "freshness" (J.enum ~name:"capacity freshness" freshness)
      in
      let%bind evidence =
        J.optional_as fields "evidence" (J.enum ~name:"capacity evidence" evidence)
      in
      let%bind remaining = J.optional_as fields "remaining" remaining_of_json in
      (match evidence, remaining with
       | None, None -> Ok (Observed { target; freshness; remaining = None })
       | Some kind, Some value ->
         Ok (Observed { target; freshness; remaining = Some (kind, value) })
       | _ -> invalid "recovery capacity evidence and remaining must be paired")
  ;;
end

module Status = struct
  type actions =
    { rollover : bool
    ; force : bool
    }
  [@@deriving equal, sexp_of]

  type view =
    | Unavailable
    | Available of
        { current : Basis.t
        ; previous : Scope.t option
        ; capacity : Capacity.t
        ; receipt : Receipt.t option
        ; actions : actions
        }
  [@@deriving equal, sexp_of]

  type t =
    { session : Session_ref.t
    ; view : view
    }
  [@@deriving equal, sexp_of]

  let create session view =
    let local scope = Session_ref.equal session (Scope.session scope) in
    let valid =
      match view with
      | Unavailable -> true
      | Available { current; previous; capacity; receipt; actions } ->
        local current.scope
        && Option.for_all previous ~f:(fun prior ->
          local prior
          && Scope.generation prior <= Scope.generation current.scope
          && not
               (Id.Context_window.equal (Scope.window prior) (Scope.window current.scope)))
        && Option.for_all receipt ~f:(fun (receipt : Receipt.t) ->
          local receipt.expected.scope
          && Int64.(receipt.expected.revision <= current.revision)
          &&
          match receipt.outcome with
          | Pending | Failed _ -> true
          | Committed { cut_revision; _ } -> Int64.(cut_revision < current.revision))
        && Capacity.valid capacity
        && (actions.rollover || not actions.force)
    in
    if valid
    then Ok { session; view }
    else invalid "rollover status does not match its session"
  ;;

  let actions_to_json t =
    `Object [ "rollover", boolean t.rollover; "force", boolean t.force ]
  ;;

  let actions_of_json json =
    let open Result.Let_syntax in
    let%bind fields = W.fields json [ "rollover"; "force" ] in
    let%bind rollover = J.required_as fields "rollover" J.bool in
    let%map force = J.required_as fields "force" J.bool in
    { rollover; force }
  ;;

  let to_json t =
    let fields =
      match t.view with
      | Unavailable -> [ "state", `String "unavailable" ]
      | Available { current; previous; capacity; receipt; actions } ->
        [ "state", `String "available"
        ; "current", Basis.to_json current
        ; "capacity", Capacity.to_json capacity
        ; "actions", actions_to_json actions
        ]
        @ List.filter_opt
            [ optional "previous" Scope.to_json previous
            ; optional "receipt" Receipt.to_json receipt
            ]
    in
    `Object (("session", Session_ref.to_json t.session) :: fields)
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind () = admit json in
    let%bind fields = J.fields json in
    let%bind session = J.required_as fields "session" session_of_json in
    let%bind state = J.required_as fields "state" J.string in
    let closed names = Extension_codec.closed fields ("session" :: "state" :: names) in
    let%bind view =
      match state with
      | "unavailable" ->
        let%map () = closed [] in
        Unavailable
      | "available" ->
        let%bind () =
          closed [ "current"; "previous"; "capacity"; "receipt"; "actions" ]
        in
        let%bind current = J.required_as fields "current" Basis.of_json in
        let%bind previous = J.optional_as fields "previous" Scope.of_json in
        let%bind capacity = J.required_as fields "capacity" Capacity.of_json in
        let%bind receipt = J.optional_as fields "receipt" Receipt.of_json in
        let%map actions = J.required_as fields "actions" actions_of_json in
        Available { current; previous; capacity; receipt; actions }
      | _ -> invalid "invalid rollover status"
    in
    create session view
  ;;

  let validate t ~(request : Inspect.t) =
    let actions_valid =
      match t.view with
      | Unavailable -> true
      | Available { actions; _ } ->
        Option.is_some request.attachment_id || not (actions.rollover || actions.force)
    in
    if Session_ref.equal t.session request.session && actions_valid
    then Ok ()
    else invalid "rollover status differs from the request"
  ;;
end
