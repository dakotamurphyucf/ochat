open Core
module P = Agent_protocol
module Schema = Chatmd_shell_spec.Tool_schema

module Jsonaf = struct
  include Jsonaf

  let equal = exactly_equal
end

type limits =
  { max_payload_bytes : int
  ; max_payload_depth : int
  ; max_receipts : int
  ; rate_count : int
  ; rate_window_ms : int
  }
[@@deriving equal, sexp]

type context =
  { id : P.Id.Capability.t
  ; session_id : P.Id.Session.t
  ; generation : int
  ; subscription_id : P.Id.Subscription.t
  ; epoch : int
  ; source : P.Invocation.observer
  ; producer : P.Id.Principal.t
  ; namespace : string
  ; schema : Jsonaf.t
  ; created_at : P.Timestamp.t
  ; expires_at : P.Timestamp.t
  ; limits : limits
  }
[@@deriving equal, sexp]

type receipt =
  { id : P.Id.Ingress_event.t
  ; key : P.Idempotency_key.t
  ; payload : Jsonaf.t
  ; payload_sha256 : string
  ; accepted_at : P.Timestamp.t
  }
[@@deriving equal, sexp]

type t =
  { context : context
  ; receipts : receipt list
  ; revoked : string option
  }
[@@deriving equal, sexp]

type admission =
  | Duplicate of receipt
  | Accepted of t * receipt

let default_limits =
  { max_payload_bytes = 64 * 1024
  ; max_payload_depth = 64
  ; max_receipts = 256
  ; rate_count = 32
  ; rate_window_ms = 1_000
  }
;;

let error ?(retryable = false) code message =
  Error (P.Error.create code ~message ~retryable ())
;;

let valid_namespace value =
  String.is_prefix value ~prefix:"external."
  && String.length value <= 128
  && List.for_all (String.split value ~on:'.') ~f:(fun part ->
    (not (String.is_empty part))
    && String.for_all part ~f:(function
      | 'a' .. 'z' | '0' .. '9' | '_' | '-' -> true
      | _ -> false))
;;

let schema context =
  Schema.compile context.schema
  |> Result.map_error ~f:(fun _ ->
    P.Error.invalid_request "unsupported external event schema")
;;

let canonical_payload context payload =
  let open Result.Let_syntax in
  let%bind () =
    P.Json_codec.validate_limits
      ~max_depth:context.limits.max_payload_depth
      ~max_bytes:context.limits.max_payload_bytes
      payload
  in
  let%bind compiled = schema context in
  let%bind () =
    Schema.validate compiled payload
    |> Result.map_error ~f:(fun _ ->
      P.Error.invalid_request "external event does not match the registered schema")
  in
  let%bind payload = P.Json_codec.canonical payload in
  let hash =
    Jsonaf.to_string payload |> Digestif.SHA256.digest_string |> Digestif.SHA256.to_hex
  in
  Ok (payload, hash)
;;

let valid_reason reason = (not (String.is_empty reason)) && String.length reason <= 1024
let validate_id encode decode value = encode value |> decode |> Result.map ~f:ignore

let validate t =
  let open Result.Let_syntax in
  let c = t.context in
  let l = c.limits in
  let%bind () =
    Result.all_unit
      [ validate_id P.Id.Capability.to_json P.Id.Capability.of_json c.id
      ; validate_id P.Id.Session.to_json P.Id.Session.of_json c.session_id
      ; validate_id P.Id.Subscription.to_json P.Id.Subscription.of_json c.subscription_id
      ; validate_id P.Id.Principal.to_json P.Id.Principal.of_json c.producer
      ]
  in
  let%bind () =
    match
      c.generation >= 0
      && c.epoch >= 0
      && (not (String.is_empty c.source.script_id))
      && String.length c.source.script_id <= 256
      && String.length c.source.source_sha256 = 64
      && String.for_all c.source.source_sha256 ~f:(function
        | '0' .. '9' | 'a' .. 'f' -> true
        | _ -> false)
      && valid_namespace c.namespace
      && l.max_payload_bytes > 0
      && l.max_payload_depth > 0
      && l.max_receipts >= 0
      && l.rate_count >= 0
      && l.rate_window_ms > 0
      && P.Timestamp.compare c.created_at c.expires_at < 0
      && List.length t.receipts <= l.max_receipts
      && Option.for_all t.revoked ~f:valid_reason
    with
    | true -> Ok ()
    | false -> error Invalid_request "invalid external ingress registration"
  in
  let%bind _ = schema c in
  let%bind () =
    match
      List.contains_dup t.receipts ~compare:(fun a b ->
        P.Id.Ingress_event.compare a.id b.id)
      || List.contains_dup t.receipts ~compare:(fun a b ->
        P.Idempotency_key.compare a.key b.key)
    with
    | true -> error Invalid_request "duplicate external ingress receipt identity"
    | false -> Ok ()
  in
  List.fold_result t.receipts ~init:() ~f:(fun () receipt ->
    let%bind () =
      validate_id P.Id.Ingress_event.to_json P.Id.Ingress_event.of_json receipt.id
    in
    let%bind () =
      validate_id P.Idempotency_key.to_json P.Idempotency_key.of_json receipt.key
    in
    let%bind payload, hash = canonical_payload c receipt.payload in
    match
      Jsonaf.equal payload receipt.payload
      && String.equal hash receipt.payload_sha256
      && P.Timestamp.compare receipt.accepted_at c.created_at >= 0
      && P.Timestamp.compare receipt.accepted_at c.expires_at < 0
    with
    | true -> Ok ()
    | false -> error Invalid_request "invalid external ingress receipt")
;;

let validate_owner t (subscription : P.Subscription.t) =
  let c = t.context in
  let s = subscription.context in
  match
    P.Id.Session.equal c.session_id s.session_id
    && c.generation = s.generation
    && P.Id.Subscription.equal c.subscription_id s.id
    && c.epoch <= subscription.epoch
    && Option.exists s.source ~f:(P.Invocation.equal_observer c.source)
    && Option.for_all s.ingress_capability ~f:(P.Id.Capability.equal c.id)
    && P.Timestamp.compare c.created_at s.created_at >= 0
    && P.Timestamp.compare c.expires_at s.deadline <= 0
  with
  | true -> Ok ()
  | false -> error Permission_denied "external ingress subscription binding changed"
;;

let live t subscription now =
  let open Result.Let_syntax in
  let%bind () = validate_owner t subscription in
  let%bind () =
    match t.context.epoch = subscription.P.Subscription.epoch with
    | true -> Ok ()
    | false -> error Permission_denied "external ingress subscription epoch changed"
  in
  match t.revoked, subscription.P.Subscription.result with
  | Some _, _ -> error Permission_denied "external ingress registration is revoked"
  | None, Some _ -> error Invalid_state "external ingress subscription is terminal"
  | None, None ->
    (match
       P.Timestamp.compare now t.context.created_at >= 0
       && P.Timestamp.compare now t.context.expires_at < 0
     with
     | true -> Ok ()
     | false ->
       error Invalid_state "external ingress registration is outside its lifetime")
;;

let create context ~subscription =
  let open Result.Let_syntax in
  let t = { context; receipts = []; revoked = None } in
  let%bind () = validate t in
  let%map () = live t subscription context.created_at in
  t
;;

let milliseconds timestamp =
  P.Timestamp.to_time_ns timestamp
  |> Time_ns.to_int63_ns_since_epoch
  |> Int63.to_int64
  |> fun n -> Int64.(n / 1_000_000L)
;;

let prepare
      t
      ~session_id
      ~generation
      ~source
      ~subscription
      ~producer
      ~namespace
      ~key
      ~payload
      ~now
      ~create_event_id
  =
  let open Result.Let_syntax in
  let%bind () =
    match
      P.Id.Principal.equal producer t.context.producer
      && P.Id.Session.equal session_id t.context.session_id
      && generation = t.context.generation
      && P.Invocation.equal_observer source t.context.source
      && String.equal namespace t.context.namespace
    with
    | true -> Ok ()
    | false ->
      error Permission_denied "external ingress producer or namespace is not authorized"
  in
  let%bind () = live t subscription now in
  let%bind payload, payload_sha256 = canonical_payload t.context payload in
  match
    List.find t.receipts ~f:(fun receipt -> P.Idempotency_key.equal receipt.key key)
  with
  | Some receipt ->
    (match String.equal receipt.payload_sha256 payload_sha256 with
     | true -> Ok (Duplicate receipt)
     | false -> error Conflict "external ingress retry key has a different payload")
  | None ->
    let%bind () =
      match List.length t.receipts >= t.context.limits.max_receipts with
      | true -> error Resource_limit "external ingress receipt capacity is exhausted"
      | false -> Ok ()
    in
    let cutoff =
      Chat_response.Automatic_turn_policy.cutoff_ms
        ~now_ms:(milliseconds now)
        ~window_ms:t.context.limits.rate_window_ms
    in
    let count =
      List.count t.receipts ~f:(fun receipt ->
        Int64.(milliseconds receipt.accepted_at >= cutoff))
    in
    let%bind () =
      match count >= t.context.limits.rate_count with
      | true ->
        error ~retryable:true Resource_limit "external ingress rate limit is exhausted"
      | false -> Ok ()
    in
    let id = create_event_id () in
    let%bind () =
      match
        List.exists t.receipts ~f:(fun receipt -> P.Id.Ingress_event.equal id receipt.id)
      with
      | true -> error Conflict "external ingress event identity is already retained"
      | false -> Ok ()
    in
    let receipt = { id; key; payload; payload_sha256; accepted_at = now } in
    Ok (Accepted ({ t with receipts = t.receipts @ [ receipt ] }, receipt))
;;

let revoke t ~reason =
  match valid_reason reason, t.revoked with
  | false, _ -> error Invalid_request "invalid external ingress revocation reason"
  | true, Some _ -> Ok t
  | true, None -> Ok { t with revoked = Some reason }
;;

let validate_transition ~subscription ~previous next =
  let open Result.Let_syntax in
  let%bind () = validate next in
  let%bind () = validate_owner next subscription in
  match previous with
  | None ->
    (match next.receipts, next.revoked with
     | [], None -> live next subscription next.context.created_at
     | _ -> error Conflict "new external ingress registration already has outcomes")
  | Some previous ->
    let%bind () =
      match equal_context previous.context next.context with
      | true -> Ok ()
      | false -> error Conflict "external ingress registration binding is immutable"
    in
    let rec suffix before after =
      match before, after with
      | [], rest -> Ok rest
      | before :: bs, after :: rest when equal_receipt before after -> suffix bs rest
      | _ -> error Conflict "external ingress receipts cannot be removed or changed"
    in
    let%bind added = suffix previous.receipts next.receipts in
    (match added, previous.revoked, next.revoked with
     | [], None, _ -> Ok ()
     | [], Some before, Some after when String.equal before after -> Ok ()
     | [ receipt ], None, None ->
       let%bind admitted =
         prepare
           previous
           ~session_id:next.context.session_id
           ~generation:next.context.generation
           ~source:next.context.source
           ~subscription
           ~producer:next.context.producer
           ~namespace:next.context.namespace
           ~key:receipt.key
           ~payload:receipt.payload
           ~now:receipt.accepted_at
           ~create_event_id:(fun () -> receipt.id)
       in
       (match admitted with
        | Accepted (expected, _) when equal expected next -> Ok ()
        | _ -> error Conflict "external ingress transition differs from its admission")
     | _ -> error Conflict "invalid external ingress admission or revocation transition")
;;
