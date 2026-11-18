open! Core
module P = Agent_protocol
module D = Document_schema
module X = Persistence_codec
module C = D.Extension_carrier
module Data = Goal_policy_contributions_data
module Carrier = Goal_policy_contributions_carrier
module Receipt = Goal_policy_receipt
module Pending = Receipt.Pending
module Request = Receipt.Request
module PC = Policy_canonical_usage

let invalid = Receipt.invalid
let domain = Receipt.domain

module Disposition = struct
  type t =
    | Added
    | Replaced
    | Duplicate
    | Stale
  [@@deriving equal, sexp_of]
end

module Update = struct
  type accounting = Carrier.t

  module Reason = struct
    type t =
      | Initialize_goal of P.Id.Goal.t
      | Reserve of Request.t
      | Import of PC.Projection.t
      | Close_before_writer of Pending.t

    let to_json = function
      | Initialize_goal id ->
        `Object [ "kind", `String "initialize_goal"; "goal_id", P.Id.Goal.to_json id ]
      | Reserve request ->
        `Object [ "kind", `String "reserve"; "request", Request.to_json request ]
      | Import projection ->
        `Object
          [ "kind", `String "import"; "projection", PC.Projection.to_json projection ]
      | Close_before_writer pending ->
        `Object
          [ "kind", `String "close_before_writer"; "pending", Pending.to_json pending ]
    ;;

    let of_json raw =
      let open Result.Let_syntax in
      let%bind f = X.object_ raw in
      let%bind kind = X.required f "kind" P.Json_codec.string in
      match kind with
      | "initialize_goal" ->
        let%map id = X.required f "goal_id" P.Id.Goal.of_json in
        Initialize_goal id
      | "reserve" ->
        let%map request = X.required f "request" Request.of_json in
        Reserve request
      | "import" ->
        let%map projection = X.required f "projection" PC.Projection.of_json in
        Import projection
      | "close_before_writer" ->
        let%map pending = X.required f "pending" Pending.of_json in
        Close_before_writer pending
      | _ -> invalid "unknown causal Goal Update reason"
    ;;

    let shape =
      X.tagged_shape_exn
        ~discriminator:"kind"
        [ "initialize_goal", X.fields_shape [ "kind"; "goal_id" ]
        ; "reserve", X.shape_exn [ "kind", D.Shape.value; "request", Request.shape ]
        ; ( "import"
          , X.shape_exn [ "kind", D.Shape.value; "projection", PC.Projection.shape ] )
        ; ( "close_before_writer"
          , X.shape_exn [ "kind", D.Shape.value; "pending", Pending.shape ] )
        ]
    ;;
  end

  type t =
    { candidate : accounting
    ; reason : Reason.t
    }

  let candidate t = t.candidate
  let reason t = t.reason

  let limits =
    D.Limits.create
      ~max_bytes:8388608
      ~max_depth:112
      ~max_fields:600000
      ~max_nodes:1200000
    |> domain
    |> Result.map_error ~f:(fun error -> error.P.Error.message)
    |> Result.ok_or_failwith
  ;;

  let to_json t =
    let open Result.Let_syntax in
    let%bind candidate = Carrier.to_document t.candidate in
    let raw =
      `Object
        [ "accounting", D.Document.json candidate; "reason", Reason.to_json t.reason ]
    in
    let%map () = D.Json.validate raw ~limits |> domain in
    raw
  ;;

  let of_json raw =
    let open Result.Let_syntax in
    let%bind () = D.Json.validate raw ~limits |> domain in
    let%bind f = X.object_ raw in
    let%bind candidate =
      X.required f "accounting" (fun raw ->
        let%bind doc = D.Document.inspect raw ~limits:Carrier.limits |> domain in
        Carrier.of_document doc)
    in
    let%map reason = X.required f "reason" Reason.of_json in
    { candidate; reason }
  ;;

  let shape = X.shape_exn [ "accounting", D.Shape.value; "reason", Reason.shape ]
end

let make_update candidate reason =
  let%map.Result candidate = Carrier.admit candidate in
  { Update.candidate; reason }
;;

let original original ~owner =
  match original with
  | Some original when P.Session_ref.equal owner (Carrier.value original).owner ->
    Ok original
  | Some _ -> invalid "causal Goal book has another full owner"
  | None -> Carrier.admit (C.of_authored_value { Data.owner; books = [] })
;;

let initialize_goal original_ ~owner ~goal ~account ~before_ledger =
  let open Result.Let_syntax in
  let%bind original = original original_ ~owner in
  let id = P.Goal.id goal in
  let%bind () =
    if
      P.Session_ref.equal owner (P.Goal.session goal)
      && Option.is_none (Data.find_book (Carrier.value original) id)
    then Ok ()
    else invalid "causal known Goal initialization is duplicate or foreign"
  in
  (* Ordered integration separately requires a genuine missing-to-present Goal
     creation. This finite check proves the supplied account is precisely its
     real before-ledger bootstrap, never a zero fabricated from current totals. *)
  let%bind expected = Goal_accounting_document.create goal ~ledger:before_ledger in
  let%bind expected =
    Goal_accounting_document.resolve expected ~goal ~ledger:before_ledger
  in
  let%bind actual =
    Goal_accounting_document.resolve account ~goal ~ledger:before_ledger
  in
  let%bind () =
    if Goal_accounting.equal expected actual
    then Ok ()
    else invalid "new causal Goal account differs from actual bootstrap"
  in
  let data =
    Data.set_book
      (Carrier.value original)
      { Data.Book.goal_id = id; history = Known_from_creation; slots = [] }
  in
  let%bind candidate = Carrier.with_value original data in
  make_update candidate (Initialize_goal id)
;;

module Reserve_result = struct
  type t =
    | Reserved of Update.t * Pending.t
    | Already_pending of Pending.t
    | Busy of Pending.t
end

let reserve original_ ~owner ~request =
  let open Result.Let_syntax in
  let stream = Request.stream request in
  let target = PC.Stream.target stream in
  let%bind () =
    if
      P.Session_ref.equal owner (Policy_cause_data.Goal_target.owner target)
      && not (P.Session_ref.equal owner (PC.Stream.callee stream))
    then Ok ()
    else invalid "causal Reserve requires a remote callee and exact Goal owner"
  in
  let%bind original = original original_ ~owner in
  let data = Carrier.value original in
  let id = PC.Stream.id stream in
  let goal_id = Policy_cause_data.Goal_target.goal_id target in
  let existing = Data.find_slot data id in
  let%bind slot =
    match existing with
    | Some (_, slot) when PC.Stream.equal slot.stream stream -> Ok slot
    | Some _ -> invalid "causal stream digest collides with full descriptor"
    | None ->
      Ok
        { Data.Slot.stream
        ; confirmed_prefix = 0L
        ; last_nonce = 0L
        ; pending = None
        ; last = None
        ; projection = None
        }
  in
  match slot.pending with
  | Some pending ->
    if Request.matches request pending
    then Ok (Reserve_result.Already_pending pending)
    else Ok (Busy pending)
  | None ->
    let%bind nonce = Receipt.add slot.last_nonce 1L in
    let%bind next_prefix = Receipt.add slot.confirmed_prefix 1L in
    let%bind pending = Request.pending request ~nonce ~next_prefix in
    let data =
      if Option.is_some (Data.find_book data goal_id)
      then data
      else
        Data.set_book data { Data.Book.goal_id; history = Historical_unknown; slots = [] }
    in
    let%bind data = Data.set_slot data ~goal_id { slot with pending = Some pending } in
    let%bind candidate = Carrier.with_value original data in
    let%bind candidate = Carrier.retain_slot candidate ~stream ~projection:None in
    let%map update = make_update candidate (Reserve request) in
    Reserve_result.Reserved (update, pending)
;;

let same_head left right =
  match PC.Projection.head left, PC.Projection.head right with
  | Some left, Some right ->
    let lw = PC.Head.witness left
    and rw = PC.Head.witness right in
    D.Json.equal
      (Goal_descendants.Canonical.Fact.to_json (PC.Head.canonical left))
      (Goal_descendants.Canonical.Fact.to_json (PC.Head.canonical right))
    && Policy_cause_data.Key.equal (PC.Head.cause left) (PC.Head.cause right)
    && Policy_cause_data.Allocation_work.equal (PC.Head.work left) (PC.Head.work right)
    && Policy_cause_data.Key.equal (PC.Head.Witness.cause lw) (PC.Head.Witness.cause rw)
    && Option.equal
         P.Inference_origin.equal
         (PC.Head.Witness.caller_origin lw)
         (PC.Head.Witness.caller_origin rw)
    && P.Id.Invocation.equal
         (PC.Head.Witness.child_invocation lw)
         (PC.Head.Witness.child_invocation rw)
    && String.equal
         (PC.Head.Witness.admission_sha256 lw)
         (PC.Head.Witness.admission_sha256 rw)
    && Option.equal
         String.equal
         (Option.map
            (PC.Head.Witness.forwarding lw)
            ~f:Policy_cause_data.Forwarding.Witness.known_sha256)
         (Option.map
            (PC.Head.Witness.forwarding rw)
            ~f:Policy_cause_data.Forwarding.Witness.known_sha256)
  | None, None -> true
  | Some _, None | None, Some _ -> false
;;

let import original ~projection =
  let open Result.Let_syntax in
  let data = Carrier.value original in
  let stream = PC.Projection.stream projection in
  let%bind () =
    if not (P.Session_ref.equal data.owner (PC.Stream.callee stream))
    then Ok ()
    else invalid "self causal usage must derive from actual canonical ACK"
  in
  let%bind book, slot =
    Data.find_slot data (PC.Stream.id stream)
    |> Result.of_option
         ~error:(P.Error.invalid_request "causal import has no retained reservation slot")
  in
  let%bind () =
    if PC.Stream.equal stream slot.stream
    then Ok ()
    else invalid "causal import changes full stream"
  in
  let replace slot (disposition : Disposition.t) =
    let%bind data = Data.set_slot data ~goal_id:book.goal_id slot in
    let%bind candidate = Carrier.with_value original data in
    let%bind candidate =
      Carrier.retain_slot candidate ~stream ~projection:(Some projection)
    in
    let%map update = make_update candidate (Import projection) in
    update, disposition
  in
  let preserve (disposition : Disposition.t) =
    let%map update = make_update original (Import projection) in
    update, disposition
  in
  let confirm () =
    match slot.pending with
    | None -> invalid "causal prefix advance has no exact pending"
    | Some pending ->
      let%bind () = Receipt.confirms pending ~projection in
      replace
        { slot with
          confirmed_prefix = PC.Projection.admitted_attempts projection
        ; last_nonce = Pending.nonce pending
        ; pending = None
        ; last = Some (Confirmed pending)
        ; projection = Some projection
        }
        Disposition.Added
  in
  match slot.projection with
  | None -> confirm ()
  | Some previous ->
    let revision = PC.Projection.session_revision projection
    and old_revision = PC.Projection.session_revision previous in
    let prefix = PC.Projection.admitted_attempts projection
    and old_prefix = slot.confirmed_prefix in
    if Int64.(revision < old_revision && prefix <= old_prefix)
    then preserve Stale
    else if Int64.equal revision old_revision
    then
      if
        D.Json.equal
          (PC.Projection.known_json previous)
          (PC.Projection.known_json projection)
      then (
        (* Native values have projected unknown children away. Establish exact
           raw compatibility through the existing carrier, not a lossy/raw
           comparison against that native projection. Pending is untouched. *)
        let%bind candidate =
          Carrier.retain_slot original ~stream ~projection:(Some projection)
        in
        let%map update = make_update candidate (Import projection) in
        update, Disposition.Duplicate)
      else invalid "same causal source revision conflicts"
    else if Int64.(revision < old_revision || prefix < old_prefix)
    then invalid "causal source revision and prefix regress inconsistently"
    else if Int64.equal prefix old_prefix
    then
      if same_head previous projection
      then replace { slot with projection = Some projection } Replaced
      else invalid "causal correction changes selected canonical head"
    else confirm ()
;;

let close_before_writer original ~pending =
  let open Result.Let_syntax in
  let data = Carrier.value original in
  let%bind book, slot =
    Data.find_slot data (Pending.stream_id pending)
    |> Result.of_option
         ~error:(P.Error.invalid_request "causal close has no exact retained stream")
  in
  let%bind () = Receipt.validate_pending pending ~stream:slot.stream in
  match slot.pending, slot.last with
  | None, Some (No_writer_closed previous) when Pending.equal previous pending ->
    make_update original (Close_before_writer pending)
  | Some current, _ when Pending.equal current pending ->
    let%bind data =
      Data.set_slot
        data
        ~goal_id:book.goal_id
        { slot with
          last_nonce = Pending.nonce pending
        ; pending = None
        ; last = Some (No_writer_closed pending)
        }
    in
    let%bind candidate = Carrier.with_value original data in
    make_update candidate (Close_before_writer pending)
  | (None | Some _), (None | Some (Confirmed _ | No_writer_closed _)) ->
    invalid "causal close is stale or contradicts current pending"
;;
