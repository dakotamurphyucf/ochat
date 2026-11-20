open! Core
module P = Agent_protocol
module Scope = P.Recovery_window.Scope
module J = P.Json_codec
module D = Document_schema

let nonnegative = J.bounded_int64 ~min:0L ~max:Int64.max_value
let revision_json revision = `Number (Int64.to_string revision)
let invalid message = Error (P.Error.invalid_request message)
let conflict message = Error (P.Error.create Conflict ~message ~retryable:true ())

module Expected = struct
  type t =
    { scope : Scope.t
    ; revision : int64
    }
  [@@deriving equal, sexp_of]

  let create scope ~revision =
    if Int64.(revision < 0L)
    then invalid "invalid recovery rollover revision"
    else Ok { scope; revision }
  ;;

  let to_json t =
    `Object [ "scope", Scope.to_json t.scope; "revision", revision_json t.revision ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = J.fields json in
    let%bind scope = J.required_as fields "scope" Scope.of_json in
    let%bind revision = J.required_as fields "revision" nonnegative in
    create scope ~revision
  ;;
end

module Requester = struct
  type t =
    | Operator of
        { principal : P.Id.Principal.t
        ; key : P.Idempotency_key.t
        ; force : bool
        }
    | Native of
        { principal : P.Id.Principal.t
        ; operation : P.Id.Operation.t
        ; invocation : P.Id.Invocation.t
        }
  [@@deriving equal, sexp_of]

  module Key = struct
    module T = struct
      type t =
        | Operator of P.Id.Principal.t * P.Idempotency_key.t
        | Native of P.Id.Principal.t * P.Id.Operation.t * P.Id.Invocation.t
      [@@deriving compare, sexp_of]
    end

    include T
    include Comparator.Make (T)
  end

  let key = function
    | Operator { principal; key; _ } -> Key.Operator (principal, key)
    | Native { principal; operation; invocation } ->
      Key.Native (principal, operation, invocation)
  ;;

  let same_key a b = Int.equal (Key.compare (key a) (key b)) 0

  let to_json t =
    let principal id = "principal", P.Id.Principal.to_json id in
    `Object
      (match t with
       | Operator { principal = id; key; force } ->
         [ "kind", `String "operator"
         ; principal id
         ; "key", P.Idempotency_key.to_json key
         ; ("force", if force then `True else `False)
         ]
       | Native { principal = id; operation; invocation } ->
         [ "kind", `String "native"
         ; principal id
         ; "operation", P.Id.Operation.to_json operation
         ; "invocation", P.Id.Invocation.to_json invocation
         ])
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = J.fields json in
    let%bind kind = J.required_as fields "kind" J.string in
    let%bind principal = J.required_as fields "principal" P.Id.Principal.of_json in
    match kind with
    | "operator" ->
      let%bind key = J.required_as fields "key" P.Idempotency_key.of_json in
      let%map force = J.required_as fields "force" J.bool in
      Operator { principal; key; force }
    | "native" ->
      let%bind operation = J.required_as fields "operation" P.Id.Operation.of_json in
      let%map invocation = J.required_as fields "invocation" P.Id.Invocation.of_json in
      Native { principal; operation; invocation }
    | _ -> invalid "unknown recovery rollover requester"
  ;;
end

module Intent = struct
  type t =
    { id : P.Id.Operation.t
    ; expected : Expected.t
    ; requester : Requester.t
    ; admitted_at : P.Timestamp.t
    }
  [@@deriving equal, sexp_of]

  let create ~id ~expected ~requester ~admitted_at =
    { id; expected; requester; admitted_at }
  ;;

  let public_fields t =
    [ "id", P.Id.Operation.to_json t.id
    ; "expected", Expected.to_json t.expected
    ; "admitted_at", P.Timestamp.to_json t.admitted_at
    ]
  ;;

  let to_json t = `Object (("requester", Requester.to_json t.requester) :: public_fields t)

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = J.fields json in
    let%bind id = J.required_as fields "id" P.Id.Operation.of_json in
    let%bind expected = J.required_as fields "expected" Expected.of_json in
    let%bind requester = J.required_as fields "requester" Requester.of_json in
    let%map admitted_at = J.required_as fields "admitted_at" P.Timestamp.of_json in
    create ~id ~expected ~requester ~admitted_at
  ;;
end

module Receipt = struct
  type supersession =
    | Basis_changed
    | Cancelled
    | Checkpoint_required
  [@@deriving equal, sexp_of]

  type committed =
    { successor : Scope.t
    ; cut_revision : int64
    ; archive : Chatml_source.Digest.t
    }
  [@@deriving equal, sexp_of]

  type terminal =
    | Committed of committed
    | Superseded of supersession
  [@@deriving equal, sexp_of]

  type status =
    | Pending
    | Terminal of terminal
  [@@deriving equal, sexp_of]

  type t =
    { intent : Intent.t
    ; status : status
    }
  [@@deriving equal, sexp_of]

  let supersession_name = function
    | Basis_changed -> "basis_changed"
    | Cancelled -> "cancelled"
    | Checkpoint_required -> "checkpoint_required"
  ;;

  let status_to_json t =
    `Object
      (match t with
       | Pending -> [ "kind", `String "pending" ]
       | Terminal (Superseded reason) ->
         [ "kind", `String "superseded"; "reason", `String (supersession_name reason) ]
       | Terminal (Committed { successor; cut_revision; archive }) ->
         [ "kind", `String "committed"
         ; "successor", Scope.to_json successor
         ; "cut_revision", revision_json cut_revision
         ; "archive", `String (Chatml_source.Digest.to_hex archive)
         ])
  ;;

  let status_of_json json =
    let open Result.Let_syntax in
    let%bind fields = J.fields json in
    let%bind kind = J.required_as fields "kind" J.string in
    match kind with
    | "pending" -> Ok Pending
    | "superseded" ->
      let%bind reason = J.required_as fields "reason" J.string in
      let%map reason =
        match reason with
        | "basis_changed" -> Ok Basis_changed
        | "cancelled" -> Ok Cancelled
        | "checkpoint_required" -> Ok Checkpoint_required
        | _ -> invalid "unknown recovery rollover supersession"
      in
      Terminal (Superseded reason)
    | "committed" ->
      let%bind successor = J.required_as fields "successor" Scope.of_json in
      let%bind cut_revision = J.required_as fields "cut_revision" nonnegative in
      let%bind archive = J.required_as fields "archive" J.string in
      let%map archive =
        Chatml_source.Digest.of_hex archive
        |> Result.map_error ~f:(fun _ ->
          P.Error.invalid_request "invalid rollover archive digest")
      in
      Terminal (Committed { successor; cut_revision; archive })
    | _ -> invalid "unknown recovery rollover status"
  ;;

  let validate t =
    let expected = t.intent.expected in
    match t.status with
    | Pending | Terminal (Superseded _) -> Ok ()
    | Terminal (Committed { successor; cut_revision; _ }) ->
      if
        P.Session_ref.equal (Scope.session expected.scope) (Scope.session successor)
        && Int.equal (Scope.generation expected.scope) (Scope.generation successor)
        && (not (Scope.equal expected.scope successor))
        && Int64.(cut_revision >= expected.revision)
      then Ok ()
      else invalid "rollover terminal receipt differs from its admitted intent"
  ;;

  let to_public_json t =
    `Object (("status", status_to_json t.status) :: Intent.public_fields t.intent)
  ;;

  let to_json t =
    `Object [ "intent", Intent.to_json t.intent; "status", status_to_json t.status ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = J.fields json in
    let%bind intent = J.required_as fields "intent" Intent.of_json in
    let%bind status = J.required_as fields "status" status_of_json in
    let receipt = { intent; status } in
    let%map () = validate receipt in
    receipt
  ;;
end

type t = Receipt.t list

let find t ~id =
  List.find t ~f:(fun receipt -> P.Id.Operation.equal receipt.Receipt.intent.id id)
;;

let pending t =
  List.find_map t ~f:(fun receipt ->
    match receipt.Receipt.status with
    | Pending -> Some receipt.intent
    | Terminal _ -> None)
;;

let previous t ~requester ~expected =
  match
    Option.bind
      t
      ~f:
        (List.find ~f:(fun receipt ->
           Requester.same_key requester receipt.Receipt.intent.requester))
  with
  | None -> Ok None
  | Some receipt
    when Requester.equal requester receipt.intent.requester
         && Expected.equal expected receipt.intent.expected -> Ok (Some receipt)
  | Some _ ->
    Error
      (P.Error.create
         Idempotency_conflict
         ~message:"rollover retry arguments changed"
         ~retryable:false
         ())
;;

module Change = struct
  type t =
    | Admit of Intent.t
    | Settle of
        { id : P.Id.Operation.t
        ; terminal : Receipt.terminal
        }

  let to_json = function
    | Admit intent -> `Object [ "kind", `String "admit"; "intent", Intent.to_json intent ]
    | Settle { id; terminal } ->
      `Object
        [ "kind", `String "settle"
        ; "id", P.Id.Operation.to_json id
        ; "status", Receipt.status_to_json (Terminal terminal)
        ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = J.fields json in
    let%bind kind = J.required_as fields "kind" J.string in
    match kind with
    | "admit" ->
      let%map intent = J.required_as fields "intent" Intent.of_json in
      Admit intent
    | "settle" ->
      let%bind id = J.required_as fields "id" P.Id.Operation.of_json in
      let%bind status = J.required_as fields "status" Receipt.status_of_json in
      (match status with
       | Pending -> invalid "settlement requires a terminal disposition"
       | Terminal terminal -> Ok (Settle { id; terminal }))
    | _ -> invalid "unknown recovery rollover change"
  ;;
end

let to_json t =
  `Object [ "version", `Number "1"; "receipts", `Array (List.map t ~f:Receipt.to_json) ]
;;

let bounded t =
  D.Json.validate ~limits:D.Limits.default (to_json t)
  |> Result.map_error ~f:(fun _ ->
    P.Error.create
      Resource_limit
      ~message:"recovery rollover ledger capacity exceeded"
      ~retryable:false
      ())
;;

let apply t ~active ~revision change =
  let open Result.Let_syntax in
  let receipts = Option.value t ~default:[] in
  let%bind next =
    match change with
    | Change.Admit intent ->
      let%bind duplicate =
        previous t ~requester:intent.requester ~expected:intent.expected
      in
      if Option.is_some duplicate || Option.is_some (find receipts ~id:intent.id)
      then conflict "rollover admission identity already retained"
      else if
        not
          (Scope.equal active intent.expected.scope
           && Int64.equal revision intent.expected.revision)
      then conflict "expected recovery window or revision changed"
      else if Option.is_some (pending receipts)
      then conflict "another rollover intent is pending"
      else Ok (Receipt.{ intent; status = Pending } :: receipts)
    | Settle { id; terminal } ->
      (match find receipts ~id with
       | Some ({ status = Pending; _ } as receipt) ->
         let settled = { receipt with status = Terminal terminal } in
         let%map () = Receipt.validate settled in
         List.map receipts ~f:(fun previous ->
           if P.Id.Operation.equal previous.intent.id id then settled else previous)
       | Some { status = Terminal current; _ }
         when Receipt.equal_terminal current terminal -> Ok receipts
       | Some _ | None -> conflict "rollover settlement has no matching pending intent")
  in
  let%map () = bounded next in
  next
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind () =
    D.Json.validate ~limits:D.Limits.default json
    |> Result.map_error ~f:(fun _ ->
      P.Error.invalid_request "invalid recovery rollover ledger bounds")
  in
  let%bind fields = J.fields json in
  let%bind version = J.required_as fields "version" (J.bounded_int ~min:1 ~max:1) in
  if not (Int.equal version 1)
  then invalid "unsupported recovery rollover ledger"
  else (
    let%bind receipts = J.required_as fields "receipts" (J.list Receipt.of_json) in
    let ids = Hash_set.create (module P.Id.Operation) in
    let%map _ =
      List.fold_result
        receipts
        ~init:(false, Set.empty (module Requester.Key))
        ~f:(fun (active, requesters) (receipt : Receipt.t) ->
          let pending = Receipt.equal_status receipt.status Pending in
          let key = Requester.key receipt.intent.requester in
          if
            Hash_set.mem ids receipt.intent.id
            || (active && pending)
            || Set.mem requesters key
          then invalid "duplicate recovery rollover identity or pending intent"
          else (
            Hash_set.add ids receipt.intent.id;
            Ok (active || pending, Set.add requesters key)))
    in
    receipts)
;;

let validate_state t ~windows ~revision =
  let open Result.Let_syntax in
  let%bind ledger =
    Result.of_option
      windows
      ~error:(P.Error.invalid_request "rollover ledger requires recovery windows")
  in
  let owner = Recovery_window_ledger.owner ledger in
  let windows = Recovery_window_ledger.windows ledger in
  List.fold_result t ~init:() ~f:(fun () (receipt : Receipt.t) ->
    let expected = receipt.intent.expected in
    if
      (not (P.Session_ref.equal owner (Scope.session expected.scope)))
      || Int64.(expected.revision >= revision)
      || not
           (List.exists windows ~f:(fun window ->
              Scope.equal (Recovery_window_record.scope window) expected.scope))
    then invalid "rollover receipt has no owning recovery window"
    else (
      match receipt.status with
      | Pending | Terminal (Superseded _) -> Ok ()
      | Terminal (Committed { successor; cut_revision; _ }) ->
        if
          Int64.(cut_revision < revision)
          && List.exists windows ~f:(fun window ->
            Scope.equal (Recovery_window_record.scope window) successor
            &&
            match Recovery_window_record.origin window with
            | Successor { previous; operation } ->
              P.Id.Context_window.equal previous (Scope.window expected.scope)
              && P.Id.Operation.equal operation receipt.intent.id
            | Root | New_generation _ -> false)
        then Ok ()
        else invalid "rollover terminal receipt has no matching successor"))
;;

let check_unchanged t ~incoming =
  if Option.equal (List.equal Receipt.equal) t incoming
  then Ok ()
  else invalid "replacement changed durable rollover receipts"
;;
