open! Core
module P = Agent_protocol
module D = Document_schema
module X = Persistence_codec
module R = Agent_store.Delegation_store
module Scope = Agent_store.Delegation_goal_origin.Scope
module Pending = Goal_descendant_pending
module Included = Goal_descendant_included
module Occurrence = Goal_descendant_occurrence

module Last = struct
  type t =
    | Included of Included.t
    | No_writer_closed of Pending.t
  [@@deriving equal, sexp_of]
end

type t =
  { scope : Scope.t
  ; reference : R.Reference.t
  ; confirmed_prefix : int64
  ; last_nonce : int64
  ; pending : Pending.t option
  ; last : Last.t option
  }
[@@deriving equal, sexp_of]

let invalid message = Error (P.Error.invalid_request message)
let scope t = t.scope
let reference t = t.reference
let confirmed_prefix t = t.confirmed_prefix
let last_nonce t = t.last_nonce
let pending t = t.pending
let last t = t.last

let limits =
  D.Limits.create ~max_bytes:262144 ~max_depth:64 ~max_fields:16384 ~max_nodes:32768
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let preflight raw =
  D.Json.validate raw ~limits
  |> Result.map_error ~f:(fun error ->
    P.Error.invalid_request (Sexp.to_string_hum (D.Error.sexp_of_t error)))
;;

let last_to_json = function
  | Last.Included included ->
    `Object
      [ "kind", `String "included"
      ; "pending", Pending.to_json (Included.pending included)
      ]
  | No_writer_closed pending ->
    `Object [ "kind", `String "no_writer_closed"; "pending", Pending.to_json pending ]
;;

let to_json t =
  `Object
    [ "scope", Scope.to_json t.scope
    ; "reference", R.reference_to_jsonaf t.reference
    ; "confirmed_prefix", X.int64_json t.confirmed_prefix
    ; "last_nonce", X.int64_json t.last_nonce
    ; "pending", X.option_json Pending.to_json t.pending
    ; "last", X.option_json last_to_json t.last
    ]
;;

let admit t =
  let%map.Result () = preflight (to_json t) in
  t
;;

let create ~scope ~reference =
  let%bind.Result () = Pending.validate_branch ~scope ~branch_reference:reference in
  admit
    { scope
    ; reference
    ; confirmed_prefix = 0L
    ; last_nonce = 0L
    ; pending = None
    ; last = None
    }
;;

let same_branch t pending =
  Scope.equal t.scope (Pending.scope pending)
  && R.Reference.equal t.reference (Pending.branch_reference pending)
;;

let successor value =
  if Int64.equal value Int64.max_value
  then invalid "descendant branch counter exhausted"
  else Ok (Int64.succ value)
;;

module Reserve_result = struct
  type branch = t

  type t =
    | Reserved of branch * Pending.t
    | Already_pending of Pending.t
    | Busy of Pending.t
end

let reserve t ~occurrence =
  let open Result.Let_syntax in
  match t.pending with
  | Some pending ->
    if Occurrence.equal occurrence (Pending.occurrence pending)
    then Ok (Reserve_result.Already_pending pending)
    else Ok (Reserve_result.Busy pending)
  | None ->
    let%bind nonce = successor t.last_nonce in
    let%bind next_prefix = successor t.confirmed_prefix in
    let%bind pending =
      Pending.create
        ~scope:t.scope
        ~branch_reference:t.reference
        ~nonce
        ~next_prefix
        ~occurrence
    in
    let%map candidate = admit { t with last_nonce = nonce; pending = Some pending } in
    Reserve_result.Reserved (candidate, pending)
;;

let confirm t ~included =
  let claim = Included.pending included in
  match t.pending with
  | Some pending when Pending.equal pending claim ->
    admit
      { t with
        confirmed_prefix = Pending.next_prefix pending
      ; pending = None
      ; last = Some (Last.Included included)
      }
  | Some _ | None ->
    (match t.last with
     | Some (Last.Included previous) when Included.equal previous included -> Ok t
     | Some (Last.Included _ | No_writer_closed _) | None ->
       invalid "descendant confirmation does not match the current pending")
;;

let close_before_writer t ~pending =
  match t.pending with
  | Some current when Pending.equal current pending ->
    admit { t with pending = None; last = Some (Last.No_writer_closed pending) }
  | Some _ | None ->
    (match t.last with
     | Some (Last.No_writer_closed previous) when Pending.equal previous pending -> Ok t
     | Some (Last.Included _ | No_writer_closed _) | None ->
       invalid "descendant no-writer closure does not match the current pending")
;;

module Action = struct
  type t =
    | Reserve of Occurrence.t
    | Confirm of Included.t
    | Close_before_writer of Pending.t
end

let validate_update t ~incoming ~action =
  let open Result.Let_syntax in
  let%bind expected =
    match action with
    | Action.Reserve occurrence ->
      let%map reserved = reserve t ~occurrence in
      (match reserved with
       | Reserve_result.Reserved (candidate, _) -> candidate
       | Already_pending _ | Busy _ -> t)
    | Confirm included -> confirm t ~included
    | Close_before_writer pending -> close_before_writer t ~pending
  in
  if equal expected incoming
  then Ok ()
  else invalid "descendant branch update differs from its exact domain action"
;;

let last_pending = function
  | Last.Included included -> Included.pending included
  | No_writer_closed pending -> pending
;;

let validate t =
  let open Result.Let_syntax in
  let%bind () = Pending.validate_branch ~scope:t.scope ~branch_reference:t.reference in
  let%bind () =
    if Int64.(t.confirmed_prefix < 0L || t.last_nonce < t.confirmed_prefix)
    then invalid "invalid descendant branch counters"
    else Ok ()
  in
  let%bind () =
    match t.last with
    | None ->
      if Int64.equal t.confirmed_prefix 0L
      then Ok ()
      else invalid "descendant branch prefix has no terminal evidence"
    | Some terminal ->
      let pending = last_pending terminal in
      let expected_prefix =
        match terminal with
        | Last.Included _ -> Pending.next_prefix pending
        | No_writer_closed _ -> Int64.pred (Pending.next_prefix pending)
      in
      if same_branch t pending && Int64.equal t.confirmed_prefix expected_prefix
      then Ok ()
      else invalid "descendant branch terminal frontier differs"
  in
  let previous_nonce =
    Option.value_map t.last ~default:0L ~f:(fun terminal ->
      Pending.nonce (last_pending terminal))
  in
  match t.pending with
  | None ->
    if Int64.equal t.last_nonce previous_nonce
    then Ok ()
    else invalid "descendant branch nonce has no frontier evidence"
  | Some pending ->
    let%bind nonce = successor previous_nonce in
    let%bind next_prefix = successor t.confirmed_prefix in
    if
      same_branch t pending
      && Int64.equal t.last_nonce nonce
      && Int64.equal (Pending.nonce pending) nonce
      && Int64.equal (Pending.next_prefix pending) next_prefix
    then Ok ()
    else invalid "descendant branch pending frontier differs"
;;

let last_of_json raw =
  let open Result.Let_syntax in
  let%bind fields = X.object_ raw in
  let%bind kind = X.required fields "kind" P.Json_codec.string in
  match kind with
  | "included" ->
    let%map pending = X.required fields "pending" Pending.of_json in
    Last.Included (Included.create pending)
  | "no_writer_closed" ->
    let%map pending = X.required fields "pending" Pending.of_json in
    Last.No_writer_closed pending
  | _ -> invalid "unknown descendant branch terminal kind"
;;

let of_json raw =
  let open Result.Let_syntax in
  let%bind () = preflight raw in
  let%bind fields = X.object_ raw in
  let%bind scope = X.required fields "scope" Scope.of_json in
  let%bind reference =
    X.required fields "reference" (fun raw ->
      R.reference_of_jsonaf raw
      |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error)
  in
  let%bind confirmed_prefix = X.required fields "confirmed_prefix" X.nonnegative_int64 in
  let%bind last_nonce = X.required fields "last_nonce" X.nonnegative_int64 in
  let%bind pending = X.required fields "pending" (X.nullable Pending.of_json) in
  let%bind last = X.required fields "last" (X.nullable last_of_json) in
  let t = { scope; reference; confirmed_prefix; last_nonce; pending; last } in
  let%map () = validate t in
  t
;;

let last_shape = X.shape_exn [ "kind", D.Shape.value; "pending", Pending.shape ]

let shape =
  X.shape_exn
    [ "scope", Scope.shape
    ; "reference", R.reference_shape
    ; "confirmed_prefix", D.Shape.value
    ; "last_nonce", D.Shape.value
    ; "pending", D.Shape.nullable Pending.shape
    ; "last", D.Shape.nullable last_shape
    ]
;;
