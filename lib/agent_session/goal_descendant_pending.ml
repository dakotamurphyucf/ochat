open! Core
module P = Agent_protocol
module D = Agent_store.Delegation_store
module O = Agent_store.Delegation_goal_origin
module X = Persistence_codec

type t =
  { scope : O.Scope.t
  ; branch_reference : D.Reference.t
  ; nonce : int64
  ; next_prefix : int64
  ; occurrence : Goal_descendant_occurrence.t
  }
[@@deriving equal, sexp_of]

let scope t = t.scope
let branch_reference t = t.branch_reference
let nonce t = t.nonce
let next_prefix t = t.next_prefix
let occurrence t = t.occurrence
let invalid message = Error (P.Error.invalid_request message)

let validate_branch ~scope ~branch_reference =
  let open Result.Let_syntax in
  let%bind () =
    D.validate_reference branch_reference
    |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
  in
  let branch = O.Scope.branch scope in
  let key = branch_reference.D.Reference.key in
  if
    not
      (P.Id.Session.equal (O.Anchor.parent_session_id branch) key.parent_session_id
       && Int.equal (O.Anchor.parent_generation branch) key.parent_generation
       && P.Id.Principal.equal (O.Anchor.principal_id branch) key.principal_id
       && P.Idempotency_key.equal (O.Anchor.key branch) key.idempotency_key
       && P.Id.Session.equal
            (O.Anchor.child_session_id branch)
            branch_reference.child_session_id)
  then invalid "descendant pending scope and branch differ"
  else Ok ()
;;

let create ~scope ~branch_reference ~nonce ~next_prefix ~occurrence =
  let%bind.Result () = validate_branch ~scope ~branch_reference in
  if Int64.(nonce <= 0L || next_prefix <= 0L || nonce < next_prefix)
  then invalid "descendant pending requires positive prefix <= nonce"
  else Ok { scope; branch_reference; nonce; next_prefix; occurrence }
;;

let to_json t =
  `Object
    [ "scope", O.Scope.to_json t.scope
    ; "branch_reference", D.reference_to_jsonaf t.branch_reference
    ; "nonce", X.int64_json t.nonce
    ; "next_prefix", X.int64_json t.next_prefix
    ; "occurrence", Goal_descendant_occurrence.to_json t.occurrence
    ]
;;

let of_json raw =
  let open Result.Let_syntax in
  let%bind fields = X.object_ raw in
  let%bind scope = X.required fields "scope" O.Scope.of_json in
  let%bind branch_reference =
    X.required fields "branch_reference" (fun raw ->
      D.reference_of_jsonaf raw
      |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error)
  in
  let%bind nonce = X.required fields "nonce" X.nonnegative_int64 in
  let%bind next_prefix = X.required fields "next_prefix" X.nonnegative_int64 in
  let%bind occurrence =
    X.required fields "occurrence" Goal_descendant_occurrence.of_json
  in
  create ~scope ~branch_reference ~nonce ~next_prefix ~occurrence
;;

let shape =
  X.shape_exn
    [ "scope", O.Scope.shape
    ; "branch_reference", D.reference_shape
    ; "nonce", Document_schema.Shape.value
    ; "next_prefix", Document_schema.Shape.value
    ; "occurrence", Goal_descendant_occurrence.shape
    ]
;;

let capacity_projection ~scope ~branch_reference =
  `Object
    [ "scope", O.Scope.to_json scope
    ; "branch_reference", D.reference_to_jsonaf branch_reference
    ; "nonce", X.int64_json Int64.max_value
    ; "next_prefix", X.int64_json Int64.max_value
    ; "occurrence", Goal_descendant_occurrence.capacity_projection ()
    ]
;;
