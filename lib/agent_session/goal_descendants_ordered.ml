open! Core
module P = Agent_protocol
module D = Document_schema
module L = Inference_ledger
module A = Goal_descendants
module C = Child_contributions
module O = Agent_store.Delegation_goal_origin
module F = Canonical_usage_fold
module Occurrence = Goal_descendant_occurrence

let invalid message = Error (P.Error.invalid_request message)

let ledger result =
  Result.map_error result ~f:(fun error ->
    P.Error.invalid_request (Sexp.to_string_hum (L.Error.sexp_of_t error)))
;;

let marks ledger =
  F.Marks.of_ledger ledger
  |> Result.map_error ~f:(fun error ->
    P.Error.invalid_request (Sexp.to_string_hum (F.Error.sexp_of_t error)))
;;

let document_equal left right =
  D.Json.equal (D.Document.json left) (D.Document.json right)
;;

let equal_encoded left right ~encode =
  let open Result.Let_syntax in
  let%bind left = encode left in
  let%map right = encode right in
  D.Json.equal left right
;;

let equal_optional left right ~equal =
  match left, right with
  | None, None -> Ok true
  | None, Some _ | Some _, None -> Ok false
  | Some left, Some right -> equal left right
;;

let equal_ledger left right =
  let open Result.Let_syntax in
  let%bind left = L.to_document left |> ledger in
  let%map right = L.to_document right |> ledger in
  document_equal left right
;;

let equal_accounting left right =
  let open Result.Let_syntax in
  let%bind left = A.to_document left in
  let%map right = A.to_document right in
  document_equal left right
;;

module Projection = struct
  type t =
    { ledger : L.t
    ; accounting : A.t option
    ; children : C.t option
    ; catalog : Goal_catalog.t option
    ; accounts : Goal_accounts.t option
    ; delegation : Agent_store.Delegation_store.Reference.t option
    ; active_operation : P.Operation.t option
    ; invocations : P.Invocation.t list
    ; moderator_executions : P.Moderator_execution.t list
    }
end

let context ~session_id (projection : Projection.t) =
  if not (P.Id.Session.equal session_id (L.session_id projection.ledger))
  then invalid "ordered descendant ledger belongs to another session"
  else (
    match projection.accounting with
    | None -> Ok ()
    | Some accounting ->
      A.validate_context
        accounting
        ~session_id
        ~ledger:projection.ledger
        ~children:projection.children
        ~catalog:projection.catalog
        ~accounts:projection.accounts
        ~delegation:projection.delegation)
;;

let validate_ordinary_children = C.validate_ordinary_update

let validate_known_accounting previous incoming =
  match previous, incoming with
  | None, None -> Ok ()
  | None, Some _ -> invalid "descendant accounting requires an actual ordered creation"
  | Some _, None -> invalid "descendant accounting cannot be discarded"
  | Some previous, Some incoming ->
    let open Result.Let_syntax in
    let%bind merged = A.adopt previous ~incoming in
    let%bind equal = equal_accounting merged incoming in
    if equal then Ok () else invalid "descendant accounting omitted preserved fields"
;;

let inherited = function
  | None -> false
  | Some accounting ->
    Option.exists (A.origin accounting) ~f:(fun origin ->
      not (List.is_empty (O.scopes origin)))
;;

let count_change before after =
  let open Result.Let_syntax in
  let%bind before = marks before in
  let%map after = marks after in
  Int64.(after.attempt - before.attempt), Int64.(after.turn - before.turn)
;;

let validate_checkpoint ~(before : Projection.t) ~(incoming : Projection.t) =
  let open Result.Let_syntax in
  let%bind () = validate_known_accounting before.accounting incoming.accounting in
  let%bind () = validate_ordinary_children before.children ~incoming:incoming.children in
  let%bind () = L.validate_update before.ledger ~incoming:incoming.ledger |> ledger in
  let%bind attempts, turns = count_change before.ledger incoming.ledger in
  let%bind () =
    if inherited before.accounting && not (Int64.equal attempts 0L && Int64.equal turns 0L)
    then invalid "inherited canonical admission lacks ordered inclusion evidence"
    else Ok ()
  in
  let%bind () =
    if
      Option.equal
        Agent_store.Delegation_store.Reference.equal
        before.delegation
        incoming.delegation
    then Ok ()
    else invalid "ordered descendant delegation reference changed"
  in
  context ~session_id:(L.session_id before.ledger) incoming
;;

type edge =
  { before : L.t
  ; after : L.t
  ; association : Projection.t
  ; admitted : bool
  ; consumed : bool
  ; required : bool
  }

type t =
  { session_id : P.Id.Session.t
  ; session_revision : int64
  ; original_ledger : L.t
  ; current : Projection.t
  ; edge : edge option
  ; required_publication_revision : int64 option
  }

let current t = t.current

let expected_session_revision t =
  if Int64.(t.session_revision < 0L || t.session_revision = max_value)
  then invalid "descendant candidate session revision exhausted"
  else Ok (Int64.succ t.session_revision)
;;

let validate_publication t ~revision =
  match t.required_publication_revision with
  | None -> Ok ()
  | Some required when Int64.equal required revision -> Ok ()
  | Some _ -> invalid "local Included receipt differs from actual publication revision"
;;

let begin_ ~session_id ~session_revision projection =
  let open Result.Let_syntax in
  let%bind () =
    if Int64.(session_revision < 0L)
    then invalid "negative descendant original session revision"
    else context ~session_id projection
  in
  Ok
    { session_id
    ; session_revision
    ; original_ledger = projection.ledger
    ; current = projection
    ; edge = None
    ; required_publication_revision = None
    }
;;

type step =
  | Catalog of Goal_catalog.t
  | Accounts of Goal_accounts.t
  | Canonical of L.t
  | Reset_generation of int
  | Ordinary_children of C.t
  | Active_operation of P.Operation.t option
  | Invocation of P.Invocation.t
  | Moderator_execution of P.Moderator_execution.t
  | Replacement of Projection.t
  | Update of
      { update : A.Update.t
      ; creation : (P.Goal.t * Goal_accounting_document.t) option
      }

let canonical t (incoming : Projection.t) =
  let open Result.Let_syntax in
  let%bind () = L.validate_update t.current.ledger ~incoming:incoming.ledger |> ledger in
  let%bind unchanged = equal_ledger t.current.ledger incoming.ledger in
  if unchanged
  then Ok { t with current = incoming }
  else (
    let%bind () =
      if Option.exists t.edge ~f:(fun edge -> edge.required && not edge.consumed)
      then invalid "canonical edge replaced an unresolved inherited inclusion"
      else Ok ()
    in
    let%bind attempts, turns = count_change t.current.ledger incoming.ledger in
    let admitted =
      (Int64.equal attempts 1L && Int64.equal turns 0L)
      || (Int64.equal attempts 0L && Int64.equal turns 1L)
    in
    let progress = not (Int64.equal attempts 0L && Int64.equal turns 0L) in
    let required = inherited t.current.accounting && progress in
    let%bind () =
      if required && not admitted
      then invalid "inherited admission must be one exact canonical dimension edge"
      else Ok ()
    in
    let edge =
      { before = t.current.ledger
      ; after = incoming.ledger
      ; association = incoming
      ; admitted
      ; consumed = false
      ; required
      }
    in
    Ok { t with current = incoming; edge = Some edge })
;;

let actual_association t edge fact =
  let frame = Occurrence.frame (A.Canonical.Fact.occurrence fact) in
  let generation = Occurrence.Frame.generation frame in
  let open Result.Let_syntax in
  let%bind () =
    if
      P.Id.Session.equal
        t.session_id
        (P.Session_ref.session_id (Occurrence.Frame.session frame))
      && Int.equal generation (L.generation edge.after)
    then Ok ()
    else invalid "local inclusion fact belongs to another session or generation"
  in
  match Occurrence.kind (A.Canonical.Fact.occurrence fact) with
  | Host_turn _ ->
    (match A.Canonical.Fact.operation fact, edge.association.active_operation with
     | Some operation, Some active
       when D.Json.equal (P.Operation.to_json operation) (P.Operation.to_json active) ->
       Ok ()
     | Some _, Some _ | Some _, None | None, Some _ | None, None ->
       invalid "local Turn inclusion lacks its actual active operation")
  | Attempt { ordinal = _; operation_id; invocation_id } ->
    let%bind () =
      match invocation_id with
      | None -> Ok ()
      | Some id ->
        if
          List.exists edge.association.invocations ~f:(fun invocation ->
            P.Id.Invocation.equal id invocation.P.Invocation.context.id
            && P.Id.Session.equal t.session_id invocation.context.session_id
            && Int.equal generation invocation.context.generation)
        then Ok ()
        else invalid "local inclusion invocation lacks its retained association"
    in
    (* Tracked admission already carries an actual canonical Handle row.
       Untracked labels have no row, so an operation label must resolve against
       actual retained host facts. Event contexts retain their original operation
       even when another operation currently drains that callback. These checks
       validate DATA association; they grant no callback or execution authority. *)
    (match A.Canonical.Fact.tracking fact, operation_id with
     | Tracked, _ | Untracked _, None -> Ok ()
     | Untracked _, Some id ->
       let active =
         Option.exists edge.association.active_operation ~f:(fun operation ->
           P.Id.Operation.equal id operation.id
           && Int.equal generation operation.generation
           &&
           match operation.state with
           | Running -> true
           | Starting | Cancelling | Completed | Failed _ | Cancelled | Interrupted _ ->
             false)
       in
       let event =
         List.exists edge.association.moderator_executions ~f:(fun receipt ->
           P.Id.Session.equal
             t.session_id
             receipt.P.Moderator_execution.context.session_id
           && Int.equal generation receipt.context.generation
           && Option.exists receipt.context.operation_id ~f:(P.Id.Operation.equal id)
           &&
           match receipt.status, receipt.intent with
           | Running, _ | Completed _, Some (Pending | Waiting_compaction _) -> true
           | Completed _, (None | Some (Applied | Discarded _))
           | (Failed _ | Interrupted _), _ -> false)
       in
       if active || event
       then Ok ()
       else invalid "untracked local inclusion operation lacks its retained association")
;;

let update t update ~creation =
  let open Result.Let_syntax in
  let%bind revision = expected_session_revision t in
  let%bind before_ledger, after_ledger, edge =
    match A.Update.reason update with
    | Register_new_root _ -> Ok (t.original_ledger, t.original_ledger, t.edge)
    | Record_included { fact; receipts = _ } ->
      (match t.edge with
       | Some edge when edge.admitted && not edge.consumed ->
         let%map () = actual_association t edge fact in
         edge.before, edge.after, Some { edge with consumed = true }
       | Some _ | None ->
         invalid "local inclusion lacks an unconsumed actual admission edge")
    | Register_child _ | Register_branch _ | Reserve _ | Import _ | Close_before_writer _
      -> Ok (t.current.ledger, t.current.ledger, t.edge)
  in
  let%bind () =
    A.validate_transition
      t.current.accounting
      ~update
      ~before_ledger
      ~after_ledger
      ~before_session_revision:t.session_revision
      ~after_session_revision:revision
      ~before_children:t.current.children
      ~creation
      ~catalog:t.current.catalog
      ~accounts:t.current.accounts
  in
  let required_publication_revision =
    match A.Update.reason update with
    | Record_included _ -> Some revision
    | Register_new_root _
    | Register_child _
    | Register_branch _
    | Reserve _
    | Import _
    | Close_before_writer _ -> t.required_publication_revision
  in
  let children =
    match A.Update.children update with
    | Preserve -> t.current.children
    | Replace children -> Some children
  in
  Ok
    { t with
      current =
        { t.current with accounting = Some (A.Update.accounting update); children }
    ; edge
    ; required_publication_revision
    }
;;

let advance t = function
  | Catalog catalog -> Ok { t with current = { t.current with catalog = Some catalog } }
  | Accounts accounts ->
    Ok { t with current = { t.current with accounts = Some accounts } }
  | Canonical ledger -> canonical t { t.current with ledger }
  | Reset_generation generation ->
    let%bind.Result ledger = L.with_generation t.current.ledger ~generation |> ledger in
    canonical t { t.current with ledger }
  | Ordinary_children children ->
    let%map.Result () =
      validate_ordinary_children t.current.children ~incoming:(Some children)
    in
    { t with current = { t.current with children = Some children } }
  | Active_operation active_operation ->
    Ok { t with current = { t.current with active_operation } }
  | Invocation invocation ->
    let invocations =
      invocation
      :: List.filter t.current.invocations ~f:(fun old ->
        not
          (P.Id.Invocation.equal
             invocation.P.Invocation.context.id
             old.P.Invocation.context.id))
    in
    Ok { t with current = { t.current with invocations } }
  | Moderator_execution execution ->
    let moderator_executions =
      execution
      :: List.filter t.current.moderator_executions ~f:(fun old ->
        not
          (P.Id.Moderator_execution.equal
             execution.P.Moderator_execution.context.id
             old.P.Moderator_execution.context.id))
    in
    Ok { t with current = { t.current with moderator_executions } }
  | Replacement incoming ->
    let open Result.Let_syntax in
    let%bind () = validate_known_accounting t.current.accounting incoming.accounting in
    let%bind () =
      validate_ordinary_children t.current.children ~incoming:incoming.children
    in
    let%bind () =
      if
        Option.equal
          Agent_store.Delegation_store.Reference.equal
          t.current.delegation
          incoming.delegation
      then Ok ()
      else invalid "replacement changed descendant delegation reference"
    in
    canonical t incoming
  | Update { update = incoming; creation } -> update t incoming ~creation
;;

let check t ~incoming =
  let open Result.Let_syntax in
  let%bind () =
    if Option.exists t.edge ~f:(fun edge -> edge.required && not edge.consumed)
    then invalid "ordered candidate omitted required inherited inclusion"
    else Ok ()
  in
  let%bind same_ledger = equal_ledger t.current.ledger incoming.Projection.ledger in
  let%bind same_accounting =
    equal_optional t.current.accounting incoming.accounting ~equal:equal_accounting
  in
  let%bind same_children =
    equal_optional
      t.current.children
      incoming.children
      ~equal:(equal_encoded ~encode:C.to_jsonaf)
  in
  let%bind same_catalog =
    equal_optional
      t.current.catalog
      incoming.catalog
      ~equal:(equal_encoded ~encode:Goal_catalog.to_jsonaf)
  in
  let%bind same_accounts =
    equal_optional
      t.current.accounts
      incoming.accounts
      ~equal:(equal_encoded ~encode:Goal_accounts.to_jsonaf)
  in
  let same_operation =
    Option.equal
      (fun left right ->
         D.Json.equal (P.Operation.to_json left) (P.Operation.to_json right))
      t.current.active_operation
      incoming.active_operation
  in
  let%bind () =
    if
      same_ledger
      && same_accounting
      && same_children
      && same_catalog
      && same_accounts
      && same_operation
      && List.equal P.Invocation.equal t.current.invocations incoming.invocations
      && List.equal
           P.Moderator_execution.equal
           t.current.moderator_executions
           incoming.moderator_executions
      && Option.equal
           Agent_store.Delegation_store.Reference.equal
           t.current.delegation
           incoming.delegation
    then Ok ()
    else invalid "ordered descendant projection differs from actual final candidate"
  in
  context ~session_id:t.session_id incoming
;;
