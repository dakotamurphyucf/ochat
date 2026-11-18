open! Core
module P = Agent_protocol
module D = Agent_store.Delegation_store
module Origin = Agent_store.Delegation_goal_origin
module Scope = Origin.Scope
module Anchor = Origin.Anchor
module GD = Goal_descendants
module C = Child_contributions

type t =
  { scope : Scope.t
  ; reference : D.Reference.t
  ; origin : Origin.t
  }

let scope t = t.scope
let reference t = t.reference
let conflict message = Error (P.Error.create Conflict ~message ~retryable:false ())

let anchor_matches anchor (record : D.record) =
  P.Id.Session.equal (Anchor.parent_session_id anchor) record.key.parent_session_id
  && Int.equal (Anchor.parent_generation anchor) record.key.parent_generation
  && P.Id.Principal.equal (Anchor.principal_id anchor) record.key.principal_id
  && P.Idempotency_key.equal (Anchor.key anchor) record.key.idempotency_key
  && P.Id.Session.equal (Anchor.child_session_id anchor) record.admission.child_session_id
  && P.Id.Transaction.equal (Anchor.transaction_id anchor) record.admission.transaction_id
;;

let validate t ~accounting ~children =
  let open Result.Let_syntax in
  let%bind () =
    if
      P.Session_ref.equal (GD.session accounting) (Scope.owner t.scope)
      && List.mem (GD.roots accounting) (Scope.goal_id t.scope) ~equal:P.Id.Goal.equal
    then Ok ()
    else conflict "descendant route has no matching actual root"
  in
  let%bind () =
    if
      List.exists
        (GD.branches accounting ~goal_id:(Scope.goal_id t.scope))
        ~f:(fun branch ->
          Scope.equal (GD.Branch.scope branch) t.scope
          && D.Reference.equal (GD.Branch.reference branch) t.reference)
    then Ok ()
    else conflict "descendant route has no exact retained branch"
  in
  let%bind actual_origin = C.origin children ~reference:t.reference in
  if
    Option.exists actual_origin ~f:(Origin.equal t.origin)
    && List.mem (Origin.scopes t.origin) t.scope ~equal:Scope.equal
  then Ok ()
  else conflict "descendant route child origin differs from private admission"
;;

let canonical_scopes scopes =
  let rec loop = function
    | [] | [ _ ] -> Ok ()
    | left :: (right :: _ as rest) ->
      if Scope.compare_root left right < 0
      then loop rest
      else conflict "descendant route requires the complete canonical root vector"
  in
  loop scopes
;;

let select ~session ~scopes ~records ~accounting ~children =
  let open Result.Let_syntax in
  let%bind () = canonical_scopes scopes in
  let%bind () =
    if P.Session_ref.equal session (GD.session accounting)
    then Ok ()
    else conflict "descendant route accounting belongs to another session"
  in
  List.filter scopes ~f:(fun scope -> P.Session_ref.equal (Scope.owner scope) session)
  |> List.map ~f:(fun scope ->
    let%bind record =
      match List.filter records ~f:(anchor_matches (Scope.branch scope)) with
      | [ record ] -> Ok record
      | [] -> conflict "descendant route first-child private admission is absent"
      | _ :: _ :: _ -> conflict "descendant route first-child private admission repeats"
    in
    let%bind origin =
      Result.of_option
        record.admission.goal_origin
        ~error:(P.Error.invalid_request "descendant route first-child origin is unknown")
    in
    let route = { scope; reference = D.reference record; origin } in
    let%map () = validate route ~accounting ~children in
    route)
  |> Result.all
;;
