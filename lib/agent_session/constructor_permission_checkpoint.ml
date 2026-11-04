open! Core
module P = Agent_protocol

let equal_permission left right =
  Sexp.equal (P.Permission.sexp_of_t left) (P.Permission.sexp_of_t right)
;;

let equal_grant left right = Sexp.equal (P.Grant.sexp_of_t left) (P.Grant.sexp_of_t right)

type t =
  { permissions : P.Permission.t list
  ; grants : P.Grant.t list
  ; lifecycle : Session_state.Lifecycle.t
  }

let apply t (state : Session_state.t) =
  { state with permissions = t.permissions; grants = t.grants; lifecycle = t.lifecycle }
;;

let prepare
      ~(previous : Session_state.t)
      ~(incoming : Session_state.t)
      ~invocation_id
      ~permissions
      ~grant
      ~(lifecycle : Session_state.Lifecycle.t)
  =
  let conflict () =
    Error
      (P.Error.create
         Conflict
         ~message:"constructor permission ACK changed unselected facts"
         ~retryable:false
         ())
  in
  let selected_valid (permission : P.Permission.t) =
    P.Id.Session.equal permission.session_id previous.identity.session_id
    && Int.equal permission.generation previous.identity.generation
    && (match permission.owner with
        | Invocation id -> P.Id.Invocation.equal invocation_id id
        | Operation _ -> false)
    &&
    match
      List.find previous.permissions ~f:(fun old ->
        P.Id.Permission.equal old.id permission.id)
    with
    | None ->
      P.Permission.equal_state permission.state Pending
      && Option.is_none permission.resolution
    | Some old ->
      P.Permission.equal_state old.state Pending
      && (not (P.Permission.equal_state permission.state Pending))
      && Option.is_some permission.resolution
      && equal_permission
           old
           { permission with state = old.state; resolution = old.resolution }
  in
  let expected_permissions =
    List.fold permissions ~init:previous.permissions ~f:(fun retained permission ->
      permission
      :: List.filter retained ~f:(fun old ->
        not (P.Id.Permission.equal old.id permission.id)))
  in
  let expected_grants =
    Option.value_map grant ~default:previous.grants ~f:(fun grant ->
      grant :: previous.grants)
  in
  if
    (not (List.is_empty permissions))
    && Option.is_none
         (List.find_a_dup permissions ~compare:(fun left right ->
            P.Id.Permission.compare left.id right.id))
    && List.for_all permissions ~f:selected_valid
    && List.equal equal_permission expected_permissions incoming.permissions
    && List.equal equal_grant expected_grants incoming.grants
    && Option.for_all grant ~f:(fun grant ->
      P.Id.Session.equal grant.session_id previous.identity.session_id
      && P.Grant.equal_state grant.state Active
      && not
           (List.exists previous.grants ~f:(fun old -> P.Id.Grant.equal old.id grant.id)))
    && P.Session.equal_desired_state lifecycle.desired previous.lifecycle.desired
    && Sexp.equal
         (Session_state.Lifecycle.sexp_of_t lifecycle)
         (Session_state.Lifecycle.sexp_of_t incoming.lifecycle)
  then Ok { permissions = incoming.permissions; grants = incoming.grants; lifecycle }
  else conflict ()
;;
