open! Core
module P = Agent_protocol
module Actor = Operator_authorization

let required_scope = function
  | Agent_store.Developer_command_receipt.Kind.Check -> P.Scope.Developer_check
  | Fixture_run -> P.Scope.Developer_fixture_run
  | Live_run -> P.Scope.Developer_live_run
;;

let denied message = P.Error.create P.Error.Permission_denied ~message ~retryable:false ()

let authorize_principal kind ~principal =
  let scope = required_scope kind in
  if P.Principal.has_scope principal scope
  then Ok ()
  else Error (denied ("missing scope " ^ P.Scope.to_string scope))
;;

let check_now kind ~original ~current =
  if (not (Actor.is_current original)) || not (Actor.is_current current)
  then Error (denied "developer actor is no longer current")
  else (
    let original_principal = Actor.principal original in
    let current_principal = Actor.principal current in
    if
      (not (P.Id.Principal.equal original_principal.id current_principal.id))
      || not
           (String.equal
              original_principal.authentication_kind
              current_principal.authentication_kind)
    then Error (denied "developer actor identity changed")
    else
      let open Result.Let_syntax in
      let%bind () = authorize_principal kind ~principal:original_principal in
      authorize_principal kind ~principal:current_principal)
;;
