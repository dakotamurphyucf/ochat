open Core
module Error = Protocol_error

type conversation =
  { session_id : Id.Session.t
  ; generation : int
  }
[@@deriving compare, equal, sexp_of]

type developer =
  { host_id : Id.Server.t
  ; run_id : Id.Developer_run.t
  ; epoch : int
  ; principal_id : Id.Principal.t
  }
[@@deriving compare, equal, sexp_of]

type t =
  | Conversation of conversation
  | Developer of developer
[@@deriving compare, equal, sexp_of]

let validate = function
  | Conversation { session_id; generation } ->
    let open Result.Let_syntax in
    let%bind () =
      Extension_codec.validate_id Id.Session.to_json Id.Session.of_json session_id
    in
    if generation < 0
    then Error (Error.invalid_request "negative conversation generation")
    else Ok ()
  | Developer { host_id; run_id; epoch; principal_id } ->
    let open Result.Let_syntax in
    let%bind () =
      Extension_codec.validate_id Id.Server.to_json Id.Server.of_json host_id
    in
    let%bind () =
      Extension_codec.validate_id Id.Developer_run.to_json Id.Developer_run.of_json run_id
    in
    let%bind () =
      Extension_codec.validate_id Id.Principal.to_json Id.Principal.of_json principal_id
    in
    if epoch < 0
    then Error (Error.invalid_request "negative developer run epoch")
    else Ok ()
;;

let conversation ~session_id ~generation =
  let owner = Conversation { session_id; generation } in
  Result.map (validate owner) ~f:(fun () -> owner)
;;

let developer ~host_id ~run_id ~epoch ~principal_id =
  let owner = Developer { host_id; run_id; epoch; principal_id } in
  Result.map (validate owner) ~f:(fun () -> owner)
;;
