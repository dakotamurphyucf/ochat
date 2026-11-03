open! Core

type t = Goal.t

let sexp_of_t t = Jsonaf.sexp_of_t (Goal.to_json t)

let t_of_sexp sexp =
  match Goal.of_json (Jsonaf.t_of_sexp sexp) with
  | Ok t -> t
  | Error error -> Sexplib.Conv.of_sexp_error error.Protocol_error.message sexp
;;

let list_of_json = function
  | `Array values when List.length values <= 16 ->
    List.map values ~f:Goal.of_json |> Result.all
  | _ ->
    Error
      (Protocol_error.invalid_request
         "goal projection exceeds sixteen goals or is not an array")
;;

let validate goals ~session_id ~generation =
  let open Result.Let_syntax in
  let invalid () =
    Error
      (Protocol_error.invalid_request
         "goal projection has duplicate, foreign, future or excessive goals")
  in
  if
    List.length goals > 16
    || List.count goals ~f:(fun goal ->
         match Goal.state goal with
         | Open Enabled -> true
         | Open (Held _) | Closed _ -> false)
       > 1
  then invalid ()
  else (
    let%map _ =
      List.fold_result goals ~init:[] ~f:(fun seen goal ->
        if
          List.exists seen ~f:(Id.Goal.equal (Goal.id goal))
          || (not
                (Id.Session.equal (Session_ref.session_id (Goal.session goal)) session_id))
          || (Goal.source goal).generation > generation
        then invalid ()
        else Ok (Goal.id goal :: seen))
    in
    ())
;;
