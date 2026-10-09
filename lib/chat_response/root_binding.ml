open! Core
module Runtime = Inference_runtime

type current =
  { binding : Runtime.Context.Owned_binding.t
  ; context : Runtime.Context.t
  }

type t =
  { session : Runtime.Session.t
  ; mutable current : current option
  ; mutable borrowed : bool
  ; mutable closed : bool
  }

let create session = { session; current = None; borrowed = false; closed = false }

let retire t =
  let current = t.current in
  t.current <- None;
  Option.iter current ~f:(fun current ->
    Runtime.Context.Owned_binding.close current.binding)
;;

let close t =
  t.closed <- true;
  if not t.borrowed then retire t
;;

let select t resolved =
  let open Result.Let_syntax in
  let reused =
    Option.bind t.current ~f:(fun current ->
      if
        Inference.Observation.Transport_policy.equal
          (Runtime.Context.transport_policy current.context)
          (Runtime.Context.transport_policy resolved)
      then
        Runtime.Context.derive_in_session
          current.context
          ~target:(Runtime.Context.target resolved)
        |> Result.ok
        |> Option.map ~f:(fun context -> { current with context })
      else None)
  in
  match reused with
  | Some current ->
    t.current <- Some current;
    Ok current.context
  | None ->
    let%bind binding = Runtime.Context.open_owned_binding resolved t.session in
    let context = Runtime.Context.Owned_binding.context binding in
    if t.closed || Runtime.Session.is_closed t.session
    then (
      Runtime.Context.Owned_binding.close binding;
      Error Runtime.Preparation_error.Session_closed)
    else (
      let previous = t.current in
      t.current <- Some { binding; context };
      Option.iter previous ~f:(fun previous ->
        Runtime.Context.Owned_binding.close previous.binding);
      Ok context)
;;

let with_context t ~resolved ~f =
  if t.closed || Runtime.Session.is_closed t.session
  then Error Runtime.Preparation_error.Session_closed
  else if t.borrowed
  then Error Runtime.Preparation_error.Invalid_preparation
  else (
    t.borrowed <- true;
    let finish () =
      t.borrowed <- false;
      if t.closed then Eio.Cancel.protect (fun () -> retire t)
    in
    match
      let open Result.Let_syntax in
      let%map context = select t resolved in
      f
        (Runtime.Context.with_preparation_lifetime context ~is_open:(fun () ->
           not t.closed))
    with
    | result ->
      finish ();
      result
    | exception exn ->
      let backtrace = Stdlib.Printexc.get_raw_backtrace () in
      (try finish () with
       | _ -> ());
      Exn.raise_with_original_backtrace exn backtrace)
;;

let wrap t (source : Root_context.t) : Root_context.t =
  { with_context =
      (fun ~previous ~history f ->
        source.with_context ~previous ~history (fun resolved ~on_dispatch ->
          match with_context t ~resolved ~f:(fun context -> f context ~on_dispatch) with
          | Ok result -> result
          | Error error -> raise_s [%sexp (error : Runtime.Preparation_error.t)]))
  }
;;
