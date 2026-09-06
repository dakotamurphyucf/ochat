open Core

type t =
  { done_ : unit Eio.Promise.or_exn
  ; mutable cancel : Eio.Cancel.t option
  ; mutable interrupted : bool
  }

let start ~sw f =
  let done_, resolver = Eio.Promise.create () in
  let t = { done_; cancel = None; interrupted = false } in
  Eio.Fiber.fork ~sw (fun () ->
    let result =
      try
        Eio.Cancel.sub (fun cancel ->
          t.cancel <- Some cancel;
          if t.interrupted then Eio.Cancel.cancel cancel Exit;
          f ());
        Ok ()
      with
      | Eio.Cancel.Cancelled _ ->
        t.interrupted <- true;
        Ok ()
      | exn -> Error exn
    in
    t.cancel <- None;
    Eio.Promise.resolve resolver result);
  t
;;

let finished t = Eio.Promise.is_resolved t.done_
let interrupted t = t.interrupted
let await t = Eio.Promise.await_exn t.done_

let stop t =
  if not (finished t)
  then (
    t.interrupted <- true;
    Option.iter t.cancel ~f:(fun cancel -> Eio.Cancel.cancel cancel Exit))
;;

let drain ~clock readers =
  match
    Eio.Time.with_timeout clock 0.5 (fun () ->
      List.iter readers ~f:await;
      Ok ())
  with
  | Ok () -> ()
  | Error `Timeout ->
    List.iter readers ~f:stop;
    Eio.Time.with_timeout_exn clock 0.5 (fun () -> List.iter readers ~f:await)
;;
