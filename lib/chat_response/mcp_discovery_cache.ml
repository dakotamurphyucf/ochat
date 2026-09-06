open! Core

type 'a t =
  { now : unit -> float
  ; load : unit -> 'a
  ; mutex : Eio.Mutex.t
  ; mutable cached : (float * 'a) option
  }

let create ~now ~load = { now; load; mutex = Eio.Mutex.create (); cached = None }
let invalidate t = Eio.Mutex.use_rw ~protect:true t.mutex (fun () -> t.cached <- None)

let load t =
  match t.cached with
  | Some (expires_at, value) when Float.(expires_at > t.now ()) -> value
  | _ ->
    let value = t.load () in
    t.cached <- Some (t.now () +. 300., value);
    value
;;

let get t =
  let result =
    Eio.Mutex.use_rw ~protect:false t.mutex (fun () ->
      try Ok (load t) with
      | exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ()))
  in
  match result with
  | Ok value -> value
  | Error (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
;;
