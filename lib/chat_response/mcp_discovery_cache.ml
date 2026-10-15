open! Core

type 'a t =
  { now : unit -> float
  ; load : unit -> 'a
  ; mutex : Eio.Mutex.t
  ; mutable cached : (float * 'a) option
  }

let create ~now ~load = { now; load; mutex = Eio.Mutex.create (); cached = None }
let invalidate t = Eio.Mutex.use_rw ~protect:true t.mutex (fun () -> t.cached <- None)

let load t ~before_load =
  match t.cached with
  | Some (expires_at, value) when Float.(expires_at > t.now ()) -> Ok value
  | _ ->
    Result.map (before_load ()) ~f:(fun () ->
      let value = t.load () in
      t.cached <- Some (t.now () +. 300., value);
      value)
;;

let get_checked t ~before_load =
  let result =
    Eio.Mutex.use_rw ~protect:false t.mutex (fun () ->
      try Ok (load t ~before_load) with
      | exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ()))
  in
  match result with
  | Ok value -> value
  | Error (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
;;

let get t =
  match get_checked t ~before_load:(fun () -> Ok ()) with
  | Ok value -> value
  | Error _ -> assert false (* The always-Ok check cannot reject. *)
;;
