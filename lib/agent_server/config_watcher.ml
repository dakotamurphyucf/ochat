open Core

type hooks =
  { prepare : Config_diff.t -> Config.t -> (unit, Config.Diagnostic.t list) result
  ; commit : Config_diff.t -> Config.t -> unit
  ; audit : Config_diff.t -> unit
  }

type t =
  { env : Eio_unix.Stdenv.base
  ; path : string
  ; hooks : hooks
  ; mutex : Eio.Mutex.t
  ; mutable current : Config.t
  ; mutable last_mtime : float option
  ; mutable last_error : Config.Diagnostic.t list option
  ; closed : bool Atomic.t
  ; stop : unit Eio.Stream.t
  }

let config_path t = Eio.Path.(Eio.Stdenv.fs t.env / t.path)

let mtime t =
  try Some (Eio.Path.stat ~follow:true (config_path t)).mtime with
  | _ -> None
;;

let create ~env ~path ~initial ~hooks =
  let t =
    { env
    ; path
    ; hooks
    ; mutex = Eio.Mutex.create ()
    ; current = initial
    ; last_mtime = None
    ; last_error = None
    ; closed = Atomic.make false
    ; stop = Eio.Stream.create 1
    }
  in
  t.last_mtime <- mtime t;
  t
;;

let current t = Eio.Mutex.use_ro t.mutex (fun () -> t.current)

let load_validated t =
  let open Result.Let_syntax in
  let%bind raw = Config_parser.load ~env:t.env ~path:t.path in
  Config_validator.validate ~env:t.env raw
;;

let reload t =
  let result =
    let open Result.Let_syntax in
    let%bind candidate = load_validated t in
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      let diff = Config_diff.between ~previous:t.current ~current:candidate in
      let%bind () = t.hooks.prepare diff candidate in
      t.current <- candidate;
      t.last_mtime <- mtime t;
      t.hooks.commit diff candidate;
      t.hooks.audit diff;
      Ok diff)
  in
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () -> t.last_error <- Result.error result);
  result
;;

let changed t =
  match t.last_mtime, mtime t with
  | None, None -> false
  | Some previous, Some current -> Float.(current > previous)
  | None, Some _ | Some _, None -> true
;;

let rec poll clock every t =
  match
    Eio.Fiber.first
      (fun () ->
         Eio.Time.sleep clock every;
         `Poll)
      (fun () ->
         Eio.Stream.take t.stop;
         `Stop)
  with
  | `Stop -> ()
  | `Poll ->
    if changed t then ignore (reload t : (Config_diff.t, Config.Diagnostic.t list) result);
    poll clock every t
;;

let run ~sw ~clock ~every t = Eio.Fiber.fork ~sw (fun () -> poll clock every t)
let close t = if Atomic.compare_and_set t.closed false true then Eio.Stream.add t.stop ()

let status t =
  Eio.Mutex.use_ro t.mutex (fun () -> not (Atomic.get t.closed), t.last_error)
;;
