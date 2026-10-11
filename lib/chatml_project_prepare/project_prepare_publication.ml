open Core
module Captured = Chatml_source
module Completed = Project_prepare_result
module Result = Completed
module Error = Project_prepare_error
module Deadline = Project_prepare_deadline
module Durable = Agent_store.Durable_file

module Expected = struct
  type t =
    | Absent
    | Exact of Captured.t
end

type state =
  | Available
  | Busy
  | Uncertain
  | Closed
[@@deriving equal, sexp_of]

type phase =
  | Preparing
  | Writing of Expected.t

type pending =
  { previous : Expected.t
  ; completed : unit Eio.Promise.t
  ; mutable context : Eio.Cancel.t option
  ; mutable requested : exn option
  ; mutable phase : phase
  }

type status =
  | Idle of Expected.t
  | Active of pending
  | Indeterminate
  | Released

type t =
  { directory : [ `Close | Eio.Fs.dir_ty ] Eio.Path.t
  ; switch : Eio.Switch.t
  ; basename : string
  ; maximum : int
  ; mutable status : status
  }

exception Owner_released

let state t =
  if Option.is_some (Eio.Switch.get_error t.switch)
  then Closed
  else (
    match t.status with
    | Idle _ -> Available
    | Active { phase = Preparing; _ } -> Busy
    | Active { phase = Writing _; _ } | Indeterminate -> Uncertain
    | Released -> Closed)
;;

let stop pending reason =
  Eio.Cancel.protect (fun () ->
    pending.requested <- Some reason;
    Option.iter pending.context ~f:(fun context -> Eio.Cancel.cancel context reason);
    Eio.Promise.await pending.completed)
;;

let close t =
  let previous = t.status in
  t.status <- Released;
  match previous with
  | Active pending -> stop pending Owner_released
  | Idle _ | Indeterminate | Released -> ()
;;

let open_owned directory ~sw ~basename ~(expected : Expected.t) ~max_lock_bytes =
  let _, relative = directory in
  if
    max_lock_bytes < 0
    || String.length basename > 1024
    || String.contains basename '/'
    || (not (String.is_empty relative))
    ||
    match expected with
    | Absent -> false
    | Exact source -> Captured.byte_length source > max_lock_bytes
  then Error (Agent_store.Store_error.Corrupt "invalid selected-lock publication input")
  else (
    match Source.Portable_path.create basename with
    | Error _ ->
      Error (Agent_store.Store_error.Corrupt "invalid selected-lock publication basename")
    | Ok _ ->
      let t =
        { directory
        ; switch = sw
        ; basename
        ; maximum = max_lock_bytes
        ; status = Idle expected
        }
      in
      Eio.Switch.on_release sw (fun () -> close t);
      Ok t)
;;

let settle t pending outcome =
  match t.status with
  | Active current when phys_equal current pending ->
    t.status
    <- (match outcome, pending.phase with
        | `Delivered, Writing expected -> Idle expected
        | (`Failed | `Abandoned), Writing _ -> Indeterminate
        | (`Delivered | `Failed | `Abandoned), Preparing -> Idle pending.previous)
  | Idle _ | Active _ | Indeterminate | Released -> ()
;;

let unavailable = function
  | Busy ->
    Error.create
      (Publication (Agent_store.Store_error.Locked (Some "selected lock busy")))
  | Uncertain ->
    Error.create
      (Publication_uncertain
         (Agent_store.Store_error.Corrupt "selected lock requires reconciliation"))
  | Closed ->
    Error.create
      (Publication (Agent_store.Store_error.Corrupt "selected lock owner closed"))
  | Available -> assert false
;;

let poll ~deadline ~clock ~checkpoint =
  Eio.Fiber.check ();
  let open Core.Result.Let_syntax in
  let%bind () = Deadline.check deadline ~clock in
  checkpoint ();
  Eio.Fiber.check ();
  Deadline.check deadline ~clock
;;

let same_bytes left right ~poll =
  if String.length left <> String.length right
  then Ok false
  else (
    let rec loop offset =
      let open Core.Result.Let_syntax in
      let%bind () = if offset mod 4096 = 0 then poll () else Ok () in
      if offset = String.length left
      then Ok true
      else if Char.equal left.[offset] right.[offset]
      then loop (offset + 1)
      else Ok false
    in
    loop 0)
;;

let check_expected t expected ~poll =
  let open Core.Result.Let_syntax in
  let%bind () = poll () in
  match
    Durable.load_bounded_in
      ~directory:t.directory
      ~basename:t.basename
      ~max_bytes:t.maximum
  with
  | Error (Missing _) ->
    (match expected with
     | Expected.Absent -> Ok ()
     | Exact _ -> Error (Error.create Stale_selection))
  | Error error -> Error (Error.create (Publication error))
  | Ok contents ->
    (match expected with
     | Absent -> Error (Error.create Stale_selection)
     | Exact source ->
       let%bind equal = same_bytes contents (Captured.contents source) ~poll in
       if equal then Ok () else Error (Error.create Stale_selection))
;;

let prepare_and_write t pending ~deadline ~clock ~checkpoint ~prepare =
  let poll () = poll ~deadline ~clock ~checkpoint in
  let open Core.Result.Let_syntax in
  let%bind () = check_expected t pending.previous ~poll in
  let%bind candidate = prepare () in
  let source = Chatml_project_build.lock_source (Completed.build candidate) in
  if Captured.byte_length source > t.maximum
  then Error (Error.create Limit_exceeded)
  else (
    let next = Expected.Exact source in
    let writing = Writing next in
    let contents = Captured.contents source in
    let%bind () = check_expected t pending.previous ~poll in
    let%bind () = poll () in
    pending.phase <- writing;
    Eio.Cancel.protect (fun () ->
      Durable.replace_in
        ~directory:t.directory
        ~durability:Flush_file_and_directory
        ~basename:t.basename
        contents)
    |> Core.Result.map_error ~f:(fun error -> Error.create (Publication_uncertain error))
    |> Core.Result.map ~f:(fun () -> candidate))
;;

let run t ~deadline ~clock ~checkpoint ~prepare =
  match state t, t.status with
  | (Busy | Uncertain | Closed), _ -> Error (unavailable (state t))
  | Available, (Active _ | Indeterminate | Released) -> assert false
  | Available, Idle previous ->
    let completed, resolve_completed = Eio.Promise.create () in
    let result, resolve_result = Eio.Promise.create () in
    let pending =
      { previous; completed; context = None; requested = None; phase = Preparing }
    in
    t.status <- Active pending;
    (try
       Eio.Fiber.fork_daemon ~sw:t.switch (fun () ->
         let outcome =
           try
             Ok
               (Fun.protect
                  ~finally:(fun () -> pending.context <- None)
                  (fun () ->
                     Eio.Cancel.sub (fun context ->
                       pending.context <- Some context;
                       Option.iter pending.requested ~f:(Eio.Cancel.cancel context);
                       Eio.Fiber.check ();
                       prepare_and_write t pending ~deadline ~clock ~checkpoint ~prepare)))
           with
           | exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ())
         in
         Eio.Promise.resolve resolve_result outcome;
         Eio.Promise.resolve resolve_completed ();
         `Stop_daemon)
     with
     | exn ->
       let backtrace = Stdlib.Printexc.get_raw_backtrace () in
       settle t pending `Failed;
       Exn.raise_with_original_backtrace exn backtrace);
    (try
       match Eio.Promise.await result with
       | Ok (Ok value) ->
         (* Resolved promises do not themselves check cancellation. This is only
            delivery admission, never another progress/deadline callback. *)
         Eio.Fiber.check ();
         if equal_state (state t) Closed
         then (
           settle t pending `Failed;
           Error (unavailable Closed))
         else (
           settle t pending `Delivered;
           Ok value)
       | Ok (Error error) ->
         settle t pending `Failed;
         Error error
       | Error (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
     with
     | exn ->
       let backtrace = Stdlib.Printexc.get_raw_backtrace () in
       stop pending exn;
       settle t pending `Abandoned;
       Exn.raise_with_original_backtrace exn backtrace)
;;
