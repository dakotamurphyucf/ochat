open Core
module Baseline = Prompt_project_baseline
module Durable = Agent_store.Durable_file
module Receipt = Agent_store.Prompt_source_receipt
module Id = Agent_protocol.Id

type state =
  | Available
  | Busy
  | Uncertain
  | Closed
[@@deriving equal, sexp_of]

module Untracked = struct
  type t =
    | Missing_index
    | Invalid_index of Baseline.Error.t
end

module Error = struct
  type t =
    | Busy
    | Closed
    | Uncertain
    | Stale_snapshot
    | Foreign_snapshot
    | Untracked of Untracked.t
    | Baseline of Baseline.Error.t
    | Store of Agent_store.Store_error.t
    | Publication_uncertain of Agent_store.Store_error.t
end

module Work = struct
  type t =
    { mutable remaining : int
    ; checkpoint : unit -> unit
    ; reject : unit -> unit
    }

  let poll t =
    if t.remaining <= 0 then t.reject ();
    t.remaining <- t.remaining - 1;
    Eio.Fiber.check ();
    t.checkpoint ();
    Eio.Fiber.check ()
  ;;

  let equal t left right =
    poll t;
    if String.length left <> String.length right
    then false
    else (
      let index = ref 0 in
      let same = ref true in
      while !same && !index < String.length left do
        if !index mod 4096 = 0 then poll t;
        same := Char.equal left.[!index] right.[!index];
        incr index
      done;
      !same)
  ;;

  let run limits ~checkpoint ~f =
    let module Stop = struct
      exception Limit
    end
    in
    let t =
      { remaining = Baseline.Limits.max_work_steps limits
      ; checkpoint
      ; reject = (fun () -> raise Stop.Limit)
      }
    in
    try f t with
    | Stop.Limit -> Error (Error.Baseline Baseline.Error.Limit_exceeded)
  ;;
end

type history =
  | Tracked of Baseline.Document.t
  | Unknown of Untracked.t

type known =
  { history : history
  ; expected : string option
  }

type phase =
  | Preparing
  | Writing of known

type pending =
  { previous : known
  ; admitted : unit Eio.Promise.t
  ; mutable completed : unit Eio.Promise.or_exn option
  ; mutable context : Eio.Cancel.t option
  ; mutable requested : exn option
  ; mutable phase : phase
  }

type status =
  | Idle of known
  | Active of pending
  | Indeterminate
  | Released

type t =
  { directory : [ `Close | Eio.Fs.dir_ty ] Eio.Path.t
  ; switch : Eio.Switch.t
  ; identity : unit ref
  ; mutable workers : Eio.Switch.t option
  ; limits : Baseline.Limits.t
  ; mutable status : status
  }

module Snapshot = struct
  type t =
    { owner : unit ref
    ; known : known
    ; limits : Baseline.Limits.t
    }

  type previous =
    | First_admission
    | Established of Baseline.Entry.t
    | Untracked of Untracked.t

  let generation t =
    match t.known.history with
    | Tracked document -> Some (Baseline.Document.generation document)
    | Unknown _ -> None
  ;;

  let previous t ~receipt ~checkpoint =
    Work.run t.limits ~checkpoint ~f:(fun work ->
      match t.known.history with
      | Unknown reason -> Ok (Untracked reason)
      | Tracked document ->
        let match_entry =
          List.find (Baseline.Document.entries document) ~f:(fun entry ->
            Work.poll work;
            Id.Prompt_definition.equal
              (Baseline.Entry.definition_id entry)
              (Receipt.definition_id receipt))
        in
        (match match_entry with
         | None -> Ok First_admission
         | Some entry ->
           Work.poll work;
           (* Both receipts were admitted with the schema's4096-byte root ceiling. *)
           if Receipt.equal receipt (Baseline.Entry.source_receipt entry)
           then Ok (Established entry)
           else Error (Error.Baseline Baseline.Error.Foreign_operator_intent)))
  ;;

  let protected_revisions t =
    match t.known.history with
    | Unknown _ -> []
    | Tracked document ->
      List.map (Baseline.Document.entries document) ~f:Baseline.Entry.revision_id
  ;;
end

module Candidate = struct
  type t =
    { owner : unit ref
    ; previous : known
    ; next : known
    ; snapshot : Snapshot.t
    ; bytes : string
    }
end

let basename = "script-project-baselines.json"

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
    Eio.Promise.await pending.admitted;
    Option.iter pending.completed ~f:(fun completed ->
      ignore (Eio.Promise.await completed : (unit, exn) Result.t)))
;;

let close t =
  let previous = t.status in
  t.status <- Released;
  match previous with
  | Active pending -> stop pending Owner_released
  | Idle _ | Indeterminate | Released -> ()
;;

let unavailable = function
  | Busy -> Error.Busy
  | Closed -> Closed
  | Uncertain -> Uncertain
  | Available -> assert false
;;

let settle t pending delivered =
  match t.status with
  | Active current when phys_equal current pending ->
    t.status
    <- (match pending.phase with
        | Preparing -> Idle pending.previous
        | Writing next -> if delivered then Idle next else Indeterminate)
  | Idle _ | Active _ | Indeterminate | Released -> ()
;;

let run t ~checkpoint ~f =
  match state t, t.status with
  | (Busy | Uncertain | Closed), _ -> Error (unavailable (state t))
  | Available, (Active _ | Indeterminate | Released) -> assert false
  | Available, Idle previous ->
    Eio.Switch.check t.switch;
    let admitted, resolve_admitted = Eio.Promise.create () in
    let result, resolve_result = Eio.Promise.create () in
    let pending =
      { previous
      ; admitted
      ; completed = None
      ; context = None
      ; requested = None
      ; phase = Preparing
      }
    in
    t.status <- Active pending;
    let completed =
      try
        let completed =
          Eio.Fiber.fork_promise ~sw:(Option.value_exn t.workers) (fun () ->
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
                          Work.run t.limits ~checkpoint ~f:(fun work -> f pending work))))
              with
              | exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ())
            in
            Eio.Promise.resolve resolve_result outcome)
        in
        pending.completed <- Some completed;
        Eio.Promise.resolve resolve_admitted ();
        completed
      with
      | exn ->
        let backtrace = Stdlib.Printexc.get_raw_backtrace () in
        Eio.Promise.resolve resolve_admitted ();
        settle t pending false;
        Exn.raise_with_original_backtrace exn backtrace
    in
    (try
       (* This promise settles even if switch cancellation skips our callback. *)
       Eio.Promise.await_exn completed;
       match Eio.Promise.await result with
       | Ok (Ok value) ->
         (* Resolved promises do not check cancellation. Delivery only: no callback
           or deadline poll after the writer has committed. *)
         Eio.Fiber.check ();
         if equal_state (state t) Closed
         then (
           settle t pending false;
           Error Error.Closed)
         else (
           settle t pending true;
           Ok value)
       | Ok (Error error) ->
         settle t pending false;
         Error error
       | Error (exn, trace) -> Exn.raise_with_original_backtrace exn trace
     with
     | exn ->
       let trace = Stdlib.Printexc.get_raw_backtrace () in
       stop pending exn;
       settle t pending false;
       Exn.raise_with_original_backtrace exn trace)
;;

let load directory limits =
  Durable.load_bounded_in
    ~directory
    ~basename
    ~max_bytes:(Document_schema.Limits.max_bytes (Baseline.Limits.document limits))
;;

let check_expected t expected work =
  Work.poll work;
  match load t.directory t.limits with
  | Error (Missing _) ->
    (match expected with
     | None -> Ok ()
     | Some _ -> Error Error.Stale_snapshot)
  | Error error -> Error (Error.Store error)
  | Ok current ->
    (match expected with
     | None -> Error Error.Stale_snapshot
     | Some expected ->
       if Work.equal work current expected then Ok () else Error Error.Stale_snapshot)
;;

let open_owner directory sw limits known =
  let t =
    { directory
    ; switch = sw
    ; identity = ref ()
    ; limits
    ; workers = None
    ; status = Idle known
    }
  in
  let ready, resolve_ready = Eio.Promise.create () in
  let resolved = ref false in
  let resolve outcome =
    if not !resolved
    then (
      resolved := true;
      Eio.Promise.resolve resolve_ready outcome)
  in
  Eio.Switch.on_release sw (fun () ->
    close t;
    (* A cancelled daemon may never enter its callback. No worker was admitted;
       settle startup independently of that callback. *)
    resolve (Error None));
  Eio.Fiber.fork_daemon ~sw (fun () ->
    (try
       Eio.Switch.run (fun workers ->
         t.workers <- Some workers;
         Fun.protect
           ~finally:(fun () -> close t)
           (fun () ->
              Eio.Fiber.check ();
              resolve (Ok ());
              Eio.Fiber.await_cancel ()))
     with
     | exn ->
       let trace = Stdlib.Printexc.get_raw_backtrace () in
       resolve (Error (Some (exn, trace)));
       (match exn with
        | Eio.Cancel.Cancelled _ when Eio.Fiber.is_cancelled () -> ()
        | _ -> Exn.raise_with_original_backtrace exn trace));
    `Stop_daemon);
  match Eio.Promise.await ready with
  | Ok () -> Ok t
  | Error None -> Error Error.Closed
  | Error (Some (exn, trace)) -> Exn.raise_with_original_backtrace exn trace
;;

let check_directory directory =
  let _, relative = directory in
  if String.is_empty relative
  then Ok ()
  else
    Error
      (Error.Store
         (Agent_store.Store_error.Corrupt "baseline requires opened owned directory"))
;;

let open_existing directory ~sw ~limits ~checkpoint =
  Work.run limits ~checkpoint ~f:(fun work ->
    let open Result.Let_syntax in
    Eio.Switch.check sw;
    let%bind () = check_directory directory in
    Work.poll work;
    match load directory limits with
    | Error (Missing _) ->
      open_owner directory sw limits { history = Unknown Missing_index; expected = None }
    | Error error -> Error (Error.Store error)
    | Ok bytes ->
      let history =
        match
          Baseline.Document.decode ~limits ~checkpoint:(fun () -> Work.poll work) bytes
        with
        | Ok document -> Tracked document
        | Error error -> Unknown (Invalid_index error)
      in
      open_owner directory sw limits { history; expected = Some bytes })
;;

let create_new directory ~sw ~limits ~checkpoint =
  Work.run limits ~checkpoint ~f:(fun work ->
    let open Result.Let_syntax in
    Eio.Switch.check sw;
    let%bind () = check_directory directory in
    Work.poll work;
    let%bind () =
      match load directory limits with
      | Error (Missing _) -> Ok ()
      | Error error -> Error (Error.Store error)
      | Ok _ -> Error Error.Stale_snapshot
    in
    let%bind document =
      Baseline.Document.empty ~limits |> Result.map_error ~f:(fun e -> Error.Baseline e)
    in
    let%bind bytes =
      Baseline.Document.encode document ~limits ~checkpoint:(fun () -> Work.poll work)
      |> Result.map_error ~f:(fun e -> Error.Baseline e)
    in
    let previous = { history = Tracked document; expected = None } in
    let next = { history = Tracked document; expected = Some bytes } in
    let%bind t = open_owner directory sw limits previous in
    run
      t
      ~checkpoint:(fun () -> Work.poll work)
      ~f:(fun pending inner ->
        let%bind () = check_expected t None inner in
        Work.poll inner;
        let writing = Writing next in
        let outcome = Ok t in
        pending.phase <- writing;
        Eio.Cancel.protect (fun () ->
          Durable.replace_in
            ~directory
            ~basename
            ~durability:Flush_file_and_directory
            bytes)
        |> Result.map_error ~f:(fun e -> Error.Publication_uncertain e)
        |> Result.bind ~f:(fun () -> outcome)))
;;

let snapshot t ~checkpoint =
  run t ~checkpoint ~f:(fun pending work ->
    let open Result.Let_syntax in
    let%map () = check_expected t pending.previous.expected work in
    { Snapshot.owner = t.identity; known = pending.previous; limits = t.limits })
;;

let same_known work left right =
  Option.equal (Work.equal work) left.expected right.expected
  &&
  match left.history, right.history with
  | Tracked left, Tracked right ->
    Int.equal (Baseline.Document.generation left) (Baseline.Document.generation right)
  | Unknown _, Unknown _ -> true
  | Tracked _, Unknown _ | Unknown _, Tracked _ -> false
;;

let prepare t ~snapshot ~successes ~checkpoint =
  if not (phys_equal snapshot.Snapshot.owner t.identity)
  then Error Error.Foreign_snapshot
  else
    run t ~checkpoint ~f:(fun pending work ->
      let open Result.Let_syntax in
      if not (same_known work snapshot.known pending.previous)
      then Error Error.Stale_snapshot
      else (
        match snapshot.known.history with
        | Unknown reason -> Error (Error.Untracked reason)
        | Tracked document ->
          let%bind next_document =
            Baseline.Document.replace_successes
              document
              ~entries:successes
              ~limits:t.limits
              ~checkpoint:(fun () -> Work.poll work)
            |> Result.map_error ~f:(fun e -> Error.Baseline e)
          in
          let%map bytes =
            Baseline.Document.encode next_document ~limits:t.limits ~checkpoint:(fun () ->
              Work.poll work)
            |> Result.map_error ~f:(fun e -> Error.Baseline e)
          in
          let next = { history = Tracked next_document; expected = Some bytes } in
          { Candidate.owner = t.identity
          ; previous = pending.previous
          ; next
          ; bytes
          ; snapshot = { Snapshot.owner = t.identity; known = next; limits = t.limits }
          }))
;;

let publish t candidate ~checkpoint =
  if not (phys_equal candidate.Candidate.owner t.identity)
  then Error Error.Foreign_snapshot
  else
    run t ~checkpoint ~f:(fun pending work ->
      let open Result.Let_syntax in
      if not (same_known work candidate.previous pending.previous)
      then Error Error.Stale_snapshot
      else (
        let%bind () = check_expected t candidate.previous.expected work in
        let outcome = Ok candidate.snapshot in
        let writing = Writing candidate.next in
        Work.poll work;
        pending.phase <- writing;
        match
          Eio.Cancel.protect (fun () ->
            Durable.replace_in
              ~directory:t.directory
              ~basename
              ~durability:Flush_file_and_directory
              candidate.bytes)
        with
        | Ok () -> outcome
        | Error error -> Error (Error.Publication_uncertain error)))
;;
