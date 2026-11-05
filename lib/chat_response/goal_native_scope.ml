open! Core

type handlers =
  { deliverable : int -> (bool, string) result
  ; accept : int -> (unit, string) result
  ; rollback : int -> unit
  ; prepare : int list -> (unit, string) result
  }

type t =
  { active : bool Atomic.t
  ; handlers : handlers
  }

let create handlers = { active = Atomic.make true; handlers }
let close t = Atomic.set t.active false
let is_open t = Atomic.get t.active
let same t other = phys_equal t other

(* Like Execution_gate, lexical routing supports ordinary synchronous callers
   and an explicitly captured context on a native worker domain. An absent
   context stays absent; these slots never mint callback authority. *)
let context key synchronous =
  match Eio.Fiber.get key with
  | value -> `Fiber, Option.value value ~default:(Domain.DLS.get synchronous)
  | exception Effect.Unhandled Eio.Private.Effects.Get_context ->
    `Synchronous, Domain.DLS.get synchronous
;;

let bind key synchronous value f =
  match fst (context key synchronous) with
  | `Fiber -> Eio.Fiber.with_binding key value f
  | `Synchronous ->
    let previous = Domain.DLS.get synchronous in
    Domain.DLS.set synchronous value;
    Exn.protect ~f ~finally:(fun () -> Domain.DLS.set synchronous previous)
;;

let scope_key = Eio.Fiber.create_key ()
let synchronous_scope = Domain.DLS.new_key (fun () -> None)
let with_scope t f = bind scope_key synchronous_scope (Some t) f
let current () = snd (context scope_key synchronous_scope) |> Option.filter ~f:is_open

let handlers t =
  let check () = if is_open t then Ok () else Error "Goal callback transaction ended" in
  { deliverable =
      (fun ticket -> Result.bind (check ()) ~f:(fun () -> t.handlers.deliverable ticket))
  ; accept =
      (fun ticket -> Result.bind (check ()) ~f:(fun () -> t.handlers.accept ticket))
  ; rollback = (fun ticket -> t.handlers.rollback ticket)
  ; prepare =
      (fun tickets -> Result.bind (check ()) ~f:(fun () -> t.handlers.prepare tickets))
  }
;;

type state =
  | Open of int list
  | Closed

type collection =
  { owner : t
  ; entries : state Atomic.t
  }

let collection_key = Eio.Fiber.create_key ()
let synchronous_collection = Domain.DLS.new_key (fun () -> None)
let capture () = snd (context collection_key synchronous_collection)
let with_context captured f = bind collection_key synchronous_collection captured f
let owner t = t.owner

let collect scope f =
  match scope with
  | None -> with_context None (fun () -> f (), [])
  | Some owner ->
    let context = { owner; entries = Atomic.make (Open []) } in
    Exn.protect
      ~finally:(fun () -> Atomic.set context.entries Closed)
      ~f:(fun () ->
        with_context (Some context) (fun () ->
          let result = f () in
          match Atomic.exchange context.entries Closed with
          | Open entries -> result, List.rev entries
          | Closed -> assert false))
;;

let emit ticket =
  let rec append context =
    if ticket < 0 || not (is_open context.owner)
    then Error "Goal native receipt scope ended"
    else (
      match Atomic.get context.entries with
      | Closed -> Error "Goal native collection ended"
      | Open entries as previous ->
        if List.mem entries ticket ~equal:Int.equal
        then Ok ()
        else if Atomic.compare_and_set context.entries previous (Open (ticket :: entries))
        then Ok ()
        else append context)
  in
  match capture () with
  | None -> Error "Goal native transaction route is not installed"
  | Some context -> append context
;;
