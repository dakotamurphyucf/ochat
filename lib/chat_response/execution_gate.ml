open Core

type t =
  { id : int
  ; mutex : Eio.Mutex.t
  }

type error =
  | Reentrant
  | Wait_cycle
  | Resource_limit

type lease =
  { owner : t
  ; mutable active : bool
  }

type request =
  { target : t
  ; sources : lease list
  }

let next_id = Atomic.make 0
let graph_mutex = Stdlib.Mutex.create ()
let requests : request list ref = ref []
let ancestry : lease list Eio.Fiber.key = Eio.Fiber.create_key ()
let synchronous_ancestry = Domain.DLS.new_key (fun () -> [])

let locked f =
  Stdlib.Mutex.lock graph_mutex;
  Exn.protect ~f ~finally:(fun () -> Stdlib.Mutex.unlock graph_mutex)
;;

let create () = { id = Atomic.fetch_and_add next_id 1; mutex = Eio.Mutex.create () }

let error_message = function
  | Reentrant -> "moderator_reentrancy: synchronous call would re-enter an active owner"
  | Wait_cycle ->
    "moderator_wait_cycle: synchronous call would create an owner wait cycle"
  | Resource_limit ->
    "invocation.coordination_limit: too many active or waiting owner acquisitions"
;;

let current_ancestry () =
  match Eio.Fiber.get ancestry with
  | value -> `Fiber, Option.value value ~default:(Domain.DLS.get synchronous_ancestry)
  | exception Effect.Unhandled _ -> `Synchronous, Domain.DLS.get synchronous_ancestry
;;

let bind mode value f =
  match mode with
  | `Fiber -> Eio.Fiber.with_binding ancestry value f
  | `Synchronous ->
    let previous = Domain.DLS.get synchronous_ancestry in
    Domain.DLS.set synchronous_ancestry value;
    Exn.protect ~f ~finally:(fun () -> Domain.DLS.set synchronous_ancestry previous)
;;

type context = lease list

let capture_context () = snd (current_ancestry ())

let with_context captured f =
  let mode, current = current_ancestry () in
  let combined =
    locked (fun () ->
      let seen = Int.Hash_set.create () in
      List.filter (captured @ current) ~f:(fun lease ->
        if (not lease.active) || Hash_set.mem seen lease.owner.id
        then false
        else (
          Hash_set.add seen lease.owner.id;
          true)))
  in
  bind mode combined f
;;

let inherit_context f =
  let captured = capture_context () in
  fun () -> with_context captured f
;;

let without_context f =
  let mode, _ = current_ancestry () in
  bind mode [] f
;;

(* All graph/lease reads and writes happen under [graph_mutex]. The critical
   sections never perform effects, so a contended graph cannot block an actor
   while waiting for handler work. The per-owner mutex is acquired outside it. *)
let would_cycle target sources =
  let adjacency = Int.Table.create () in
  List.iter !requests ~f:(fun request ->
    List.iter request.sources ~f:(fun source ->
      if source.active
      then Hashtbl.add_multi adjacency ~key:source.owner.id ~data:request.target.id));
  let ancestors =
    Int.Hash_set.of_list (List.map sources ~f:(fun source -> source.owner.id))
  in
  let seen = Int.Hash_set.create () in
  let rec visit = function
    | [] -> false
    | id :: rest ->
      if Hash_set.mem ancestors id
      then true
      else if Hash_set.mem seen id
      then visit rest
      else (
        Hash_set.add seen id;
        visit (Option.value (Hashtbl.find adjacency id) ~default:[] @ rest))
  in
  visit [ target.id ]
;;

let admit target inherited =
  locked (fun () ->
    let sources = List.filter inherited ~f:(fun source -> source.active) in
    if List.exists sources ~f:(fun source -> Int.equal source.owner.id target.id)
    then Error Reentrant
    else if List.length sources >= 64 || List.length !requests >= 4096
    then Error Resource_limit
    else if would_cycle target sources
    then Error Wait_cycle
    else (
      let request = { target; sources } in
      requests := request :: !requests;
      Ok request))
;;

let remove request =
  requests := List.filter !requests ~f:(fun current -> not (phys_equal current request))
;;

let with_access target f =
  let mode, inherited = current_ancestry () in
  match admit target inherited with
  | Error _ as failure -> failure
  | Ok request ->
    Exn.protect
      ~finally:(fun () -> locked (fun () -> remove request))
      ~f:(fun () ->
        Eio.Mutex.use_ro target.mutex (fun () ->
          let lease = { owner = target; active = true } in
          Exn.protect
            ~finally:(fun () ->
              locked (fun () ->
                lease.active <- false;
                remove request))
            ~f:(fun () -> Ok (bind mode (lease :: request.sources) f))))
;;
