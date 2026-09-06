open Core

type mode =
  | Shared
  | Exclusive
[@@deriving compare, equal, sexp]

type lease =
  { conflict_domain : string
  ; session_id : Agent_protocol.Id.Session.t
  ; mode : mode
  ; nonce : int
  }

type holders =
  { shared : lease list
  ; exclusive : lease option
  }

type t =
  { mutex : Eio.Mutex.t
  ; mutable domains : (string, holders) Map.Poly.t
  ; mutable next_nonce : int
  }

let create () = { mutex = Eio.Mutex.create (); domains = Map.Poly.empty; next_nonce = 0 }
let empty = { shared = []; exclusive = None }

let conflict domain =
  Error
    (Agent_protocol.Error.create
       Conflict
       ~message:("workspace conflict domain is already leased: " ^ domain)
       ~retryable:true
       ())
;;

let can_acquire holders = function
  | Shared -> Option.is_none holders.exclusive
  | Exclusive -> Option.is_none holders.exclusive && List.is_empty holders.shared
;;

let add holders lease =
  match lease.mode with
  | Shared -> { holders with shared = lease :: holders.shared }
  | Exclusive -> { holders with exclusive = Some lease }
;;

let acquire t ~conflict_domain ~session_id ~mode =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    let holders = Map.find t.domains conflict_domain |> Option.value ~default:empty in
    if not (can_acquire holders mode)
    then conflict conflict_domain
    else (
      let lease = { conflict_domain; session_id; mode; nonce = t.next_nonce } in
      t.next_nonce <- t.next_nonce + 1;
      t.domains <- Map.set t.domains ~key:conflict_domain ~data:(add holders lease);
      Ok lease))
;;

let same_lease left right = left.nonce = right.nonce

let remove holders lease =
  match lease.mode with
  | Shared ->
    { holders with
      shared = List.filter holders.shared ~f:(fun value -> not (same_lease value lease))
    }
  | Exclusive ->
    { holders with
      exclusive =
        Option.filter holders.exclusive ~f:(fun value -> not (same_lease value lease))
    }
;;

let release t lease =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    match Map.find t.domains lease.conflict_domain with
    | None -> ()
    | Some holders ->
      let holders = remove holders lease in
      if List.is_empty holders.shared && Option.is_none holders.exclusive
      then t.domains <- Map.remove t.domains lease.conflict_domain
      else t.domains <- Map.set t.domains ~key:lease.conflict_domain ~data:holders)
;;

let active t ~conflict_domain =
  Eio.Mutex.use_ro t.mutex (fun () ->
    match Map.find t.domains conflict_domain with
    | None -> 0
    | Some holders ->
      List.length holders.shared
      + Option.value_map holders.exclusive ~default:0 ~f:(fun _ -> 1))
;;
