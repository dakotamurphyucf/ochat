open Core

type mode =
  | Shared
  | Exclusive
[@@deriving compare, equal, sexp]

type holder =
  | Session of Agent_protocol.Id.Session.t
  | Developer of Agent_protocol.Execution_owner.developer

type lease =
  { conflict_domain : string
  ; holder : holder
  ; mode : mode
  ; nonce : int
  ; issuer : unit ref
  }

type holders =
  { shared : lease list
  ; exclusive : lease option
  }

type t =
  { mutex : Eio.Mutex.t
  ; mutable domains : holders String.Map.t
  ; mutable next_nonce : int
  ; issuer : unit ref
  }

let create () =
  { mutex = Eio.Mutex.create ()
  ; domains = String.Map.empty
  ; next_nonce = 0
  ; issuer = ref ()
  }
;;

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

let acquire_holder t ~conflict_domain ~holder ~mode =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    let holders = Map.find t.domains conflict_domain |> Option.value ~default:empty in
    if not (can_acquire holders mode)
    then conflict conflict_domain
    else (
      let lease =
        { conflict_domain; holder; mode; nonce = t.next_nonce; issuer = t.issuer }
      in
      t.next_nonce <- t.next_nonce + 1;
      t.domains <- Map.set t.domains ~key:conflict_domain ~data:(add holders lease);
      Ok lease))
;;

let acquire t ~conflict_domain ~session_id ~mode =
  acquire_holder t ~conflict_domain ~holder:(Session session_id) ~mode
;;

let acquire_developer t ~conflict_domain ~owner ~mode =
  acquire_holder t ~conflict_domain ~holder:(Developer owner) ~mode
;;

let same_lease (left : lease) (right : lease) =
  phys_equal left.issuer right.issuer && Int.equal left.nonce right.nonce
;;

let remove holders (lease : lease) =
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

let release t (lease : lease) =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    match
      if phys_equal t.issuer lease.issuer
      then Map.find t.domains lease.conflict_domain
      else None
    with
    | None -> ()
    | Some holders ->
      let holders = remove holders lease in
      if List.is_empty holders.shared && Option.is_none holders.exclusive
      then t.domains <- Map.remove t.domains lease.conflict_domain
      else t.domains <- Map.set t.domains ~key:lease.conflict_domain ~data:holders)
;;

let lease_present t (lease : lease) =
  phys_equal t.issuer lease.issuer
  &&
  match Map.find t.domains lease.conflict_domain with
  | None -> false
  | Some holders ->
    List.exists holders.shared ~f:(same_lease lease)
    || Option.exists holders.exclusive ~f:(same_lease lease)
;;

let is_active t (lease : lease) =
  Eio.Mutex.use_ro t.mutex (fun () -> lease_present t lease)
;;

let active_other_than t (lease : lease) ~conflict_domain =
  Eio.Mutex.use_ro t.mutex (fun () ->
    if
      (not (lease_present t lease))
      || not (String.equal conflict_domain lease.conflict_domain)
    then true
    else (
      match Map.find t.domains conflict_domain with
      | None -> true
      | Some holders ->
        List.exists holders.shared ~f:(fun other -> not (same_lease lease other))
        || Option.exists holders.exclusive ~f:(fun other -> not (same_lease lease other))))
;;

let active t ~conflict_domain =
  Eio.Mutex.use_ro t.mutex (fun () ->
    match Map.find t.domains conflict_domain with
    | None -> 0
    | Some holders ->
      List.length holders.shared
      + Option.value_map holders.exclusive ~default:0 ~f:(fun _ -> 1))
;;
