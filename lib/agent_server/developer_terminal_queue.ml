open! Core

module Error = struct
  type t =
    | Closed
    | Limit_exceeded
    | Busy
    | Stale
  [@@deriving equal, sexp_of]
end

type kind =
  | Root
  | Native
[@@deriving equal, sexp_of]

type witness = unit ref

type 'intent t =
  { domain_id : int
  ; max_native_occupancy : int
  ; completed : 'intent callback Queue.t
  ; mutable draining : 'intent drain option
  ; mutable native_occupancy : int
  ; mutable root_occupied : bool
  ; mutable closed : bool
  ; mutable effect_witness : witness option
  }

and 'intent callback =
  { issuer : 'intent t
  ; kind : kind
  ; mutable state : 'intent callback_state
  }

and 'intent callback_state =
  | Active
  | Completed of 'intent
  | Draining of 'intent
  | Retained of 'intent
  | Settled
  | Abandoned

and 'intent drain =
  { issuer : 'intent t
  ; callback : 'intent callback
  ; payload : 'intent
  ; mutable live : bool
  }

let same_domain t = Int.equal t.domain_id (Stdlib.Domain.self () :> int)
let stale = Error Error.Stale

let create ~max_native_occupancy =
  if max_native_occupancy <= 0 || max_native_occupancy > 16384
  then Error Error.Limit_exceeded
  else
    Ok
      { domain_id = (Stdlib.Domain.self () :> int)
      ; max_native_occupancy
      ; completed = Queue.create ()
      ; draining = None
      ; native_occupancy = 0
      ; root_occupied = false
      ; closed = false
      ; effect_witness = Some (ref ())
      }
;;

let reserve t ~kind =
  if not (same_domain t)
  then stale
  else if t.closed
  then Error Error.Closed
  else if (not (Queue.is_empty t.completed)) || Option.is_some t.draining
  then Error Error.Busy
  else (
    match kind with
    | Root when t.root_occupied -> Error Error.Busy
    | Native when t.native_occupancy >= t.max_native_occupancy ->
      Error Error.Limit_exceeded
    | Root ->
      t.root_occupied <- true;
      Ok { issuer = t; kind; state = Active }
    | Native ->
      t.native_occupancy <- t.native_occupancy + 1;
      Ok { issuer = t; kind; state = Active })
;;

let release_occupancy t kind =
  match kind with
  | Root -> t.root_occupied <- false
  | Native -> t.native_occupancy <- t.native_occupancy - 1
;;

let valid_callback t callback = same_domain t && phys_equal callback.issuer t

let abandon t callback =
  if not (valid_callback t callback)
  then stale
  else (
    match callback.state with
    | Active ->
      callback.state <- Abandoned;
      release_occupancy t callback.kind;
      Ok ()
    | Completed _ | Draining _ | Retained _ | Settled | Abandoned -> stale)
;;

let complete t callback ~intent =
  if not (valid_callback t callback)
  then stale
  else (
    match callback.state with
    | Active ->
      callback.state <- Completed intent;
      Queue.enqueue t.completed callback;
      t.effect_witness <- None;
      Ok ()
    | Completed _ | Draining _ | Retained _ | Settled | Abandoned -> stale)
;;

let effect_witness t =
  if
    same_domain t
    && (not t.closed)
    && Queue.is_empty t.completed
    && Option.is_none t.draining
  then t.effect_witness
  else None
;;

let check_effect_witness t witness =
  Option.value_map (effect_witness t) ~default:false ~f:(phys_equal witness)
;;

let take_head t =
  if not (same_domain t)
  then stale
  else if Option.is_some t.draining
  then Error Error.Busy
  else (
    match Queue.peek t.completed with
    | None -> Ok None
    | Some callback ->
      (match callback.state with
       | Completed payload | Retained payload ->
         callback.state <- Draining payload;
         let drain = { issuer = t; callback; payload; live = true } in
         t.draining <- Some drain;
         Ok (Some drain)
       | Active | Draining _ | Settled | Abandoned -> stale))
;;

let payload drain = drain.payload

let same_head (a : _ drain) (b : _ drain) =
  phys_equal a.issuer b.issuer && phys_equal a.callback b.callback
;;

let valid_drain t (drain : _ drain) =
  same_domain t
  && phys_equal drain.issuer t
  && drain.live
  && Option.value_map t.draining ~default:false ~f:(phys_equal drain)
  && Option.value_map (Queue.peek t.completed) ~default:false ~f:(fun callback ->
    phys_equal callback drain.callback)
;;

let retain t drain =
  if not (valid_drain t drain)
  then stale
  else (
    drain.callback.state <- Retained drain.payload;
    drain.live <- false;
    t.draining <- None;
    Ok ())
;;

let settle t drain =
  if not (valid_drain t drain)
  then stale
  else (
    match Queue.dequeue t.completed with
    | None -> stale
    | Some callback ->
      callback.state <- Settled;
      release_occupancy t callback.kind;
      drain.live <- false;
      t.draining <- None;
      if Queue.is_empty t.completed && not t.closed then t.effect_witness <- Some (ref ());
      Ok ())
;;

let begin_close t =
  if not (same_domain t)
  then stale
  else (
    t.closed <- true;
    t.effect_witness <- None;
    Ok ())
;;

let has_native_occupancy t = t.native_occupancy > 0

let is_empty t =
  (not t.root_occupied)
  && Int.equal t.native_occupancy 0
  && Queue.is_empty t.completed
  && Option.is_none t.draining
;;
