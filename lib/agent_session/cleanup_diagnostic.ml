open! Core
module Failure = Callback_cleanup.Failure

type t =
  { payload : Failure.t list ref
  ; delivered : bool ref
  ; domain : int
  }

(* Contents never change. A mutable carrier guarantees actual alias identity even
   when independent producers share the exact same immutable failure payload. *)
let create failures =
  { payload = ref failures
  ; delivered = ref false
  ; domain = (Stdlib.Domain.self () :> int)
  }
;;

let equal (t : t) (other : t) = phys_equal t.payload other.payload
let failures t = !(t.payload)

let take_legacy_diagnostic_exn ?primary diagnostics =
  let diagnostics =
    List.fold diagnostics ~init:[] ~f:(fun reversed diagnostic ->
      if List.mem reversed diagnostic ~equal then reversed else diagnostic :: reversed)
    |> List.rev
  in
  if Option.exists primary ~f:(fun primary -> not (List.mem diagnostics primary ~equal))
  then invalid_arg "legacy cleanup primary is not an actual report group";
  List.iter diagnostics ~f:(fun t ->
    if not (Int.equal t.domain (Stdlib.Domain.self () :> int))
    then invalid_arg "legacy disposal delivery belongs to another domain");
  let pending t = not !(t.delivered) in
  let preferred =
    Option.bind primary ~f:(fun t ->
      if pending t
      then Option.map (List.hd (failures t)) ~f:(fun failure -> t, failure)
      else None)
  in
  let first =
    List.find_map diagnostics ~f:(fun t ->
      if pending t
      then Option.map (List.hd (failures t)) ~f:(fun failure -> t, failure)
      else None)
  in
  List.iter diagnostics ~f:(fun t -> t.delivered := true);
  Option.first_some preferred first
;;
