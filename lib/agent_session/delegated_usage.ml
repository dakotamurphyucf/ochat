open! Core
module P = Agent_protocol
module D = Document_schema
module C = D.Extension_carrier
module X = Persistence_codec
module S = Delegated_usage_state

let domain result =
  Result.map_error result ~f:(fun error ->
    P.Error.invalid_request (Sexp.to_string_hum (D.Error.sexp_of_t error)))
;;

let limits =
  D.Limits.create ~max_bytes:(256 * 1024) ~max_depth:64 ~max_fields:16384 ~max_nodes:32768
  |> domain
  |> Result.map_error ~f:(fun error -> error.P.Error.message)
  |> Result.ok_or_failwith
;;

let codec =
  X.codec_exn
    ~version:1
    ~limits
    ~kind:"ochat.delegated_usage"
    ~shape:S.shape
    ~decode:S.of_json
    ~encode:(fun state -> Ok (S.to_json state))
;;

type t = S.t C.t

let reference t = S.reference (C.value t)
let validate t ~ledger = S.validate (C.value t) ~ledger
let usage t ~ledger = S.usage (C.value t) ~ledger

(* This codec probes only carrier preservation and maximum known growth. Its
   values are never decoded as accounting evidence or published as a partition. *)
let probe_codec =
  X.codec_exn
    ~version:1
    ~limits
    ~kind:"ochat.delegated_usage"
    ~shape:S.shape
    ~decode:(fun raw -> Ok raw)
    ~encode:(fun raw -> Ok raw)
;;

let probe_document t document =
  let open Result.Let_syntax in
  let%bind carrier = D.Domain_codec.decode probe_codec document in
  D.Domain_codec.encode
    probe_codec
    (C.with_value carrier (S.capacity_projection (C.value t)))
;;

let probe_raw t =
  let open Result.Let_syntax in
  let%bind document = D.Domain_codec.encode codec t in
  probe_document t document
;;

let reserved_growth_of_document t current =
  let open Result.Let_syntax in
  let%bind future = probe_document t current |> domain in
  let%bind before = Capacity_growth.measure (D.Document.json current) ~limits in
  let%map after = Capacity_growth.measure (D.Document.json future) ~limits in
  Capacity_growth.positive_difference after ~previous:before
;;

let reserved_growth t =
  let open Result.Let_syntax in
  let%bind document = D.Domain_codec.encode codec t |> domain in
  reserved_growth_of_document t document
;;

let validate_capacity_of_document t document =
  let open Result.Let_syntax in
  let%bind growth = reserved_growth_of_document t document in
  let%bind remaining = Capacity_growth.remaining_limits growth limits in
  D.Document.validate document ~limits:remaining |> domain
;;

let validate_capacity t =
  let open Result.Let_syntax in
  let%bind document = D.Domain_codec.encode codec t |> domain in
  validate_capacity_of_document t document
;;

let of_document document =
  let open Result.Let_syntax in
  let%bind t = D.Domain_codec.decode codec document |> domain in
  let%map () = validate_capacity t in
  t
;;

let to_document t =
  let open Result.Let_syntax in
  let%bind document = D.Domain_codec.encode codec t |> domain in
  let%map () = validate_capacity_of_document t document in
  document
;;

let create ~reference ~ledger =
  let open Result.Let_syntax in
  let%bind state = S.create ~reference ~ledger in
  let t = C.of_authored_value state in
  let%map () = validate_capacity t in
  t
;;

let reconcile t ~before ~after =
  let open Result.Let_syntax in
  let%bind candidate = S.reconcile (C.value t) ~before ~after in
  if S.equal candidate (C.value t)
  then Ok t
  else (
    let t = C.with_value t candidate in
    let%map () = validate_capacity t in
    t)
;;

let adopt previous ~incoming =
  let open Result.Let_syntax in
  let%bind () = S.validate_update (C.value previous) ~incoming:(C.value incoming) in
  let%bind candidate = D.Domain_codec.adopt codec ~previous ~incoming |> domain in
  let%map () = validate_capacity candidate in
  candidate
;;

let validate_transition previous ~incoming ~before ~after =
  let open Result.Let_syntax in
  let%bind () =
    S.validate_transition (C.value previous) ~incoming:(C.value incoming) ~before ~after
  in
  let%bind merged = adopt previous ~incoming in
  let%bind merged = to_document merged in
  let%bind actual = to_document incoming in
  if D.Json.equal (D.Document.json merged) (D.Document.json actual)
  then Ok ()
  else Error (P.Error.invalid_request "delegated usage dropped preserved fields")
;;

let can_retire t =
  match probe_raw t with
  | Ok _ -> Ok true
  | Error (D.Error.Extension_conflict _) -> Ok false
  | Error error -> domain (Error error)
;;
