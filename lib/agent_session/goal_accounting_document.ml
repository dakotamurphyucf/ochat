open! Core
module P = Agent_protocol
module D = Document_schema
module C = D.Extension_carrier
module X = Persistence_codec
module A = Goal_accounting
module S = A.Stored

let domain result =
  Result.map_error result ~f:(fun e ->
    P.Error.invalid_request (Sexp.to_string_hum (D.Error.sexp_of_t e)))
;;

let accounting result =
  Result.map_error result ~f:(fun e ->
    P.Error.invalid_request (Sexp.to_string_hum (A.Error.sexp_of_t e)))
;;

let limits =
  D.Limits.create ~max_bytes:(256 * 1024) ~max_depth:64 ~max_fields:16384 ~max_nodes:32768
  |> domain
  |> Result.map_error ~f:(fun e -> e.P.Error.message)
  |> Result.ok_or_failwith
;;

let codec =
  X.codec_exn
    ~version:1
    ~limits
    ~kind:"ochat.goal_accounting"
    ~shape:S.shape
    ~decode:S.of_json
    ~encode:(fun s -> Ok (S.to_json s))
;;

type t = S.t C.t

let stored = C.value
let resolve t ~goal ~ledger = S.resolve (stored t) ~goal ~ledger |> accounting

(* The identity codec is only a preservation/capacity probe. It never restores
   accounting, allocates a Handle, or admits these maximal numbers as evidence. *)
let probe_codec =
  X.codec_exn
    ~version:1
    ~limits
    ~kind:"ochat.goal_accounting"
    ~shape:S.shape
    ~decode:(fun j -> Ok j)
    ~encode:(fun j -> Ok j)
;;

let probe_from_current t current ~ordinal =
  let open Result.Let_syntax in
  let%bind carrier = D.Domain_codec.decode probe_codec current in
  let raw = S.retirement_projection (stored t) ~ordinal in
  D.Domain_codec.encode probe_codec (C.with_value carrier raw)
;;

let probe_raw t ~ordinal =
  let open Result.Let_syntax in
  let%bind current = D.Domain_codec.encode codec t in
  probe_from_current t current ~ordinal
;;

let growth_from_current t current =
  if List.is_empty (S.ordinals (stored t))
  then Ok Capacity_growth.zero
  else
    let open Result.Let_syntax in
    let%bind future = probe_from_current t current ~ordinal:None |> domain in
    let%bind before = Capacity_growth.measure (D.Document.json current) ~limits in
    let%map after = Capacity_growth.measure (D.Document.json future) ~limits in
    Capacity_growth.positive_difference after ~previous:before
;;

let reserved_growth t =
  if List.is_empty (S.ordinals (stored t))
  then Ok Capacity_growth.zero
  else
    let open Result.Let_syntax in
    let%bind current = D.Domain_codec.encode codec t |> domain in
    growth_from_current t current
;;

let validate_capacity_with_growth document growth =
  let open Result.Let_syntax in
  let%bind remaining = Capacity_growth.remaining_limits growth limits in
  D.Document.validate document ~limits:remaining |> domain
;;

let validate_capacity t =
  let open Result.Let_syntax in
  let%bind document = D.Domain_codec.encode codec t |> domain in
  let%bind growth = growth_from_current t document in
  validate_capacity_with_growth document growth
;;

module Export = struct
  type source = t

  type t =
    { document : D.Document.t
    ; growth : Capacity_growth.t
    }

  let prepare source =
    let open Result.Let_syntax in
    let%bind document = D.Domain_codec.encode codec source |> domain in
    let%bind growth = growth_from_current source document in
    let%map () = validate_capacity_with_growth document growth in
    { document; growth }
  ;;

  let document t = t.document
  let reserved_growth t = t.growth
end

let of_document document =
  let open Result.Let_syntax in
  let%bind t = D.Domain_codec.decode codec document |> domain in
  let%map () = validate_capacity t in
  t
;;

let to_document t = Result.map (Export.prepare t) ~f:Export.document

let create goal ~ledger =
  let open Result.Let_syntax in
  let%bind () =
    match P.Goal.state goal, P.Goal.episode goal with
    | Open (Held _), None ->
      if
        Option.is_none (P.Goal.achievement_request goal)
        && Option.is_none (P.Goal.request_settlement goal)
        && Option.is_none (P.Goal.previous_episode_settlement goal)
      then Ok ()
      else
        Error
          (P.Error.invalid_request
             "historical goal proof cannot bootstrap zero accounting")
    | Open Enabled, _ | Closed _, _ | Open (Held _), Some _ ->
      Error
        (P.Error.invalid_request
           "only a held goal before its first episode may bootstrap accounting")
  in
  let%bind account = A.create goal ~ledger |> accounting in
  let t = C.of_authored_value (S.capture account) in
  let%map () = validate_capacity t in
  t
;;

let apply t plan =
  let open Result.Let_syntax in
  let before = S.capture (A.Plan.before_accounting plan) in
  let incoming = S.capture (A.Plan.candidate plan) in
  let%bind () =
    if S.equal before (stored t)
    then Ok ()
    else Error (P.Error.invalid_request "accounting plan does not match stored frontier")
  in
  let%bind () = S.validate_update (stored t) ~incoming in
  let candidate = C.with_value t incoming in
  let%map () = validate_capacity candidate in
  candidate
;;

let adopt previous ~incoming =
  let open Result.Let_syntax in
  let%bind () = S.validate_update (stored previous) ~incoming:(stored incoming) in
  let%bind candidate = D.Domain_codec.adopt codec ~previous ~incoming |> domain in
  let%map () = validate_capacity candidate in
  candidate
;;

let can_retire t ~ordinal =
  if not (List.mem (S.ordinals (stored t)) ordinal ~equal:Int64.equal)
  then Ok true
  else (
    match probe_raw t ~ordinal:(Some ordinal) with
    | Ok _ -> Ok true
    | Error (D.Error.Extension_conflict _) -> Ok false
    | Error e -> domain (Error e))
;;
