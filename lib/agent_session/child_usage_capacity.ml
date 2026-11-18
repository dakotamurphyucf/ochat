open! Core
module P = Agent_protocol
module X = Persistence_codec
module O = Agent_store.Delegation_goal_origin
module Snapshot = Child_usage_snapshot
module Goal_scope = Snapshot.Goal_scope
module Pending = Goal_descendant_pending
module Included = Goal_descendant_included

type t =
  { extra : Capacity_growth.t
  ; required_depth : int
  }

let extra t = t.extra
let required_depth t = t.required_depth
let invalid message = Error (P.Error.invalid_request message)
let scoped_limits = Goal_scope.capacity_limits

let current_entry snapshot scope =
  Option.bind snapshot ~f:(fun snapshot ->
    Option.bind (Snapshot.goal_scopes snapshot) ~f:(fun entries ->
      List.find entries ~f:(fun entry -> O.Scope.equal scope (Goal_scope.scope entry))))
;;

let current_included snapshot scope =
  Option.bind (current_entry snapshot scope) ~f:Goal_scope.included
;;

let projection ~reference ~origin ~snapshot =
  match Snapshot.capacity_projection ~reference with
  | `Object fields ->
    `Object
      (fields
       @ [ ( "goal_scopes"
           , `Array
               (List.map (O.scopes origin) ~f:(fun scope ->
                  Goal_scope.capacity_projection
                    ~scope
                    ~included:(current_included snapshot scope)
                    ~maximum_attempt:false)) )
         ])
  | `Null | `False | `True | `String _ | `Number _ | `Array _ -> assert false
;;

let scope_probe_limits =
  X.limits ~max_bytes:(64 * 1024 * 1024)
  |> Result.map_error ~f:(fun error ->
    Sexp.to_string_hum (Document_schema.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let scope_probe_kind = "ochat.child_scope_capacity"

let scope_probe_codec =
  X.codec_exn
    ~version:1
    ~limits:scope_probe_limits
    ~kind:scope_probe_kind
    ~shape:Goal_scope.shape
    ~decode:(fun raw -> Ok raw)
    ~encode:(fun raw -> Ok raw)
;;

let domain result =
  Result.map_error result ~f:(fun error ->
    P.Error.invalid_request (Sexp.to_string_hum (Document_schema.Error.sexp_of_t error)))
;;

let validate_scoped_raw raw =
  Document_schema.Json.validate ~limits:scoped_limits raw |> domain
;;

let capture ~origin ~snapshot ~raw_snapshot =
  let open Result.Let_syntax in
  let%bind () =
    match snapshot with
    | None -> Ok ()
    | Some snapshot -> Snapshot.validate_origin snapshot ~origin
  in
  let%bind raw_entries =
    match snapshot, raw_snapshot with
    | None, None -> Ok []
    | None, Some _ -> invalid "raw scoped capacity snapshot has no typed owner"
    | Some snapshot, raw ->
      let raw = Option.value raw ~default:(Snapshot.to_json snapshot) in
      let%bind decoded = Snapshot.of_json raw in
      let%bind () =
        if String.equal (Snapshot.digest decoded) (Snapshot.digest snapshot)
        then Ok ()
        else invalid "raw scoped capacity snapshot differs from its typed owner"
      in
      let%bind fields = X.object_ raw in
      X.required fields "goal_scopes" (fun raw ->
        let%bind () = validate_scoped_raw raw in
        X.list
          (fun raw ->
             let%bind fields = X.object_ raw in
             let%map id = X.required fields "scope_id" P.Json_codec.string in
             id, raw)
          raw)
  in
  let%bind raw_entries =
    match String.Map.of_alist raw_entries with
    | `Ok entries -> Ok entries
    | `Duplicate_key _ -> invalid "repeated raw scoped capacity identity"
  in
  let%bind reversed, extra, required_depth =
    List.fold_result
      (O.scopes origin)
      ~init:([], Capacity_growth.zero, 1)
      ~f:(fun (baselines, extra, required_depth) scope ->
        let included = current_included snapshot scope in
        let baseline =
          Goal_scope.capacity_projection ~scope ~included ~maximum_attempt:false
        in
        let raw =
          Map.find raw_entries (O.Scope.root_id scope)
          |> Option.value
               ~default:
                 (Option.value_map
                    (current_entry snapshot scope)
                    ~default:baseline
                    ~f:Goal_scope.to_json)
        in
        let%bind document =
          Document_schema.Document.create
            ~limits:scope_probe_limits
            ~kind:scope_probe_kind
            ~version:1
            ~payload:raw
          |> domain
        in
        let%bind carrier =
          Document_schema.Domain_codec.decode scope_probe_codec document |> domain
        in
        let encode value =
          Document_schema.Domain_codec.encode
            scope_probe_codec
            (Document_schema.Extension_carrier.with_value carrier value)
        in
        let%bind baseline_document = encode baseline |> domain in
        let baseline = Document_schema.Document.payload baseline_document in
        let%bind measured_baseline =
          Capacity_growth.measure baseline ~limits:scoped_limits
        in
        let%bind baseline_depth = Capacity_depth.measure baseline ~limits:scoped_limits in
        let alternative =
          match included with
          | None -> None
          | Some included ->
            (match
               Goal_descendant_occurrence.kind
                 (Pending.occurrence (Included.pending included))
             with
             | Attempt { ordinal = _; operation_id = _; invocation_id = _ } -> None
             | Host_turn { operation_id = _ } ->
               Some
                 (Goal_scope.capacity_projection
                    ~scope
                    ~included:(Some included)
                    ~maximum_attempt:true))
        in
        let%bind additional, local_depth =
          match alternative with
          | None -> Ok (Capacity_growth.zero, baseline_depth)
          | Some alternative ->
            (match encode alternative with
             | Error (Document_schema.Error.Extension_conflict _) ->
               Ok (Capacity_growth.zero, baseline_depth)
             | Error error -> domain (Error error)
             | Ok alternative ->
               let raw = Document_schema.Document.payload alternative in
               let%bind measured = Capacity_growth.measure raw ~limits:scoped_limits in
               let%map depth = Capacity_depth.measure raw ~limits:scoped_limits in
               ( Capacity_growth.positive_difference measured ~previous:measured_baseline
               , Int.max baseline_depth depth ))
        in
        let%map extra = Capacity_growth.add extra additional in
        (* Entry is an actual child of the goal_scopes array. *)
        baseline :: baselines, extra, Int.max required_depth (1 + local_depth))
  in
  let%bind remaining = Capacity_growth.remaining_limits extra scoped_limits in
  let%bind () =
    Document_schema.Json.validate ~limits:remaining (`Array (List.rev reversed)) |> domain
  in
  let%map () =
    Capacity_depth.validate required_depth ~ancestors:0 ~limits:scoped_limits
  in
  { extra; required_depth }
;;

let extra_growth ~origin ~snapshot ~raw_snapshot =
  let%map.Result capacity = capture ~origin ~snapshot ~raw_snapshot in
  capacity.extra
;;
