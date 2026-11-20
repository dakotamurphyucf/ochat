open! Core
module Payload = History_entry.Payload
module Json = Document_schema.Json

module Id = struct
  include History_entry.Id
  include Comparator.Make (History_entry.Id)
end

type error =
  | Unavailable
  | Invalid_output
  | Limit_exceeded
  | Expired
[@@deriving equal, sexp_of]

exception Rejected of error

type state =
  | Pending
  | Preparing
  | Retired

type t =
  { call : History_entry.t
  ; output : History_entry.t
  ; materialize : unit -> (Payload.Output.t, error) result
  ; refresh : scope:Transcript.Scope.t -> unit -> bool
  ; state : state Atomic.t
  }

let create ~call ~output ~materialize ~refresh =
  { call; output; materialize; refresh; state = Atomic.make Pending }
;;

let retire t = Atomic.set t.state Retired
let expire_pending t = ignore (Atomic.compare_and_set t.state Pending Retired : bool)
let max_outputs_per_submission = 16

let preparing t =
  match Atomic.get t.state with
  | Preparing -> true
  | Pending | Retired -> false
;;

module Materialized = struct
  type pending = t

  type t =
    { sources : pending list
    ; mutable contents : (Id.t, Payload.Output.t, Id.comparator_witness) Map.t
    ; started : bool Atomic.t
    ; submitted : bool Atomic.t
    }

  let max_parts_per_output = 16
  let max_bytes = 1024 * 1024

  let limits =
    match
      Document_schema.Limits.create
        ~max_bytes
        ~max_depth:64
        ~max_fields:100_000
        ~max_nodes:100_000
    with
    | Ok limits -> limits
    | Error _ -> assert false
  ;;

  let validated_output entry =
    let semantic = Payload.semantic (History_entry.payload entry) in
    match
      Payload.Semantic.view semantic, (Payload.Semantic.metadata semantic).call_id
    with
    | Result { relation = Bound call; kind; _ }, Value call_id
      when not (String.is_empty call_id) -> Ok (call, kind, call_id)
    | _ -> Error Invalid_output
  ;;

  let call_alias entry =
    let semantic = Payload.semantic (History_entry.payload entry) in
    Payload.Semantic.view semantic, (Payload.Semantic.metadata semantic).call_id
  ;;

  let same_payload a b =
    Json.equal
      (Payload.to_json (History_entry.payload a))
      (Payload.to_json (History_entry.payload b))
  ;;

  type alias_owner =
    | Ordinary of Id.t
    | Ambiguous
    | Protected of Id.t

  let other_occurrence ~aliases active entry =
    match call_alias entry with
    | ((Call _ | Result _) as view), Value alias when Set.mem aliases alias ->
      let owner = Map.find active alias in
      (match view, owner with
       | _, Some (Protected _) -> Error Invalid_output
       | Call _, None ->
         Ok (Map.set active ~key:alias ~data:(Ordinary (History_entry.id entry)))
       | Call _, Some (Ordinary _ | Ambiguous) ->
         Ok (Map.set active ~key:alias ~data:Ambiguous)
       | Result { relation = Bound call; _ }, Some (Ordinary current)
         when Id.equal call current -> Ok (Map.remove active alias)
       | _ -> Ok active)
    | _ -> Ok active
  ;;

  let validate sources history =
    let open Result.Let_syntax in
    let%bind outputs, calls, aliases =
      List.fold_result
        sources
        ~init:(Map.empty (module Id), Map.empty (module Id), String.Set.empty)
        ~f:(fun (outputs, calls, aliases) source ->
          let output = History_entry.id source.output in
          let call = History_entry.id source.call in
          let%bind bound_call, kind, alias = validated_output source.output in
          match call_alias source.call with
          | Call { kind = call_kind; _ }, Value call_alias
            when Id.equal call bound_call
                 && (not (Id.equal call output))
                 && Payload.Call_kind.equal kind call_kind
                 && String.equal alias call_alias
                 && (not (Map.mem outputs output || Map.mem outputs call))
                 && not (Map.mem calls output || Map.mem calls call) ->
            Ok
              ( Map.set outputs ~key:output ~data:(source, alias)
              , Map.set calls ~key:call ~data:(source.call, alias)
              , Set.add aliases alias )
          | _ -> Error Invalid_output)
    in
    let%bind seen, _ =
      List.fold_result
        history
        ~init:(Set.empty (module Id), String.Map.empty)
        ~f:(fun (seen, active) entry ->
          let id = History_entry.id entry in
          match Map.find outputs id, Map.find calls id with
          | None, None ->
            let%map active = other_occurrence ~aliases active entry in
            seen, active
          | None, Some (committed, alias)
            when (not (Set.mem seen id || Map.mem active alias))
                 && same_payload committed entry ->
            Ok (Set.add seen id, Map.set active ~key:alias ~data:(Protected id))
          | Some (source, alias), None
            when (not (Set.mem seen id)) && same_payload source.output entry ->
            (match Map.find active alias with
             | Some (Protected active_call)
               when Id.equal active_call (History_entry.id source.call) ->
               Ok (Set.add seen id, Map.remove active alias)
             | _ -> Error Invalid_output)
          | _ -> Error Invalid_output)
    in
    if Set.length seen = Map.length outputs + Map.length calls
    then Ok ()
    else Error Invalid_output
  ;;

  let materialize sources =
    let open Result.Let_syntax in
    let%map _, contents =
      List.fold_result
        sources
        ~init:(max_bytes, Map.empty (module Id))
        ~f:(fun (remaining, contents) source ->
          if not (Atomic.compare_and_set source.state Pending Preparing)
          then Error Expired
          else (
            let%bind value = source.materialize () in
            match value with
            | Payload.Output.Content parts
              when Stdlib.List.compare_length_with parts max_parts_per_output > 0 ->
              Error Limit_exceeded
            | _ ->
              let%bind bytes =
                Json.validate_and_measure ~limits (Payload.Output.to_json value)
                |> Result.map_error ~f:(function
                  | Limit_exceeded _ -> Limit_exceeded
                  | _ -> Invalid_output)
              in
              if bytes > remaining
              then Error Limit_exceeded
              else if not (preparing source)
              then Error Expired
              else
                Ok
                  ( remaining - bytes
                  , Map.set contents ~key:(History_entry.id source.output) ~data:value )))
    in
    { sources; contents; started = Atomic.make false; submitted = Atomic.make false }
  ;;

  let prepare sources ~history =
    let prepare () =
      let open Result.Let_syntax in
      if Stdlib.List.compare_length_with sources max_outputs_per_submission > 0
      then Error Limit_exceeded
      else (
        let%bind () = if List.is_empty sources then Ok () else validate sources history in
        materialize sources)
    in
    match prepare () with
    | Ok _ as result -> result
    | Error _ as result ->
      List.iter sources ~f:expire_pending;
      result
  ;;

  let find t id = Map.find t.contents id
  let has_live_results t = not (List.is_empty t.sources)
  let release_contents t = t.contents <- Map.empty (module Id)

  let bind t ~scope =
    if
      (not (List.for_all t.sources ~f:preparing))
      || not (Atomic.compare_and_set t.started false true)
    then Error Expired
    else (
      match scope.Transcript.Scope.relation with
      | Nested _ when not (List.is_empty t.sources) -> Error Unavailable
      | Root | Nested _ ->
        Ok
          (fun () ->
            if (not (List.for_all t.sources ~f:preparing)) || Atomic.get t.submitted
            then raise (Rejected Expired);
            let checks = List.map t.sources ~f:(fun source -> source.refresh ~scope) in
            fun () ->
              if not (Atomic.compare_and_set t.submitted false true)
              then raise (Rejected Expired);
              if not (List.for_all t.sources ~f:preparing) then raise (Rejected Expired);
              if not (List.for_all checks ~f:(fun check -> check ()))
              then raise (Rejected Unavailable)))
  ;;
end
