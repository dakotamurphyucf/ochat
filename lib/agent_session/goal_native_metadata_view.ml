open! Core
module P = Agent_protocol

module Account = struct
  type t =
    { revision : int64
    ; admitted_turns : int64
    ; admitted_attempts : int64
    ; untracked_turns : int64
    ; untracked_attempts : int64
    ; usage : Goal_usage.t
    }

  let capture document ~goal ~ledger =
    let open Result.Let_syntax in
    let%bind account = Goal_accounting_document.resolve document ~goal ~ledger in
    let%map usage =
      Goal_accounting.usage account ~ledger
      |> Result.map_error ~f:(fun _ ->
        P.Error.create
          Invalid_state
          ~message:"Goal accounting cannot resolve canonical usage"
          ~retryable:false
          ())
    in
    { revision = Goal_accounting.revision account
    ; admitted_turns = Goal_accounting.admitted_turns account
    ; admitted_attempts = Goal_accounting.admitted_attempts account
    ; untracked_turns = Goal_accounting.untracked_turns account
    ; untracked_attempts = Goal_accounting.untracked_attempts account
    ; usage
    }
  ;;

  let to_json t =
    let count value = `String (Int64.to_string value) in
    `Object
      [ "revision", count t.revision
      ; "admitted_turns", count t.admitted_turns
      ; "admitted_attempts", count t.admitted_attempts
      ; "untracked_turns", count t.untracked_turns
      ; "untracked_attempts", count t.untracked_attempts
      ; "usage", Goal_usage.Persistence.to_json t.usage
      ]
  ;;
end

module Governance = struct
  type t = Goal_policy_totals.View.t

  let to_json t =
    let module V = Goal_policy_totals.View in
    let count n = `String (Int64.to_string n) in
    `Object
      [ "local_usage", Goal_usage.Persistence.to_json (V.local_usage t)
      ; "physical_usage", Goal_usage.Persistence.to_json (V.physical_usage t)
      ; "causal_usage", Goal_usage.Persistence.to_json (V.causal_usage t)
      ; "total_usage", Goal_usage.Persistence.to_json (V.total_usage t)
      ; ("physical_unknown", if V.physical_unknown t then `True else `False)
      ; ( "historical_causal_unknown"
        , if V.historical_causal_unknown t then `True else `False )
      ; "admitted_attempts", count (V.committed t ~dimension:Attempt)
      ; "admitted_turns", count (V.committed t ~dimension:Host_turn)
      ; "pending_attempts", count (V.pending t ~dimension:Attempt)
      ; "pending_turns", count (V.pending t ~dimension:Host_turn)
      ]
  ;;
end

module Current_run = struct
  type t =
    { id : P.Id.Run.t
    ; revision : int64
    }

  let capture (state : Session_state.t) ~session ~source goal =
    let open Result.Let_syntax in
    match P.Goal.state goal, P.Goal.episode goal, state.run_state, source with
    | Open _, Some episode, Some index, Some source ->
      let%bind run = Goal_run_evidence.episode_run index goal episode in
      let installation = Run_state.installation index in
      (match run.lifecycle, installation.source with
       | (Admitted | Active | Waiting _), Some _ ->
         let%map installed =
           Run_source_installation.captured
             installation
             ~generation:state.identity.generation
         in
         if
           P.Session_ref.equal session (P.Goal.session goal)
           && P.Session_ref.equal session run.session
           && P.Run_source.equal source installed
           && P.Run_source.equal (P.Goal.source goal) installed
           && P.Run_source.equal run.source installed
         then Some { id = run.id; revision = run.revision }
         else None
       | Terminal _, _ | (Admitted | Active | Waiting _), None -> Ok None)
    | Closed _, _, _, _ | Open _, None, _, _ | Open _, _, None, _ | Open _, _, _, None ->
      Ok None
  ;;

  let to_json t =
    `Object
      [ "run_id", P.Id.Run.to_json t.id
      ; "revision", `String (Int64.to_string t.revision)
      ]
  ;;
end

type row =
  { goal : P.Goal.t
  ; current_run : Current_run.t option
  ; accounting : Account.t option
  ; governance : Governance.t option
  }

type t =
  { session : P.Session_ref.t
  ; generation : int
  ; catalog_revision : int64
  ; source : P.Run_source.t option
  ; goals : row list
  }

let source (state : Session_state.t) =
  let open Result.Let_syntax in
  match state.moderator with
  | Some _ ->
    let%map _, source = Goal_catalog_source.capture state in
    Some source
  | None ->
    (match Option.map state.run_state ~f:Run_state.installation with
     | None | Some { source = None; _ } -> Ok None
     | Some installation ->
       let%map source =
         Run_source_installation.captured
           installation
           ~generation:state.identity.generation
       in
       Some source)
;;

let capture_snapshot (state : Session_state.t) ~session ~goal_id ~allow_missing =
  let open Result.Let_syntax in
  let%bind () =
    if P.Id.Session.equal (P.Session_ref.session_id session) state.identity.session_id
    then Ok ()
    else Error (P.Error.invalid_request "Goal metadata belongs to another session")
  in
  let catalog = Option.value state.goal_catalog ~default:Goal_catalog.empty in
  let%bind goals =
    match goal_id with
    | None -> Ok (Goal_catalog.goals catalog)
    | Some id ->
      (match Goal_catalog.find catalog id with
       | Some goal -> Ok [ goal ]
       | None ->
         if allow_missing
         then Ok []
         else Error (P.Error.invalid_request "Goal metadata identity is absent"))
  in
  let%bind source = source state in
  let%map goals =
    List.fold_result goals ~init:[] ~f:(fun rows goal ->
      let%bind current_run = Current_run.capture state ~session ~source goal in
      let%bind accounting =
        match
          Option.bind state.goal_accounts ~f:(fun accounts ->
            Goal_accounts.find accounts (P.Goal.id goal))
        with
        | None -> Ok None
        | Some document ->
          Account.capture document ~goal ~ledger:state.inference_ledger
          |> Result.map ~f:Option.some
      in
      let%map governance =
        match accounting with
        | None -> Ok None
        | Some _ ->
          let%bind context = Session_state.goal_policy_context state ~owner:session in
          Goal_policy_totals.capture context ~goal_id:(P.Goal.id goal)
          |> Result.map ~f:Option.some
      in
      { goal; current_run; accounting; governance } :: rows)
  in
  { session
  ; generation = state.identity.generation
  ; catalog_revision = Goal_catalog.revision catalog
  ; source
  ; goals = List.rev goals
  }
;;

let capture state ~session ~goal_id =
  capture_snapshot state ~session ~goal_id ~allow_missing:false
;;

let capture_query state ~session ~goal_id =
  capture_snapshot state ~session ~goal_id ~allow_missing:true
;;

let to_json t =
  let value =
    `Object
      [ "session", P.Session_ref.to_json t.session
      ; "generation", `String (Int.to_string t.generation)
      ; "catalog_revision", `String (Int64.to_string t.catalog_revision)
      ; "source", Option.value_map t.source ~default:`Null ~f:P.Run_source.to_json
      ; ( "goals"
        , `Array
            (List.map t.goals ~f:(fun row ->
               `Object
                 [ "goal", P.Goal.to_json row.goal
                 ; ( "current_run"
                   , Option.value_map
                       row.current_run
                       ~default:`Null
                       ~f:Current_run.to_json )
                 ; ( "accounting"
                   , Option.value_map row.accounting ~default:`Null ~f:Account.to_json )
                 ; ( "governance"
                   , Option.value_map row.governance ~default:`Null ~f:Governance.to_json
                   )
                 ])) )
      ]
  in
  (* At most sixteen 128-KiB Goal projections plus bounded aggregate usage; no
     retained provider rows, private accounting bindings or command histories. *)
  P.Json_codec.validate_limits ~max_bytes:(4 * 1024 * 1024) ~max_depth:72 value
  |> Result.map ~f:(fun () -> value)
;;
