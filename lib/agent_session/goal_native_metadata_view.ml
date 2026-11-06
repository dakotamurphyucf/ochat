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

type row =
  { goal : P.Goal.t
  ; accounting : Account.t option
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

let capture (state : Session_state.t) ~session ~goal_id =
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
      Goal_catalog.find catalog id
      |> Result.of_option
           ~error:(P.Error.invalid_request "Goal metadata identity is absent")
      |> Result.map ~f:List.return
  in
  let%bind goals =
    List.fold_result goals ~init:[] ~f:(fun rows goal ->
      let%map accounting =
        match
          Option.bind state.goal_accounts ~f:(fun accounts ->
            Goal_accounts.find accounts (P.Goal.id goal))
        with
        | None -> Ok None
        | Some document ->
          Account.capture document ~goal ~ledger:state.inference_ledger
          |> Result.map ~f:Option.some
      in
      { goal; accounting } :: rows)
  in
  let%map source = source state in
  { session
  ; generation = state.identity.generation
  ; catalog_revision = Goal_catalog.revision catalog
  ; source
  ; goals = List.rev goals
  }
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
                 ; ( "accounting"
                   , Option.value_map row.accounting ~default:`Null ~f:Account.to_json )
                 ])) )
      ]
  in
  (* At most sixteen 128-KiB Goal projections plus bounded aggregate usage; no
     retained provider rows, private accounting bindings or command histories. *)
  P.Json_codec.validate_limits ~max_bytes:(4 * 1024 * 1024) ~max_depth:72 value
  |> Result.map ~f:(fun () -> value)
;;
