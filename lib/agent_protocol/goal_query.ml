open! Core
module J = Json_codec
module W = Goal_wire

module Features = struct
  let inspect = "sessions.goals"
end

let list_method = "session.goals"
let get_method = "session.goal"

module Request = struct
  type target =
    | All
    | One of Id.Goal.t
  [@@deriving sexp_of]

  type t =
    { session : Session_ref.t
    ; attachment_id : Id.Attachment.t option
    ; target : target
    }
  [@@deriving sexp_of]

  let methods = [ list_method; get_method ]

  let method_name t =
    match t.target with
    | All -> list_method
    | One _ -> get_method
  ;;

  let params t =
    let fields =
      [ "session", Session_ref.to_json t.session
      ; "attachment_id", W.option_json Id.Attachment.to_json t.attachment_id
      ]
    in
    match t.target with
    | All -> `Object fields
    | One id -> `Object (("goal_id", Id.Goal.to_json id) :: fields)
  ;;

  let of_method_and_params ~method_ ~params =
    let open Core.Result.Let_syntax in
    let%bind () = Extension_codec.validate_json ~max_bytes:4096 ~max_depth:8 params in
    let%bind names =
      if String.equal method_ list_method
      then Ok [ "session"; "attachment_id" ]
      else if String.equal method_ get_method
      then Ok [ "session"; "attachment_id"; "goal_id" ]
      else W.invalid "unknown Goal query method"
    in
    let%bind fields = W.fields params names in
    let%bind session = J.required_as fields "session" W.session in
    let%bind attachment_id =
      J.required_as fields "attachment_id" (W.nullable Id.Attachment.of_json)
    in
    let%map target =
      if String.equal method_ list_method
      then Ok All
      else (
        let%map id = J.required_as fields "goal_id" Id.Goal.of_json in
        One id)
    in
    { session; attachment_id; target }
  ;;
end

module Availability = Goal_query_observation.Availability
module Run_observation = Goal_query_observation.Run_observation
module Usage = Goal_query_accounting.Usage
module Accounting = Goal_query_accounting.Accounting
module Action = Goal_query_observation.Action

let max_envelope_bytes = 4 * 1024 * 1024
let max_depth = 72

module Goal_summary = Goal_query_observation.Goal_summary
module Execution_blocker = Goal_query_observation.Execution_blocker

module View = struct
  type row =
    { goal : Goal_summary.t
    ; run : Run_observation.t
    ; accounting : Accounting.t option
    ; actions : Action.t list
    ; execution_blockers : Execution_blocker.t list
    }

  type t =
    { session : Session_ref.t
    ; generation : int
    ; session_revision : int64
    ; catalog_revision : int64
    ; source : Run_source.t option
    ; availability : Availability.t
    ; can_create : bool
    ; goals : row list
    }

  let row_to_json row =
    `Object
      [ "goal", Goal_summary.to_json row.goal
      ; "run", Run_observation.to_json row.run
      ; "accounting", W.option_json Accounting.to_json row.accounting
      ; "actions", `Array (List.map row.actions ~f:Action.to_json)
      ; ( "execution_blockers"
        , `Array (List.map row.execution_blockers ~f:Execution_blocker.to_json) )
      ]
  ;;

  let to_json t =
    `Object
      [ "session", Session_ref.to_json t.session
      ; "generation", W.generation_json t.generation
      ; "session_revision", W.counter_json t.session_revision
      ; "catalog_revision", W.counter_json t.catalog_revision
      ; "source", W.option_json Run_source.to_json t.source
      ; "availability", Availability.to_json t.availability
      ; "can_create", W.bool_json t.can_create
      ; "goals", `Array (List.map t.goals ~f:row_to_json)
      ]
  ;;

  let validate_run (row : row) ~(availability : Availability.t) =
    match row.run with
    | Run_observation.Observed { revision; _ } when Int64.(revision < 0L) ->
      W.invalid "negative Goal Run observation revision"
    | Observed { status = Retained; _ } when not (Availability.equal availability Stopped)
      -> W.invalid "retained nonterminal Goal Run requires an observed session stop"
    | Observed { status = Admitted | Active | Waiting; _ }
      when not (Availability.equal availability Active) ->
      W.invalid "live Goal Run observation requires an active session owner"
    | No_episode | Unavailable | Observed _ -> Ok ()
  ;;

  let create
        ~session
        ~generation
        ~session_revision
        ~catalog_revision
        ~source
        ~availability
        ~can_create
        ~goals
    =
    let open Core.Result.Let_syntax in
    if generation < 0 || Int64.(session_revision < 0L || catalog_revision < 0L)
    then W.invalid "invalid Goal view revisions"
    else (
      let%bind () =
        match source with
        | None -> Ok ()
        | Some source ->
          let%bind () = Run_source.validate source in
          if Int.equal source.generation generation
          then Ok ()
          else W.invalid "Goal current source generation mismatch"
      in
      let%bind () =
        let compare_id left right = Id.Goal.compare left.goal.id right.goal.id in
        if List.length goals > 16 || List.contains_dup goals ~compare:compare_id
        then W.invalid "invalid or oversized Goal view rows"
        else Ok ()
      in
      let%bind () =
        List.fold_result goals ~init:() ~f:(fun () row ->
          let%bind () = validate_run row ~availability in
          let%bind () =
            if
              Session_ref.equal session row.goal.session
              && row.goal.source.generation <= generation
            then Ok ()
            else W.invalid "Goal projection refers to another host"
          in
          let%bind () =
            if
              List.contains_dup row.actions ~compare:Action.compare
              || List.contains_dup
                   row.execution_blockers
                   ~compare:Execution_blocker.compare
            then W.invalid "duplicate Goal action or execution blocker"
            else Ok ()
          in
          match row.accounting with
          | None -> Ok ()
          | Some accounting -> Accounting.validate accounting)
      in
      let t =
        { session
        ; generation
        ; session_revision
        ; catalog_revision
        ; source
        ; availability
        ; can_create
        ; goals
        }
      in
      let%map () =
        Extension_codec.validate_json
          ~max_bytes:max_envelope_bytes
          ~max_depth:(max_depth - 1)
          (to_json t)
      in
      t)
  ;;

  let row_of_json ~session json =
    let open Core.Result.Let_syntax in
    let%bind fields =
      W.fields json [ "goal"; "run"; "accounting"; "actions"; "execution_blockers" ]
    in
    let%bind goal = J.required_as fields "goal" (Goal_summary.of_json ~session) in
    let%bind run = J.required_as fields "run" Run_observation.of_json in
    let%bind accounting =
      J.required_as fields "accounting" (W.nullable Accounting.of_json)
    in
    let%bind actions = J.required_as fields "actions" (J.list Action.of_json) in
    let%map execution_blockers =
      J.required_as fields "execution_blockers" (J.list Execution_blocker.of_json)
    in
    { goal; run; accounting; actions; execution_blockers }
  ;;

  let rows_of_json ~session = function
    | `Array rows when List.length rows <= 16 ->
      J.list (row_of_json ~session) (`Array rows)
    | _ -> W.invalid "invalid or oversized Goal view rows"
  ;;

  let of_json json =
    let open Core.Result.Let_syntax in
    let%bind fields =
      W.fields
        json
        [ "session"
        ; "generation"
        ; "session_revision"
        ; "catalog_revision"
        ; "source"
        ; "availability"
        ; "can_create"
        ; "goals"
        ]
    in
    let%bind session = J.required_as fields "session" W.session in
    let%bind generation = J.required_as fields "generation" W.generation in
    let%bind session_revision = J.required_as fields "session_revision" W.counter in
    let%bind catalog_revision = J.required_as fields "catalog_revision" W.counter in
    let%bind source = J.required_as fields "source" (W.nullable W.source) in
    let%bind availability = J.required_as fields "availability" Availability.of_json in
    let%bind can_create = J.required_as fields "can_create" J.bool in
    let%bind goals = J.required_as fields "goals" (rows_of_json ~session) in
    create
      ~session
      ~generation
      ~session_revision
      ~catalog_revision
      ~source
      ~availability
      ~can_create
      ~goals
  ;;
end

module Result = struct
  type outcome =
    | Available of View.t
    | Unavailable of Session_ref.t

  type t =
    | List of outcome
    | Get of outcome

  let session = function
    | Available view -> view.session
    | Unavailable session -> session
  ;;

  let method_name = function
    | List _ -> list_method
    | Get _ -> get_method
  ;;

  let payload = function
    | List (Available view) | Get (Available view) ->
      W.tagged "available" [ "view", View.to_json view ]
    | List (Unavailable session) | Get (Unavailable session) ->
      W.tagged "unavailable" [ "session", Session_ref.to_json session ]
  ;;

  let of_method_and_result ~method_ ~result =
    let open Core.Result.Let_syntax in
    let%bind () =
      Extension_codec.validate_json ~max_bytes:max_envelope_bytes ~max_depth result
    in
    let%bind kind, fields = W.tag result in
    let%bind outcome =
      match kind with
      | "available" ->
        let%bind () = Extension_codec.closed fields [ "kind"; "view" ] in
        let%map view = J.required_as fields "view" View.of_json in
        Available view
      | "unavailable" ->
        let%bind () = Extension_codec.closed fields [ "kind"; "session" ] in
        let%map session = J.required_as fields "session" W.session in
        Unavailable session
      | _ -> W.invalid "unknown Goal query outcome"
    in
    if String.equal method_ list_method
    then Ok (List outcome)
    else if String.equal method_ get_method
    then (
      match outcome with
      | Available { goals = _ :: _ :: _; _ } ->
        W.invalid "Goal lookup returned multiple rows"
      | Available _ | Unavailable _ -> Ok (Get outcome))
    else W.invalid "unknown Goal query method"
  ;;
end
