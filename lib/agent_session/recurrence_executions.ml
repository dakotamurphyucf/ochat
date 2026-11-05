open! Core
module P = Agent_protocol
module D = Document_schema
module X = Persistence_codec
module Execution = Recurrence_execution
module Intent = Recurrence_execution_intent
module Attempt = Recurrence_execution_attempt

module Occurrence = struct
  include Recurrence_identity.Occurrence
  include Comparator.Make (Recurrence_identity.Occurrence)
end

type row =
  { execution : Execution.t
  ; json : Jsonaf.t
  }

type present =
  { rows : row list
  ; by_occurrence : row Map.M(Occurrence).t
  }

type t =
  | Missing
  | Null
  | Present of present

let empty = Missing
let invalid message = Error (P.Error.invalid_request message)

let stored = function
  | Missing | Null -> []
  | Present present -> present.rows
;;

let rows t = List.map (stored t) ~f:(fun row -> row.execution)

let find_row t occurrence =
  match t with
  | Missing | Null -> None
  | Present present -> Map.find present.by_occurrence occurrence
;;

let find t occurrence = Option.map (find_row t occurrence) ~f:(fun row -> row.execution)
let occurrence row = Intent.occurrence (Execution.intent row.execution)

let fields = function
  | Missing -> []
  | Null -> [ "executions", `Null ]
  | Present present ->
    [ "executions", `Array (List.map present.rows ~f:(fun row -> row.json)) ]
;;

let limits =
  D.Limits.create
    ~max_bytes:(2 * 1024 * 1024)
    ~max_depth:48
    ~max_fields:32768
    ~max_nodes:65536
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

(* These are defensive retained-inventory bounds, not policy defaults. The joint
   frontier also counts protected children which have not acquired execution
   rows; record only moves an already-claimed occurrence. *)
let check rows =
  let open Result.Let_syntax in
  if List.length rows > 512
  then invalid "execution table retention exhausted"
  else (
    let%bind () =
      D.Json.validate ~limits (`Array (List.map rows ~f:(fun row -> row.json)))
      |> Result.map_error ~f:(fun error ->
        P.Error.invalid_request (Sexp.to_string_hum (D.Error.sexp_of_t error)))
    in
    let%map by_occurrence, _, _ =
      List.fold_result
        rows
        ~init:(Map.empty (module Occurrence), 0, 0)
        ~f:(fun (map, pending, settled) row ->
          let key = occurrence row in
          let pending, settled =
            if Execution.is_pending row.execution
            then pending + 1, settled
            else pending, settled + 1
          in
          if Map.mem map key || pending > 256 || settled > 256
          then invalid "duplicate or excessive retained execution inventory"
          else Ok (Map.set map ~key ~data:row, pending, settled))
    in
    { rows; by_occurrence })
;;

let initial execution =
  let attempts = Execution.attempts execution in
  match attempts with
  | [ attempt ] ->
    Int.equal (Attempt.number (Execution.Attempt.request attempt)) 0
    && Option.is_none (Execution.Attempt.rejection attempt)
    && Option.is_none (Execution.Attempt.uncertainty attempt)
    && Option.is_none (Execution.Attempt.retry_resolution attempt)
    && Option.is_none (Execution.Attempt.origin attempt)
    && Option.is_none (Execution.Attempt.terminal attempt)
  | [] | _ :: _ :: _ -> false
;;

let record t ~intent ~attempt =
  let open Result.Let_syntax in
  let%bind () = Attempt.validate attempt ~intent in
  if not (Int.equal (Attempt.number attempt) 0)
  then invalid "initial execution record requires attempt zero"
  else (
    match find t (Intent.occurrence intent) with
    | Some retained ->
      let original = List.hd_exn (Execution.attempts retained) in
      if
        Intent.equal (Execution.intent retained) intent
        && Attempt.equal (Execution.Attempt.request original) attempt
      then Ok (t, retained)
      else invalid "execution occurrence already retained another original request"
    | None ->
      let%bind execution = Execution.create ~intent ~attempt in
      let row =
        { execution
        ; json =
            `Object
              [ "occurrence", Occurrence.to_json (Intent.occurrence intent)
              ; "execution", Execution.to_json execution
              ]
        }
      in
      let%map present = check (stored t @ [ row ]) in
      Present present, execution)
;;

type change =
  | Associated
  | Settled of Occurrence.t list

type imported =
  | Absent
  | Retained of Execution.t
  | Changed of
      { execution : Execution.t
      ; change : change
      }

let with_execution row execution =
  match row.json with
  | `Object fields ->
    { execution
    ; json =
        `Object
          (List.map fields ~f:(fun (name, value) ->
             ( name
             , if String.equal name "execution"
               then Execution.to_json execution
               else value )))
    }
  | _ -> assert false
;;

let import t ~origin ~terminal ~max_history =
  let open Result.Let_syntax in
  if max_history < 1 || max_history > 256
  then invalid "execution history bound must be between 1 and 256"
  else (
    let key = Recurrence_run_origin.occurrence origin in
    match find_row t key with
    | None -> Ok (t, Absent)
    | Some previous ->
      let%bind execution = Execution.associate previous.execution origin in
      let%bind execution =
        match terminal with
        | None -> Ok execution
        | Some terminal -> Execution.settle execution terminal
      in
      if Execution.equal previous.execution execution
      then Ok (t, Retained previous.execution)
      else (
        let updated = with_execution previous execution in
        let newly_terminal =
          Execution.is_pending previous.execution && not (Execution.is_pending execution)
        in
        let next, retired =
          if not newly_terminal
          then
            ( List.map (stored t) ~f:(fun row ->
                if Occurrence.equal key (occurrence row) then updated else row)
            , [] )
          else (
            (* Detail order records controller import order, not receipt UTC or
               slot order. A pending earlier slot never blocks bounded terminal
               detail retirement after out-of-order completions. *)
            let next =
              List.filter (stored t) ~f:(fun row ->
                not (Occurrence.equal key (occurrence row)))
              @ [ updated ]
            in
            let terminals =
              List.filter next ~f:(fun row -> not (Execution.is_pending row.execution))
            in
            let retired =
              List.take terminals (Int.max 0 (List.length terminals - max_history))
              |> List.map ~f:occurrence
            in
            ( List.filter next ~f:(fun row ->
                not (List.mem retired (occurrence row) ~equal:Occurrence.equal))
            , retired ))
        in
        let%map present = check next in
        let change = if newly_terminal then Settled retired else Associated in
        Present present, Changed { execution; change }))
;;

let of_field = function
  | None -> Ok Missing
  | Some `Null -> Ok Null
  | Some json ->
    let open Result.Let_syntax in
    let%bind () =
      D.Json.validate ~limits json
      |> Result.map_error ~f:(fun error ->
        P.Error.invalid_request (Sexp.to_string_hum (D.Error.sexp_of_t error)))
    in
    let%bind rows =
      match json with
      | `Array rows when List.length rows <= 512 ->
        List.map rows ~f:(fun json ->
          let%bind fields = X.object_ json in
          let%bind key = X.required fields "occurrence" Occurrence.of_json in
          let%bind execution = X.required fields "execution" Execution.of_json in
          let row = { execution; json } in
          if Occurrence.equal key (occurrence row)
          then Ok row
          else invalid "execution table wrapper changed its occurrence")
        |> Result.all
      | _ -> invalid "invalid bounded execution table"
    in
    let%map present = check rows in
    Present present
;;

let retains_wrapper previous incoming =
  match previous.json, incoming.json with
  | `Object old, `Object next ->
    List.for_all old ~f:(fun (name, value) ->
      String.equal name "execution"
      || Option.value_map
           (List.Assoc.find next name ~equal:String.equal)
           ~default:false
           ~f:(D.Json.equal value))
  | _ -> assert false
;;

let same_facts previous incoming =
  Intent.equal (Execution.intent previous) (Execution.intent incoming)
  && List.equal
       (fun old next ->
          let module A = Execution.Attempt in
          Attempt.equal (A.request old) (A.request next)
          && Option.equal P.Error.equal_code (A.rejection old) (A.rejection next)
          && Option.equal P.Error.equal_code (A.uncertainty old) (A.uncertainty next)
          && Option.equal
               Recurrence_retry_resolution.equal
               (A.retry_resolution old)
               (A.retry_resolution next)
          && Option.equal Recurrence_run_origin.equal (A.origin old) (A.origin next)
          && Option.equal
               Recurrence_run_reference.equal
               (A.terminal old)
               (A.terminal next))
       (Execution.attempts previous)
       (Execution.attempts incoming)
;;

let validate_transition t ~incoming =
  let open Result.Let_syntax in
  let%bind _ = check (stored incoming) in
  let%bind () =
    match t, incoming with
    | Missing, Missing | Null, Null | (Missing | Null | Present _), Present _ -> Ok ()
    | Missing, Null | Null, Missing | Present _, (Missing | Null) ->
      invalid "execution table presence regressed"
  in
  let old = stored t
  and next = stored incoming in
  let%bind changed, removed =
    List.fold_result old ~init:([], []) ~f:(fun (changed, removed) previous ->
      match find_row incoming (occurrence previous) with
      | None ->
        if Execution.is_pending previous.execution
        then invalid "unresolved execution detail cannot retire"
        else Ok (changed, previous :: removed)
      | Some row ->
        let%bind () =
          if retains_wrapper previous row
          then Execution.validate_transition previous.execution ~incoming:row.execution
          else invalid "execution identity or complete wrapper custody changed"
        in
        if same_facts previous.execution row.execution
        then Ok (changed, removed)
        else Ok ((previous, row) :: changed, removed))
  in
  let added =
    List.filter next ~f:(fun row -> Option.is_none (find_row t (occurrence row)))
  in
  let keys rows = List.map rows ~f:occurrence in
  let same_order expected =
    if List.equal Occurrence.equal expected (keys next)
    then Ok ()
    else invalid "execution detail order changed outside its checked transition"
  in
  match added, changed with
  | [ row ], [] when initial row.execution && List.is_empty removed ->
    same_order (keys old @ [ occurrence row ])
  | [], [] when List.is_empty removed -> same_order (keys old)
  | [], [ (previous, row) ] ->
    let newly_terminal =
      Execution.is_pending previous.execution && not (Execution.is_pending row.execution)
    in
    if newly_terminal
    then (
      let terminal_order =
        List.filter old ~f:(fun old ->
          (not (Occurrence.equal (occurrence previous) (occurrence old)))
          && not (Execution.is_pending old.execution))
      in
      let removed_keys = keys (List.rev removed) in
      let%bind () =
        if
          List.equal
            Occurrence.equal
            removed_keys
            (keys (List.take terminal_order (List.length removed)))
        then Ok ()
        else invalid "terminal detail retirement did not remove the oldest whole bundles"
      in
      let remaining =
        List.filter old ~f:(fun old ->
          (not (Occurrence.equal (occurrence previous) (occurrence old)))
          && not (List.mem removed_keys (occurrence old) ~equal:Occurrence.equal))
      in
      same_order (keys remaining @ [ occurrence row ]))
    else if List.is_empty removed
    then same_order (keys old)
    else invalid "execution retirement requires one actual new terminal"
  | [], [] | [ _ ], [] | _ :: _ :: _, _ | _ :: _, _ :: _ | [], _ :: _ :: _ ->
    invalid "one transition cannot add or import multiple execution facts"
;;

let rotate t ~intent ~previous ~resolution ~next =
  let open Result.Let_syntax in
  let%bind () = Attempt.validate previous ~intent in
  let%bind () = Attempt.validate next ~intent in
  let%bind () =
    Recurrence_retry_resolution.validate resolution ~intent ~attempt:previous
  in
  let key = Intent.occurrence intent in
  let%bind row =
    Result.of_option
      (find_row t key)
      ~error:(P.Error.invalid_request "retry execution occurrence is not retained")
  in
  let%bind () =
    if Intent.equal (Execution.intent row.execution) intent
    then Ok ()
    else invalid "retry changed the complete retained intent"
  in
  let current = Execution.current_attempt row.execution in
  if Attempt.equal (Execution.Attempt.request current) next
  then (
    match List.rev (Execution.attempts row.execution) with
    | _current :: predecessor :: _
      when Attempt.equal (Execution.Attempt.request predecessor) previous
           && Option.equal
                Recurrence_retry_resolution.equal
                (Execution.Attempt.retry_resolution predecessor)
                (Some resolution) -> Ok (t, row.execution)
    | [] | [ _ ] | _ :: _ :: _ -> invalid "retry duplicate changed its predecessor")
  else if not (Attempt.equal (Execution.Attempt.request current) previous)
  then invalid "retry requires the exact current predecessor"
  else (
    let%bind resolved = Execution.resolve_retry row.execution resolution in
    let%bind execution = Execution.append_attempt resolved next in
    let updated = with_execution row execution in
    let%bind present =
      check
        (List.map (stored t) ~f:(fun row ->
           if Occurrence.equal (occurrence row) key then updated else row))
    in
    let incoming = Present present in
    let%map () = validate_transition t ~incoming in
    incoming, execution)
;;

let reserved_growth t =
  List.fold_result (rows t) ~init:Capacity_growth.zero ~f:(fun growth execution ->
    let%bind.Result child = Execution.reserved_growth execution in
    Capacity_growth.add growth child)
;;

let required_transition_steps t =
  List.sum
    (module Int)
    (rows t)
    ~f:(fun execution ->
      let current = Execution.current_attempt execution in
      if Option.is_some (Execution.Attempt.terminal current)
      then 0
      else if Option.is_some (Execution.Attempt.origin current)
      then 1
      else if Option.is_some (Execution.Attempt.retry_resolution current)
      then 0
      else 2)
;;

let capacity_fields = function
  | Missing -> []
  | Null -> [ "executions", `Null ]
  | Present present ->
    [ ( "executions"
      , `Array
          (List.map present.rows ~f:(fun row ->
             match row.json with
             | `Object fields ->
               `Object
                 (List.map fields ~f:(fun (name, json) ->
                    ( name
                    , if String.equal name "execution"
                      then (
                        let future = Execution.capacity_projection row.execution in
                        let current = Execution.current_attempt row.execution in
                        if
                          Option.is_some (Execution.Attempt.terminal current)
                          || Option.is_some (Execution.Attempt.retry_resolution current)
                        then future
                        else
                          (* Accounting-only wrapper. Its prefix is deeper than
                           the actual Frontier basis/control-history/alarm slots,
                           so maximum child depth is conservative at every outer
                           profile as well as bytes/fields/nodes. All original
                           execution and row unknowns remain present. *)
                          `Object
                            [ "execution", future
                            ; "settled_counter", `String (Int64.to_string Int64.max_value)
                            ; ( "basis"
                              , Recurrence_execution_capacity.profile
                                  Recurrence_delay_basis.limits )
                            ; ( "archive"
                              , `Object
                                  [ "kind", `String "retired_delay_basis"
                                  ; ( "value"
                                    , Recurrence_execution_capacity.profile
                                        Recurrence_delay_basis.limits )
                                  ] )
                            ; ( "alarm"
                              , Recurrence_execution_capacity.profile
                                  Recurrence_alarm.limits )
                            ])
                      else json )))
             | _ -> assert false)) )
    ]
;;

let requires_semantics = function
  | Missing | Null -> false
  | Present _ -> true
;;
