open! Core
module P = Agent_protocol
module D = Document_schema

type entry =
  { run : P.Run.t
  ; missing : P.Run_work.t list
  ; receipt : bool
  }

type t =
  { entries : entry list
  ; intents : Run_intent.t list
  ; steps : int
  }

let is_empty t = List.is_empty t.entries
let remaining_steps t = t.steps
let maximum_decimal = `String (Int64.to_string Int64.max_value)
let maximum_timestamp = `String "2099-12-31T23:59:59.999999999Z"
let maximum_operation = `String ("op_" ^ String.make 93 'x')

let replace json changes =
  match json with
  | `Object fields ->
    let retained =
      List.filter fields ~f:(fun (name, _) ->
        not (List.Assoc.mem changes name ~equal:String.equal))
    in
    `Object (retained @ changes)
  | _ -> assert false
;;

let live (run : P.Run.t) =
  match run.lifecycle with
  | Terminal _ -> false
  | Admitted | Active | Waiting _ -> true
;;

let validate_canonical_keys (run : P.Run.t) receipts =
  if not (live run)
  then Ok ()
  else (
    let occupied =
      List.exists receipts ~f:(fun receipt ->
        P.Id.Principal.equal receipt.P.Run_receipt.principal_id run.principal_id
        && List.exists [ "run-terminal:"; "run-interrupted:" ] ~f:(fun namespace ->
          match
            String.chop_prefix
              (P.Idempotency_key.to_string receipt.key)
              ~prefix:(namespace ^ P.Id.Run.to_string run.id ^ ":")
          with
          | None -> false
          | Some suffix ->
            (match Int64.of_string_opt suffix with
             | Some revision ->
               String.equal suffix (Int64.to_string revision)
               && Int64.(revision > run.revision)
             | None -> false)))
    in
    if occupied
    then Error (P.Error.invalid_request "canonical Run terminal key is already occupied")
    else Ok ())
;;

let capture ~runs ~intents ~receipts =
  let open Result.Let_syntax in
  let%bind () = P.Run_limits.check_count (List.length runs) in
  let%bind () = P.Run_limits.check_count (List.length intents) in
  let%bind () = P.Run_limits.check_count (List.length receipts) in
  (* Bound the entire graph before constructing sets or future witnesses. This
     also bounds total work across Runs, not merely each individual array. *)
  let%bind () =
    let%bind limits =
      D.Limits.create
        ~max_bytes:P.Run_limits.max_document_bytes
        ~max_depth:P.Run_limits.max_depth
        ~max_fields:100_000
        ~max_nodes:1_000_000
      |> Result.map_error ~f:(fun error ->
        P.Error.invalid_request (Sexp.to_string_hum (D.Error.sexp_of_t error)))
    in
    D.Json.validate
      ~limits
      (`Array
          [ `Array (List.map runs ~f:P.Run.to_json)
          ; `Array (List.map intents ~f:Run_intent.to_jsonaf)
          ; `Array (List.map receipts ~f:P.Run_receipt.to_json)
          ])
    |> Result.map_error ~f:(fun error ->
      P.Error.invalid_request (Sexp.to_string_hum (D.Error.sexp_of_t error)))
  in
  let%bind _ =
    List.fold_result runs ~init:String.Set.empty ~f:(fun seen run ->
      let id = P.Id.Run.to_string run.P.Run.id in
      if Set.mem seen id
      then Error (P.Error.invalid_request "duplicate Run in capacity basis")
      else Ok (Set.add seen id))
  in
  let%bind entries, steps =
    List.fold_result runs ~init:([], 0) ~f:(fun (entries, steps) run ->
      let%bind () = P.Run.validate run in
      let%bind () = validate_canonical_keys run receipts in
      let receipt =
        not
          (List.exists receipts ~f:(fun receipt ->
             P.Run_receipt.Kind.equal receipt.P.Run_receipt.kind Terminal
             && P.Id.Run.equal receipt.run_id run.id
             && Int64.equal receipt.run_revision run.revision))
      in
      if not (live run || receipt)
      then Ok (entries, steps)
      else (
        let proven =
          Set.of_list
            (module P.Run_work)
            (List.map run.terminal_work ~f:(fun proof -> proof.P.Run_work.Terminal.work))
        in
        let missing =
          List.filter run.owned_work ~f:(fun work -> not (Set.mem proven work))
        in
        let waits =
          List.count intents ~f:(fun intent ->
            P.Id.Run.equal intent.Run_intent.receipt.run_id run.id
            &&
            match intent.disposition, intent.action with
            | Pending, Wait _ -> true
            | _ -> false)
        in
        let remaining = if live run then 1 + List.length missing + (2 * waits) else 0 in
        let%bind () =
          if Int64.(run.revision > max_value - of_int remaining)
          then
            Error
              (P.Error.invalid_request "Run lacks reserved terminal revision headroom")
          else Ok ()
        in
        Ok ({ run; missing; receipt } :: entries, steps + remaining)))
  in
  let%map () =
    P.Run_limits.check_count
      (List.length receipts + List.count entries ~f:(fun entry -> entry.receipt))
  in
  { entries = List.rev entries; intents; steps }
;;

type projection =
  | Recovery
  | Completion
  | Failure

let terminal t (run : P.Run.t) = function
  | Recovery -> `Object [ "kind", `String "interrupted" ]
  | Failure -> `Object [ "kind", `String "failed"; "code", `String (String.make 96 'x') ]
  | Completion ->
    List.find_map t.intents ~f:(fun intent ->
      if P.Id.Run.equal intent.Run_intent.receipt.run_id run.id
      then (
        match intent.disposition, intent.action with
        | Pending, Finish { terminal; relinquish = _ } ->
          Some (P.Run.Terminal.to_json terminal)
        | _ -> None)
      else None)
    |> Option.value ~default:(`Object [ "kind", `String "completed" ])
;;

let receipt_json entry =
  let run = entry.run in
  `Object
    [ "run_id", P.Id.Run.to_json run.id
    ; "principal_id", P.Id.Principal.to_json run.principal_id
    ; "source", P.Run_source.to_json run.source
    ; ( "key"
      , `String
          ("run-interrupted:"
           ^ P.Id.Run.to_string run.id
           ^ ":"
           ^ Int64.to_string Int64.max_value) )
    ; "request_sha256", `String (String.make 64 'f')
    ; "kind", `String "terminal"
    ; "run_revision", maximum_decimal
    ; "session_revision", maximum_decimal
    ; "committed_at", maximum_timestamp
    ]
;;

let project t ~index projection =
  let entries =
    List.map t.entries ~f:(fun entry -> P.Id.Run.to_string entry.run.id, entry)
    |> String.Map.of_alist_exn
  in
  let run_json (run : P.Run.t) entry =
    let current = P.Run.to_json run in
    if not (live run)
    then current
    else (
      let added =
        List.map entry.missing ~f:(fun work ->
          `Object
            [ "work", P.Run_work.to_json work
            ; ( "outcome"
              , `String
                  (match projection with
                   | Recovery -> "unconfirmed"
                   | Completion -> "succeeded"
                   | Failure -> "failed") )
            ; "revision", maximum_decimal
            ])
      in
      replace
        current
        [ "revision", maximum_decimal
        ; "updated_at", maximum_timestamp
        ; ( "lifecycle"
          , `Object [ "kind", `String "terminal"; "terminal", terminal t run projection ]
          )
        ; ( "terminal_work"
          , `Array (List.map run.terminal_work ~f:P.Run_work.Terminal.to_json @ added) )
        ])
  in
  let replace_array name f =
    match D.Json.field index ~name with
    | Value (`Array values) -> `Array (List.map values ~f)
    | _ -> assert false
  in
  let runs =
    replace_array "runs" (fun json ->
      match D.Json.field json ~name:"id" with
      | Value (`String id) ->
        Option.value_map (Map.find entries id) ~default:json ~f:(fun entry ->
          run_json entry.run entry)
      | _ -> assert false)
  in
  let intents =
    `Array
      (List.map t.intents ~f:(fun intent ->
         match intent.disposition with
         | Consumed _ | Retired -> Run_intent.to_jsonaf intent
         | Pending ->
           let disposition =
             match projection with
             | Recovery -> `Object [ "kind", `String "retired" ]
             | Completion | Failure ->
               `Object [ "kind", `String "consumed"; "operation_id", maximum_operation ]
           in
           replace (Run_intent.to_jsonaf intent) [ "disposition", disposition ]))
  in
  let receipts =
    match D.Json.field index ~name:"receipts" with
    | Value (`Array values) ->
      `Array
        (values
         @ List.filter_map t.entries ~f:(fun entry ->
           if entry.receipt then Some (receipt_json entry) else None))
    | _ -> assert false
  in
  replace index [ "runs", runs; "intents", intents; "receipts", receipts ]
;;

let projections t ~index =
  if is_empty t
  then []
  else List.map [ Recovery; Completion; Failure ] ~f:(project t ~index)
;;

let reserved_growth t ~index ~limits ~preserve =
  let open Result.Let_syntax in
  let%bind original = Capacity_growth.measure index ~limits in
  List.fold_result
    (projections t ~index)
    ~init:Capacity_growth.zero
    ~f:(fun growth projected ->
      let%bind projected = preserve projected in
      let%map measured = Capacity_growth.measure projected ~limits in
      Capacity_growth.maximum
        growth
        (Capacity_growth.positive_difference measured ~previous:original))
;;
