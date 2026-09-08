open Core
module P = Agent_protocol
module I = P.Invocation

type t =
  { deltas : Session_delta.t list
  ; appended : P.History.entry list
  ; next_sequence : int
  }

let plan_selected ~accept ~state ~namespace ~first_sequence ~reason =
  let open Result.Let_syntax in
  let invalid message = Error (P.Error.invalid_request message) in
  let%bind () =
    if
      first_sequence < 0
      || Int64.(
           of_int first_sequence
           < max
               state.Session_state.conversation.next_history_sequence
               state.conversation.reserved_history_through)
    then invalid "invocation recovery would reuse reserved history IDs"
    else Ok ()
  in
  let working = ref state
  and deltas = ref []
  and appended = ref []
  and next = ref first_sequence in
  let apply delta =
    let%map value = Session_delta.apply !working delta in
    working := value;
    deltas := delta :: !deltas
  in
  let history () = !working.conversation.canonical_history in
  let index invocation =
    List.findi state.conversation.canonical_history ~f:(fun _ entry ->
      Option.exists invocation.I.context.call_entry_id ~f:(fun id ->
        P.History.Id.compare id entry.id = 0))
    |> Option.value_map ~default:Int.max_value ~f:fst
  in
  let ordered =
    List.filter state.invocations ~f:(fun invocation ->
      accept invocation
      &&
      match invocation.observation, invocation.status with
      | Some { status = Observing; _ }, _ -> true
      | _, Published _ -> false
      | _, Resolved _ -> Option.is_none invocation.publication_discarded
      | _, (Admitted | Dispatching) -> true)
    |> List.stable_sort ~compare:(fun a b -> Int.compare (index a) (index b))
  in
  let%bind () =
    List.fold_result ordered ~init:() ~f:(fun () invocation ->
      let%bind invocation =
        match invocation.I.status with
        | Admitted | Dispatching ->
          let%bind cancelled = I.cancel invocation ~reason in
          let%map () = apply (Invocation_reconciled cancelled) in
          cancelled
        | Resolved _ | Published _ -> Ok invocation
      in
      let%bind invocation =
        match invocation.observation with
        | Some { status = Observing; _ } ->
          let%bind failed =
            I.fail_observation
              invocation
              ~reason:"observation interrupted before durable acknowledgement"
          in
          let%map () = apply (Invocation_reconciled failed) in
          failed
        | None | Some { status = Awaiting | Observed | Observation_failed _; _ } ->
          Ok invocation
      in
      match invocation.status with
      | Published _ -> Ok ()
      | Admitted | Dispatching -> assert false
      | Resolved _ when Option.is_some invocation.publication_discarded -> Ok ()
      | Resolved _ when not (I.equal_origin invocation.context.origin Model) -> Ok ()
      | Resolved outcome ->
        if
          not
            (List.exists (history ()) ~f:(fun entry ->
               Option.exists invocation.context.call_entry_id ~f:(fun id ->
                 P.History.Id.compare id entry.id = 0)))
        then (
          let reason =
            if Option.is_none invocation.context.call_entry_id
            then "legacy invocation has no canonical call occurrence"
            else "canonical call removed before initial result publication"
          in
          let%bind discarded = I.discard_publication invocation ~reason in
          apply (Invocation_reconciled discarded))
        else (
          let%bind found =
            Invocation_history.recover_output ~history:(history ()) invocation
          in
          let%bind entry =
            match found with
            | `Existing entry -> Ok entry
            | `Missing kind ->
              if !next = Int.max_value
              then invalid "history sequence space is exhausted"
              else (
                let%bind id =
                  History_entry.Id.create ~namespace ~sequence:!next
                  |> Result.map_error ~f:P.Error.invalid_request
                in
                let%bind () =
                  if
                    List.exists
                      (history () @ state.conversation.deferred_user_entries)
                      ~f:(fun entry -> P.History.Id.compare entry.id id = 0)
                    || List.exists state.invocations ~f:(fun inv ->
                      List.exists
                        (Option.to_list inv.context.call_entry_id
                         @ Option.to_list inv.output_entry_id)
                        ~f:(fun existing -> P.History.Id.compare existing id = 0))
                  then
                    invalid "recovery allocation collides with retained history evidence"
                  else Ok ()
                in
                Int.incr next;
                let output =
                  Openai.Responses.Tool_output.Output.Text
                    (Jsonaf.to_string (I.outcome_to_json outcome))
                in
                let call_id = Option.value_exn invocation.context.provider_call_id in
                let item =
                  match kind with
                  | I.Function ->
                    Openai.Responses.Item.Function_call_output
                      { call_id
                      ; output
                      ; _type = "function_call_output"
                      ; id = None
                      ; status = None
                      }
                  | Custom ->
                    Custom_tool_call_output
                      { call_id; output; _type = "custom_tool_call_output"; id = None }
                in
                let entry = History_entry.create_with_id ~id item in
                let encoded = History_codec.to_protocol entry in
                let%map () = apply (Canonical_entries_appended [ encoded ]) in
                appended := encoded :: !appended;
                entry)
          in
          let%bind published =
            I.publish_with_history invocation ~output_entry_id:(History_entry.id entry)
          in
          apply (Invocation_reconciled published)))
  in
  Ok { deltas = List.rev !deltas; appended = List.rev !appended; next_sequence = !next }
;;

let plan ~state ~namespace ~first_sequence ~reason =
  plan_selected ~accept:(fun _ -> true) ~state ~namespace ~first_sequence ~reason
;;

let plan_foreground ~state ~namespace ~first_sequence ~reason =
  plan_selected
    ~accept:(fun invocation ->
      I.equal_origin invocation.context.origin Model
      && Option.is_none invocation.context.parent_job)
    ~state
    ~namespace
    ~first_sequence
    ~reason
;;
