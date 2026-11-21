open! Core
module P = Agent_protocol
module B = Recovery_rollover_batch
module R = Recovery_effective_record
module E = Recovery_effective_attempt
module H = Inference_ledger.Handle
module Authored = Recovery_authored_checkpoint
module Checkpoint = Recovery_rollover.Checkpoint
module Scope = P.Recovery_window.Scope
module Evidence = Recovery_rollover_evidence

let conflict message = Error (P.Error.create Conflict ~message ~retryable:true ())

let lookup_invocation (state : Session_state.t) binding =
  List.find state.invocations ~f:(Authored.matches_invocation binding)
  |> Result.of_option
       ~error:(P.Error.invalid_request "checkpoint invocation is unavailable")
;;

let call_in history id =
  List.find history ~f:(fun (entry : P.History.entry) -> P.History.Id.equal entry.id id)
  |> Result.of_option ~error:(P.Error.invalid_request "checkpoint call is unavailable")
;;

let check_call call =
  let open Result.Let_syntax in
  match%bind Evidence.semantics call with
  | Call { name; kind = Function; _ }
    when Evidence.original call && String.equal name Authored.name -> Ok ()
  | _ -> conflict "checkpoint call no longer has its original canonical source"
;;

let registration_attempt (state : Session_state.t) ~scope ~operation ~call =
  let open Result.Let_syntax in
  let%bind () =
    match state.active_operation, state.lifecycle.observed with
    | Some selected, Running_turn active
      when P.Id.Operation.equal selected.id operation
           && P.Id.Operation.equal active operation
           && Int.equal selected.generation (Scope.generation scope)
           &&
           match selected.kind, selected.state with
           | Turn _, Running -> true
           | _ -> false -> Ok ()
    | _ -> conflict "checkpoint is outside its original foreground operation"
  in
  let%bind record, handle, _ = Evidence.require_attempt state ~scope ~operation in
  if
    List.exists (R.members record) ~f:(fun member ->
      P.History.Id.equal
        call
        (P.Recovery_window.Citation.occurrence (R.Member.citation member)))
  then conflict "checkpoint reused a prior prepared call"
  else Ok (R.source_revision record, H.ordinal handle)
;;

let check_source state ~scope (binding : Authored.t) =
  let open Result.Let_syntax in
  let selected =
    List.find (Evidence.records state) ~f:(fun record ->
      Int64.equal binding.source_revision (R.source_revision record)
      && Scope.equal (R.scope record) scope
      && P.Id.Operation.equal (R.operation record) binding.operation)
  in
  let%bind source =
    Result.of_option
      selected
      ~error:(P.Error.invalid_request "checkpoint attempt source is unavailable")
  in
  match R.custody source, R.attempt source with
  | Live, Some attempt
    when Int64.equal (E.ordinal attempt) binding.attempt_ordinal
         && Option.exists
              (Option.bind
                 state.recovery_effective
                 ~f:Recovery_effective_ledger.current_incarnation)
              ~f:(Int64.equal (R.overlay_incarnation source)) ->
    Result.map
      (Evidence.check_terminal state ~operation:binding.operation attempt)
      ~f:ignore
  | (Lost | Archived _), _ | Live, _ ->
    conflict "checkpoint attempt no longer has its exact live source"
;;

let moderator_digest (state : Session_state.t) =
  match state.moderator with
  | None -> Ok None
  | Some json ->
    let%map.Result () =
      Document_schema.Json.validate ~limits:Document_schema.Limits.default json
      |> Result.map_error ~f:(fun _ ->
        P.Error.invalid_request "checkpoint moderator source exceeds document limits")
    in
    Some (Chatml_source.Digest.of_contents (Jsonaf.to_string json) ~checkpoint:ignore)
;;

let instruction_digest (state : Session_state.t) =
  match state.instruction_selection with
  | None -> Ok None
  | Some selected ->
    let%map.Result json = Instruction_selection.to_jsonaf selected in
    Some (Chatml_source.Digest.of_contents (Jsonaf.to_string json) ~checkpoint:ignore)
;;

let instruction_revision (state : Session_state.t) =
  Option.map state.instruction_selection ~f:Instruction_selection.revision
;;

let source_context_matches
      (binding : Authored.t)
      ~configuration_revision
      ~prompt_revision
      ~instruction_revision
      ~instruction_digest
  =
  Int64.equal configuration_revision binding.configuration_revision
  && P.Id.Prompt_revision.equal prompt_revision binding.prompt_revision
  && Option.equal Int64.equal instruction_revision binding.instruction_revision
  && Option.equal Chatml_source.Digest.equal instruction_digest binding.instruction_digest
;;

let%test_unit
    "authored checkpoint source rejects independent prompt and instruction changes"
  =
  let ok = function
    | Ok value -> value
    | Error _ -> failwith "invalid checkpoint fixture"
  in
  let generator = P.Id.Generator.create ~bytes:(fun len -> String.make len 'a') in
  let digest text = Chatml_source.Digest.of_contents text ~checkpoint:ignore in
  let prompt = ok (P.Id.Prompt_revision.of_string "prv_original") in
  let other_prompt = ok (P.Id.Prompt_revision.of_string "prv_replacement") in
  let selected = Some (digest "captured instruction source") in
  let replacement = Some (digest "replacement instruction source") in
  let binding =
    ok
      (Authored.create
         ~principal:(P.Id.Principal.create_with generator)
         ~key:(digest "registration")
         ~operation:(P.Id.Operation.create_with generator)
         ~invocation:(P.Id.Invocation.create_with generator)
         ~call:(ok (P.History.Id.of_string "4:test:1"))
         ~attempt_ordinal:1L
         ~source_revision:1L
         ~configuration_revision:4L
         ~prompt_revision:prompt
         ~moderator:None
         ~catalog_revision:(ok (Recovery_notes.Catalog.Catalog_revision.of_int64 0L))
         ~pending_revision:P.Pending_input.Revision.zero
         ~instruction_revision:(Some 3L)
         ~instruction_digest:selected
         ~covered_rows:2)
  in
  let matches prompt_revision instruction_revision instruction_digest =
    source_context_matches
      binding
      ~configuration_revision:4L
      ~prompt_revision
      ~instruction_revision
      ~instruction_digest
  in
  assert (matches prompt (Some 3L) selected);
  assert (not (matches other_prompt (Some 3L) selected));
  assert (not (matches prompt (Some 4L) selected));
  assert (not (matches prompt (Some 3L) replacement));
  assert (not (matches prompt None None))
;;

let check_pending_revision (state : Session_state.t) (binding : Authored.t) =
  if
    P.Pending_input.Revision.equal
      state.conversation.pending_revision
      binding.pending_revision
  then Ok ()
  else conflict "authoritative pending input changed after checkpoint capture"
;;

let check_metadata (state : Session_state.t) ~scope binding =
  let open Result.Let_syntax in
  let%bind instructions = instruction_digest state in
  let%bind () =
    if
      source_context_matches
        binding
        ~configuration_revision:state.spec.configuration_revision
        ~prompt_revision:state.spec.prompt_revision_id
        ~instruction_revision:(instruction_revision state)
        ~instruction_digest:instructions
    then Ok ()
    else conflict "checkpoint configuration, prompt or instruction source changed"
  in
  let%bind () = check_source state ~scope binding in
  let%bind current = moderator_digest state in
  if not (Option.equal Chatml_source.Digest.equal binding.moderator current)
  then conflict "checkpoint effective moderator source changed"
  else (
    match state.recovery_notes with
    | Some catalog
      when Recovery_notes.Catalog.Catalog_revision.equal
             binding.catalog_revision
             (Recovery_notes.Catalog.revision catalog) -> Ok ()
    | Some _ | None -> conflict "checkpoint note catalog changed")
;;

let capture_checkpoint
      (state : Session_state.t)
      ~scope
      ~principal
      ~operation
      ~invocation
      ~key
  =
  let open Result.Let_syntax in
  let context = invocation.P.Invocation.context in
  let%bind call =
    Result.of_option
      context.call_entry_id
      ~error:(P.Error.invalid_request "checkpoint requires a canonical model call")
  in
  let%bind source_revision, attempt_ordinal =
    registration_attempt state ~scope ~operation ~call
  in
  let%bind catalog =
    Result.of_option
      state.recovery_notes
      ~error:(P.Error.invalid_request "checkpoint requires committed local notes")
  in
  let history = state.conversation.canonical_history in
  if Stdlib.List.compare_length_with history Recovery_window_record.max_members > 0
  then conflict "checkpoint exceeds the recovery window history bound"
  else (
    let%bind moderator = moderator_digest state in
    let%bind instructions = instruction_digest state in
    Authored.create
      ~principal
      ~key
      ~operation
      ~invocation:context.id
      ~call
      ~attempt_ordinal
      ~source_revision
      ~configuration_revision:state.spec.configuration_revision
      ~prompt_revision:state.spec.prompt_revision_id
      ~moderator
      ~catalog_revision:(Recovery_notes.Catalog.revision catalog)
      ~pending_revision:state.conversation.pending_revision
      ~instruction_revision:(instruction_revision state)
      ~instruction_digest:instructions
      ~covered_rows:(List.length history))
;;

let check_checkpoint_registration (state : Session_state.t) checkpoint =
  let open Result.Let_syntax in
  match Checkpoint.recipe checkpoint with
  | Owner_selection _ -> conflict "checkpoint is not an authored registration"
  | Authored binding ->
    let scope = (Checkpoint.expected checkpoint).scope in
    let%bind invocation = lookup_invocation state binding in
    let%bind () =
      if
        P.Id.Session.equal
          invocation.context.session_id
          (P.Session_ref.session_id (Scope.session scope))
        && Int.equal invocation.context.generation (Scope.generation scope)
        && P.Invocation.equal_status invocation.status Dispatching
      then Ok ()
      else conflict "checkpoint invocation is no longer dispatching at this cut"
    in
    let%bind request = Authored.Request.of_json invocation.context.input in
    let%bind () =
      match request.action with
      | Register notes
        when P.Id.Context_window.equal request.window (Scope.window scope)
             && Chatml_source.Digest.equal binding.key request.key
             && List.equal
                  Recovery_notes.Catalog.equal_selection
                  notes
                  (Checkpoint.notes checkpoint) -> Ok ()
      | Register _ | Readback ->
        conflict "checkpoint evidence differs from the selected call"
    in
    let%bind () = check_metadata state ~scope binding in
    let%bind () = check_pending_revision state binding in
    let%bind source_revision, ordinal =
      registration_attempt state ~scope ~operation:binding.operation ~call:binding.call
    in
    let history = state.conversation.canonical_history in
    let%bind () =
      if
        Int64.equal source_revision binding.source_revision
        && Int64.equal ordinal binding.attempt_ordinal
        && Stdlib.List.compare_length_with history binding.covered_rows = 0
      then Ok ()
      else conflict "checkpoint coverage or actual foreground attempt changed"
    in
    let%bind call = call_in history binding.call in
    check_call call
;;

let accepted = P.Invocation.Complete (Authored.Receipt.to_json Accepted)
let accepted_output = Jsonaf.to_string (P.Invocation.outcome_to_json accepted)

let check_acknowledgment state binding invocation ~covered ~suffix =
  let open Result.Let_syntax in
  let%bind output =
    Result.of_option
      invocation.P.Invocation.output_entry_id
      ~error:(P.Error.invalid_request "checkpoint result was not published")
  in
  let pair = B.{ call = binding.Authored.call; output } in
  let%bind () =
    if
      Evidence.published state pair invocation
      && P.Invocation.equal_status invocation.status (Published accepted)
    then Ok ()
    else conflict "checkpoint result is not the acknowledged registration"
  in
  let%bind call = call_in covered binding.call in
  let%bind output_row = call_in suffix output in
  let%bind () = Evidence.pair_is_bound ~call ~output:output_row in
  match%bind Evidence.semantics output_row with
  | Result { output = Text text; _ } when String.equal text accepted_output -> Ok output
  | _ -> conflict "checkpoint acknowledgment changed"
;;

let check_checkpoint ?checkpoint_history (state : Session_state.t) checkpoint =
  let open Result.Let_syntax in
  match Checkpoint.recipe checkpoint with
  | Owner_selection _ -> conflict "checkpoint is not an authored registration"
  | Authored binding ->
    let scope = (Checkpoint.expected checkpoint).scope in
    let%bind () = check_metadata state ~scope binding in
    let%bind () = check_pending_revision state binding in
    let%bind invocation = lookup_invocation state binding in
    let history = state.conversation.canonical_history in
    let covered, suffix = List.split_n history binding.covered_rows in
    let%bind () =
      Checkpoint.check_contents checkpoint ~history:covered ~catalog:state.recovery_notes
    in
    let%bind output = check_acknowledgment state binding invocation ~covered ~suffix in
    let pre_batch_suffix =
      match checkpoint_history with
      | None -> suffix
      | Some prefix -> List.drop prefix binding.covered_rows
    in
    (match pre_batch_suffix with
     | [] when Option.is_some checkpoint_history -> Ok ()
     | [ entry ] when P.History.Id.equal entry.id output -> Ok ()
     | _ -> conflict "substantive history follows the authored checkpoint")
;;
