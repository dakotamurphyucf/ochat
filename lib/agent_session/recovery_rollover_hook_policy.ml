open! Core

let error code message = Agent_protocol.Error.create code ~message ~retryable:false ()

let checkpoint_hints checkpoint =
  let module H = Recovery_rollover_hint_ref in
  List.take (Recovery_rollover.Checkpoint.notes checkpoint) H.Selection.max_references
  |> List.map ~f:H.local_note
;;

let checkpoint_offer = function
  | None -> Chat_response.Context_rollover_hook.Missing
  | Some selection ->
    Checkpointed
      (List.map (checkpoint_hints selection) ~f:Recovery_rollover_hint_ref.to_json)
;;

let resolve
      state
      ~(event : Chat_response.Context_rollover_hook.t)
      ~execution
      ~view_transcript
      decision
  =
  let module H = Recovery_rollover_hooks in
  let module Ref = Recovery_rollover_hint_ref in
  let module Hook = Chat_response.Context_rollover_hook in
  let open Result.Let_syntax in
  let id = execution in
  match event, (decision : Hook.Decision.t) with
  | Before _, Veto -> Ok (H.Complete_before (id, Veto))
  | Before { window; checkpoint; _ }, Proceed { hints; continuation } ->
    let%map hints =
      Option.value_map hints ~default:(Ok None) ~f:(fun hints ->
        let%bind refs = Ref.Selection.of_json (`Array hints) in
        let scoped () =
          Recovery_visibility.create
            state
            ~owner:(Agent_protocol.Recovery_window.Scope.session window)
        in
        let visibility = lazy (scoped ()) in
        let check reference =
          match (reference : Ref.t) with
          | Local_note _ ->
            (match checkpoint with
             | Checkpointed allowed ->
               let encoded = Ref.to_json reference in
               List.exists allowed ~f:(Jsonaf.exactly_equal encoded)
             | Missing | Archive_only -> false)
          | Canonical_text { citation; _ } ->
            view_transcript
            &&
              (match Lazy.force visibility with
              | Error _ -> false
              | Ok visibility ->
                (match Recovery_visibility.select visibility citation with
                 | Eligible _ -> true
                 | Unavailable | Withdrawn | Expired -> false))
          | Shared_note _ ->
            (* A reference is a bounded routing proposal. The other source's
               live grant and exact recipient are checked during materialization. *)
            true
        in
        if List.for_all (Ref.Selection.references refs) ~f:check
        then Ok (Some refs)
        else
          Error
            (error Permission_denied "rollover hook proposed unavailable source evidence"))
    in
    H.Complete_before (id, Proceed { hints; continuation })
  | After _, Followup decision -> Ok (Complete_after (id, decision))
  | _ -> Error (error Invalid_state "authored rollover decision does not match its phase")
;;

let choose_followup hooks ~default =
  let module H = Recovery_rollover_hooks in
  let module C = Recovery_rollover_continuation.Hook in
  match hooks with
  | None -> default
  | Some hooks ->
    (match H.before hooks, H.after hooks with
     | Completed (_, Proceed { continuation; _ }), Completed (_, after) ->
       (match continuation, after with
        | Some Finish, _ | _, Finish -> C.Finish
        | Some Wait, _ | _, Wait -> C.Wait
        | (None | Some Continue), Continue -> C.Continue)
     | _ -> C.Wait)
;;
