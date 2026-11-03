open! Core
module P = Agent_protocol
module Port = Instruction_source_port
module Command = Instruction_command
module Request = Instruction_source_request
module Admission = Instruction_native_admission

let invalid message = Error (P.Error.invalid_request message)

module Source = struct
  type t =
    { use : Port.Use.t
    ; request : Request.t
    ; captured : Port.Captured.t option
    }

  let fresh use ~request ~captured =
    let open Result.Let_syntax in
    let%bind () = Port.Captured.check_now captured ~use in
    if Request.matches request (Port.Captured.contents captured)
    then Ok { use; request; captured = Some captured }
    else invalid "instruction capture differs from descriptor"
  ;;

  let retry use ~request = { use; request; captured = None }
end

type decision =
  | Retained of Command.Receipt.t
  | Prepared of
      { selection : Instruction_selection.t
      ; receipt : Command.Receipt.t
      }

let effect_kind request =
  match Command.change request with
  | Command.Change.Replace_project _ | Replace_skills _ -> Admission.Effect.Activate
  | Remove_project | Remove_skills _ | Clear -> Deactivate
;;

let check_now (state : Session_state.t) ~admission ~(source : Source.t option) ~request =
  let open Result.Let_syntax in
  let%bind () = Admission.check_now admission state ~effect_kind:(effect_kind request) in
  if
    (not (P.Id.Session.equal (Command.session_id request) state.identity.session_id))
    || not (Int.equal (Command.generation request) state.identity.generation)
  then invalid "instruction command targets another session generation"
  else (
    match Command.change request, source with
    | (Command.Change.Remove_project | Remove_skills _ | Clear), None -> Ok ()
    | (Replace_project capture | Replace_skills capture), Some source ->
      let%bind () =
        Port.Use.check_now
          source.use
          ~roots:(Request.Selection.roots (Request.selection source.request))
      in
      if not (Request.matches source.request capture)
      then invalid "instruction replacement differs from selected source"
      else (
        match source.captured with
        | None -> Ok ()
        | Some captured ->
          let%bind () = Port.Captured.check_now captured ~use:source.use in
          if
            Document_schema.Json.equal
              (Instruction_capture.to_json capture)
              (Instruction_capture.to_json (Port.Captured.contents captured))
          then Ok ()
          else invalid "instruction replacement differs from genuine capture")
    | (Replace_project _ | Replace_skills _), None
    | (Remove_project | Remove_skills _ | Clear), Some _ ->
      invalid "instruction source/effect mismatch")
;;

let prepare state ~admission ~source ~request ~now =
  let open Result.Let_syntax in
  let%bind () = check_now state ~admission ~source ~request in
  let previous =
    Option.value
      state.Session_state.instruction_selection
      ~default:Instruction_selection.empty
  in
  let%bind decision =
    Instruction_selection.prepare
      previous
      ~request
      ~principal_id:(Operator_authorization.principal (Admission.actor admission)).id
      ~session_id:state.identity.session_id
      ~generation:state.identity.generation
      ~now
  in
  match decision with
  | Retained receipt -> Ok (Retained receipt)
  | Commit { selection; receipt } ->
    (match Command.change request, source with
     | (Command.Change.Replace_project _ | Replace_skills _), Some { captured = None; _ }
       -> invalid "fresh instruction replacement requires a genuine current capture"
     | ( (Replace_project _ | Replace_skills _ | Remove_project | Remove_skills _ | Clear)
       , _ ) -> Ok (Prepared { selection; receipt }))
;;
