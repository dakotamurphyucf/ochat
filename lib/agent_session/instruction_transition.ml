open! Core
module P = Agent_protocol

type decision =
  | Retained of Instruction_command.Receipt.t
  | Prepared of
      { transition : Session_transition.t
      ; receipt : Instruction_command.Receipt.t
      }

let prepare basis ~request ~principal_id ~now ~limits =
  let open Result.Let_syntax in
  let previous = Session_state_document.value basis in
  let selection =
    Option.value previous.instruction_selection ~default:Instruction_selection.empty
  in
  let%bind decision =
    Instruction_selection.prepare
      selection
      ~request
      ~principal_id
      ~session_id:previous.identity.session_id
      ~generation:previous.identity.generation
      ~now
  in
  match decision with
  | Instruction_selection.Retained receipt -> Ok (Retained receipt)
  | Commit { selection; receipt } ->
    let%bind transition =
      Session_transition.apply
        ~now
        previous
        ~delta:(Instruction_selection_changed selection)
        ~payloads:[]
    in
    let%map _ =
      Session_document_transition.admit
        basis
        ~delta:transition.delta
        ~next:transition.state
        ~limits
      |> Result.map_error ~f:(fun error ->
        P.Error.invalid_request
          (Sexp.to_string_hum (Document_schema.Error.sexp_of_t error)))
    in
    Prepared { transition; receipt }
;;
