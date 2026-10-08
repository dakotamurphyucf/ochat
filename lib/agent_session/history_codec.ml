open! Core
module Payload = History_entry.Payload

let invalid message =
  Agent_protocol.Error.create Invalid_state ~message ~retryable:false ()
;;

let classification semantic =
  match Payload.Semantic.view semantic with
  | Message { role; _ } ->
    ( (match role with
       | Payload.Role.System | Developer -> Agent_protocol.History.System
       | User -> User
       | Assistant -> Assistant
       | Tool -> Tool)
    , Agent_protocol.History.Message )
  | Call _ -> Assistant, Tool_call
  | Result _ -> Tool, Tool_output
  | Reasoning _ -> Assistant, Reasoning
  | Unknown _ -> Assistant, Other
;;

let to_canonical ?(provenance = Agent_protocol.History.Canonical) entry =
  let payload = History_entry.payload entry in
  let role, kind = classification (Payload.semantic payload) in
  Agent_protocol.History.
    { id = History_entry.id entry
    ; role
    ; kind
    ; payload = Payload.to_json payload
    ; provenance
    ; redacted = false
    }
;;

let of_canonical entry =
  let open Result.Let_syntax in
  if entry.Agent_protocol.History.redacted
  then Error (invalid "redacted history cannot be canonical model input")
  else (
    let%bind () = Agent_protocol.History.validate_entry entry in
    let%bind payload = Payload.of_json entry.payload |> Result.map_error ~f:invalid in
    let role, kind = classification (Payload.semantic payload) in
    if
      not
        (Agent_protocol.History.equal_role role entry.role
         && Agent_protocol.History.equal_kind kind entry.kind)
    then
      Error
        (invalid
           "canonical history classification differs from its neutral semantic payload")
    else Ok (History_entry.create_with_id ~id:entry.id payload))
;;

let to_protocol = to_canonical
let of_protocol = of_canonical

let to_presentation ?provenance entry =
  let canonical = to_canonical ?provenance entry in
  Result.map
    (Openai.Responses_history.to_presentation_item (History_entry.payload entry))
    ~f:(fun item -> { canonical with payload = Openai.Responses.Item.jsonaf_of_t item })
  |> Result.map_error ~f:invalid
;;

let canonical_encoder ~previous =
  let provenance = Hashtbl.create (module History_entry.Id) in
  List.iter previous ~f:(fun entry ->
    Hashtbl.set provenance ~key:entry.Agent_protocol.History.id ~data:entry.provenance);
  fun entry ->
    to_canonical ?provenance:(Hashtbl.find provenance (History_entry.id entry)) entry
;;

let all_to_protocol ?(previous = []) entries =
  List.map entries ~f:(canonical_encoder ~previous)
;;

let all_of_protocol entries = Result.all (List.map entries ~f:of_canonical)

let user_text ~id text =
  let semantic =
    Payload.Semantic.create
      (Message
         { form = Input
         ; role = User
         ; content =
             [ Payload.Content.Text { text; annotations = []; logprobs = Absent } ]
         ; phase = Absent
         })
      ~metadata:Payload.Metadata.empty
    |> Result.ok_or_failwith
  in
  History_entry.create_with_id ~id (Payload.authored semantic)
;;
