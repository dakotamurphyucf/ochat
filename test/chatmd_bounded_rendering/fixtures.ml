open! Core
module P = History_entry.Payload

let id sequence =
  History_entry.Id.create ~namespace:"h" ~sequence |> Result.ok_or_failwith
;;

let entry sequence view =
  let payload =
    P.Semantic.create view ~metadata:P.Metadata.empty
    |> Result.ok_or_failwith
    |> P.authored
  in
  History_entry.create_with_id ~id:(id sequence) payload
;;

let examples () =
  [ entry
      0
      (Message
         { form = Output
         ; role = Assistant
         ; content = [ Text { text = "hello"; annotations = []; logprobs = Absent } ]
         ; phase = Absent
         })
  ; entry
      1
      (Call
         { kind = Custom
         ; name = "inspect"
         ; namespace = Value "tools"
         ; input_bytes = " { \"x\" : 1 } "
         ; async = Value true
         })
  ; entry
      2
      (Result
         { kind = Custom
         ; relation = Bound (id 1)
         ; output = Content [ Refusal "no"; Image { uri = "urn:image"; detail = Null } ]
         })
  ; entry 3 (Reasoning { readable_summary = [ "a"; "b" ] })
  ; entry 4 (Unknown { provider_kind = "future" })
  ]
;;

let limits
      ?(max_bytes = 1_000_000)
      ?(max_work_steps = 10_000_000)
      ?(json_limits = Document_schema.Limits.default)
      ()
  =
  match
    History_chatmd.Rendering.Limits.create ~max_bytes ~max_work_steps ~json_limits
  with
  | Ok limits -> limits
  | Error error -> raise_s [%sexp (error : History_chatmd.Rendering.Error.t)]
;;

let full entry =
  match Agent_protocol.Public.History.full entry ~provenance:Canonical with
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : Agent_protocol.Error.t)]
;;
