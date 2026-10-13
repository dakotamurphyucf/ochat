open! Core
module E = Session_decode_evidence
module P = Agent_protocol
module D = Document_schema

type t =
  { limits : D.Limits.t
  ; canonical_history : P.History.entry E.Sequence.t option
  ; invocations : P.Invocation.t E.Sequence.t option
  ; moderator_executions : P.Moderator_execution.t E.Sequence.t option
  ; inference_ledger : Inference_ledger.t E.Field.t option
  }

let ledger_of_jsonaf ~limits json =
  let open Result.Let_syntax in
  let%bind document =
    D.Document.inspect ~limits json
    |> Result.map_error ~f:(fun error ->
      P.Error.invalid_request (Sexp.to_string_hum (D.Error.sexp_of_t error)))
  in
  Inference_ledger.of_document document ~limits:Inference_ledger.Limits.default
  |> Result.map_error ~f:(fun error ->
    P.Error.invalid_request (Sexp.to_string_hum (Inference_ledger.Error.sexp_of_t error)))
;;

module Capture = struct
  type evidence = t
  type t = { mutable evidence : evidence }

  let create ?previous ~limits () =
    let evidence =
      match previous with
      | Some previous when D.Limits.equal previous.limits limits -> previous
      | None | Some _ ->
        { limits
        ; canonical_history = None
        ; invocations = None
        ; moderator_executions = None
        ; inference_ledger = None
        }
    in
    { evidence }
  ;;

  let sequence t json ~get ~set ~decode =
    let result =
      match get t.evidence with
      | None -> E.Sequence.admit ~identity_field:"id" ~decode json
      | Some previous -> E.Sequence.readmit previous ~json
    in
    Result.map result ~f:(fun admitted ->
      t.evidence <- set t.evidence admitted;
      E.Sequence.values admitted)
  ;;

  let canonical_history t json =
    sequence
      t
      json
      ~get:(fun evidence -> evidence.canonical_history)
      ~set:(fun evidence admitted -> { evidence with canonical_history = Some admitted })
      ~decode:P.History.entry_of_json
  ;;

  let invocations t json =
    sequence
      t
      json
      ~get:(fun evidence -> evidence.invocations)
      ~set:(fun evidence admitted -> { evidence with invocations = Some admitted })
      ~decode:P.Invocation.Storage.of_json
  ;;

  let moderator_executions t json =
    sequence
      t
      json
      ~get:(fun evidence -> evidence.moderator_executions)
      ~set:(fun evidence admitted ->
        { evidence with moderator_executions = Some admitted })
      ~decode:P.Moderator_execution.of_json
  ;;

  let inference_ledger t json =
    let result =
      match t.evidence.inference_ledger with
      | None -> E.Field.admit ~decode:(ledger_of_jsonaf ~limits:t.evidence.limits) json
      | Some previous -> E.Field.readmit previous ~json
    in
    Result.map result ~f:(fun admitted ->
      t.evidence <- { t.evidence with inference_ledger = Some admitted };
      E.Field.value admitted)
  ;;

  let finish t = t.evidence
end
