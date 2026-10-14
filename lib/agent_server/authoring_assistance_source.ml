open! Core
module Current = Authoring_context_service_core.Current
module Ledger = Chatml_authoring_context
module Wire_source = Agent_protocol.Authoring.Source
module Context = Agent_protocol.Authoring.Context
module Error = Authoring_context_service_core.Error

type basis =
  | Project of Ledger.Inputs.t
  | Document of Ledger.Document.Inputs.t

let same_revision actual requested =
  Chatml_source.Id.equal (Chatml_source.id actual) (Wire_source.id requested)
  && Chatml_source.Digest.equal
       (Chatml_source.digest actual)
       (Wire_source.digest requested)
  && Int.equal (Chatml_source.byte_length actual) (Wire_source.byte_length requested)
;;

let admit current ~source ~checkpoint =
  let open Result.Let_syntax in
  let%bind actual, basis =
    match Current.basis current with
    | Current.Project ->
      let%bind inputs = Current.project_inputs current ~checkpoint in
      (match
         List.find (Ledger.Inputs.sources inputs) ~f:(fun candidate ->
           checkpoint ();
           Chatml_source.Id.equal (Chatml_source.id candidate) (Wire_source.id source))
       with
       | None -> Error Error.Source_unavailable
       | Some candidate -> Ok (candidate, Project inputs))
    | Current.Document ->
      let%map inputs = Current.document_inputs current ~checkpoint in
      Ledger.Document.Inputs.source inputs, Document inputs
  in
  let editable = Context.editable_sources current.context.view in
  let editor_revision =
    List.find_map editable ~f:(fun candidate ->
      checkpoint ();
      if Chatml_source.Id.equal (Wire_source.id candidate) (Wire_source.id source)
      then Some (Wire_source.editor_revision candidate)
      else None)
    |> Option.value ~default:None
  in
  if
    same_revision actual source
    && Option.equal Int.equal editor_revision (Wire_source.editor_revision source)
  then Ok (actual, basis)
  else Error Error.Stale_source
;;
