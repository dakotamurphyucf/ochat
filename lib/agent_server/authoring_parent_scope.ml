open! Core
module P = Agent_protocol
module Bundle = P.Authoring_bundle.Bundle
module Store = Agent_store.Authoring_upload_store
module Upload = Authoring_upload_host
module Ledger = Chatml_authoring_context
module Sources = Authoring_source_host
module Rows = Authoring_document_selection_rows
module Selection = Authoring_script_selection
module Markdown = Prompt.Chat_markdown
module Binding = Chatml_prompt_binding
module Captured = Chatml_source

module Error = struct
  type t =
    | Foreign_operation
    | Stale_parent
    | Unauthorized
    | Busy
    | Prior_unavailable
  [@@deriving sexp_of]
end

module Operation = Upload.Parent_admission.Operation

module Origin = struct
  type selected_document_evidence =
    { capture : Sources.Document_context.Parsed_capture.t
    ; rows : Rows.t
    ; original_actor : Operator_authorization.t
    ; script_selection : Selection.Policy.t option
    }

  type document_evidence =
    { inputs : Ledger.Document.Inputs.t
    ; selected : selected_document_evidence option
    }

  type t =
    | Project of Chatml_project_build.t
    | Document of document_evidence
    | Uploaded_project of
        { previous : Chatml_project_build.t
        ; pin : Store.Published_pin.t
        }
end

type document_witness =
  { inputs : Ledger.Document.Inputs.t
  ; selected_evidence : Origin.selected_document_evidence
  ; parsed : Markdown.Parsed.t
  ; declaration : Markdown.Declaration.t
  ; metadata : Sources.Document_context.Project_capture.t option
  ; selected : Selection.Selected.t
  }

type t =
  { operation : Operation.t
  ; host : P.Id.Server.t
  ; connection : Connection_context.t
  ; actor : Operator_authorization.t
  ; expected : P.Authoring.Context.Expected.t
  ; live : bool ref
  ; domain : int
  ; check_current_now : unit -> (unit, Error.t) result
  ; origin : Origin.t
  ; mutable selected_document_witnesses : document_witness list
  ; mutable selected_document_witness_count : int
  }

let domain () = (Stdlib.Domain.self () :> int)

let check_now t ~operation ~host ~connection ~actor ~expected =
  if (not !(t.live)) || not (Int.equal t.domain (domain ()))
  then Error Error.Stale_parent
  else if not (Operation.equal t.operation operation)
  then Error Error.Foreign_operation
  else if
    (not (P.Id.Server.equal t.host host))
    || (not (phys_equal t.connection connection))
    || (not (phys_equal t.actor actor))
    || not (P.Authoring.Context.Expected.equal t.expected expected)
  then Error Error.Stale_parent
  else
    let open Result.Let_syntax in
    let%bind () = t.check_current_now () in
    List.fold_result
      t.selected_document_witnesses
      ~init:()
      ~f:(fun () { inputs; selected_evidence; parsed; declaration; metadata; selected } ->
        match selected_evidence.script_selection with
        | None -> Error Error.Prior_unavailable
        | Some policy ->
          if
            Selection.Policy.same_now
              policy
              ~original_actor:selected_evidence.original_actor
              ~actor:t.actor
              ~current:inputs
              ~parsed
              ~declaration
              ~metadata
              ~selected
          then Ok ()
          else Error Error.Prior_unavailable)
;;

let issue ~operation ~host ~connection ~actor ~expected ~live ~check_current_now ~origin =
  let t =
    { operation
    ; host
    ; connection
    ; actor
    ; expected
    ; live
    ; domain = domain ()
    ; check_current_now
    ; origin
    ; selected_document_witnesses = []
    ; selected_document_witness_count = 0
    }
  in
  let open Result.Let_syntax in
  let%bind () = check_now t ~operation ~host ~connection ~actor ~expected in
  match origin with
  | Origin.Document { inputs; selected = Some { capture; rows; _ } } ->
    if Rows.matches rows ~inputs ~capture then Ok t else Error Error.Stale_parent
  | Origin.Document { selected = None; _ } -> Ok t
  | Project _ | Uploaded_project _ -> Ok t
;;

let same_source_metadata source metadata =
  Captured.Id.equal (Captured.id source) (P.Authoring.Source.id metadata)
  && Captured.Digest.equal (Captured.digest source) (P.Authoring.Source.digest metadata)
  && Int.equal (Captured.byte_length source) (P.Authoring.Source.byte_length metadata)
;;

let selected_project manifest ~selector ~checkpoint =
  match Bundle.Manifest.basis manifest, selector with
  | Project project, None -> Ok project
  | Chatmd_document document, Some selector ->
    let matches =
      List.filter (Bundle.Manifest.Chatmd_document.scripts document) ~f:(fun script ->
        checkpoint ();
        Bundle.Manifest.Script.Selector.equal
          selector
          (Bundle.Manifest.Script.selector script))
    in
    (match matches with
     | [ script ] ->
       (match Bundle.Manifest.Script.choice script with
        | Source_only -> Error Error.Prior_unavailable
        | Selected_project project -> Ok project)
     | [] | _ :: _ -> Error Error.Prior_unavailable)
  | Project _, Some _ | Chatmd_document _, None -> Error Error.Prior_unavailable
;;

let source_for_input selected_evidence input ~checkpoint =
  let matches =
    List.filter
      (Sources.Document_context.Parsed_capture.input_sources
         selected_evidence.Origin.capture)
      ~f:(fun (candidate, _) ->
        checkpoint ();
        phys_equal candidate input)
  in
  match matches with
  | [ (_, source) ] -> Ok source
  | [] | _ :: _ -> Error Error.Prior_unavailable
;;

let selector_for_declaration selected_evidence parsed declaration ~checkpoint =
  let open Result.Let_syntax in
  let%bind binding =
    Binding.of_declaration parsed declaration ~checkpoint
    |> Result.map_error ~f:(fun _ -> Error.Prior_unavailable)
  in
  let%bind registration =
    source_for_input selected_evidence (Binding.registration_input binding) ~checkpoint
  in
  let%bind declaring =
    source_for_input selected_evidence (Binding.parser_input binding) ~checkpoint
  in
  Bundle.Manifest.Script.Selector.create
    ~registration_source:(Captured.id registration)
    ~declaration_source:(Captured.id declaring)
    ~start_byte:(Binding.start_byte binding)
    ~end_byte:(Binding.end_byte binding)
    ~script_id:(Binding.script_id binding)
    ~body_digest:(Binding.script_digest binding)
    ~body_byte_length:(String.length (Markdown.Declaration.script_body declaration))
  |> Result.map_error ~f:(fun _ -> Error.Prior_unavailable)
;;

let document_previous t evidence selector ~checkpoint =
  let open Result.Let_syntax in
  let%bind selected_evidence =
    evidence.Origin.selected |> Result.of_option ~error:Error.Prior_unavailable
  in
  let parsed_bundle =
    Sources.Document_context.Parsed_capture.parsed selected_evidence.capture
    |> Markdown.Generated.parsed
  in
  let parsed_units =
    parsed_bundle.parsed_root :: List.map parsed_bundle.parsed_agents ~f:snd
  in
  let%bind found =
    List.fold_result parsed_units ~init:None ~f:(fun found parsed ->
      List.fold_result
        (Markdown.Parsed.declarations parsed)
        ~init:found
        ~f:(fun found declaration ->
          checkpoint ();
          let%bind actual =
            selector_for_declaration selected_evidence parsed declaration ~checkpoint
          in
          if not (Bundle.Manifest.Script.Selector.equal actual selector)
          then Ok found
          else (
            match found with
            | Some _ -> Error Error.Prior_unavailable
            | None ->
              let%bind row =
                Rows.find
                  selected_evidence.rows
                  ~capture:selected_evidence.capture
                  ~declaration
                  ~checkpoint
                |> Result.map_error ~f:(fun _ -> Error.Prior_unavailable)
              in
              (match row with
               | None -> Error Error.Prior_unavailable
               | Some row ->
                 (match Rows.Row.status row with
                  | Host_prepared { selected; _ } ->
                    Ok (Some (Selection.Selected.build selected, None))
                  | Independently_selected selected ->
                    (match selected_evidence.script_selection with
                     | None -> Error Error.Prior_unavailable
                     | Some policy ->
                       let metadata = Rows.Row.metadata row in
                       if
                         Selection.Policy.same_now
                           policy
                           ~original_actor:selected_evidence.original_actor
                           ~actor:t.actor
                           ~current:evidence.inputs
                           ~parsed
                           ~declaration
                           ~metadata
                           ~selected
                       then
                         Ok
                           (Some
                              ( Selection.Selected.build selected
                              , Some
                                  { inputs = evidence.inputs
                                  ; selected_evidence
                                  ; parsed
                                  ; declaration
                                  ; metadata
                                  ; selected
                                  } ))
                       else Error Error.Prior_unavailable)
                  | No_validated_build _ -> Error Error.Prior_unavailable)))))
  in
  found |> Result.of_option ~error:Error.Prior_unavailable
;;

let previous_for t manifest ~selector ~checkpoint =
  let open Result.Let_syntax in
  let%bind () =
    check_now
      t
      ~operation:t.operation
      ~host:t.host
      ~connection:t.connection
      ~actor:t.actor
      ~expected:t.expected
  in
  let%bind project = selected_project manifest ~selector ~checkpoint in
  let%bind previous =
    match Bundle.Manifest.Project.mode project with
    | Exact_lock _ -> Ok None
    | Compose { selected_previous_lock = None; _ } -> Ok None
    | Compose { selected_previous_lock = Some metadata; _ } ->
      let%bind build, witness =
        match t.origin with
        | Origin.Project build -> Ok (build, None)
        | Uploaded_project { previous; _ } -> Ok (previous, None)
        | Document evidence ->
          (match selector with
           | None -> Error Error.Prior_unavailable
           | Some selector -> document_previous t evidence selector ~checkpoint)
      in
      if same_source_metadata (Chatml_project_build.lock_source build) metadata
      then (
        match witness with
        | None -> Ok (Some build)
        | Some witness ->
          checkpoint ();
          if t.selected_document_witness_count >= Bundle.Wire_limits.max_members
          then Error Error.Prior_unavailable
          else (
            t.selected_document_witnesses <- witness :: t.selected_document_witnesses;
            t.selected_document_witness_count <- t.selected_document_witness_count + 1;
            Ok (Some build)))
      else Error Error.Prior_unavailable
  in
  let%map () =
    check_now
      t
      ~operation:t.operation
      ~host:t.host
      ~connection:t.connection
      ~actor:t.actor
      ~expected:t.expected
  in
  previous
;;

let parent_pin t =
  match t.origin with
  | Origin.Uploaded_project { pin; _ } -> Some pin
  | Project _ | Document _ -> None
;;

let host_error = function
  | Error.Foreign_operation | Stale_parent -> Upload.Error.Stale_capture
  | Unauthorized -> Upload.Error.Unauthorized
  | Busy -> Upload.Error.Busy
  | Prior_unavailable -> Upload.Error.Selection_unavailable
;;

let admission t =
  Upload.Parent_admission.create
    ~host_id:t.host
    ~expected:t.expected
    ~check_now:(fun operation ~connection ~actor ~expected ->
      check_now t ~operation ~host:t.host ~connection ~actor ~expected
      |> Result.map_error ~f:host_error)
    ~previous_for:(fun manifest ~selector ~checkpoint ->
      previous_for t manifest ~selector ~checkpoint |> Result.map_error ~f:host_error)
    ~parent_pin:(parent_pin t)
    ()
;;
