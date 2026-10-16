open! Core
module P = Agent_protocol
module Capture = Chatml_developer_capture
module Contract = Chatml.Chatml_compiler_contract
module Sources = Authoring_source_host
module Ledger = Chatml_authoring_context
module Build = Chatml_project_build
module Prepared = Chatml_prepared_program
module Captured = Chatml_source
module Owner = Authoring_context_service_core
module Scope = Authoring_context_owner

let error code message = P.Error.create code ~message ~retryable:false ()
let unavailable () = error P.Error.Workspace_unavailable "developer source unavailable"
let stale () = error P.Error.Conflict "developer capture changed"
let denied () = error P.Error.Permission_denied "developer source denied"
let invalid () = error P.Error.Invalid_request "invalid developer capture request"
let limit () = error P.Error.Resource_limit "developer capture limit"

let source_error = function
  | Sources.Error.Unauthorized | Expired_actor -> denied ()
  | Context_limit | Result_limit | Document_bundle_limit
  | Document_parser Prompt.Chat_markdown.Generated.Parser_limit -> limit ()
  | Busy -> error P.Error.Conflict "developer source busy"
  | Closed | Foreign_domain -> stale ()
  | Foreign_root
  | Unsupported_source_selection
  | Sources _
  | Root _
  | Local _
  | Project _
  | Source_capture _
  | Document_parser _
  | Project_declaration_unavailable
  | Invalid_project_declaration
  | Selected_script_unavailable -> unavailable ()
;;

let owner_error error = Owner.Error.to_protocol error

let capture_error = function
  | Capture.Error.Input_limit -> limit ()
  | Invalid_origin
  | Stale_source _
  | Duplicate_source _
  | Incompatible_contract
  | Unsupported_target
  | Invalid_entrypoint ->
    error P.Error.Invalid_request "invalid developer graph or entrypoint"
;;

module Limits = struct
  type t =
    { max_prepared_input_bytes : int
    ; max_document_origin_bytes : int
    ; max_editor_versions : int
    ; build : Build.Limits.t
    }

  let create
        ~max_prepared_input_bytes
        ~max_document_origin_bytes
        ~max_editor_versions
        ~build
    =
    if
      max_prepared_input_bytes <= 0
      || max_prepared_input_bytes > 16 * 1024 * 1024
      || max_document_origin_bytes <= 0
      || max_document_origin_bytes > 1024 * 1024
      || max_editor_versions <= 0
      || max_editor_versions > 4096
    then Or_error.error_string "invalid developer capture limits"
    else
      Ok
        { max_prepared_input_bytes
        ; max_document_origin_bytes
        ; max_editor_versions
        ; build
        }
  ;;
end

module Selected_contract = struct
  module Tool_fixture = struct
    type t =
      { name : string
      ; input_schema : Chatmd_shell_spec.Tool_schema.t
      ; output_schema : Chatmd_shell_spec.Tool_schema.t
      }

    let name t = t.name
    let input_schema t = t.input_schema
    let output_schema t = t.output_schema
  end

  type t =
    { contract : Contract.t
    ; fingerprint : Captured.Digest.t
    ; identity_bytes : int
    ; tool_fixture : Tool_fixture.t option
    }

  let of_standard_entry entry =
    let contract = Authoring_standard_contracts.Entry.contract entry in
    if
      not
        (match Contract.kind contract with
         | Contract.Kind.Standard Contract.Target.One_off_v1 -> true
         | Standard _ | Native -> false)
    then Error (invalid ())
    else
      Ok
        { contract
        ; fingerprint = Authoring_standard_contracts.Entry.fingerprint entry
        ; identity_bytes = Authoring_standard_contracts.Entry.identity_bytes entry
        ; tool_fixture = None
        }
  ;;

  let of_standard_tool_entry entry ~tool_name ~input_schema ~output_schema =
    let contract = Authoring_standard_contracts.Entry.contract entry in
    if
      String.is_empty tool_name
      || String.length tool_name > 256
      || not
           (match Contract.kind contract with
            | Contract.Kind.Standard Contract.Target.Tool_v1 -> true
            | Standard _ | Native -> false)
    then Error (invalid ())
    else (
      let input = Chatmd_shell_spec.Tool_schema.to_json input_schema
      and output = Chatmd_shell_spec.Tool_schema.to_json output_schema in
      let limits =
        Document_schema.Limits.create
          ~max_bytes:1_048_576
          ~max_depth:128
          ~max_fields:100_000
          ~max_nodes:100_000
      in
      let open Result.Let_syntax in
      let%bind limits = limits |> Result.map_error ~f:(fun _ -> invalid ()) in
      let%bind input_bytes =
        Document_schema.Json.validate_and_measure_with_checkpoint
          ~limits
          ~checkpoint:(fun () -> ())
          input
        |> Result.map_error ~f:(fun _ -> invalid ())
      in
      let%bind output_bytes =
        Document_schema.Json.validate_and_measure_with_checkpoint
          ~limits
          ~checkpoint:(fun () -> ())
          output
        |> Result.map_error ~f:(fun _ -> invalid ())
      in
      let identity =
        [%sexp
          (Captured.Digest.to_hex (Authoring_standard_contracts.Entry.fingerprint entry)
           : string)
        , (tool_name : string)
        , (Jsonaf.to_string input : string)
        , (Jsonaf.to_string output : string)]
        |> Sexp.to_string_mach
      in
      Ok
        { contract
        ; fingerprint = Captured.Digest.of_contents identity ~checkpoint:(fun () -> ())
        ; identity_bytes =
            Authoring_standard_contracts.Entry.identity_bytes entry
            + String.length tool_name
            + input_bytes
            + output_bytes
        ; tool_fixture =
            Some { Tool_fixture.name = tool_name; input_schema; output_schema }
        })
  ;;

  let contract t = t.contract
  let tool_fixture t = t.tool_fixture

  let same_identity a b =
    Int.equal a.identity_bytes b.identity_bytes
    && Captured.Digest.equal a.fingerprint b.fingerprint
  ;;
end

module Request = struct
  type t =
    { entrypoint : string
    ; target : Contract.Target.t
    ; input : Jsonaf.t
    ; fixtures : Capture.Fixture.t option
    ; json_bounds : Capture.Json_bounds.t
    }

  let create ~entrypoint ~target ~input ~fixtures ~json_bounds ~checkpoint =
    if String.is_empty entrypoint || String.length entrypoint > 256
    then Error (invalid ())
    else (
      match Capture.Json_bounds.measure json_bounds input ~checkpoint with
      | Error _ -> Error (limit ())
      | Ok _ -> Ok { entrypoint; target; input; fixtures; json_bounds })
  ;;
end

module Target_policy = struct
  type t =
    { local_project :
        Operator_authorization.t
        -> root:Sources.Root.t
        -> project:Source.Portable_path.t
        -> selection:Chatml_project.Selection.request
        -> Selected_contract.t option
    ; local_source :
        Operator_authorization.t
        -> root:Sources.Root.t
        -> member:Source.Portable_path.t
        -> Selected_contract.t option
    ; authoring_project :
        Operator_authorization.t
        -> context:P.Authoring.Context.t
        -> current:Ledger.Inputs.t
        -> Selected_contract.t option
    ; document_project :
        Operator_authorization.t
        -> context:P.Authoring.Context.t
        -> current:Ledger.Document.Inputs.t
        -> parsed:Prompt.Chat_markdown.Parsed.t
        -> declaration:Prompt.Chat_markdown.Declaration.t
        -> build:Build.t
        -> metadata:Sources.Document_context.Project_capture.t option
        -> Selected_contract.t option
    ; document_source :
        Operator_authorization.t
        -> context:P.Authoring.Context.t
        -> current:Ledger.Document.Inputs.t
        -> parsed:Prompt.Chat_markdown.Parsed.t
        -> declaration:Prompt.Chat_markdown.Declaration.t
        -> script:Captured.t
        -> Selected_contract.t option
    }

  let create
        ~local_project
        ~local_source
        ~authoring_project
        ~document_project
        ~document_source
    =
    { local_project; local_source; authoring_project; document_project; document_source }
  ;;
end

module Local_project = struct
  type t =
    { root : Sources.Root.t
    ; dependency_roots : Sources.Root.t list
    ; project : Source.Portable_path.t
    ; selection : Chatml_project.Selection.request
    }

  let create ~root ~dependency_roots ~project ~selection =
    { root; dependency_roots; project; selection }
  ;;
end

module Local_source = struct
  type t =
    { root : Sources.Root.t
    ; member : Source.Portable_path.t
    }

  let create ~root ~member = { root; member }
end

module Document_script = struct
  type t = { selector : P.Authoring.Analysis.Target.selector }

  let create ~selector = { selector }
end

type t =
  { sources : Sources.t option
  ; authoring : Owner.t
  ; policy : Target_policy.t
  ; limits : Limits.t
  }

let create ~authoring ~policy ~limits =
  { sources = authoring.Owner.sources; authoring; policy; limits }
;;

let same_actor original actor =
  Operator_authorization.is_current original
  && Operator_authorization.is_current actor
  && P.Id.Principal.equal
       (Operator_authorization.principal original).id
       (Operator_authorization.principal actor).id
;;

let selected_for_pair ~original_actor ~actor ~selected select =
  if not (same_actor original_actor actor)
  then Error (denied ())
  else (
    match select original_actor, select actor with
    | Some original, Some current
      when Selected_contract.same_identity selected original
           && Selected_contract.same_identity selected current -> Ok ()
    | Some _, Some _ -> Error (stale ())
    | Some _, None | None, Some _ | None, None -> Error (denied ()))
;;

module Candidate = struct
  type t =
    { capture : Capture.t
    ; original_actor : Operator_authorization.t
    ; request_actor : Operator_authorization.t
    ; selected : Selected_contract.t
    ; full_owner :
        actor:Operator_authorization.t
        -> checkpoint:(unit -> unit)
        -> (unit, P.Error.t) result
    ; owner_now : actor:Operator_authorization.t -> (unit, P.Error.t) result
    ; selection : Operator_authorization.t -> Selected_contract.t option
    ; domain : int
    ; mutable busy : bool
    ; mutable checked_nonce : unit ref option
    }

  type candidate = t

  module Checked = struct
    type t =
      { candidate : candidate
      ; actor : Operator_authorization.t
      ; nonce : unit ref
      }
  end

  let capture t = t.capture
  let selected_contract t = t.selected

  let check_domain t =
    if Int.equal t.domain (Stdlib.Domain.self () :> int) then Ok () else Error (stale ())
  ;;

  let check_current t ~actor ~checkpoint =
    let open Result.Let_syntax in
    let%bind () = check_domain t in
    if t.busy
    then Error (error P.Error.Conflict "developer capture busy")
    else (
      t.busy <- true;
      t.checked_nonce <- None;
      let nonce = ref () in
      let outcome =
        Exn.protect
          ~f:(fun () ->
            let%bind () =
              selected_for_pair
                ~original_actor:t.original_actor
                ~actor
                ~selected:t.selected
                t.selection
            in
            let%bind () = t.full_owner ~actor ~checkpoint in
            let%bind () = t.owner_now ~actor in
            selected_for_pair
              ~original_actor:t.original_actor
              ~actor
              ~selected:t.selected
              t.selection)
          ~finally:(fun () -> t.busy <- false)
      in
      match outcome with
      | Error _ as error -> error
      | Ok () ->
        t.checked_nonce <- Some nonce;
        Ok { Checked.candidate = t; actor; nonce })
  ;;

  let check_now t ~(checked : Checked.t) ~actor =
    let open Result.Let_syntax in
    let%bind () = check_domain t in
    if
      t.busy
      || (not (phys_equal checked.candidate t))
      || (not (phys_equal checked.actor actor))
      || not
           (Option.value_map t.checked_nonce ~default:false ~f:(phys_equal checked.nonce))
    then Error (stale ())
    else (
      let%bind () = t.owner_now ~actor in
      selected_for_pair
        ~original_actor:t.original_actor
        ~actor
        ~selected:t.selected
        t.selection)
  ;;

  let final_admission t ~actor ~capture ~checkpoint =
    if not (phys_equal capture t.capture && phys_equal actor t.request_actor)
    then Error (stale ())
    else
      Result.map (check_current t ~actor ~checkpoint) ~f:(fun checked () ->
        check_now t ~checked ~actor)
  ;;
end

let make_candidate
      ~capture
      ~original_actor
      ~request_actor
      ~selected
      ~full_owner
      ~owner_now
      ~selection
  =
  ({ Candidate.capture
   ; original_actor
   ; request_actor
   ; selected
   ; full_owner
   ; owner_now
   ; selection
   ; domain = (Stdlib.Domain.self () :> int)
   ; busy = false
   ; checked_nonce = None
   }
   : Candidate.t)
;;

let selected_initial ~original_actor ~actor select =
  if not (same_actor original_actor actor)
  then Error (denied ())
  else (
    match select original_actor, select actor with
    | Some first, Some second when Selected_contract.same_identity first second ->
      Ok first
    | Some _, Some _ -> Error (stale ())
    | Some _, None | None, Some _ | None, None -> Error (denied ()))
;;

let capture_program ~prepared ~origin ~selected ~request ~checkpoint =
  let selected_target =
    match Contract.kind (Selected_contract.contract selected) with
    | Contract.Kind.Standard target -> Some target
    | Native -> None
  in
  if
    not
      (Option.value_map
         selected_target
         ~default:false
         ~f:(Contract.Target.equal request.Request.target))
  then Error (stale ())
  else
    Capture.create_prepared
      ~prepared
      ~origin
      ~target:request.Request.target
      ~entrypoint:request.Request.entrypoint
      ~input:request.input
      ~fixtures:request.fixtures
      ~json_bounds:request.json_bounds
      ~checkpoint
    |> Result.map_error ~f:capture_error
;;

let local_revision digest = "capture:" ^ Captured.Digest.to_hex digest

let local_origin prepared =
  let graph = Prepared.fingerprint prepared in
  Capture.Origin.local_saved ~graph ~source_revision:(local_revision graph)
  |> Result.map_error ~f:(fun _ -> invalid ())
;;

let local_owner_now sources roots ~actor =
  let open Result.Let_syntax in
  List.fold roots ~init:(Ok ()) ~f:(fun result root ->
    let%bind () = result in
    Sources.check_root_current sources ~actor root |> Result.map_error ~f:source_error)
;;

let prepare_local_project
      t
      ~original_actor
      ~actor
      ~(local : Local_project.t)
      ~request
      ~clock
      ~checkpoint
  =
  let open Result.Let_syntax in
  let%bind sources = t.sources |> Result.of_option ~error:(unavailable ()) in
  let select actor =
    t.policy.local_project
      actor
      ~root:local.root
      ~project:local.project
      ~selection:local.selection
  in
  let%bind selected = selected_initial ~original_actor ~actor select in
  let%bind context =
    Sources.Context.open_root
      sources
      ~actor:original_actor
      ~root:local.root
      ~dependency_roots:local.dependency_roots
      ~project:local.project
      ~selection:local.selection
      ~contract:(Selected_contract.contract selected)
    |> Result.map_error ~f:source_error
  in
  let%bind acquired =
    Exn.protect
      ~f:(fun () ->
        Sources.Context.capture context ~actor ~clock ~checkpoint
        |> Result.map_error ~f:source_error)
      ~finally:(fun () -> Sources.Context.close_and_wait context)
  in
  let prepared = Sources.Capture.prepared acquired in
  let%bind origin = local_origin prepared in
  let%bind capture = capture_program ~prepared ~origin ~selected ~request ~checkpoint in
  let roots = local.root :: local.dependency_roots in
  let owner_now ~actor = local_owner_now sources roots ~actor in
  let full_owner ~actor ~checkpoint =
    checkpoint ();
    owner_now ~actor
  in
  let%bind () = owner_now ~actor in
  let%bind () = selected_for_pair ~original_actor ~actor ~selected select in
  Ok
    (make_candidate
       ~capture
       ~original_actor
       ~request_actor:actor
       ~selected
       ~full_owner
       ~owner_now
       ~selection:select)
;;

let prepare_local_source
      t
      ~original_actor
      ~actor
      ~(source : Local_source.t)
      ~request
      ~checkpoint
  =
  let open Result.Let_syntax in
  let%bind sources = t.sources |> Result.of_option ~error:(unavailable ()) in
  let select actor =
    t.policy.local_source actor ~root:source.root ~member:source.member
  in
  let%bind selected = selected_initial ~original_actor ~actor select in
  let%bind observed =
    Sources.read_root_member
      sources
      ~actor
      ~root:source.root
      ~member:source.member
      ~checkpoint
    |> Result.map_error ~f:source_error
  in
  let captured = Sources.Document_capture.source observed in
  let%bind prepared =
    Prepared.single_captured
      ~compiler_contract:
        (Selected_contract.contract selected |> Contract.fingerprint ~checkpoint)
      ~source:captured
      ~max_input_bytes:t.limits.max_prepared_input_bytes
      ~checkpoint
      ()
    |> Result.map_error ~f:(fun _ -> limit ())
  in
  let%bind origin = local_origin prepared in
  let%bind capture = capture_program ~prepared ~origin ~selected ~request ~checkpoint in
  let owner_now ~actor = local_owner_now sources [ source.root ] ~actor in
  let full_owner ~actor ~checkpoint =
    checkpoint ();
    owner_now ~actor
  in
  let%bind () = owner_now ~actor in
  let%bind () = selected_for_pair ~original_actor ~actor ~selected select in
  Ok
    (make_candidate
       ~capture
       ~original_actor
       ~request_actor:actor
       ~selected
       ~full_owner
       ~owner_now
       ~selection:select)
;;

let overlay_error = function
  | Authoring_project_overlay.Error.Context error -> owner_error error
  | No_selected_contract | Invalid_overlay _ | Lock_identity_unavailable -> unavailable ()
  | Work_limit -> limit ()
;;

let editor_versions current inputs ~max_versions ~checkpoint =
  let open Result.Let_syntax in
  let changed = Ledger.Inputs.changed_sources inputs in
  if List.length changed > max_versions
  then Error (limit ())
  else
    List.fold changed ~init:(Ok []) ~f:(fun result source ->
      let%bind versions = result in
      checkpoint ();
      match
        Ledger.version
          current.Owner.Current.owner
          ~identity:(Ledger.Inputs.identity inputs)
          ~source
      with
      | None -> Error (stale ())
      | Some version -> Ok (version :: versions))
    |> Result.map ~f:List.rev
;;

let project_origin current inputs prepared ~limits ~checkpoint =
  let open Result.Let_syntax in
  let graph = Prepared.fingerprint prepared in
  match current.Owner.Current.context.owner with
  | Owner.Host_project _ ->
    let%bind versions =
      editor_versions
        current
        inputs
        ~max_versions:limits.Limits.max_editor_versions
        ~checkpoint
    in
    Capture.Origin.editor_snapshot
      ~graph
      ~context:(Ledger.Inputs.identity inputs)
      ~generation:(Ledger.Inputs.generation inputs)
      ~versions
      ~max_versions:limits.max_editor_versions
      ~checkpoint
    |> Result.map_error ~f:(fun _ -> stale ())
  | Uploaded_project upload ->
    let rebound = Owner.Upload.Context_capture.rebound upload in
    let receipt = Owner.Upload.Rebound.receipt rebound in
    (match Owner.Upload.Rebound.basis rebound with
     | Owner.Upload.Rebound.Project validated ->
       let original = Chatml_authoring_bundle_project.Validated.build validated in
       if
         not
           (Captured.Digest.equal
              (Prepared.fingerprint (Build.prepared original))
              (Prepared.fingerprint (Build.prepared (Ledger.Inputs.base inputs))))
       then Error (stale ())
       else
         Capture.Origin.uploaded_bundle
           ~graph
           ~receipt_id:
             (P.Id.Authoring_bundle.to_string
                (P.Authoring_bundle.Bundle.Receipt.bundle_id receipt))
         |> Result.map_error ~f:(fun _ -> stale ())
     | Chatmd_document _ -> Error (unavailable ()))
  | Host_document _ | Supplied_document _ -> Error (unavailable ())
;;

let with_authoring_current t ~actor ~connection ~expected ~checkpoint ~f =
  let exception Host_preparation_error of P.Error.t in
  match
    Scope.with_current
      t.authoring
      ~actor
      ~connection
      ~expected
      ~checkpoint
      ~f:(fun current ~checkpoint ->
        match f current ~checkpoint with
        | Ok value -> Ok value
        | Error error -> raise (Host_preparation_error error))
  with
  | result -> Result.map_error result ~f:owner_error
  | exception Host_preparation_error error -> Error error
;;

let prepare_authoring_project t ~actor ~connection ~expected ~request ~checkpoint =
  let open Result.Let_syntax in
  let result =
    with_authoring_current
      t
      ~actor
      ~connection
      ~expected
      ~checkpoint
      ~f:(fun current ~checkpoint ->
        let original_actor = current.context.original_actor in
        let%bind pin =
          Scope.Candidate_pin.of_current t.authoring current ~connection
          |> Result.map_error ~f:owner_error
        in
        let%bind inputs =
          Owner.Current.project_inputs current ~checkpoint
          |> Result.map_error ~f:owner_error
        in
        let view = current.context.view in
        let select actor =
          t.policy.authoring_project actor ~context:view ~current:inputs
        in
        let%bind selected = selected_initial ~original_actor ~actor select in
        let%bind overlay =
          Authoring_project_overlay.prepare_with_contract
            current
            ~contract:(Selected_contract.contract selected)
            ~limits:t.limits.build
            ~checkpoint
          |> Result.map_error ~f:overlay_error
        in
        let build = Authoring_project_overlay.Prepared.build overlay in
        let prepared = Build.prepared build in
        let%bind origin =
          project_origin current inputs prepared ~limits:t.limits ~checkpoint
        in
        let%bind capture =
          capture_program ~prepared ~origin ~selected ~request ~checkpoint
        in
        let owner_now ~actor =
          Scope.Candidate_pin.check_now pin ~actor |> Result.map_error ~f:owner_error
        in
        let full_owner ~actor ~checkpoint =
          let scoped =
            Scope.with_current
              t.authoring
              ~actor
              ~connection
              ~expected
              ~checkpoint
              ~final_check:(fun () -> Scope.Candidate_pin.check_now pin ~actor)
              ~f:(fun fresh ~checkpoint ->
                let open Result.Let_syntax in
                let%bind () = Scope.Candidate_pin.matches_current pin fresh in
                let%bind actual = Owner.Current.project_inputs fresh ~checkpoint in
                if
                  Ledger.Identity.equal
                    (Ledger.Inputs.identity actual)
                    (Ledger.Inputs.identity inputs)
                  && Ledger.Generation.equal
                       (Ledger.Inputs.generation actual)
                       (Ledger.Inputs.generation inputs)
                then Ok ()
                else Error Owner.Error.Stale_context)
          in
          scoped |> Result.map_error ~f:owner_error
        in
        let%bind () = owner_now ~actor in
        let%bind () = selected_for_pair ~original_actor ~actor ~selected select in
        Ok
          (make_candidate
             ~capture
             ~original_actor
             ~request_actor:actor
             ~selected
             ~full_owner
             ~owner_now
             ~selection:select))
  in
  result
;;

let selected_declaration parsed selector ~checkpoint =
  match selector with
  | P.Authoring.Analysis.Target.Project -> Error (invalid ())
  | Selected_script script_id ->
    if String.is_empty script_id || String.length script_id > 256
    then Error (invalid ())
    else (
      let rec visit count found = function
        | [] -> found |> Result.of_option ~error:(unavailable ())
        | _ :: _ when count >= 1024 -> Error (limit ())
        | declaration :: rest ->
          checkpoint ();
          if
            String.equal
              (Prompt.Chat_markdown.Declaration.script_id declaration)
              script_id
          then (
            match found with
            | Some _ -> Error (invalid ())
            | None -> visit (count + 1) (Some declaration) rest)
          else visit (count + 1) found rest
      in
      visit 0 None (Prompt.Chat_markdown.Parsed.declarations parsed))
;;

let document_origin t ~inputs ~captured ~declaration ~script_source ~prepared ~checkpoint =
  let open Result.Let_syntax in
  let root_source = Sources.Document_context.Parsed_capture.source captured in
  let parsed_receipt = Sources.Document_context.Parsed_capture.parsed captured in
  let range = Prompt.Chat_markdown.Declaration.range declaration in
  let declaring_input = Chatmd_parser_input.Range.input range in
  let namespace = Captured.Id.package_namespace (Captured.id root_source) in
  let%bind admitted =
    Authoring_project_declaration.admit_input
      ~parsed:parsed_receipt
      ~input_sources:(Sources.Document_context.Parsed_capture.input_sources captured)
      ~declaration
      ~package_namespace:namespace
      ~checkpoint
    |> Result.map_error ~f:(fun _ -> stale ())
  in
  let declaring_source = Authoring_project_declaration.Admitted_input.source admitted in
  let%bind declaring_input_digest =
    Chatmd_parser_input.identity_digest declaring_input ~checkpoint
    |> Captured.Digest.of_hex
    |> Result.map_error ~f:(fun _ -> invalid ())
  in
  Capture.Origin.document_script
    ~graph:(Prepared.fingerprint prepared)
    ~context:(Ledger.Document.Inputs.identity inputs)
    ~generation:(Ledger.Document.Inputs.generation inputs)
    ~root_source
    ~declaring_source
    ~declaring_input_digest
    ~input_effective_bytes:(Chatmd_parser_input.effective_byte_length declaring_input)
    ~declaration_start_byte:(Chatmd_parser_input.Range.start_byte range)
    ~declaration_end_byte:(Chatmd_parser_input.Range.end_byte range)
    ~script_id:(Prompt.Chat_markdown.Declaration.script_id declaration)
    ~script_source
    ~max_identity_bytes:t.limits.max_document_origin_bytes
    ~checkpoint
  |> Result.map_error ~f:(function
    | Capture.Origin.Error.Invalid_provenance ->
      error P.Error.Invalid_request "invalid document origin"
    | Identity_limit -> limit ())
;;

let selected_document_build row ~declaration ~contract ~limits ~checkpoint =
  let open Result.Let_syntax in
  let selected =
    match Authoring_document_selection_rows.Row.status row with
    | Host_prepared { selected; _ } | Independently_selected selected -> Ok selected
    | No_validated_build _ -> Error (unavailable ())
  in
  let%bind selected = selected in
  let raw_build = Authoring_script_selection.Selected.build selected in
  let%bind build =
    Authoring_selected_script_overlay.recompose_entry
      raw_build
      ~contract
      ~contents:(Prompt.Chat_markdown.Declaration.script_body declaration)
      ~max_body_bytes:limits.Limits.max_prepared_input_bytes
      ~limits:limits.build
      ~checkpoint
    |> Result.map_error ~f:(fun _ -> unavailable ())
  in
  Ok (raw_build, build)
;;

let prepare_document_script
      t
      ~actor
      ~connection
      ~expected
      ~(script : Document_script.t)
      ~request
      ~checkpoint
  =
  with_authoring_current
    t
    ~actor
    ~connection
    ~expected
    ~checkpoint
    ~f:(fun current ~checkpoint ->
      let open Result.Let_syntax in
      let original_actor = current.context.original_actor in
      let%bind pin =
        Scope.Candidate_pin.of_current t.authoring current ~connection
        |> Result.map_error ~f:owner_error
      in
      let%bind inputs =
        Owner.Current.document_inputs current ~checkpoint
        |> Result.map_error ~f:owner_error
      in
      let%bind captured =
        Owner.Current.document_capture current ~checkpoint
        |> Result.map_error ~f:owner_error
        |> Result.bind ~f:(Result.of_option ~error:(unavailable ()))
      in
      let parsed_receipt = Sources.Document_context.Parsed_capture.parsed captured in
      let parsed = (Prompt.Chat_markdown.Generated.parsed parsed_receipt).parsed_root in
      let%bind declaration = selected_declaration parsed script.selector ~checkpoint in
      let%bind row =
        Owner.Current.selection_row current ~declaration ~checkpoint
        |> Result.map_error ~f:owner_error
      in
      let view = current.context.view in
      let selected_row =
        match row with
        | Some row ->
          (match Authoring_document_selection_rows.Row.status row with
           | Host_prepared { selected; _ } | Independently_selected selected ->
             Some (row, Authoring_script_selection.Selected.build selected)
           | No_validated_build _ -> None)
        | None -> None
      in
      let%bind selected, build, script_source, select =
        match selected_row with
        | Some (row, raw_build) ->
          let metadata = Authoring_document_selection_rows.Row.metadata row in
          let select actor =
            t.policy.document_project
              actor
              ~context:view
              ~current:inputs
              ~parsed
              ~declaration
              ~build:raw_build
              ~metadata
          in
          let%bind selected = selected_initial ~original_actor ~actor select in
          let%bind _, build =
            selected_document_build
              row
              ~declaration
              ~contract:(Selected_contract.contract selected)
              ~limits:t.limits
              ~checkpoint
          in
          let%bind assessed =
            Chatmd_authoring_analysis.Script.assess
              inputs
              ~parsed:(Some parsed)
              ~script_id:(Some (Prompt.Chat_markdown.Declaration.script_id declaration))
              ~selection:(Validated_build build)
              ~max_membership_work_steps:4096
              ~checkpoint
            |> Result.map_error ~f:(fun _ -> stale ())
          in
          (match assessed with
           | Chatmd_authoring_analysis.Script.Ready proof ->
             Ok
               ( selected
               , Some build
               , Chatmd_authoring_analysis.Script.selected_source proof
               , select )
           | Unavailable _ -> Error (unavailable ()))
        | None ->
          if
            Option.is_some
              (Prompt.Chat_markdown.Declaration.project_selection declaration)
          then Error (unavailable ())
          else (
            let%bind script_source =
              Sources.Document_context.Parsed_capture.script_source
                captured
                ~declaration
                ~max_script_bytes:t.limits.max_prepared_input_bytes
                ~max_locator_bytes:1024
                ~max_path_components:128
                ~checkpoint
              |> Result.map_error ~f:source_error
            in
            let select actor =
              t.policy.document_source
                actor
                ~context:view
                ~current:inputs
                ~parsed
                ~declaration
                ~script:script_source
            in
            let%map selected = selected_initial ~original_actor ~actor select in
            selected, None, script_source, select)
      in
      let%bind prepared =
        match build with
        | Some build -> Ok (Build.prepared build)
        | None ->
          Prepared.single_captured
            ~compiler_contract:
              (Contract.fingerprint (Selected_contract.contract selected) ~checkpoint)
            ~source:script_source
            ~max_input_bytes:t.limits.max_prepared_input_bytes
            ~checkpoint
            ()
          |> Result.map_error ~f:(fun _ -> limit ())
      in
      let%bind origin =
        document_origin
          t
          ~inputs
          ~captured
          ~declaration
          ~script_source
          ~prepared
          ~checkpoint
      in
      let%bind capture =
        capture_program ~prepared ~origin ~selected ~request ~checkpoint
      in
      let owner_now ~actor =
        Scope.Candidate_pin.check_now pin ~actor |> Result.map_error ~f:owner_error
      in
      let full_owner ~actor ~checkpoint =
        Scope.with_current
          t.authoring
          ~actor
          ~connection
          ~expected
          ~checkpoint
          ~final_check:(fun () -> Scope.Candidate_pin.check_now pin ~actor)
          ~f:(fun fresh ~checkpoint ->
            let open Result.Let_syntax in
            let%bind () = Scope.Candidate_pin.matches_current pin fresh in
            let%bind actual = Owner.Current.document_inputs fresh ~checkpoint in
            let%bind captured_now = Owner.Current.document_capture fresh ~checkpoint in
            if
              Ledger.Identity.equal
                (Ledger.Document.Inputs.identity actual)
                (Ledger.Document.Inputs.identity inputs)
              && Ledger.Generation.equal
                   (Ledger.Document.Inputs.generation actual)
                   (Ledger.Document.Inputs.generation inputs)
              && Option.exists captured_now ~f:(phys_equal captured)
            then Ok ()
            else Error Owner.Error.Stale_context)
        |> Result.map_error ~f:owner_error
      in
      let%bind () = owner_now ~actor in
      let%bind () = selected_for_pair ~original_actor ~actor ~selected select in
      Ok
        (make_candidate
           ~capture
           ~original_actor
           ~request_actor:actor
           ~selected
           ~full_owner
           ~owner_now
           ~selection:select))
;;
