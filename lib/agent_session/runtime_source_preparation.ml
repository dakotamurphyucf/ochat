open! Core
module CM = Prompt.Chat_markdown
module Plan = Chat_response.Script_contract_plan
module Captured = Chat_response.Captured_script
module Script = Chat_response.Script_program
module Startup = Chat_response.Startup_compilation
module Spec = Chatmd_shell_spec.Extension_spec

module Moderator = struct
  type t =
    | No_moderator
    | Legacy of Agent_protocol.Invocation.observer
    | Extension of Agent_protocol.Invocation.observer
  [@@deriving equal, sexp_of]
end

module Selection = struct
  type t =
    | Authored of Prompt_revision.t
    | Captured of Captured_recurrence_source.t
    | Authored_child of
        { source : Prompt_child_source.t
        ; reference : Agent_store.Delegation_store.Reference.t
        }
    | Generated of Generated_definition.t
end

type t =
  { selection : Selection.t
  ; invocation : Runtime_script_preparation.Invocation.t
  ; moderator : Moderator.t
  ; programs : Chatml_prompt_programs.Restored.t option
  }

let selection t = t.selection

let artifact t =
  match t.selection with
  | Authored revision -> Prompt_revision.artifact revision
  | Captured source ->
    Prompt_revision.artifact (Captured_recurrence_source.revision source)
  | Authored_child { source; reference = _ } ->
    Prompt_revision.artifact (Prompt_child_source.revision source)
  | Generated definition -> Generated_definition.artifact definition
;;

let definition_id t =
  match t.selection with
  | Authored revision -> Some (Prompt_revision.definition revision).id
  | Captured source ->
    Some (Prompt_revision.definition (Captured_recurrence_source.revision source)).id
  | Authored_child { source; reference = _ } ->
    Some (Prompt_revision.definition (Prompt_child_source.revision source)).id
  | Generated definition ->
    (Generated_definition.artifact definition).prompt_definition_id
;;

let invocation t = t.invocation
let moderator t = t.moderator
let programs t = t.programs

let failure message =
  Agent_protocol.Error.create Invalid_state ~message ~retryable:false ()
;;

let compilation_error error =
  failure (error.Chatml_compilation.code ^ ": " ^ error.message)
;;

type moderator_kind =
  | Legacy
  | Extension

let contract ~plan ~id ~kind =
  let candidates =
    List.filter (Plan.programs plan) ~f:(fun program ->
      String.equal (CM.Declaration.script_id (Plan.Program.declaration program)) id
      &&
      match Plan.Program.role program, kind with
      | Legacy_moderator _, Legacy -> true
      | Standard (Moderator_v1 | Delegated_moderator_v1), Extension -> true
      | (Legacy_moderator _ | Standard _ | Native_hook _), _ -> false)
  in
  match candidates with
  | [ program ] -> Ok (Plan.Program.contract program)
  | [] -> Error (failure "selected moderator lacks its authentic captured role")
  | _ -> Error (failure "selected moderator has ambiguous captured roles")
;;

let selected ~plan ~programs ~declaration ~id ~kind ~has_project ~checkpoint =
  let open Result.Let_syntax in
  match plan, programs with
  | None, None ->
    if has_project
    then Error (failure "configured moderator requires captured program custody")
    else Ok None
  | Some plan, Some programs ->
    let%bind contract = contract ~plan ~id ~kind in
    let%map selected =
      Captured.select programs ~declaration ~contract ~checkpoint
      |> Result.map_error ~f:compilation_error
    in
    Some (Captured.prepared selected)
  | Some plan, None ->
    let%bind _ = contract ~plan ~id ~kind in
    (* The revision owner has validated genuine absence of an active graph.
       Its parser/role can still be present for ordinary single-file code. *)
    if has_project
    then Error (failure "configured moderator requires captured program custody")
    else Ok None
  | None, Some _ ->
    Error (failure "moderator selection lacks matching parser and program custody")
;;

let source_identity ~id ~source ~prepared ~checkpoint =
  let single_file_revision =
    Chatml_source.Digest.of_contents source ~checkpoint |> Chatml_source.Digest.to_hex
  in
  Agent_protocol.Invocation.
    { script_id = id
    ; source_sha256 = Script.source_revision_of_prepared prepared ~single_file_revision
    }
;;

let select_moderator ~elements ~limits ~plan ~programs ~checkpoint =
  let open Result.Let_syntax in
  let%bind () =
    if List.length elements > 16_384
    then Error (failure "source selection exceeds the runtime declaration limit")
    else Ok ()
  in
  let extensions, legacy =
    List.fold elements ~init:([], None) ~f:(fun (extensions, legacy) element ->
      checkpoint ();
      match element with
      | CM.Extension_script script
        when Spec.equal_script_kind script.kind Moderator_script ->
        script :: extensions, legacy
      | Script script -> extensions, Some script
      | _ -> extensions, legacy)
  in
  match extensions, legacy with
  | _ :: _ :: _, _ -> Error (failure "only one lifecycle moderator may be selected")
  | [ script ], _ ->
    let%bind () =
      Chat_response.Extension_compiler.validate_script
        ~max_source_bytes:
          (Int.min limits.Chatml_compilation.max_source_bytes (1024 * 1024))
        script
      |> Result.map_error ~f:(fun diagnostics ->
        diagnostics
        |> List.map ~f:Chatmd_shell_spec.Diagnostic.to_string
        |> String.concat ~sep:"\n"
        |> failure)
    in
    let%map prepared =
      selected
        ~plan
        ~programs
        ~declaration:(Extension script)
        ~id:script.id
        ~kind:Extension
        ~has_project:(Option.is_some script.project_selection)
        ~checkpoint
    in
    Moderator.Extension
      (source_identity
         ~id:script.id
         ~source:(Spec.script_text script)
         ~prepared
         ~checkpoint)
  | [], Some script ->
    let source =
      match script.source with
      | Inline source | Src { source_text = source; _ } -> source
    in
    if String.length source > limits.Chatml_compilation.max_source_bytes
    then Error (failure "selected moderator exceeds the host source byte limit")
    else (
      let%map prepared =
        selected
          ~plan
          ~programs
          ~declaration:(Moderator script)
          ~id:script.id
          ~kind:Legacy
          ~has_project:(Option.is_some script.project_selection)
          ~checkpoint
      in
      Moderator.Legacy (source_identity ~id:script.id ~source ~prepared ~checkpoint))
  | [], None -> Ok Moderator.No_moderator
;;

let prepare_revision selection revision ~script_preparation ~invocation ~env ~checkpoint =
  Runtime_script_preparation.with_revision_plan
    script_preparation
    revision
    ~env
    ~invocation
    ~checkpoint
    ~f:(fun startup ~compilation_limits ~plan ~programs ->
      let checkpoint () = Startup.checkpoint startup in
      Startup.within startup ~map_error:compilation_error ~f:(fun () ->
        let open Result.Let_syntax in
        let%map moderator =
          select_moderator
            ~elements:(Prompt_revision.elements revision)
            ~limits:compilation_limits
            ~plan
            ~programs
            ~checkpoint
        in
        { selection; invocation; moderator; programs }))
;;

let prepare revision ~script_preparation ~invocation ~env ~checkpoint =
  prepare_revision
    (Authored revision)
    revision
    ~script_preparation
    ~invocation
    ~env
    ~checkpoint
;;

let prepare_captured source ~script_preparation ~invocation ~env ~checkpoint =
  prepare_revision
    (Captured source)
    (Captured_recurrence_source.revision source)
    ~script_preparation
    ~invocation
    ~env
    ~checkpoint
;;

let prepare_authored_child
      source
      ~(record : Agent_store.Delegation_store.record)
      ~script_preparation
      ~invocation
      ~env
      ~checkpoint
  =
  let module B = Chat_response.Background_request.Agent_artifact in
  let open Result.Let_syntax in
  let selected = Prompt_child_source.reference source in
  let%bind () =
    match record.admission.authored_tool, record.admission.captured_recurrence with
    | Some _, None
      when Agent_protocol.Id.Prompt_revision.equal
             (B.revision_id selected)
             record.admission.revision_id
           && String.equal
                (Chatml_source.Digest.to_hex (B.manifest_sha256 selected))
                record.admission.manifest_sha256 -> Ok ()
    | (None | Some _), (None | Some _) ->
      Error (failure "authored child source differs from its recorded selection")
  in
  prepare_revision
    (Authored_child { source; reference = Agent_store.Delegation_store.reference record })
    (Prompt_child_source.revision source)
    ~script_preparation
    ~invocation
    ~env
    ~checkpoint
;;

let prepare_generated definition ~invocation =
  let module Manager = Chat_response.Moderator_manager in
  let open Result.Let_syntax in
  let admission = Generated_definition.admission definition in
  let%map _, artifact =
    Manager.Registry.of_definition
      Manager.Registry.empty
      (Chat_response.Generated_admission.definition admission)
    |> Result.map_error ~f:failure
  in
  let moderator =
    match artifact with
    | None -> Moderator.No_moderator
    | Some artifact ->
      Moderator.Extension
        { script_id = Manager.Registry.script_id artifact
        ; source_sha256 = Manager.Registry.source_hash artifact
        }
  in
  { selection = Generated definition; invocation; moderator; programs = None }
;;

let validate_target t (state : Session_state.t) =
  let module P = Agent_protocol in
  let module D = Agent_store.Delegation_store in
  let open Result.Let_syntax in
  let mismatch () =
    Error
      (P.Error.create
         Conflict
         ~message:"source binding does not match the retained target selection"
         ~retryable:false
         ())
  in
  let%bind () =
    match t.selection with
    | Captured source ->
      let%bind () =
        Recurrence_child_origin.validate_source
          (Captured_recurrence_source.origin source)
          state
      in
      if Option.is_none state.spec.prompt_definition_id then Ok () else mismatch ()
    | Authored_child { source = _; reference } ->
      let selected =
        match state.spec.protocol.prompt with
        | Generated revision -> P.Id.Prompt_revision.equal revision reference.revision_id
        | Catalog _ | Local_path _ -> false
      in
      if
        selected
        && Option.is_none state.spec.prompt_definition_id
        && P.Id.Session.equal state.identity.session_id reference.child_session_id
        && Option.exists state.spec.delegation ~f:(D.Reference.equal reference)
      then Ok ()
      else mismatch ()
    | Authored _ | Generated _ ->
      let matches =
        match state.spec.prompt_definition_id with
        | Some id -> Option.exists (definition_id t) ~f:(P.Id.Prompt_definition.equal id)
        | None -> Option.is_none (artifact t).prompt_definition_id
      in
      if matches then Ok () else mismatch ()
  in
  if
    P.Id.Prompt_revision.equal (artifact t).revision_id state.spec.prompt_revision_id
    && Runtime_script_preparation.Invocation.equal
         t.invocation
         (Runtime_script_preparation.Invocation.of_root state.spec.root_invocation)
  then Ok ()
  else mismatch ()
;;
