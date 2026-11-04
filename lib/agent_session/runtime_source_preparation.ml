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

type t =
  { revision : Prompt_revision.t
  ; invocation : Runtime_script_preparation.Invocation.t
  ; moderator : Moderator.t
  ; programs : Chatml_prompt_programs.Restored.t option
  }

let revision t = t.revision
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

let prepare revision ~script_preparation ~invocation ~env ~checkpoint =
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
        { revision; invocation; moderator; programs }))
;;
