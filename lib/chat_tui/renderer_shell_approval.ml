open! Core
open Notty
module P = Renderer_shell_security_palette
module B = Renderer_shell_border
module S = Model.Shell_security_page_state

let safe attr text = I.string attr (Util.sanitize ~strip:true text)
let row ~width attr text = safe attr text |> I.hsnap ~align:`Left width

let choice_enabled request = function
  | choice ->
    let scope =
      match choice with
      | S.Once -> Chatmd_shell_spec.Shell_spec.Once
      | Exact_session -> Exact_session
      | Prefix_session -> Prefix_session
      | Durable_exact -> Durable_exact
    in
    List.mem
      request.Shell_runtime.Approval_broker.scopes
      scope
      ~equal:Chatmd_shell_spec.Shell_spec.equal_approval_scope
;;

let choice_label = function
  | S.Once -> "1 Once"
  | Exact_session -> "2 Session"
  | Prefix_session -> "3 Prefix"
  | Durable_exact -> "4 Durable"
;;

let choices (modal : S.approval_modal) =
  let visible =
    [ S.Once; Exact_session ]
    @ if modal.S.more_options then [ Prefix_session; Durable_exact ] else []
  in
  visible
  |> List.filter ~f:(choice_enabled modal.S.request)
  |> List.map ~f:(fun choice ->
    let attr =
      if S.equal_approval_choice choice modal.selected then P.selected else P.secondary
    in
    safe attr (" " ^ choice_label choice ^ " "))
  |> I.hcat
;;

let border_line ~width left fill right =
  let middle = String.concat (List.init (Int.max 0 (width - 2)) ~f:(fun _ -> fill)) in
  row ~width P.border (left ^ middle ^ right)
;;

let framed ~width body =
  let border = B.current () in
  I.vcat
    ([ border_line ~width border.top_left border.horizontal border.top_right ]
     @ List.map body ~f:(fun image ->
       I.hcat
         [ safe P.border (border.vertical ^ " ")
         ; image
         ; safe P.border (" " ^ border.vertical)
         ]
       |> I.hsnap ~align:`Left width)
     @ [ border_line ~width border.bottom_left border.horizontal border.bottom_right ])
;;

let risk_label context =
  if
    List.exists context.Shell_access.Context.effects ~f:(function
      | Shell_access.Effect.Write_path _ | Arbitrary_code | Privilege_change -> true
      | Read_path _ | Network | Child_processes | Unknown _ -> false)
  then "HIGH RISK"
  else "APPROVAL"
;;

let request_kind = function
  | Shell_access.Context.Structured -> "structured"
  | Script_file -> "script file"
  | Raw_shell -> "raw shell"
;;

let choose_body ~inner (modal : S.approval_modal) =
  let request = modal.S.request in
  let approval = request.request in
  let context = approval.Shell_access.Approval.context in
  let effects =
    Shell_access.Effect.to_strings context.effects |> String.concat ~sep:", "
  in
  let detail_lines =
    [ "Command", approval.display_command
    ; "Executable", context.executable.canonical_path
    ; "Fingerprint", String.prefix context.executable.fingerprint.sha256 16
    ; "Working dir", context.cwd
    ; "Effects", effects
    ; "Runtime", request.runtime_id
    ; "Manifest", String.prefix request.manifest_sha256 16
    ]
  in
  let details =
    if not modal.details_expanded
    then []
    else
      detail_lines
      |> List.concat_map ~f:(fun (label, value) ->
        [ row ~width:inner P.secondary label; row ~width:inner P.primary ("  " ^ value) ])
  in
  let more =
    if modal.more_options
    then
      [ row
          ~width:inner
          P.amber
          "Broader grants require exact identity checks and can be revoked."
      ; row
          ~width:inner
          P.secondary
          "Prefix uses the current argv as its prefix; durable survives restart."
      ]
    else []
  in
  [ row
      ~width:inner
      P.title
      (sprintf
         "Shell command needs approval                         %s"
         (risk_label context))
  ; row
      ~width:inner
      P.secondary
      (sprintf "Request %s · %d waiting" request.id modal.queue_count)
  ; row ~width:inner P.surface ""
  ; row ~width:inner P.secondary "Command"
  ; row ~width:inner P.primary ("  " ^ approval.display_command)
  ; row ~width:inner P.secondary (sprintf "%s · %s" context.cwd effects)
  ; row ~width:inner P.surface ""
  ]
  @ details
  @ [ row ~width:inner P.surface ""; choices modal |> I.hsnap ~align:`Left inner ]
  @ more
  @ [ row ~width:inner P.surface ""
    ; row
        ~width:inner
        P.secondary
        "Enter approve   d deny   m more options   i details   Esc deny"
    ]
;;

let confirmation_body ~inner (modal : S.approval_modal) =
  let request = modal.S.request in
  let identity = request.request.Shell_access.Approval.identity in
  let heading, description, scope_lines =
    match modal.stage with
    | S.Confirm_prefix prefix ->
      ( "Confirm prefix grant"
      , "Trailing arguments may be accepted for the rest of this session."
      , [ "Argv prefix", String.concat ~sep:" " prefix ] )
    | Confirm_durable ->
      ( "Confirm durable grant"
      , "This exact identity survives restart and remains revocable."
      , [ "Runtime", request.runtime_id
        ; "Manifest", String.prefix request.manifest_sha256 20
        ; "Request kind", request_kind identity.request_kind
        ; "Command", request.request.display_command
        ; "Command identity", String.prefix identity.command_hash 20
        ; "Executable", String.prefix identity.executable_sha256 20
        ; "Cwd", String.prefix identity.cwd_sha256 20
        ; "Environment", String.prefix identity.environment_sha256 20
        ; "Stdin", Option.value identity.stdin_sha256 ~default:"none"
        ; "Stdin bytes", Int.to_string identity.stdin_bytes
        ; "Script", Option.value identity.script_sha256 ~default:"none"
        ] )
    | Choose | Deny_reason _ -> assert false
  in
  [ row ~width:inner P.red heading
  ; row ~width:inner P.amber description
  ; row ~width:inner P.surface ""
  ]
  @ List.concat_map scope_lines ~f:(fun (label, value) ->
    [ row ~width:inner P.secondary label; row ~width:inner P.primary ("  " ^ value) ])
  @ [ row ~width:inner P.surface ""
    ; row ~width:inner P.secondary "Enter confirm trust   Esc go back"
    ]
;;

let deny_body ~inner (modal : S.approval_modal) editor =
  let before = String.prefix editor.Shell_security_page_state.reason editor.cursor in
  let after = String.drop_prefix editor.reason editor.cursor in
  [ row ~width:inner P.red "Deny shell request"
  ; row ~width:inner P.secondary ("Request " ^ modal.S.request.id)
  ; row ~width:inner P.surface ""
  ; row ~width:inner P.secondary "Reason"
  ; row ~width:inner P.primary ("  " ^ before ^ "▏" ^ after)
  ; row ~width:inner P.surface ""
  ; row ~width:inner P.secondary "Enter confirm denial   Esc go back"
  ]
;;

let card ~width (modal : S.approval_modal) =
  let inner = Int.max 0 (width - 4) in
  let body =
    match modal.S.stage with
    | Choose -> choose_body ~inner modal
    | Confirm_prefix _ | Confirm_durable -> confirmation_body ~inner modal
    | Deny_reason editor -> deny_body ~inner modal editor
  in
  framed ~width body
;;

let grant_revoke_card ~width (modal : S.grant_revoke_modal) =
  let inner = Int.max 0 (width - 4) in
  let body =
    match modal.stage with
    | S.Confirm_revoke ->
      [ row ~width:inner P.red "Revoke persisted shell grant"
      ; row
          ~width:inner
          P.amber
          "Future matching commands will require authorization again."
      ; row ~width:inner P.surface ""
      ; row ~width:inner P.secondary "Grant"
      ; row ~width:inner P.primary ("  " ^ modal.grant_id)
      ; row ~width:inner P.secondary "Runtime"
      ; row ~width:inner P.primary ("  " ^ modal.runtime_id)
      ; row ~width:inner P.secondary "Command identity"
      ; row ~width:inner P.primary ("  " ^ modal.command_sha256)
      ; row ~width:inner P.surface ""
      ; row ~width:inner P.secondary "Enter revoke   Esc cancel"
      ]
    | Revoking ->
      [ row ~width:inner P.amber "Revoking shell grant"
      ; row ~width:inner P.secondary "Persisting mutation and chained audit event…"
      ]
    | Revoke_failed message ->
      [ row ~width:inner P.red "Grant revocation needs attention"
      ; row ~width:inner P.secondary message
      ; row ~width:inner P.surface ""
      ; row ~width:inner P.secondary "Enter or Esc close"
      ]
  in
  framed ~width body
;;

let moderator_choices ~inner modal choices =
  choices
  |> Array.to_list
  |> List.mapi ~f:(fun index choice ->
    let selected = Int.equal index modal.S.selected_choice in
    let attr = if selected then P.selected else P.primary in
    let marker = if selected then "▸" else " " in
    row ~width:inner attr (sprintf "%s %d  %s" marker (index + 1) choice))
;;

let moderator_body ~inner (modal : S.moderator_modal) =
  let heading, prompt, interaction =
    match modal.request with
    | Chat_response.In_memory_stream.Ask_text { prompt } ->
      let before = String.prefix modal.response modal.cursor in
      let after = String.drop_prefix modal.response modal.cursor in
      ( "Moderator input required"
      , prompt
      , [ row ~width:inner P.secondary "Response"
        ; row ~width:inner P.primary ("  " ^ before ^ "▏" ^ after)
        ; row ~width:inner P.secondary "Enter submit"
        ] )
    | Chat_response.In_memory_stream.Ask_choice { prompt; choices } ->
      ( "Moderator choice required"
      , prompt
      , moderator_choices ~inner modal choices
        @ [ row ~width:inner P.secondary "j/k select   Enter submit" ] )
  in
  [ row ~width:inner P.blue heading
  ; row ~width:inner P.primary prompt
  ; row ~width:inner P.surface ""
  ]
  @ interaction
  @ Option.value_map modal.validation_error ~default:[] ~f:(fun message ->
    [ row ~width:inner P.surface ""; row ~width:inner P.red message ])
;;

let moderator_card ~width modal =
  let inner = Int.max 0 (width - 4) in
  let body = moderator_body ~inner modal in
  framed ~width body
;;

let position ~screen_width ~screen_height modal_image =
  let width = I.width modal_image in
  let left = Int.max 0 ((screen_width - width) / 2) in
  let top =
    if screen_width < 50
    then Int.max 0 (screen_height - I.height modal_image)
    else Int.max 0 ((screen_height - I.height modal_image) / 2)
  in
  I.pad ~l:left ~t:top modal_image
;;

let position_origin ~screen_width ~screen_height modal_image =
  let left = Int.max 0 ((screen_width - I.width modal_image) / 2) in
  let top =
    if screen_width < 50
    then Int.max 0 (screen_height - I.height modal_image)
    else Int.max 0 ((screen_height - I.height modal_image) / 2)
  in
  left, top
;;

let cursor ~size:(screen_width, screen_height) ~model =
  let width = Int.max 1 (Int.min 78 (screen_width - 2)) in
  match Model.shell_approval_modal model with
  | Some ({ stage = S.Deny_reason editor; _ } as modal) ->
    let left, top = position_origin ~screen_width ~screen_height (card ~width modal) in
    Some (left + 4 + editor.cursor, top + 5)
  | Some _ -> None
  | None ->
    (match Model.shell_grant_revoke_modal model, Model.moderator_modal model with
     | Some _, _ | None, None -> None
     | None, Some ({ request = Chat_response.In_memory_stream.Ask_text _; _ } as modal) ->
       let left, top =
         position_origin ~screen_width ~screen_height (moderator_card ~width modal)
       in
       Some (left + 4 + modal.cursor, top + 5)
     | None, Some { request = Ask_choice _; _ } -> None)
;;

let overlay ~size:(screen_width, screen_height) ~model base =
  match Model.shell_approval_modal model with
  | None ->
    (match Model.shell_grant_revoke_modal model with
     | None ->
       (match Model.moderator_modal model with
        | None -> base
        | Some modal ->
          let width = Int.max 1 (Int.min 78 (screen_width - 2)) in
          let positioned =
            moderator_card ~width modal |> position ~screen_width ~screen_height
          in
          I.(positioned </> base))
     | Some modal ->
       let width = Int.max 1 (Int.min 78 (screen_width - 2)) in
       let positioned =
         grant_revoke_card ~width modal |> position ~screen_width ~screen_height
       in
       I.(positioned </> base))
  | Some modal ->
    let width = Int.max 1 (Int.min 78 (screen_width - 2)) in
    let modal_image = card ~width modal in
    let positioned = position ~screen_width ~screen_height modal_image in
    I.(positioned </> base)
;;
