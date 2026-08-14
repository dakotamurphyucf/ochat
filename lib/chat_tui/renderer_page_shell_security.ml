open! Core
open Notty
module P = Renderer_shell_security_palette
module B = Renderer_shell_border
module S = Model.Shell_security_page_state

let safe attr text = I.string attr (Util.sanitize ~strip:true text)
let row ~width attr text = safe attr text |> I.hsnap ~align:`Left width

let tabs =
  [ S.Overview, "Overview", "Overview"
  ; Runtimes, "Runtimes", "Run"
  ; Grants, "Grants", "Grant"
  ; Audit, "Audit", "Audit"
  ; Interrupted, "Interrupted", "Stop"
  ]
;;

let tab_bar ~width selected =
  let compact = width < 72 in
  tabs
  |> List.map ~f:(fun (tab, full, short) ->
    let label = if compact then short else full in
    let attr = if S.equal_tab tab selected then P.selected else P.secondary in
    safe attr (" " ^ label ^ " "))
  |> I.hcat
  |> I.hsnap ~align:`Left width
;;

let section ~width title lines =
  let body = List.map lines ~f:(row ~width P.primary) in
  I.vcat ((row ~width P.title title :: body) @ [ row ~width P.background "" ])
;;

let short_digest = function
  | None -> "none"
  | Some digest -> String.prefix digest (Int.min 16 (String.length digest))
;;

let is_yolo snapshot =
  List.exists snapshot.S.runtimes ~f:(fun runtime ->
    Option.exists runtime.profile ~f:(fun profile ->
      String.is_substring (String.lowercase profile) ~substring:"yolo"))
;;

let overview ~width snapshot =
  section
    ~width
    "Runtime posture"
    [ sprintf "Requested         %s" (short_digest snapshot.S.manifest_sha256)
    ; sprintf "Live              %s" (short_digest snapshot.live_manifest_sha256)
    ; sprintf "Live runtimes     %d" (List.length snapshot.runtimes)
    ; sprintf
        "Manifest grants   %d"
        (List.count snapshot.manifest_grants ~f:(fun grant ->
           Option.is_none grant.revoked_at_ns))
    ; sprintf
        "Active grants     %d"
        (List.count snapshot.grants ~f:(fun grant -> Option.is_none grant.revoked_at_ns))
    ; "Administrative   " ^ snapshot.administrative_policy
    ; "Signature        " ^ snapshot.signature_status
    ; "Audit            " ^ snapshot.audit_status
    ; sprintf "Interrupted      %d" (List.length snapshot.interrupted_requests)
    ]
;;

let runtimes ~width snapshot =
  if List.is_empty snapshot.S.runtimes
  then section ~width "Effective runtimes" [ "No shell runtimes are configured." ]
  else
    snapshot.runtimes
    |> List.concat_map ~f:(fun runtime ->
      [ sprintf "%s  %s" runtime.id (Option.value runtime.profile ~default:"custom")
      ; sprintf
          "  sandbox=%s  network=%b  audit=%s"
          runtime.sandbox
          runtime.network
          runtime.audit
      ; ""
      ])
    |> section ~width "Effective runtimes"
;;

let scope_name = function
  | Session.Shell_state.Approval_scope.Exact_session -> "session exact"
  | Prefix_session _ -> "session prefix"
  | Durable_exact -> "durable exact"
;;

let grants ~width ~selected snapshot =
  if List.is_empty snapshot.S.grants
  then section ~width "Approval grants" [ "No persisted approval grants." ]
  else (
    let rows =
      snapshot.grants
      |> List.concat_map ~f:(fun grant ->
        let is_selected = Option.exists selected ~f:(String.equal grant.grant_id) in
        let attr = if is_selected then P.selected else P.primary in
        let marker = if is_selected then "▸" else " " in
        let state = if Option.is_some grant.revoked_at_ns then "revoked" else "active" in
        [ row
            ~width
            attr
            (sprintf
               "%s %s  %s  %s"
               marker
               (String.prefix grant.grant_id 10)
               (scope_name grant.scope)
               state)
        ; row
            ~width
            attr
            (sprintf
               "    runtime=%s command=%s"
               grant.runtime_id
               (String.prefix grant.command_sha256 12))
        ; row ~width P.background ""
        ])
    in
    I.vcat (row ~width P.title "Approval grants" :: rows))
;;

let audit_request_summary ~width ~selected request =
  let attr = if selected then P.selected else P.primary in
  let marker = if selected then "▸" else " " in
  [ row
      ~width
      attr
      (sprintf
         "%s %s  %s  %s"
         marker
         (String.prefix request.S.request_id 18)
         request.runtime_id
         request.result)
  ; row
      ~width
      attr
      (sprintf
         "    %s  output=%d/%d"
         (String.prefix request.command_sha256 18)
         request.stdout_bytes
         request.stderr_bytes)
  ]
;;

let audit_timeline ~width request =
  let metadata =
    [ sprintf "Request kind     %s" request.S.request_kind
    ; sprintf "Effects          %s" (String.concat ~sep:", " request.effects)
    ; sprintf "Policy           %s" (Option.value request.policy_action ~default:"none")
    ; sprintf "Approval         %s" (Option.value request.approval_answer ~default:"none")
    ; sprintf "Backend          %s" (Option.value request.backend ~default:"none")
    ]
  in
  let events =
    request.events
    |> List.map ~f:(fun event -> sprintf "  %Ld  %s" event.S.sequence event.name)
  in
  section ~width "Selected request · non-executing replay" (metadata @ events)
;;

let loaded_audit ~width ~selected page =
  let status =
    [ sprintf "Path             %s" page.S.path
    ; sprintf "Integrity        %s" page.integrity
    ; sprintf "Requests         %d" page.total_requests
    ; sprintf
        "Last sequence    %s"
        (Option.value_map page.last_sequence ~default:"none" ~f:Int64.to_string)
    ]
  in
  let requests =
    page.requests
    |> List.concat_map ~f:(fun request ->
      audit_request_summary
        ~width
        ~selected:(Option.exists selected ~f:(String.equal request.request_id))
        request
      @ [ row ~width P.background "" ])
  in
  let selected_request =
    Option.bind selected ~f:(fun id ->
      List.find page.requests ~f:(fun request -> String.equal request.request_id id))
  in
  I.vcat
    ([ section ~width "Audit status" status
     ; section ~width "Recent requests · newest first · maximum 200" []
     ]
     @ requests
     @ Option.value_map selected_request ~default:[] ~f:(fun request ->
       [ audit_timeline ~width request ]))
;;

let audit ~width ~selected state snapshot =
  match state with
  | S.Audit_not_loaded ->
    section ~width "Audit and replay" [ snapshot.S.audit_status; "Press r to load." ]
  | Audit_loading _ ->
    section
      ~width
      "Audit and replay"
      [ "Loading and validating the durable audit chain…" ]
  | Audit_failed message ->
    section ~width "Audit unavailable" [ message; "Press r to retry." ]
  | Audit_loaded page -> loaded_audit ~width ~selected page
;;

let interrupted ~width snapshot =
  if List.is_empty snapshot.S.interrupted_requests
  then section ~width "Interrupted requests" [ "No interrupted requests." ]
  else
    snapshot.interrupted_requests
    |> List.concat_map ~f:(fun request ->
      [ sprintf "%s  %s" request.request_id request.reason
      ; "  " ^ request.redacted_command
      ; "  Retry always creates a new request and repeats authorization."
      ; ""
      ])
    |> section ~width "Interrupted requests"
;;

let content ~width ~selected_grant ~audit_state ~selected_audit tab snapshot =
  match tab with
  | S.Overview -> overview ~width snapshot
  | Runtimes -> runtimes ~width snapshot
  | Grants -> grants ~width ~selected:selected_grant snapshot
  | Audit -> audit ~width ~selected:selected_audit audit_state snapshot
  | Interrupted -> interrupted ~width snapshot
;;

let render ~size:(width, height) ~model =
  let width = Int.max 0 width in
  let height = Int.max 0 height in
  let inner_width = Int.max 0 (width - 4) in
  let tab = Model.shell_security_tab model in
  let snapshot = Model.shell_security_snapshot model in
  let selected_grant = Model.selected_shell_grant_id model in
  let audit_state = Model.shell_audit_load_state model in
  let selected_audit = Model.selected_shell_audit_request_id model in
  let header =
    let yolo =
      if is_yolo snapshot
      then
        [ row
            ~width
            P.red
            "  ! YOLO — unrestricted direct process execution; approvals disabled"
        ]
      else []
    in
    I.vcat
      ([ row ~width P.background ""
       ; row ~width P.title "  Shell Security"
       ; row ~width P.secondary "  Effective authority, trust, approvals, and audit"
       ]
       @ yolo
       @ [ row ~width P.background ""
         ; I.pad ~l:2 (tab_bar ~width:inner_width tab)
         ; row
             ~width
             P.border
             (String.concat (List.init width ~f:(fun _ -> (B.current ()).horizontal)))
         ])
  in
  let footer_text =
    match tab with
    | S.Grants -> " Esc chat   h/l tabs   j/k select   x revoke   :shell open"
    | Audit -> " Esc chat   h/l tabs   j/k select   r refresh   :shell open"
    | Overview | Runtimes | Interrupted ->
      " Esc chat   h/l tabs   j/k scroll   :shell open"
  in
  let footer = row ~width P.elevated footer_text in
  let viewport_height = Int.max 0 (height - I.height header - 1) in
  let body =
    content ~width:inner_width ~selected_grant ~audit_state ~selected_audit tab snapshot
    |> I.pad ~l:2
  in
  let scroll_box = Model.shell_security_scroll_box model in
  Notty_scroll_box.set_content scroll_box body;
  let viewport = Notty_scroll_box.render scroll_box ~width ~height:viewport_height in
  ( I.vcat [ header; viewport; footer ]
    |> I.vsnap ~align:`Top height
    |> I.hsnap ~align:`Left width
  , (0, 0) )
;;
