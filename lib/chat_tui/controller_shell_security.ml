open! Core
module S = Model.Shell_security_page_state
module Scope = Chatmd_shell_spec.Shell_spec

let tabs = [ S.Overview; Runtimes; Grants; Audit; Interrupted ]

let move_tab model delta =
  let current = Model.shell_security_tab model in
  let index = List.findi tabs ~f:(fun _ tab -> S.equal_tab tab current) |> Option.value_exn |> fst in
  let next = (index + delta + List.length tabs) mod List.length tabs in
  Model.set_shell_security_tab model (List.nth_exn tabs next);
  Controller_types.Redraw
;;

let scroll model ~height delta =
  Notty_scroll_box.scroll_by (Model.shell_security_scroll_box model) ~height delta;
  Controller_types.Redraw
;;

let move_grant model delta =
  Model.move_shell_grant_selection model delta;
  Controller_types.Redraw
;;

let move_audit model delta =
  Model.move_shell_audit_selection model delta;
  Controller_types.Redraw
;;

let enabled (modal : S.approval_modal) choice =
  let scope =
    match choice with
    | S.Once -> Scope.Once
    | Exact_session -> Exact_session
    | Prefix_session -> Prefix_session
    | Durable_exact -> Durable_exact
  in
  List.mem modal.S.request.scopes scope ~equal:Scope.equal_approval_scope
;;

let selected_response (modal : S.approval_modal) =
  match modal.S.selected with
  | S.Once -> Shell_runtime.Approval_broker.Approve_once
  | Exact_session -> Approve_exact_session
  | Prefix_session ->
    Approve_prefix_session
      (Shell_access.Command.to_argv modal.request.request.context.command)
  | Durable_exact -> Approve_durable_exact
;;

let begin_selected_confirmation model (modal : S.approval_modal) =
  match modal.selected with
  | S.Prefix_session ->
    Model.set_shell_approval_stage
      model
      (S.Confirm_prefix
         (Shell_access.Command.to_argv modal.request.request.context.command));
    Controller_types.Redraw
  | Durable_exact ->
    Model.set_shell_approval_stage model S.Confirm_durable;
    Controller_types.Redraw
  | Once | Exact_session ->
    Controller_types.Shell_approval_response
      (modal.request.id, selected_response modal)
;;

let select model modal choice =
  if enabled modal choice then Model.set_shell_approval_choice model choice;
  Controller_types.Redraw
;;

let handle_deny_reason model (modal : S.approval_modal) editor = function
  | `Key (`Enter, _) ->
    let reason = String.strip editor.Shell_security_page_state.reason in
    let reason = if String.is_empty reason then "denied by user" else reason in
    Controller_types.Shell_approval_response
      (modal.S.request.id, Shell_runtime.Approval_broker.Deny reason)
  | `Key (`Escape, _) ->
    Model.set_shell_approval_stage model S.Choose;
    Controller_types.Redraw
  | `Key (`Backspace, _) when editor.Shell_security_page_state.cursor > 0 ->
    let before =
      String.prefix
        editor.Shell_security_page_state.reason
        (editor.Shell_security_page_state.cursor - 1)
    in
    let after =
      String.drop_prefix
        editor.Shell_security_page_state.reason
        editor.Shell_security_page_state.cursor
    in
    editor.Shell_security_page_state.reason <- before ^ after;
    editor.Shell_security_page_state.cursor <- editor.Shell_security_page_state.cursor - 1;
    Controller_types.Redraw
  | `Key (`ASCII character, []) when Char.is_print character ->
    let before =
      String.prefix
        editor.Shell_security_page_state.reason
        editor.Shell_security_page_state.cursor
    in
    let after =
      String.drop_prefix
        editor.Shell_security_page_state.reason
        editor.Shell_security_page_state.cursor
    in
    editor.Shell_security_page_state.reason <- before ^ String.of_char character ^ after;
    editor.Shell_security_page_state.cursor <- editor.Shell_security_page_state.cursor + 1;
    Controller_types.Redraw
  | _ -> Controller_types.Redraw
;;

let handle_confirmation model (modal : S.approval_modal) = function
  | `Key (`Enter, _) ->
    Controller_types.Shell_approval_response
      (modal.S.request.id, selected_response modal)
  | `Key (`Escape, _) ->
    Model.set_shell_approval_stage model S.Choose;
    Controller_types.Redraw
  | _ -> Controller_types.Redraw
;;

let handle_choice model (modal : S.approval_modal) = function
  | `Key (`Enter, _) -> begin_selected_confirmation model modal
  | `Key (`ASCII '1', []) -> select model modal S.Once
  | `Key (`ASCII '2', []) | `Key (`ASCII ('s' | 'S'), []) ->
    select model modal Exact_session
  | `Key (`ASCII '3', []) | `Key (`ASCII ('p' | 'P'), [])
    when modal.more_options ->
    select model modal Prefix_session
  | `Key (`ASCII '4', []) when modal.more_options -> select model modal Durable_exact
  | `Key (`ASCII ('m' | 'M'), []) ->
    Model.toggle_shell_approval_more_options model;
    Controller_types.Redraw
  | `Key (`ASCII ('i' | 'I'), []) ->
    Model.toggle_shell_approval_details model;
    Controller_types.Redraw
  | `Key (`ASCII ('d' | 'D' | 'n' | 'N'), []) ->
    Model.set_shell_approval_stage model (S.Deny_reason { reason = ""; cursor = 0 });
    Controller_types.Redraw
  | `Key (`Escape, _) ->
    Controller_types.Shell_approval_response
      ( modal.request.id
      , Shell_runtime.Approval_broker.Deny "approval request was cancelled" )
  | _ -> Controller_types.Redraw
;;

let handle_modal model (modal : S.approval_modal) event =
  match modal.stage with
  | S.Choose -> handle_choice model modal event
  | Confirm_prefix _ | Confirm_durable -> handle_confirmation model modal event
  | Deny_reason editor -> handle_deny_reason model modal editor event
;;

let handle_grant_revoke model (modal : S.grant_revoke_modal) = function
  | `Key (`Escape, _) ->
    Model.close_shell_grant_revoke_modal model;
    Controller_types.Redraw
  | `Key (`Enter, _) ->
    (match modal.stage with
     | S.Confirm_revoke ->
       Controller_types.Shell_grant_revoke_requested
         (modal.generation, modal.grant_id)
     | Revoke_failed _ ->
       Model.close_shell_grant_revoke_modal model;
       Controller_types.Redraw
     | Revoking -> Controller_types.Redraw)
  | _ -> Controller_types.Redraw
;;

let replace_response (modal : S.moderator_modal) ~before ~after =
  modal.response <- before ^ after;
  modal.cursor <- String.length before;
  modal.validation_error <- None
;;

let insert_response modal text =
  let before = String.prefix modal.S.response modal.cursor ^ text in
  let after = String.drop_prefix modal.response modal.cursor in
  replace_response modal ~before ~after
;;

let backspace_response modal =
  if modal.S.cursor > 0
  then (
    let before = String.prefix modal.response (modal.cursor - 1) in
    let after = String.drop_prefix modal.response modal.cursor in
    replace_response modal ~before ~after)
;;

let move_choice modal delta choices =
  let count = Array.length choices in
  if count > 0
  then (
    modal.S.selected_choice <- (modal.S.selected_choice + delta + count) mod count;
    modal.S.response <- choices.(modal.S.selected_choice);
    modal.S.cursor <- String.length modal.S.response;
    modal.S.validation_error <- None)
;;

let handle_moderator_text (modal : S.moderator_modal) = function
  | `Key (`Escape, _) -> Controller_types.Cancel_or_quit
  | `Key (`Enter, _) ->
    Controller_types.Moderator_input_response (String.strip modal.S.response)
  | `Key (`Backspace, _) ->
    backspace_response modal;
    Redraw
  | `Key (`Arrow `Left, _) ->
    modal.S.cursor <- Int.max 0 (modal.S.cursor - 1);
    Redraw
  | `Key (`Arrow `Right, _) ->
    modal.S.cursor <- Int.min (String.length modal.S.response) (modal.S.cursor + 1);
    Redraw
  | `Key (`ASCII character, []) when Char.is_print character ->
    insert_response modal (String.of_char character);
    Redraw
  | _ -> Redraw
;;

let handle_moderator_choice (modal : S.moderator_modal) choices = function
  | `Key (`Escape, _) -> Controller_types.Cancel_or_quit
  | `Key (`Enter, _) -> Controller_types.Moderator_input_response modal.S.response
  | `Key (`Arrow `Up, _) | `Key (`ASCII ('k' | 'K'), []) ->
    move_choice modal (-1) choices;
    Redraw
  | `Key (`Arrow `Down, _) | `Key (`ASCII ('j' | 'J'), []) | `Key (`Tab, _) ->
    move_choice modal 1 choices;
    Redraw
  | `Key (`ASCII digit, []) when Char.is_digit digit ->
    let index = Char.to_int digit - Char.to_int '1' in
    if index >= 0 && index < Array.length choices
    then (
      modal.S.selected_choice <- index;
      modal.S.response <- choices.(index));
    Redraw
  | _ -> Redraw
;;

let handle_moderator_modal (modal : S.moderator_modal) event =
  match modal.request with
  | Chat_response.In_memory_stream.Ask_text _ -> handle_moderator_text modal event
  | Ask_choice { choices; _ } -> handle_moderator_choice modal choices event
;;

let handle_page ~model ~term = function
  | `Key (`Escape, _) ->
    Model.set_active_page model Model.Page_id.Chat;
    Controller_types.Redraw
  | `Key (`ASCII ('h' | 'H'), []) | `Key (`Arrow `Left, _) -> move_tab model (-1)
  | `Key (`ASCII ('l' | 'L'), []) | `Key (`Arrow `Right, _) | `Key (`Tab, _) ->
    move_tab model 1
  | `Key (`ASCII ('j' | 'J'), []) | `Key (`Arrow `Down, _)
    when S.equal_tab (Model.shell_security_tab model) S.Grants ->
    move_grant model 1
  | `Key (`ASCII ('k' | 'K'), []) | `Key (`Arrow `Up, _)
    when S.equal_tab (Model.shell_security_tab model) S.Grants ->
    move_grant model (-1)
  | `Key (`ASCII ('j' | 'J'), []) | `Key (`Arrow `Down, _)
    when S.equal_tab (Model.shell_security_tab model) S.Audit ->
    move_audit model 1
  | `Key (`ASCII ('k' | 'K'), []) | `Key (`Arrow `Up, _)
    when S.equal_tab (Model.shell_security_tab model) S.Audit ->
    move_audit model (-1)
  | `Key (`ASCII ('r' | 'R'), []) ->
    Controller_types.Shell_management_refresh_requested
      (Model.begin_shell_management_load model)
  | `Key (`ASCII ('j' | 'J'), []) | `Key (`Arrow `Down, _) ->
    scroll model ~height:(snd (Notty_eio.Term.size term) - 7) 1
  | `Key (`ASCII ('k' | 'K'), []) | `Key (`Arrow `Up, _) ->
    scroll model ~height:(snd (Notty_eio.Term.size term) - 7) (-1)
  | `Key (`Page `Down, _) ->
    let height = snd (Notty_eio.Term.size term) - 7 in
    scroll model ~height height
  | `Key (`Page `Up, _) ->
    let height = snd (Notty_eio.Term.size term) - 7 in
    scroll model ~height (-height)
  | `Key (`ASCII ('x' | 'X'), [])
    when S.equal_tab (Model.shell_security_tab model) S.Grants ->
    Model.open_shell_grant_revoke_modal model;
    Controller_types.Redraw
  | _ -> Controller_types.Unhandled
;;

let handle_key ~model ~term event =
  match Model.shell_approval_modal model with
  | Some modal -> handle_modal model modal event
  | None ->
    (match Model.shell_grant_revoke_modal model with
     | Some modal -> handle_grant_revoke model modal event
     | None ->
       (match Model.moderator_modal model with
        | Some modal -> handle_moderator_modal modal event
        | None -> handle_page ~model ~term event))
;;
