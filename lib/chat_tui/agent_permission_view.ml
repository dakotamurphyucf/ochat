open Core

let choice_label = function
  | Agent_protocol.Permission.Approve_once -> "approve once"
  | Approve_session -> "approve session"
  | Approve_prefix -> "approve prefix"
  | Durable_exact -> "durable exact"
  | Deny -> "deny"
;;

let prompt (permission : Agent_protocol.Permission.t) =
  [ "Tool permission requested"
  ; "Tool: " ^ permission.tool_name
  ; "Invocation: " ^ permission.invocation_display
  ]
  @ Option.to_list (Option.map permission.rationale ~f:(fun s -> "Rationale: " ^ s))
  |> String.concat_lines
;;

let same (left : Agent_protocol.Permission.t) (right : Agent_protocol.Permission.t) =
  Agent_protocol.Id.Permission.compare left.id right.id = 0
  && Int.equal left.generation right.generation
;;

let sync model ~current projection =
  let next =
    (Agent_projection.snapshot projection).permissions
    |> List.find ~f:(fun p -> Agent_protocol.Permission.equal_state p.state Pending)
  in
  (match current, next with
   | Some a, Some b when same a b -> ()
   | _, Some permission ->
     Model.open_moderator_modal
       model
       (Chat_response.In_memory_stream.Ask_choice
          { prompt = prompt permission
          ; choices = Array.of_list (List.map permission.choices ~f:choice_label)
          });
     Model.set_active_page model Model.Page_id.Shell_security
   | Some _, None -> Model.close_moderator_modal model
   | None, None -> ());
  next
;;
