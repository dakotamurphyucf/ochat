open! Core
module R = Relevance_judge
module S = Summarizer

(*------------------------------------------------------------------*)
(*  Helpers                                                          *)
(*------------------------------------------------------------------*)

let _render_item (item : Openai.Responses.Item.t) : string option =
  let open Openai.Responses in
  match item with
  | Item.Input_message { content; role; _ } ->
    (match content with
     | [] -> None
     | Text { text; _ } :: _ ->
       sprintf "%s: %s" (Input_message.role_to_string role) text |> Some
     | _ -> None)
  | Item.Output_message { content; _ } ->
    (match content with
     | [] -> None
     | { text; _ } :: _ -> sprintf "%s: %s" "Assistant" text |> Some)
  | Function_call { name; arguments; call_id; _ } ->
    sprintf "Function call (%s): %s(%s)" call_id name arguments |> Some
  | Function_call_output { call_id; output; _ } ->
    sprintf "Function call output (%s): %s" call_id output |> Some
  | _ -> None
;;

let build_system_summary_message
      ?(role = Openai.Responses.Input_message.User)
      (summary : string)
  : Openai.Responses.Item.t
  =
  let open Openai.Responses in
  let open Input_message in
  let text_item text : content_item = Text { text; _type = "input_text" } in
  let msg : Input_message.t =
    { role; content = [ text_item summary ]; _type = "message" }
  in
  Item.Input_message msg
;;

(*------------------------------------------------------------------*)
(*  Public API                                                       *)
(*------------------------------------------------------------------*)

let compact_history ~env ~(history : Openai.Responses.Item.t list)
  : Openai.Responses.Item.t list
  =
  (* Always protect against unexpected crashes. *)
  try
    let cfg = Config.load () in
    (* Keep only messages deemed relevant.  We do not attempt an exact token
       budget at this stage – the upcoming property tests approximate one
       token per character. *)
    (* let convo =
      history
      |> List.filter_map ~f:(fun item ->
        match render_item item with
        | None -> None
        | Some txt -> Some txt)
      |> String.concat ~sep:"\n"
    in *)
    (* let prompt msg = Printf.sprintf "message:\n%s\nconversation:\n%s" msg convo in *)
    (* If the system message is empty, we do not summarise. *)
    let relevant_items =
      history
      (* |> Eio.Fiber.List.filter ~max_fibers:10 (fun item ->
        match render_item item with
        | None -> false
        | Some txt -> R.is_relevant ?env cfg ~prompt:(prompt txt)) *)
    in
    (* Generate summary – this is an expensive call but runs once per
       compaction request. *)
    let summary =
      S.summarise ~relevant_items ~env
      |> fun s ->
      String.prefix s cfg.context_limit
      |> sprintf
           "<system-reminder>This is a message from the system that we compacted the \
            conversation history from out last session.\n\
            Here is a summary of the session that you saved:\n\
            %s\n\
            Remember this is not a message from the user, but a system reminder that you \
            should not respond to.\n\
            </system-reminder>"
    in
    Log.emit `Info
    @@ sprintf
         "Compactor.compact_history: summarised %d items to %d chars"
         (List.length history)
         (String.length summary);
    Log.emit `Debug @@ sprintf "Compactor.compact_history: summary:\n%s" summary;
    (* Build the new history, keeping the first message intact. *)
    match history with
    | [] ->
      [ build_system_summary_message ~role:System "You are a helpful assistant."
      ; build_system_summary_message summary
      ]
    | [ hd ] ->
      (* Always keep the first message intact, as it is usually a system prompt. *)
      [ hd; build_system_summary_message summary ]
    | _ -> [ List.hd_exn history; build_system_summary_message summary ]
  with
  | exn ->
    (* Fallback to identity transformation on error. *)
    eprintf "Compactor.compact_history: %s\n%!" (Exn.to_string exn);
    history
;;
