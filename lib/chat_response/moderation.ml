open! Core

module Lang = Chatml.Chatml_lang
module Runtime = Chatml_moderator_runtime
module Res = Openai.Responses
module Value_codec = Chatml.Chatml_value_codec

let parse_json_or_string (text : string) : Jsonaf.t =
  try Jsonaf.of_string text with
  | _ -> `String text
;;

let record_value (fields : (string * Lang.value) list) : Lang.value =
  Lang.VRecord (Map.of_alist_exn (module String) fields)
;;

let lang_value_of_jsonaf = Value_codec.jsonaf_to_value

let jsonaf_of_lang_value ~name (value : Lang.value) : (Jsonaf.t, string) result =
  match Value_codec.value_to_jsonaf_result value with
  | Ok json -> Ok json
  | Error msg -> Error (Printf.sprintf "%s: %s" name msg)
;;

module Phase = struct
  type t =
    | Session_start
    | Session_resume
    | Turn_start
    | Message_appended
    | Pre_tool_call
    | Post_tool_response
    | Turn_end
    | Internal_event
  [@@deriving sexp, compare]

  let to_string (t : t) : string =
    match t with
    | Session_start -> "session_start"
    | Session_resume -> "session_resume"
    | Turn_start -> "turn_start"
    | Message_appended -> "message_appended"
    | Pre_tool_call -> "pre_tool_call"
    | Post_tool_response -> "post_tool_response"
    | Turn_end -> "turn_end"
    | Internal_event -> "internal_event"
  ;;

  let of_string (value : string) : (t, string) result =
    match value with
    | "session_start" -> Ok Session_start
    | "session_resume" -> Ok Session_resume
    | "turn_start" -> Ok Turn_start
    | "message_appended" -> Ok Message_appended
    | "pre_tool_call" -> Ok Pre_tool_call
    | "post_tool_response" -> Ok Post_tool_response
    | "turn_end" -> Ok Turn_end
    | "internal_event" -> Ok Internal_event
    | _ -> Error (Printf.sprintf "Unknown moderation phase %S" value)
  ;;
end

module Message = struct
  type t =
    { id : string
    ; role : string
    ; content : string
    ; meta : Jsonaf.t
    }
  [@@deriving sexp]

  let create ~id ~role ~content ~meta = { id; role; content; meta }

  let of_value (value : Lang.value) : (t, string) result =
    let open Result.Let_syntax in
    match value with
    | Lang.VRecord fields ->
      let%bind id = Value_codec.expect_record_field "message" fields "id" in
      let%bind role = Value_codec.expect_record_field "message" fields "role" in
      let%bind content = Value_codec.expect_record_field "message" fields "content" in
      let%bind meta = Value_codec.expect_record_field "message" fields "meta" in
      let%bind id = Value_codec.expect_string "message.id" id in
      let%bind role = Value_codec.expect_string "message.role" role in
      let%bind content = Value_codec.expect_string "message.content" content in
      let%bind meta = jsonaf_of_lang_value ~name:"message.meta" meta in
      Ok { id; role; content; meta }
    | _ -> Error "message: expected record value"
  ;;

  let to_value (t : t) : Lang.value =
    record_value
      [ "id", Lang.VString t.id
      ; "role", Lang.VString t.role
      ; "content", Lang.VString t.content
      ; "meta", lang_value_of_jsonaf t.meta
      ]
  ;;
end

module Tool_desc = struct
  type t =
    { name : string
    ; description : string
    ; input_schema : Jsonaf.t
    }
  [@@deriving sexp]

  let of_request_tool (tool : Res.Request.Tool.t) : t =
    match tool with
    | Res.Request.Tool.Function fn ->
      { name = fn.name
      ; description = Option.value fn.description ~default:""
      ; input_schema = fn.parameters
      }
    | Res.Request.Tool.Custom_function fn ->
      { name = fn.name
      ; description = Option.value fn.description ~default:""
      ; input_schema = fn.format
      }
    | Res.Request.Tool.File_search _ ->
      { name = "file_search"
      ; description = ""
      ; input_schema = Res.Request.Tool.jsonaf_of_t tool
      }
    | Res.Request.Tool.Web_search _ ->
      { name = "web_search"
      ; description = ""
      ; input_schema = Res.Request.Tool.jsonaf_of_t tool
      }
  ;;

  let to_value (t : t) : Lang.value =
    record_value
      [ "name", Lang.VString t.name
      ; "description", Lang.VString t.description
      ; "input_schema", lang_value_of_jsonaf t.input_schema
      ]
  ;;
end

module Tool_call = struct
  type kind =
    | Function
    | Custom
  [@@deriving sexp, compare]

  type t =
    { id : string
    ; name : string
    ; args : Jsonaf.t
    ; kind : kind
    ; payload_text : string
    ; meta : Jsonaf.t
    }
  [@@deriving sexp]

  let of_response_item (item : Res.Item.t) : t option =
    match item with
    | Res.Item.Function_call fc ->
      Some
        { id = fc.call_id
        ; name = fc.name
        ; args = parse_json_or_string fc.arguments
        ; kind = Function
        ; payload_text = fc.arguments
        ; meta = Res.Item.jsonaf_of_t item
        }
    | Res.Item.Custom_tool_call tc ->
      Some
        { id = tc.call_id
        ; name = tc.name
        ; args = parse_json_or_string tc.input
        ; kind = Custom
        ; payload_text = tc.input
        ; meta = Res.Item.jsonaf_of_t item
        }
    | _ -> None
  ;;

  let to_value (t : t) : Lang.value =
    record_value
      [ "id", Lang.VString t.id
      ; "name", Lang.VString t.name
      ; "args", lang_value_of_jsonaf t.args
      ]
  ;;
end

let render_output_part (part : Res.Tool_output.Output_part.t) : string =
  match part with
  | Res.Tool_output.Output_part.Input_text { text } -> text
  | Res.Tool_output.Output_part.Input_image { image_url; _ } ->
    Printf.sprintf "<image src=\"%s\" />" image_url
;;

let render_tool_output (output : Res.Tool_output.Output.t) : string =
  match output with
  | Res.Tool_output.Output.Text text -> text
  | Res.Tool_output.Output.Content parts ->
    String.concat ~sep:"\n" (List.map parts ~f:render_output_part)
;;

let jsonaf_of_tool_output (output : Res.Tool_output.Output.t) : Jsonaf.t =
  match output with
  | Res.Tool_output.Output.Text text -> parse_json_or_string text
  | Res.Tool_output.Output.Content parts ->
    `Object
      [ "kind", `String "content"
      ; ( "parts"
        , `Array
            (List.map parts ~f:(function
               | Res.Tool_output.Output_part.Input_text { text } ->
                 `Object [ "type", `String "text"; "text", `String text ]
               | Res.Tool_output.Output_part.Input_image { image_url; detail } ->
                 let detail =
                   match detail with
                   | None -> `Null
                   | Some Res.Input_message.Auto -> `String "auto"
                   | Some Res.Input_message.Low -> `String "low"
                   | Some Res.Input_message.High -> `String "high"
                 in
                 `Object
                   [ "type", `String "image"
                   ; "image_url", `String image_url
                   ; "detail", detail
                   ])) )
      ]
;;

module Tool_result = struct
  type t =
    { call_id : string
    ; name : string
    ; result : Jsonaf.t
    ; kind : Tool_call.kind
    ; raw_output : string option
    ; meta : Jsonaf.t
    }
  [@@deriving sexp]

  let of_output_item ~name ~kind (item : Res.Item.t) : t option =
    match item with
    | Res.Item.Function_call_output out ->
      Some
        { call_id = out.call_id
        ; name
        ; result = jsonaf_of_tool_output out.output
        ; kind
        ; raw_output = Some (render_tool_output out.output)
        ; meta = Res.Item.jsonaf_of_t item
        }
    | Res.Item.Custom_tool_call_output out ->
      Some
        { call_id = out.call_id
        ; name
        ; result = jsonaf_of_tool_output out.output
        ; kind
        ; raw_output = Some (render_tool_output out.output)
        ; meta = Res.Item.jsonaf_of_t item
        }
    | _ -> None
  ;;

  let to_value (t : t) : Lang.value =
    record_value
      [ "call_id", Lang.VString t.call_id
      ; "name", Lang.VString t.name
      ; "result", lang_value_of_jsonaf t.result
      ]
  ;;
end

module Context = struct
  type t =
    { session_id : string
    ; now_ms : int
    ; phase : Phase.t
    ; messages : Message.t list
    ; available_tools : Tool_desc.t list
    ; session_meta : Jsonaf.t
    }
  [@@deriving sexp]

  let to_value (t : t) : Lang.value =
    record_value
      [ "session_id", Lang.VString t.session_id
      ; "now_ms", Lang.VInt t.now_ms
      ; "phase", Lang.VString (Phase.to_string t.phase)
      ; "messages", Lang.VArray (Array.of_list_map t.messages ~f:Message.to_value)
      ; ( "available_tools"
        , Lang.VArray (Array.of_list_map t.available_tools ~f:Tool_desc.to_value) )
      ; "session_meta", lang_value_of_jsonaf t.session_meta
      ]
  ;;
end

module Event = struct
  type t =
    | Session_start
    | Session_resume
    | Turn_start
    | Message_appended of Message.t
    | Pre_tool_call of Tool_call.t
    | Post_tool_response of Tool_result.t
    | Turn_end
    | Internal_event of Lang.value

  let phase (t : t) : Phase.t =
    match t with
    | Session_start -> Phase.Session_start
    | Session_resume -> Phase.Session_resume
    | Turn_start -> Phase.Turn_start
    | Message_appended _ -> Phase.Message_appended
    | Pre_tool_call _ -> Phase.Pre_tool_call
    | Post_tool_response _ -> Phase.Post_tool_response
    | Turn_end -> Phase.Turn_end
    | Internal_event _ -> Phase.Internal_event
  ;;

  let to_value (t : t) : Lang.value =
    match t with
    | Session_start -> Lang.VVariant ("Session_start", [])
    | Session_resume -> Lang.VVariant ("Session_resume", [])
    | Turn_start -> Lang.VVariant ("Turn_start", [])
    | Message_appended message ->
      Lang.VVariant ("Message_appended", [ Message.to_value message ])
    | Pre_tool_call call ->
      Lang.VVariant ("Pre_tool_call", [ Tool_call.to_value call ])
    | Post_tool_response result ->
      Lang.VVariant ("Post_tool_response", [ Tool_result.to_value result ])
    | Turn_end -> Lang.VVariant ("Turn_end", [])
    | Internal_event event -> event
  ;;
end

let natural_id_of_item (item : Res.Item.t) : string option =
  match item with
  | Res.Item.Output_message msg -> Some msg.id
  | Res.Item.Function_call { id = Some id; _ } -> Some id
  | Res.Item.Function_call { id = None; _ } -> None
  | Res.Item.Custom_tool_call { id = Some id; _ } -> Some id
  | Res.Item.Custom_tool_call { id = None; _ } -> None
  | Res.Item.Function_call_output { id = Some id; _ } -> Some id
  | Res.Item.Function_call_output { id = None; _ } -> None
  | Res.Item.Custom_tool_call_output { id = Some id; _ } -> Some id
  | Res.Item.Custom_tool_call_output { id = None; _ } -> None
  | Res.Item.Web_search_call call -> Some call.id
  | Res.Item.File_search_call call -> Some call.id
  | Res.Item.Reasoning reasoning -> Some reasoning.id
  | Res.Item.Input_message _ -> None
;;

let render_input_content_item (item : Res.Input_message.content_item) : string =
  match item with
  | Res.Input_message.Text { text; _ } -> text
  | Res.Input_message.Image { image_url; _ } ->
    Printf.sprintf "<image src=\"%s\" />" image_url
;;

let render_output_content (content : Res.Output_message.content list) : string =
  String.concat ~sep:"\n" (List.map content ~f:(fun part -> part.text))
;;

let string_of_input_role (role : Res.Input_message.role) : string =
  match role with
  | Res.Input_message.Assistant -> "assistant"
  | Res.Input_message.User -> "user"
  | Res.Input_message.System -> "system"
  | Res.Input_message.Developer -> "developer"
;;

let render_file_search (call : Res.File_search_call.t) : string =
  match call.queries with
  | [] -> "file_search"
  | queries -> "file_search: " ^ String.concat ~sep:", " queries
;;

let project_item_content (item : Res.Item.t) : string * string =
  match item with
  | Res.Item.Input_message msg ->
    ( string_of_input_role msg.role
    , String.concat ~sep:"\n" (List.map msg.content ~f:render_input_content_item) )
  | Res.Item.Output_message msg -> "assistant", render_output_content msg.content
  | Res.Item.Function_call call ->
    "assistant", Printf.sprintf "%s %s" call.name call.arguments
  | Res.Item.Custom_tool_call call ->
    "assistant", Printf.sprintf "%s %s" call.name call.input
  | Res.Item.Function_call_output out -> "tool", render_tool_output out.output
  | Res.Item.Custom_tool_call_output out -> "tool", render_tool_output out.output
  | Res.Item.Web_search_call call -> "assistant", Printf.sprintf "web_search %s" call.id
  | Res.Item.File_search_call call -> "assistant", render_file_search call
  | Res.Item.Reasoning reasoning ->
    let summary = List.map reasoning.summary ~f:(fun part -> part.text) in
    "assistant", String.concat ~sep:"\n" summary
;;

let generated_id (next_generated_id : int) : string =
  Printf.sprintf "host-message-%d" next_generated_id
;;

module Projection = struct
  type t =
    { item_ids : string list
    ; next_generated_id : int
    }
  [@@deriving sexp, compare]

  let empty = { item_ids = []; next_generated_id = 1 }

  let resolved_item_id (t : t) ~(position : int) (item : Res.Item.t) : string * int =
    match natural_id_of_item item with
    | Some id -> id, t.next_generated_id
    | None ->
      (match List.nth t.item_ids position with
       | Some id -> id, t.next_generated_id
       | None ->
         let id = generated_id t.next_generated_id in
         id, t.next_generated_id + 1)
  ;;

  let project_at (t : t) ~(position : int) (item : Res.Item.t) : t * Message.t * string =
    let id, next_generated_id = resolved_item_id t ~position item in
    let role, content = project_item_content item in
    let message =
      Message.create ~id ~role ~content ~meta:(Res.Item.jsonaf_of_t item)
    in
    { t with next_generated_id }, message, id
  ;;

  let project_item (t : t) (item : Res.Item.t) : t * Message.t =
    let position = List.length t.item_ids in
    let t, message, id = project_at t ~position item in
    { t with item_ids = t.item_ids @ [ id ] }, message
  ;;

  let project_history (t : t) (items : Res.Item.t list) : t * Message.t list =
    let next_generated_id = ref t.next_generated_id in
    let ids_rev = ref [] in
    let messages_rev = ref [] in
    List.iteri items ~f:(fun position item ->
      let snapshot = { item_ids = t.item_ids; next_generated_id = !next_generated_id } in
      let snapshot, message, id = project_at snapshot ~position item in
      next_generated_id := snapshot.next_generated_id;
      ids_rev := id :: !ids_rev;
      messages_rev := message :: !messages_rev);
    ( { item_ids = List.rev !ids_rev; next_generated_id = !next_generated_id }
    , List.rev !messages_rev )
  ;;

  let project_context
        ~(projection : t)
        ~session_id
        ~now_ms
        ~phase
        ~history
        ~available_tools
        ~session_meta
    : t * Context.t
    =
    let projection, messages = project_history projection history in
    let available_tools = List.map available_tools ~f:Tool_desc.of_request_tool in
    let context =
      Context.
        { session_id; now_ms; phase; messages; available_tools; session_meta }
    in
    projection, context
  ;;
end

module Overlay = struct
  type replacement =
    { target_id : string
    ; message : Message.t
    }
  [@@deriving sexp]

  type op =
    | Prepend_system of string
    | Append_message of Message.t
    | Replace_message of replacement
    | Delete_message of string
    | Halt of string
  [@@deriving sexp]

  type t =
    { prepended_system_messages : Message.t list
    ; appended_messages : Message.t list
    ; replacements : replacement list
    ; deleted_message_ids : string list
    ; halted_reason : string option [@jsonaf.option]
    }
  [@@deriving sexp]

  let empty =
    { prepended_system_messages = []
    ; appended_messages = []
    ; replacements = []
    ; deleted_message_ids = []
    ; halted_reason = None
    }
  ;;

  let of_runtime_turn_effect (turn_effect : Runtime.turn_effect) : (op, string) result =
    match turn_effect with
    | Prepend_system text -> Ok (Prepend_system text)
    | Append_message message ->
      Result.map (Message.of_value message) ~f:(fun message -> Append_message message)
    | Replace_message (target_id, message) ->
      Result.map (Message.of_value message) ~f:(fun message ->
        Replace_message { target_id; message })
    | Delete_message id -> Ok (Delete_message id)
    | Halt reason -> Ok (Halt reason)
  ;;

  let replacement_map (replacements : replacement list) : Message.t String.Map.t =
    List.fold replacements ~init:String.Map.empty ~f:(fun acc replacement ->
      Map.set acc ~key:replacement.target_id ~data:replacement.message)
  ;;

  let apply (t : t) (messages : Message.t list) : Message.t list =
    let deleted = Hash_set.of_list (module String) t.deleted_message_ids in
    let replacements = replacement_map t.replacements in
    let canonical =
      List.filter_map messages ~f:(fun message ->
        if Hash_set.mem deleted message.id
        then None
        else Some (Map.find replacements message.id |> Option.value ~default:message))
    in
    t.prepended_system_messages @ canonical @ t.appended_messages
  ;;
end

module Tool_moderation = struct
  type t =
    | Approve
    | Reject of string
    | Rewrite_args of Jsonaf.t
    | Redirect of string * Jsonaf.t
  [@@deriving sexp]

  let of_runtime (action : Runtime.tool_moderation) : (t, string) result =
    match action with
    | Runtime.Approve -> Ok Approve
    | Runtime.Reject reason -> Ok (Reject reason)
    | Runtime.Rewrite_args args ->
      Result.map (jsonaf_of_lang_value ~name:"Tool.rewrite_args" args) ~f:(fun args ->
        Rewrite_args args)
    | Runtime.Redirect (name, args) ->
      Result.map (jsonaf_of_lang_value ~name:"Tool.redirect" args) ~f:(fun args ->
        Redirect (name, args))
  ;;
end

module Runtime_request = struct
  type t =
    | Request_compaction
    | End_session of string
  [@@deriving sexp, compare]
end

module Outcome = struct
  type t =
    { overlay_ops : Overlay.op list
    ; tool_moderation : Tool_moderation.t option
    ; runtime_requests : Runtime_request.t list
    ; emitted_events : Lang.value list
    }

  let empty =
    { overlay_ops = []
    ; tool_moderation = None
    ; runtime_requests = []
    ; emitted_events = []
    }
  ;;

  let add_tool_moderation (t : t) (action : Tool_moderation.t) : (t, string) result =
    match t.tool_moderation with
    | None -> Ok { t with tool_moderation = Some action }
    | Some _ ->
      Error
        "Expected at most one tool moderation action for a single host event."
  ;;

  let of_runtime_effects (effects : Runtime.local_effect list) : (t, string) result =
    let open Result.Let_syntax in
    let%map outcome =
      List.fold effects ~init:(Ok empty) ~f:(fun acc local_effect ->
        let%bind acc = acc in
        match local_effect with
        | Runtime.Turn_effect turn_effect ->
          let%map op = Overlay.of_runtime_turn_effect turn_effect in
          { acc with overlay_ops = op :: acc.overlay_ops }
        | Runtime.Tool_moderation_effect action ->
          let%bind action = Tool_moderation.of_runtime action in
          add_tool_moderation acc action
        | Runtime.Request_compaction ->
          Ok
            { acc with
              runtime_requests =
                Runtime_request.Request_compaction :: acc.runtime_requests
            }
        | Runtime.End_session reason ->
          Ok
            { acc with
              runtime_requests = Runtime_request.End_session reason :: acc.runtime_requests
            }
        | Runtime.Emit_internal_event event ->
          Ok { acc with emitted_events = event :: acc.emitted_events })
    in
    { overlay_ops = List.rev outcome.overlay_ops
    ; tool_moderation = outcome.tool_moderation
    ; runtime_requests = List.rev outcome.runtime_requests
    ; emitted_events = List.rev outcome.emitted_events
    }
  ;;
end

module Capabilities = struct
  type tool_call_result =
    | Tool_ok of Jsonaf.t
    | Tool_error of string
  [@@deriving sexp]

  type model_call_result =
    | Model_ok of Jsonaf.t
    | Model_refused of string
    | Model_error of string
  [@@deriving sexp]

  type model_recipe =
    { call : payload:Jsonaf.t -> (model_call_result, string) result
    ; spawn : payload:Jsonaf.t -> (string, string) result
    }

  type t =
    { on_log : level:Runtime.log_level -> message:string -> (unit, string) result
    ; on_tool_call : name:string -> args:Jsonaf.t -> (tool_call_result, string) result
    ; on_tool_spawn : name:string -> args:Jsonaf.t -> (string, string) result
    ; model_recipes : model_recipe String.Map.t
    ; on_schedule_after_ms :
        delay_ms:int -> payload:Lang.value -> (string, string) result
    ; on_schedule_cancel : id:string -> (unit, string) result
    }

  let default =
    { on_log = (fun ~level:_ ~message:_ -> Ok ())
    ; on_tool_call = (fun ~name:_ ~args:_ -> Error "Tool.call is not configured")
    ; on_tool_spawn = (fun ~name:_ ~args:_ -> Error "Tool.spawn is not configured")
    ; model_recipes = String.Map.empty
    ; on_schedule_after_ms =
        (fun ~delay_ms:_ ~payload:_ -> Error "Schedule.after_ms is not configured")
    ; on_schedule_cancel =
        (fun ~id:_ -> Error "Schedule.cancel is not configured")
    }
  ;;

  let json_args ~name (value : Lang.value) : (Jsonaf.t, string) result =
    jsonaf_of_lang_value ~name value
  ;;

  let model_recipe (t : t) (recipe : string) : (model_recipe, string) result =
    match Map.find t.model_recipes recipe with
    | Some recipe -> Ok recipe
    | None -> Error (Printf.sprintf "Model recipe %S is not registered" recipe)
  ;;

  let runtime_handlers (t : t) : Runtime.default_handlers =
    let value_of_tool_call_result (result : tool_call_result) : Lang.value =
      match result with
      | Tool_ok json -> Lang.VVariant ("Ok", [ lang_value_of_jsonaf json ])
      | Tool_error message -> Lang.VVariant ("Error", [ Lang.VString message ])
    in
    let value_of_model_call_result (result : model_call_result) : Lang.value =
      match result with
      | Model_ok json -> Lang.VVariant ("Ok", [ lang_value_of_jsonaf json ])
      | Model_refused message -> Lang.VVariant ("Refused", [ Lang.VString message ])
      | Model_error message -> Lang.VVariant ("Error", [ Lang.VString message ])
    in
    { Runtime.default_handlers with
      on_log = (fun _session ~level ~message -> t.on_log ~level ~message)
    ; on_tool_call =
        (fun _session ~name ~args ->
          let open Result.Let_syntax in
          let%bind args = json_args ~name:"Tool.call" args in
          let%map result = t.on_tool_call ~name ~args in
          value_of_tool_call_result result)
    ; on_tool_spawn =
        (fun _session ~name ~args ->
          let open Result.Let_syntax in
          let%bind args = json_args ~name:"Tool.spawn" args in
          t.on_tool_spawn ~name ~args)
    ; on_model_call =
        (fun _session ~recipe ~payload ->
          let open Result.Let_syntax in
          let%bind recipe_handler = model_recipe t recipe in
          let%bind payload = json_args ~name:"Model.call" payload in
          let%map result = recipe_handler.call ~payload in
          value_of_model_call_result result)
    ; on_model_spawn =
        (fun _session ~recipe ~payload ->
          let open Result.Let_syntax in
          let%bind recipe_handler = model_recipe t recipe in
          let%bind payload = json_args ~name:"Model.spawn" payload in
          recipe_handler.spawn ~payload)
    ; on_schedule_after_ms =
        (fun _session ~delay_ms ~payload ->
          t.on_schedule_after_ms ~delay_ms ~payload)
    ; on_schedule_cancel = (fun _session ~id -> t.on_schedule_cancel ~id)
    }
  ;;
end
