(*********************************************************************
     Conversion of parsed ChatMarkdown (CM) structures into the          
     OpenAI Responses (Res) typed representation.  All helpers share a   
     single immutable context record [Ctx.t] so that long parameter      
     lists disappear, and accept an explicit [~run_agent] callback to    
     resolve nested <agent/> inclusions without creating a circular      
     compile-time dependency between this module and the [run_agent]     
     function that in turn relies on [Converter].                        
  *********************************************************************)

open Core
module CM = Prompt_template.Chat_markdown
module Res = Openai.Responses

type 'env ctx = 'env Ctx.t

(* Forward declarations *)

let rec string_of_items ~ctx ~run_agent (items : CM.content_item list) : string =
  let cache = Ctx.cache ctx in
  items
  |> List.map ~f:(function
    | CM.Basic b ->
      (match b.image_url, b.document_url with
       | Some { url }, _ ->
         if b.is_local
         then Printf.sprintf "<img src=\"%s\" local=\"true\"/>" url
         else Printf.sprintf "<img src=\"%s\"/>" url
       | _, Some doc_url ->
         if b.cleanup_html
         then Fetch.get_html ~ctx doc_url ~is_local:b.is_local
         else Fetch.get ~ctx doc_url ~is_local:b.is_local
       | _, _ -> Option.value ~default:"" b.text)
    | CM.Agent ({ url; is_local; items } as agent) ->
      Cache.find_or_add cache agent ~ttl:Time_ns.Span.day ~default:(fun () ->
        let prompt = Fetch.get ~ctx url ~is_local in
        (* delegate to the shared agent runner *)
        run_agent ~ctx prompt items))
  |> String.concat ~sep:"\n"

(** Convert a basic_content_item to an OpenAI input content item. *)
and convert_basic_item ~ctx (b : CM.basic_content_item) : Res.Input_message.content_item =
  let dir = Ctx.dir ctx in
  match b.image_url, b.document_url with
  | Some { url }, _ ->
    let final = if b.is_local then Io.Base64.file_to_data_uri ~dir url else url in
    Image { image_url = final; detail = "auto"; _type = "input_image" }
  | _, Some doc ->
    let txt =
      if b.cleanup_html
      then Fetch.get_html ~ctx doc ~is_local:b.is_local
      else Fetch.get ~ctx doc ~is_local:b.is_local
    in
    Text { text = txt; _type = "input_text" }
  | _ -> Text { text = Option.value ~default:"" b.text; _type = "input_text" }

and convert_content_item ~ctx ~run_agent (ci : CM.content_item)
  : Res.Input_message.content_item
  =
  let cache = Ctx.cache ctx in
  match ci with
  | CM.Basic b -> convert_basic_item ~ctx b
  | CM.Agent ({ url; is_local; items } as agent) ->
    let txt =
      Cache.find_or_add cache agent ~ttl:Time_ns.Span.day ~default:(fun () ->
        let prompt = Fetch.get ~ctx url ~is_local in
        run_agent ~ctx prompt items)
    in
    Text { text = txt; _type = "input_text" }

and convert_msg ~ctx ~run_agent (m : CM.msg) : Res.Item.t =
  let _ = Ctx.cache ctx in
  let role =
    match String.lowercase m.role with
    | "assistant" -> `Assistant
    | "user" -> `User
    | "system" -> `System
    | "developer" -> `Developer
    | "tool" -> `Tool
    | other -> failwithf "unknown role %s" other ()
  in
  match role with
  | `Assistant ->
    let text =
      match m.content with
      | None -> ""
      | Some (CM.Text t) -> t
      | Some (CM.Items items) -> string_of_items ~ctx ~run_agent items
    in
    Res.Item.Output_message
      { role = Assistant
      ; id = Option.value ~default:"" m.id
      ; status = Option.value m.status ~default:"completed"
      ; _type = "message"
      ; content = [ { annotations = []; text; _type = "output_text" } ]
      }
  | `Tool ->
    let tool_call_id = Option.value_exn m.tool_call_id in
    (match m.content with
     | Some (CM.Text t) ->
       (* function_call vs function_call_output discrimination *)
       (match m.tool_call with
        | Some { id; function_ = { name; arguments } } ->
          Res.Item.Function_call
            { name
            ; arguments
            ; call_id = id
            ; _type = "function_call"
            ; id = m.id
            ; status = None
            }
        | None ->
          Res.Item.Function_call_output
            { call_id = tool_call_id
            ; _type = "function_call_output"
            ; id = None
            ; status = None
            ; output = t
            })
     | Some (CM.Items items) ->
       (match m.tool_call with
        | Some { id; function_ = { name; _ } } ->
          Res.Item.Function_call
            { name
            ; arguments = string_of_items ~ctx ~run_agent items
            ; call_id = id
            ; _type = "function_call"
            ; id = m.id
            ; status = None
            }
        | None ->
          Res.Item.Function_call_output
            { call_id = tool_call_id
            ; _type = "function_call_output"
            ; id = None
            ; status = None
            ; output = string_of_items ~ctx ~run_agent items
            })
     | _ ->
       failwith
         "Expected function_call to be raw text arguments; found structured content.")
  | (`User | `System | `Developer) as r ->
    let role_val =
      match r with
      | `User -> Res.Input_message.User
      | `System -> Res.Input_message.System
      | `Developer -> Res.Input_message.Developer
    in
    let content_items =
      match m.content with
      | None -> []
      | Some (CM.Text t) -> [ Res.Input_message.Text { text = t; _type = "input_text" } ]
      | Some (CM.Items lst) -> List.map lst ~f:(convert_content_item ~ctx ~run_agent)
    in
    Res.Item.Input_message { role = role_val; content = content_items; _type = "message" }

(* ------------------------------------------------------------------ *)
(* Wrapper helpers for the explicit shorthand message aliases.  We keep
   them extremely small for now – simply delegate to [convert_msg] as
   the alias types [user_msg], [assistant_msg] … are all equal to
   [msg].  This makes future refactors easier: when the variants start
   diverging we only need to update the implementation here without
   touching every call-site. *)

and convert_system_msg ~ctx ~run_agent (m : CM.system_msg) : Res.Item.t =
  (* [system_msg] is an alias of [msg] but we *know* its [role] is "system"
     and that it never carries tool-call metadata.  Therefore we can
     shortcut straight to an [Input_message] constructor without any
     defensive pattern-matching on the role field. *)
  let content_items : Res.Input_message.content_item list =
    match m.content with
    | None -> []
    | Some (CM.Text t) -> [ Res.Input_message.Text { text = t; _type = "input_text" } ]
    | Some (CM.Items lst) -> List.map lst ~f:(convert_content_item ~ctx ~run_agent)
  in
  Res.Item.Input_message
    { role = Res.Input_message.System; content = content_items; _type = "message" }

and convert_developer_msg ~ctx ~run_agent (m : CM.developer_msg) : Res.Item.t =
  let content_items : Res.Input_message.content_item list =
    match m.content with
    | None -> []
    | Some (CM.Text t) -> [ Res.Input_message.Text { text = t; _type = "input_text" } ]
    | Some (CM.Items lst) -> List.map lst ~f:(convert_content_item ~ctx ~run_agent)
  in
  Res.Item.Input_message
    { role = Res.Input_message.Developer; content = content_items; _type = "message" }

and convert_user_msg ~ctx ~run_agent (m : CM.user_msg) : Res.Item.t =
  let content_items : Res.Input_message.content_item list =
    match m.content with
    | None -> []
    | Some (CM.Text t) -> [ Res.Input_message.Text { text = t; _type = "input_text" } ]
    | Some (CM.Items lst) -> List.map lst ~f:(convert_content_item ~ctx ~run_agent)
  in
  Res.Item.Input_message
    { role = Res.Input_message.User; content = content_items; _type = "message" }

and convert_assistant_msg ~ctx ~run_agent (m : CM.assistant_msg) : Res.Item.t =
  match m.id, m.status with
  | Some id, status ->
    let text =
      match m.content with
      | None -> ""
      | Some (CM.Text t) -> t
      | Some (CM.Items items) -> string_of_items ~ctx ~run_agent items
    in
    Res.Item.Output_message
      { role = Assistant
      ; id
      ; status = Option.value status ~default:"completed"
      ; _type = "message"
      ; content = [ { annotations = []; text; _type = "output_text" } ]
      }
  | None, None ->
    let content_items : Res.Input_message.content_item list =
      match m.content with
      | None -> []
      | Some (CM.Text t) -> [ Res.Input_message.Text { text = t; _type = "input_text" } ]
      | Some (CM.Items items) ->
        [ Res.Input_message.Text
            { text = string_of_items ~ctx ~run_agent items; _type = "input_text" }
        ]
    in
    Res.Item.Input_message
      { role = Res.Input_message.Assistant; content = content_items; _type = "message" }
  | None, Some _ ->
    raise
      (Failure
         "Assistant message must have both ID and status, or neither.  Found status \
          without ID.")

and convert_tool_call_msg ~ctx ~run_agent (m : CM.tool_call_msg) : Res.Item.t =
  (* Shorthand <tool_call/> – assistant *invoking* a tool.  The parser
     guarantees the presence of [tool_call] with [function_] details. *)
  let { CM.id = call_id; function_ = { name; arguments = raw_args } } =
    Option.value_exn m.tool_call
  in
  (* If the arguments were provided via structured content (Items) we need to
     serialise them back to a string; otherwise fall back to the raw_args
     captured from the attribute. *)
  let arguments =
    if String.is_empty raw_args
    then (
      match m.content with
      | Some (CM.Items items) -> string_of_items ~ctx ~run_agent items
      | Some (CM.Text t) -> t
      | None -> "")
    else raw_args
  in
  Res.Item.Function_call
    { name; arguments; call_id; _type = "function_call"; id = m.id; status = None }

and convert_tool_response_msg ~ctx ~run_agent (m : CM.tool_response_msg) : Res.Item.t =
  (* Shorthand <tool_response/> – the *output* of a previously invoked tool. *)
  let call_id = Option.value_exn m.tool_call_id in
  let output =
    match m.content with
    | None -> ""
    | Some (CM.Text t) -> t
    | Some (CM.Items items) -> string_of_items ~ctx ~run_agent items
  in
  Res.Item.Function_call_output
    { call_id; _type = "function_call_output"; id = None; status = None; output }

and convert_reasoning (r : CM.reasoning) : Res.Item.t =
  let summ =
    List.map r.summary ~f:(fun s -> { Res.Reasoning.text = s.text; _type = s._type })
  in
  Res.Item.Reasoning { id = r.id; status = r.status; _type = "reasoning"; summary = summ }
;;

let to_items ~ctx ~run_agent (els : CM.top_level_elements list) : Res.Item.t list =
  List.filter_map els ~f:(function
    | CM.Msg m -> Some (convert_msg ~ctx ~run_agent m)
    | CM.System m -> Some (convert_system_msg ~ctx ~run_agent m)
    | CM.Developer m -> Some (convert_developer_msg ~ctx ~run_agent m)
    | CM.User m -> Some (convert_user_msg ~ctx ~run_agent m)
    | CM.Assistant m -> Some (convert_assistant_msg ~ctx ~run_agent m)
    | CM.Tool_call m -> Some (convert_tool_call_msg ~ctx ~run_agent m)
    | CM.Tool_response m -> Some (convert_tool_response_msg ~ctx ~run_agent m)
    | CM.Reasoning r -> Some (convert_reasoning r)
    | CM.Config _ -> None
    | CM.Tool _ -> None)
;;
