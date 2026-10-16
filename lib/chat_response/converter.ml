(** ChatMarkdown → OpenAI JSON conversion.

    The module translates the *Abstract-Syntax Tree* produced by
    {!Prompt.Chat_markdown} into the value types exposed by
    {!module:Openai.Responses}.  The generated OCaml structures serialise to
    the exact JSON shape expected by the OpenAI Assistant and Chat
    Completions endpoints.

    {1 Side-effects}

    The conversion itself is pure with two explicit exceptions:

    • {!Fetch.get}/ {!Fetch.get_html} may be invoked when a content item
      references an external document or image.
    • `{!Cache.find_or_add}` is used to memoise nested `<agent/>` calls so
      that identical prompts are only executed once per session.

    Both operations depend on the immutable execution context {!Ctx.t}
    passed to every helper.

    {1 Design choices}

    • The public interface is intentionally tiny: {!to_items} is the single
      entry-point.  Internal helpers are kept hidden to allow for future
      refactors without breaking callers.

    • A first-class callback [~run_agent] is threaded through the call-graph
      to avoid a static dependency on {!module:Driver}.  This keeps the
      compilation unit free of cycles.

    {1 Example}

    {[
      let ctx = Ctx.create ~env ~dir ~cache in
      (* Delegate actual assistant invocation to Driver.run_agent *)
      let run_agent ~ctx prompt_xml inline_items =
        Driver.run_agent ~ctx prompt_xml inline_items
      in
      let items : Openai.Responses.Item.t list =
        Converter.to_items ~ctx ~run_agent parsed_elements
    ]}
*)

open Core
module CM = Prompt.Chat_markdown
module Res = Openai.Responses

type 'env ctx = 'env Ctx.t

exception Source_error of Agent_protocol.Error.t

let source_result = function
  | Ok value -> value
  | Error error -> raise (Source_error error)
;;

let run_nested_agent ~ctx ~run_agent ~url ~is_local items =
  let prompt = Fetch.get ~ctx url ~is_local in
  let prompt_dir = if is_local then Fetch.resolve_local_dir ~ctx url else None in
  run_agent ?prompt_dir ?session_id:(Some url) ~ctx prompt items
;;

(* A supplied owner selects and admits the actual declaration before any response
   cache lookup. Its failure is never a request to Fetch another source. *)
let run_content_agent
      ?source
      ?agent_source_runner
      ~ctx
      ~run_agent
      (agent : CM.agent_content)
  =
  match source with
  | Some source -> source_result (Converter_source.agent source agent)
  | None ->
    (match agent_source_runner with
     | Some runner -> runner.Tool.Agent_source_runner.run_content ~declaration:agent ~ctx
     | None ->
       Cache.find_or_add (Ctx.cache ctx) agent ~ttl:Time_ns.Span.day ~default:(fun () ->
         run_nested_agent
           ~ctx
           ~run_agent
           ~url:agent.url
           ~is_local:agent.is_local
           agent.items))
;;

let function_call_output_part_of_content_item
      ?source
      ?agent_source_runner
      ~ctx
      ~run_agent
      (ci : CM.content_item)
  : Res.Tool_output.Output_part.t
  =
  let dir = Ctx.dir ctx in
  match ci with
  | CM.Basic b ->
    (match b.image_url, b.document_url with
     | Some { url }, _ ->
       let image_url =
         match source with
         | Some source -> source_result (Converter_source.image source b)
         | None -> if b.is_local then Io.Base64.file_to_data_uri ~dir url else url
       in
       Res.Tool_output.Output_part.Input_image
         { image_url; detail = Some Res.Input_message.Auto }
     | _, Some doc ->
       let text =
         match source with
         | Some source -> source_result (Converter_source.document source b)
         | None ->
           (match b.cleanup_html, b.markdown, b.is_local with
            | true, _, _ -> Fetch.get_html ~ctx doc ~is_local:b.is_local
            | _, true, true ->
              let env = Ctx.env ctx in
              Webpage_markdown.Driver.(
                convert_html_file Eio.Path.(Eio.Stdenv.fs env / doc) |> Markdown.to_string)
            | _, true, false ->
              let net = Ctx.net ctx in
              let env = Ctx.env ctx in
              Webpage_markdown.Driver.(
                fetch_and_convert ~env ~net doc |> Markdown.to_string)
            | false, false, _ -> Fetch.get ~ctx doc ~is_local:b.is_local)
       in
       Res.Tool_output.Output_part.Input_text { text }
     | _, _ ->
       Res.Tool_output.Output_part.Input_text { text = Option.value ~default:"" b.text })
  | CM.Agent agent ->
    let text = run_content_agent ?source ?agent_source_runner ~ctx ~run_agent agent in
    Res.Tool_output.Output_part.Input_text { text }
;;

let function_call_output_of_items
      ?source
      ?agent_source_runner
      ~ctx
      ~run_agent
      (items : CM.content_item list)
  : Res.Tool_output.Output.t
  =
  let parts =
    List.map items ~f:(fun ci ->
      function_call_output_part_of_content_item
        ?source
        ?agent_source_runner
        ~ctx
        ~run_agent
        ci)
  in
  Res.Tool_output.Output.Content parts
;;

(* Forward declarations *)

(** [string_of_items ?source ?agent_source_runner ~ctx ~run_agent items] concatenates [items] into a plain
    UTF-8 string.

    Each element of [items] is rendered according to the following rules:

    • Inline images – converted to `<img src="…"/>` tags so that the
      assistant can reason about them.
    • Local references – resolved relative to [Ctx.dir ctx] and inlined
      into the prompt when possible.
    • Nested [`<agent/>`](https://github.com/benchlab/chatmarkdown#agent) –
      executed through the user-supplied [~run_agent] callback and cached
      (see {!Cache}).

    The helper is private but centralised here so that both *input* message
    construction and *function-call* argument serialisation share the exact
    same rendering logic.
*)
let rec string_of_items
          ?source
          ?agent_source_runner
          ~ctx
          ~run_agent
          (items : CM.content_item list)
  : string
  =
  items
  |> List.map ~f:(function
    | CM.Basic b ->
      (match b.image_url, b.document_url with
       | Some { url }, _ ->
         if b.is_local
         then Printf.sprintf "<img src=\"%s\" local=\"true\"/>" url
         else Printf.sprintf "<img src=\"%s\"/>" url
       | _, Some doc_url ->
         (match source with
          | Some source -> source_result (Converter_source.document source b)
          | None ->
            (match b.cleanup_html, b.markdown, b.is_local with
             | true, _, _ -> Fetch.get_html ~ctx doc_url ~is_local:b.is_local
             | _, true, true ->
               let env = Ctx.env ctx in
               Webpage_markdown.Driver.(
                 convert_html_file Eio.Path.(Eio.Stdenv.fs env / doc_url)
                 |> Markdown.to_string)
             | _, true, false ->
               let net = Ctx.net ctx in
               let env = Ctx.env ctx in
               Webpage_markdown.Driver.(
                 fetch_and_convert ~env ~net doc_url |> Markdown.to_string)
             | false, false, _ -> Fetch.get ~ctx doc_url ~is_local:b.is_local))
       | _, _ -> Option.value ~default:"" b.text)
    | CM.Agent agent ->
      run_content_agent ?source ?agent_source_runner ~ctx ~run_agent agent)
  |> String.concat ~sep:"\n"

(** [convert_basic_item ~ctx b] converts a
    {!type:Prompt.Chat_markdown.basic_content_item} into a
    {!Openai.Responses.Input_message.content_item}.

    Behaviour matrix:

    | ChatMarkdown attributes                              | OpenAI output |
    |------------------------------------------------------|---------------|
    | `image_url` present                                  | `Image` with   `detail = "auto"` |
    | `document_url` present and [`cleanup_html`] is `true` | `Text` where the HTML has been stripped with {!Fetch.get_html}. |
    | `document_url` present and [`cleanup_html`]=`false`   | `Text` containing the raw document fetched with {!Fetch.get}. |
    | No URL attributes                                    | `Text` built from [`text`] (defaults to the empty string). |

    Local paths are converted to data-URIs through {!Io.Base64.file_to_data_uri} so that the resulting JSON can be sent verbatim to OpenAI without further processing.
*)
and convert_basic_item ?source ~ctx (b : CM.basic_content_item)
  : Res.Input_message.content_item
  =
  let dir = Ctx.dir ctx in
  match b.image_url, b.document_url with
  | Some { url }, _ ->
    let final =
      match source with
      | Some source -> source_result (Converter_source.image source b)
      | None -> if b.is_local then Io.Base64.file_to_data_uri ~dir url else url
    in
    Image { image_url = final; detail = "auto"; _type = "input_image" }
  | _, Some doc ->
    let txt =
      match source with
      | Some source -> source_result (Converter_source.document source b)
      | None ->
        (match b.cleanup_html, b.markdown, b.is_local with
         | true, _, _ -> Fetch.get_html ~ctx doc ~is_local:b.is_local
         | _, true, true ->
           let env = Ctx.env ctx in
           Webpage_markdown.Driver.(
             convert_html_file Eio.Path.(Eio.Stdenv.fs env / doc) |> Markdown.to_string)
         | _, true, false ->
           let net = Ctx.net ctx in
           let env = Ctx.env ctx in
           Webpage_markdown.Driver.(fetch_and_convert ~env ~net doc |> Markdown.to_string)
         | false, false, _ -> Fetch.get ~ctx doc ~is_local:b.is_local)
    in
    Text { text = txt; _type = "input_text" }
  | _ -> Text { text = Option.value ~default:"" b.text; _type = "input_text" }

and convert_content_item
      ?source
      ?agent_source_runner
      ~ctx
      ~run_agent
      (ci : CM.content_item)
  : Res.Input_message.content_item
  =
  match ci with
  | CM.Basic b -> convert_basic_item ?source ~ctx b
  | CM.Agent agent ->
    let txt = run_content_agent ?source ?agent_source_runner ~ctx ~run_agent agent in
    Text { text = txt; _type = "input_text" }

and convert_msg ?source ?agent_source_runner ~ctx ~run_agent (m : CM.msg) : Res.Item.t =
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
      | Some (CM.Items items) ->
        string_of_items ?source ?agent_source_runner ~ctx ~run_agent items
    in
    Res.Item.Output_message
      { role = Assistant
      ; id = Option.value ~default:"" m.id
      ; status = Option.value m.status ~default:"completed"
      ; _type = "message"
      ; phase = None
      ; content = [ { annotations = []; text; _type = "output_text" } ]
      }
  | `Tool ->
    let is_custom_tool_call =
      match m.type_ with
      | Some ("custom_tool_call" | "custom_tool_call_output") -> true
      | _ -> false
    in
    let output : Res.Tool_output.Output.t =
      match m.content with
      | None -> Res.Tool_output.Output.Text ""
      | Some (CM.Text t) -> Res.Tool_output.Output.Text t
      | Some (CM.Items items) ->
        function_call_output_of_items ?source ?agent_source_runner ~ctx ~run_agent items
    in
    (match m.tool_call with
     | Some { id = call_id; function_ = { name; arguments = raw_args } } ->
       let arguments =
         if String.is_empty raw_args
         then (
           match m.content with
           | None -> ""
           | Some (CM.Text t) -> t
           | Some (CM.Items items) ->
             string_of_items ?source ?agent_source_runner ~ctx ~run_agent items)
         else raw_args
       in
       if is_custom_tool_call
       then
         Res.Item.Custom_tool_call
           { name; input = arguments; call_id; _type = "custom_tool_call"; id = m.id }
       else
         Res.Item.Function_call
           { name; arguments; call_id; _type = "function_call"; id = m.id; status = None }
     | None ->
       let call_id = Option.value_exn m.tool_call_id in
       if is_custom_tool_call
       then
         Res.Item.Custom_tool_call_output
           { call_id; _type = "custom_tool_call_output"; id = None; output }
       else
         Res.Item.Function_call_output
           { call_id; _type = "function_call_output"; id = None; status = None; output })
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
      | Some (CM.Items lst) ->
        List.map
          lst
          ~f:(convert_content_item ?source ?agent_source_runner ~ctx ~run_agent)
    in
    Res.Item.Input_message { role = role_val; content = content_items; _type = "message" }

(* ------------------------------------------------------------------ *)
(* Wrapper helpers for the explicit shorthand message aliases.  We keep
   them extremely small for now – simply delegate to [convert_msg] as
   the alias types [user_msg], [assistant_msg] … are all equal to
   [msg].  This makes future refactors easier: when the variants start
   diverging we only need to update the implementation here without
   touching every call-site. *)

and convert_system_msg ?source ?agent_source_runner ~ctx ~run_agent (m : CM.system_msg)
  : Res.Item.t
  =
  (* [system_msg] is an alias of [msg] but we *know* its [role] is "system"
     and that it never carries tool-call metadata.  Therefore we can
     shortcut straight to an [Input_message] constructor without any
     defensive pattern-matching on the role field. *)
  let content_items : Res.Input_message.content_item list =
    match m.content with
    | None -> []
    | Some (CM.Text t) -> [ Res.Input_message.Text { text = t; _type = "input_text" } ]
    | Some (CM.Items lst) ->
      List.map lst ~f:(convert_content_item ?source ?agent_source_runner ~ctx ~run_agent)
  in
  Res.Item.Input_message
    { role = Res.Input_message.System; content = content_items; _type = "message" }

and convert_developer_msg
      ?source
      ?agent_source_runner
      ~ctx
      ~run_agent
      (m : CM.developer_msg)
  : Res.Item.t
  =
  let content_items : Res.Input_message.content_item list =
    match m.content with
    | None -> []
    | Some (CM.Text t) -> [ Res.Input_message.Text { text = t; _type = "input_text" } ]
    | Some (CM.Items lst) ->
      List.map lst ~f:(convert_content_item ?source ?agent_source_runner ~ctx ~run_agent)
  in
  Res.Item.Input_message
    { role = Res.Input_message.Developer; content = content_items; _type = "message" }

and convert_user_msg ?source ?agent_source_runner ~ctx ~run_agent (m : CM.user_msg)
  : Res.Item.t
  =
  let content_items : Res.Input_message.content_item list =
    match m.content with
    | None -> []
    | Some (CM.Text t) -> [ Res.Input_message.Text { text = t; _type = "input_text" } ]
    | Some (CM.Items lst) ->
      List.map lst ~f:(convert_content_item ?source ?agent_source_runner ~ctx ~run_agent)
  in
  Res.Item.Input_message
    { role = Res.Input_message.User; content = content_items; _type = "message" }

and convert_assistant_msg
      ?source
      ?agent_source_runner
      ~ctx
      ~run_agent
      (m : CM.assistant_msg)
  : Res.Item.t
  =
  match m.id, m.status with
  | Some id, status ->
    let text =
      match m.content with
      | None -> ""
      | Some (CM.Text t) -> t
      | Some (CM.Items items) ->
        string_of_items ?source ?agent_source_runner ~ctx ~run_agent items
    in
    Res.Item.Output_message
      { role = Assistant
      ; id
      ; status = Option.value status ~default:"completed"
      ; _type = "message"
      ; phase = m.phase
      ; content = [ { annotations = []; text; _type = "output_text" } ]
      }
  | None, None ->
    let content_items : Res.Input_message.content_item list =
      match m.content with
      | None -> []
      | Some (CM.Text t) -> [ Res.Input_message.Text { text = t; _type = "input_text" } ]
      | Some (CM.Items items) ->
        [ Res.Input_message.Text
            { text = string_of_items ?source ?agent_source_runner ~ctx ~run_agent items
            ; _type = "input_text"
            }
        ]
    in
    Res.Item.Input_message
      { role = Res.Input_message.Assistant; content = content_items; _type = "message" }
  | None, Some _ ->
    raise
      (Failure
         "Assistant message must have both ID and status, or neither.  Found status \
          without ID.")

and convert_tool_call_msg
      ?source
      ?agent_source_runner
      ~ctx
      ~run_agent
      (m : CM.tool_call_msg)
  : Res.Item.t
  =
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
      | Some (CM.Items items) ->
        string_of_items ?source ?agent_source_runner ~ctx ~run_agent items
      | Some (CM.Text t) -> t
      | None -> "")
    else raw_args
  in
  match m.type_ with
  | Some "custom_tool_call" ->
    Res.Item.Custom_tool_call
      { name; input = arguments; call_id; _type = "custom_tool_call"; id = m.id }
  | _ ->
    Res.Item.Function_call
      { name; arguments; call_id; _type = "function_call"; id = m.id; status = None }

and convert_tool_response_msg
      ?source
      ?agent_source_runner
      ~ctx
      ~run_agent
      (m : CM.tool_response_msg)
  : Res.Item.t
  =
  (* Shorthand <tool_response/> – the *output* of a previously invoked tool. *)
  let call_id = Option.value_exn m.tool_call_id in
  let output =
    match m.content with
    | None -> Res.Tool_output.Output.Text ""
    | Some (CM.Text t) -> Text t
    | Some (CM.Items items) ->
      function_call_output_of_items ?source ?agent_source_runner ~ctx ~run_agent items
  in
  match m.type_ with
  | Some "custom_tool_call" ->
    Res.Item.Custom_tool_call_output
      { call_id; _type = "custom_tool_call_output"; id = None; output }
  | _ ->
    Res.Item.Function_call_output
      { call_id; _type = "function_call_output"; id = None; status = None; output }

and convert_reasoning (r : CM.reasoning) : Res.Item.t =
  let summ =
    List.map r.summary ~f:(fun s -> { Res.Reasoning.text = s.text; _type = s._type })
  in
  Res.Item.Reasoning { id = r.id; status = r.status; _type = "reasoning"; summary = summ }
;;

(** [to_items ?source ?agent_source_runner ~ctx ~run_agent els] walks the ChatMarkdown document and
    produces the list of {!Openai.Responses.Item.t} that forms the request
    history.

    The traversal is *total*: every constructor of
    {!type:Prompt.Chat_markdown.top_level_elements} is handled.  Elements that
    are processed elsewhere in the pipeline – namely [`<config/>`],
    [`<tool/>`], and [`<script/>`] declarations – are silently ignored.

    Invariants:
    • The output list preserves the order of appearance.
    • No item is mutated after creation; the resulting value can therefore
      be shared between fibres.

    Example – converting a small prompt:
    {[
      let open Prompt.Chat_markdown in
      let doc =
        [ Msg { role = "user"; content = Some (Text "Hello!"); id = None
               ; status = None; tool_call_id = None; tool_call = None }
        ; Assistant { role = "assistant"; content = Some (Text "Hi!")
                     ; id = None; status = None; tool_call_id = None }
        ]
      in
      let ctx = Ctx.create ~env ~dir ~cache in
      let items = Converter.to_items ?source ?agent_source_runner ~ctx ~run_agent doc in
      assert (List.length items = 2)
    ]}
*)
let convert_element ?source ?agent_source_runner ~ctx ~run_agent
  : CM.top_level_elements -> Res.Item.t option
  = function
  | CM.Msg m -> Some (convert_msg ?source ?agent_source_runner ~ctx ~run_agent m)
  | CM.System m ->
    Some (convert_system_msg ?source ?agent_source_runner ~ctx ~run_agent m)
  | CM.Developer m ->
    Some (convert_developer_msg ?source ?agent_source_runner ~ctx ~run_agent m)
  | CM.User m -> Some (convert_user_msg ?source ?agent_source_runner ~ctx ~run_agent m)
  | CM.Assistant m ->
    Some (convert_assistant_msg ?source ?agent_source_runner ~ctx ~run_agent m)
  | CM.Tool_call m ->
    Some (convert_tool_call_msg ?source ?agent_source_runner ~ctx ~run_agent m)
  | CM.Tool_response m ->
    Some (convert_tool_response_msg ?source ?agent_source_runner ~ctx ~run_agent m)
  | CM.Reasoning r -> Some (convert_reasoning r)
  | CM.Config _ -> None
  | CM.Tool _ -> None
  | CM.Shell_runtime _ -> None
  | CM.Moderator_runtime _ -> None
  | CM.Script _
  | CM.Shell_script _
  | CM.Extension_script _
  | CM.Authoring_help _
  | CM.Authoring_context _ -> None
;;

let to_items
      ?source
      ?agent_source_runner
      ~ctx
      ~run_agent
      (els : CM.top_level_elements list)
  : Res.Item.t list
  =
  List.filter_map els ~f:(convert_element ?source ?agent_source_runner ~ctx ~run_agent)
;;

(* This counts typed scalars and collection entries before either generated
   JSON projection allocates a tree. It is deliberately a conservative work
   admission, not a second JSON serializer: the shared document validator below
   still measures the exact escaped output. Declarations ignored by Converter
   need no derived JSON allocation. *)
let preflight_selected_element ~max_source_bytes ~max_output_bytes ~checkpoint element =
  let limited () =
    raise
      (Source_error
         (Agent_protocol.Error.create
            Agent_protocol.Error.Resource_limit
            ~message:"child conversion source or output exceeds limit"
            ~retryable:false
            ()))
  in
  let source = ref 0 in
  let output = ref 0 in
  let add counter limit n =
    checkpoint ();
    if n < 0 || n > limit - !counter then limited ();
    counter := !counter + n
  in
  let source_string s = add source max_source_bytes (String.length s) in
  let output_string s = add output max_output_bytes (String.length s) in
  let source_option = Option.iter ~f:source_string in
  let content ~assistant = function
    | CM.Basic basic ->
      add source max_source_bytes 16;
      source_string basic.type_;
      source_option basic.text;
      Option.iter basic.image_url ~f:(fun image -> source_string image.url);
      source_option basic.document_url;
      add output max_output_bytes 1;
      (match basic.image_url, basic.document_url with
       | Some image, _ when assistant ->
         (* The assistant's inline image is rendered as a literal tag. *)
         output_string image.url;
         add output max_output_bytes 16
       | Some _, _ | _, Some _ -> ()
       | None, None -> Option.iter basic.text ~f:output_string)
    | CM.Agent _ -> limited ()
  in
  let message (m : CM.msg) =
    add source max_source_bytes 64;
    source_string m.role;
    List.iter
      [ m.type_; m.name; m.id; m.status; m.phase; m.source_context; m.tool_call_id ]
      ~f:source_option;
    add output max_output_bytes 16;
    let assistant = String.equal m.role "assistant" in
    match m.content with
    | None -> ()
    | Some (CM.Text text) ->
      source_string text;
      output_string text
    | Some (CM.Items items) -> List.iter items ~f:(content ~assistant)
  in
  (match element with
   | CM.Msg m | Developer m | System m | User m | Assistant m -> message m
   | Reasoning r ->
     add source max_source_bytes 48;
     source_string r.id;
     source_string r._type;
     source_option r.status;
     source_option r.source_context;
     add output max_output_bytes 16;
     List.iter r.summary ~f:(fun summary ->
       add source max_source_bytes 16;
       source_string summary._type;
       source_string summary.text;
       add output max_output_bytes 8;
       output_string summary.text)
   | Config _
   | Tool _
   | Shell_runtime _
   | Moderator_runtime _
   | Script _
   | Shell_script _
   | Extension_script _
   | Authoring_context _
   | Authoring_help _ -> ()
   | Tool_call _ | Tool_response _ ->
     raise
       (Source_error
          (Agent_protocol.Error.invalid_request
             "selected child tool history needs its original admitted owner")));
  !source
;;

(* A typed lower bound on the already-converted item prevents building a large
   generated Jsonaf tree when a dynamic source used most of the shared budget.
   The exact encoded-size admission remains in Document_schema. *)
let preflight_output_item ~available ~checkpoint item =
  let limited () =
    raise
      (Source_error
         (Agent_protocol.Error.create
            Agent_protocol.Error.Resource_limit
            ~message:"child conversion output exceeds limit"
            ~retryable:false
            ()))
  in
  let used = ref 0 in
  let add n =
    checkpoint ();
    if n < 0 || n > available - !used then limited ();
    used := !used + n
  in
  let string s = add (String.length s) in
  match item with
  | Res.Item.Input_message message ->
    add 16;
    List.iter message.content ~f:(function
      | Res.Input_message.Text part ->
        add 8;
        string part.text
      | Image part ->
        add 8;
        string part.image_url)
  | Output_message message ->
    add 16;
    string message.id;
    string message.status;
    List.iter message.content ~f:(fun part ->
      add 8;
      string part.text;
      if not (List.is_empty part.annotations) then limited ())
  | Reasoning reasoning ->
    add 16;
    string reasoning.id;
    List.iter reasoning.summary ~f:(fun part ->
      add 8;
      string part.text)
  | Function_call _
  | Custom_tool_call _
  | Function_call_output _
  | Custom_tool_call_output _
  | Web_search_call _
  | File_search_call _ ->
    raise
      (Source_error
         (Agent_protocol.Error.invalid_request
            "selected child output is outside its admitted content subset"))
;;

(** Project authenticated selected messages and reasoning through the ordinary
    Converter role and inline-string traversal. Tool-call history needs its
    separate admitted owner and is rejected here. The source bundle handles
    every source-bearing item; an unexpected legacy Agent fallback is a typed denial. This wrapper
    catches only its own typed source rejection, preserving cancellation and
    unexpected exceptions. The encoded output check covers literals and joined
    strings as well as source callbacks. *)
let to_items_with_source
      ~source
      ~ctx
      ~max_output_bytes
      ~max_source_bytes
      ~remaining_output_bytes
      ~charge_work
      ~charge_output
      ~checkpoint
      elements
  =
  let error code message =
    Agent_protocol.Error.create code ~message ~retryable:false ()
  in
  if max_output_bytes < 2 || max_source_bytes <= 0
  then
    Error
      (error
         Agent_protocol.Error.Resource_limit
         "child conversion output limit must be positive")
  else (
    let denied ?prompt_dir:_ ?session_id:_ ~ctx:_ _ _ =
      raise
        (Source_error
           (error Agent_protocol.Error.Invalid_state "unbound child Agent source"))
    in
    try
      source_result (charge_work 2);
      source_result (charge_output 2);
      let remaining = ref (max_output_bytes - 2) in
      let rec convert first reversed = function
        | [] -> Ok (List.rev reversed)
        | element :: rest ->
          let open Result.Let_syntax in
          source_result (charge_work 1);
          checkpoint ();
          let source_bytes =
            preflight_selected_element
              ~max_source_bytes
              ~max_output_bytes:(Int.max 0 (!remaining - if first then 0 else 1))
              ~checkpoint
              element
          in
          source_result (charge_work source_bytes);
          let item = convert_element ~source ~ctx ~run_agent:denied element in
          (match item with
           | None -> convert first reversed rest
           | Some item ->
             let overhead = if first then 0 else 1 in
             let available =
               Int.min (!remaining - overhead) (remaining_output_bytes ())
             in
             if available <= 0
             then
               Error
                 (error
                    Agent_protocol.Error.Resource_limit
                    "child conversion output exceeds limit")
             else (
               let () = preflight_output_item ~available ~checkpoint item in
               let measured =
                 let open Result.Let_syntax in
                 let%bind limits =
                   Document_schema.Limits.create
                     ~max_bytes:available
                     ~max_depth:256
                     ~max_fields:1_000_000
                     ~max_nodes:1_000_000
                 in
                 Document_schema.Json.validate_and_measure_with_checkpoint
                   ~limits
                   ~checkpoint
                   (Res.Item.jsonaf_of_t item)
               in
               match measured with
               | Error _ ->
                 Error
                   (error
                      Agent_protocol.Error.Resource_limit
                      "child conversion output exceeds limit")
               | Ok encoded_bytes ->
                 source_result (charge_work (encoded_bytes + overhead + 1));
                 source_result (charge_output (encoded_bytes + overhead));
                 remaining := !remaining - overhead - encoded_bytes;
                 convert false (item :: reversed) rest))
      in
      convert true [] elements
    with
    | Source_error error -> Error error)
;;

type identity_bearing_item =
  { item : Res.Item.t
  ; history_id : History_entry.Id.t option
  ; source_index : int
  ; source_context : string option
  }

let history_id = function
  | CM.Msg message
  | CM.System message
  | CM.Developer message
  | CM.User message
  | CM.Assistant message
  | CM.Tool_call message
  | CM.Tool_response message -> message.ochat_history_id
  | CM.Reasoning reasoning -> reasoning.ochat_history_id
  | CM.Config _
  | CM.Tool _
  | CM.Shell_runtime _
  | CM.Moderator_runtime _
  | CM.Script _
  | CM.Shell_script _
  | CM.Extension_script _
  | CM.Authoring_help _
  | CM.Authoring_context _ -> None
;;

let source_context = function
  | CM.Msg message
  | CM.System message
  | CM.Developer message
  | CM.User message
  | CM.Assistant message
  | CM.Tool_call message
  | CM.Tool_response message -> message.source_context
  | CM.Reasoning reasoning -> reasoning.source_context
  | CM.Config _
  | CM.Tool _
  | CM.Shell_runtime _
  | CM.Moderator_runtime _
  | CM.Script _
  | CM.Shell_script _
  | CM.Extension_script _
  | CM.Authoring_help _
  | CM.Authoring_context _ -> None
;;

let to_identity_bearing_items ?source ?agent_source_runner ~ctx ~run_agent elements =
  List.filter_mapi elements ~f:(fun source_index element ->
    match to_items ?source ?agent_source_runner ~ctx ~run_agent [ element ] with
    | [] -> None
    | [ item ] ->
      Some
        { item
        ; history_id = history_id element
        ; source_index
        ; source_context = source_context element
        }
    | _ -> failwith "one prompt element produced multiple response items")
;;
