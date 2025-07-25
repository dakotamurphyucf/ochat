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

(* Forward declarations *)

(** [string_of_items ~ctx ~run_agent items] concatenates [items] into a plain
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
         (match b.cleanup_html, b.markdown, b.is_local with
          | true, _, _ -> Fetch.get_html ~ctx doc_url ~is_local:b.is_local
          | _, true, true ->
            (* If the document is local, and markdown is requested, we read it from disk and convert to Markdown. *)
            let env = Ctx.env ctx in
            Webpage_markdown.Driver.(
              convert_html_file Eio.Path.(Eio.Stdenv.fs env / doc_url)
              |> Markdown.to_string)
          | _, true, false ->
            (* If the document is not local, and markdown is requested, we fetch it over HTTP and convert to Markdown. *)
            let net = Ctx.net ctx in
            let env = Ctx.env ctx in
            (* Use the Webpage_markdown driver to fetch and convert the page to Markdown. *)
            Webpage_markdown.Driver.(
              fetch_and_convert ~env ~net doc_url |> Markdown.to_string)
          | false, false, _ ->
            (* If the document is local and no cleanup is requested, we read it from disk. *)
            Fetch.get ~ctx doc_url ~is_local:b.is_local)
       | _, _ -> Option.value ~default:"" b.text)
    | CM.Agent ({ url; is_local; items } as agent) ->
      Cache.find_or_add cache agent ~ttl:Time_ns.Span.day ~default:(fun () ->
        let prompt = Fetch.get ~ctx url ~is_local in
        (* delegate to the shared agent runner *)
        run_agent ~ctx prompt items))
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
and convert_basic_item ~ctx (b : CM.basic_content_item) : Res.Input_message.content_item =
  let dir = Ctx.dir ctx in
  match b.image_url, b.document_url with
  | Some { url }, _ ->
    let final = if b.is_local then Io.Base64.file_to_data_uri ~dir url else url in
    Image { image_url = final; detail = "auto"; _type = "input_image" }
  | _, Some doc ->
    let txt =
      match b.cleanup_html, b.markdown, b.is_local with
      | true, _, _ -> Fetch.get_html ~ctx doc ~is_local:b.is_local
      | _, true, true ->
        (* If the document is local, and markdown is requested, we read it from disk and convert to Markdown. *)
        let env = Ctx.env ctx in
        Webpage_markdown.Driver.(
          convert_html_file Eio.Path.(Eio.Stdenv.fs env / doc) |> Markdown.to_string)
      | _, true, false ->
        (* If the document is not local, and markdown is requested, we fetch it over HTTP and convert to Markdown. *)
        let net = Ctx.net ctx in
        let env = Ctx.env ctx in
        (* Use the Webpage_markdown driver to fetch and convert the page to Markdown. *)
        Webpage_markdown.Driver.(fetch_and_convert ~env ~net doc |> Markdown.to_string)
      | false, false, _ ->
        (* If the document is local and no cleanup is requested, we read it from disk. *)
        Fetch.get ~ctx doc ~is_local:b.is_local
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
      | Some (CM.Text t) -> [ Res.Input_message.Text { text = t; _type = "output_text" } ]
      | Some (CM.Items items) ->
        [ Res.Input_message.Text
            { text = string_of_items ~ctx ~run_agent items; _type = "output_text" }
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

(** [to_items ~ctx ~run_agent els] walks the ChatMarkdown document and
    produces the list of {!Openai.Responses.Item.t} that forms the request
    history.

    The traversal is *total*: every constructor of
    {!type:Prompt.Chat_markdown.top_level_elements} is handled.  Elements that
    are processed elsewhere in the pipeline – namely [`<config/>`] and
    [`<tool/>`] declarations – are silently ignored.

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
      let items = Converter.to_items ~ctx ~run_agent doc in
      assert (List.length items = 2)
    ]}
*)
let to_items ~ctx ~run_agent (els : CM.top_level_elements list) : Res.Item.t list =
  (* implementation unchanged below *)
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
