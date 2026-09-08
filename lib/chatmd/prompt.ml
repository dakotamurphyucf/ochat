open! Core
open Jsonaf.Export
module Ast = Chatmd_ast
module Script_spec = Chatmd_shell_spec.Chatmd_script_spec

let parse str =
  let lexbuf = Lexing.from_string str in
  Chatmd_parser.document (Chatmd_lexer.create ()) lexbuf
;;

module Chat_content = struct
  (* Minimal “image_url” type, analogous to what you might have in the OpenAI API code. *)
  type image_url = { url : string } [@@deriving sexp, jsonaf, hash, bin_io, compare]

  (* A single item of content, which can be text or an image or doc. *)
  type basic_content_item =
    { type_ : string [@key "type"]
    ; text : string option [@jsonaf.option]
    ; image_url : image_url option [@jsonaf.option]
    ; document_url : string option [@jsonaf.option]
    ; is_local : bool [@default false]
    ; cleanup_html : bool [@default false]
    ; markdown : bool [@default false] (* whether to convert HTML to Markdown *)
    }
  [@@deriving sexp, jsonaf, hash, bin_io, compare]

  (* Agent content: has a url, is_local, and sub-items. *)
  type agent_content =
    { url : string
    ; is_local : bool
    ; items : content_item list [@default []]
    }
  [@@deriving sexp, jsonaf, hash, bin_io, compare]

  (* content_item can be either a Basic variant or an Agent variant. *)
  and content_item =
    | Basic of basic_content_item
    | Agent of agent_content
  [@@deriving sexp, jsonaf, hash, bin_io, compare]

  type content_item_list = content_item list
  [@@deriving sexp, jsonaf, hash, bin_io, compare]

  type chat_message_content =
    | Text of string
    | Items of content_item list
  [@@deriving sexp, jsonaf, hash, bin_io, compare]

  let chat_message_content_of_jsonaf (j : Jsonaf.t) =
    match j with
    | `String s -> Text s
    | `Array _ -> Items (list_of_jsonaf content_item_of_jsonaf j)
    | _ -> failwith "chat_message_content_of_jsonaf: expected string or array of items."
  ;;

  let jsonaf_of_chat_message_content = function
    | Text s -> `String s
    | Items items -> jsonaf_of_list jsonaf_of_content_item items
  ;;

  type function_call =
    { name : string
    ; arguments : string
    }
  [@@deriving jsonaf, sexp, hash, bin_io, compare]

  type tool_call =
    { id : string
    ; function_ : function_call
    }
  [@@deriving jsonaf, sexp, hash, bin_io, compare]

  type msg =
    { role : string
    ; type_ : string option [@key "type"] [@jsonaf.option]
    ; content : chat_message_content option
          [@jsonaf.option]
          [@jsonaf.of chat_message_content_of_jsonaf]
          [@jsonaf.to jsonaf_of_chat_message_content]
    ; name : string option [@jsonaf.option]
    ; id : string option [@jsonaf.option] (* NEW *)
    ; status : string option [@jsonaf.option] (* NEW *)
    ; phase : string option [@jsonaf.option]
    ; ochat_history_id : History_entry.Id.t option [@jsonaf.option]
    ; source_context : string option [@jsonaf.option]
    ; function_call : function_call option
          [@jsonaf.option]
          (* DEPRECATED AND NO LONGER USED> TO BE REMOVED USED tool_call for function calls *)
    ; tool_call : tool_call option [@jsonaf.option]
    ; tool_call_id : string option [@jsonaf.option]
    }
  [@@deriving jsonaf, sexp, hash, bin_io, compare]

  (* Alias types for the new shorthand message variants.  We deliberately
     make them plain aliases so that they share the serialisation helpers
     with [msg] and we do not need to duplicate conversion logic. *)

  type user_msg = msg [@@deriving jsonaf, sexp, hash, bin_io, compare]
  type assistant_msg = msg [@@deriving jsonaf, sexp, hash, bin_io, compare]
  type tool_call_msg = msg [@@deriving jsonaf, sexp, hash, bin_io, compare]
  type tool_response_msg = msg [@@deriving jsonaf, sexp, hash, bin_io, compare]
  type developer_msg = msg [@@deriving jsonaf, sexp, hash, bin_io, compare]
  type system_msg = msg [@@deriving jsonaf, sexp, hash, bin_io, compare]

  type custom_tool =
    { name : string
    ; description : string option
    ; command : string
    ; source : Chatmd_shell_spec.Source_ref.t
    }
  [@@deriving jsonaf, sexp, hash, bin_io, compare]

  (* A tool that proxies its invocation to a secondary chatmd “agent” prompt. *)
  type agent_tool =
    { name : string
    ; description : string option
    ; agent : string (** URL or path to the agent chatmd file *)
    ; is_local : bool (** whether the agent file lives on disk *)
    }
  [@@deriving jsonaf, sexp, hash, bin_io, compare]

  type tool =
    | Builtin of string
    | Read_file of Chatmd_read_file_spec.t
    | Custom of custom_tool
    | Shell of Chatmd_shell_spec.Shell_tool_spec.t
    | Agent of agent_tool
    (* A tool exposed by a remote MCP server. *)
    | Mcp of mcp_tool
    | Extension of Chatmd_shell_spec.Extension_spec.tool
    | Inherited of string
  [@@deriving jsonaf, sexp, hash, bin_io, compare]

  and mcp_tool =
    { names : string list option
    ; description : string option
    ; mcp_server : string (** URI of the MCP server hosting the tool *)
    ; strict : bool (** whether to enforce strict parameter matching *)
    ; client_id_env : string option [@jsonaf.option] (** env var holding client_id *)
    ; client_secret_env : string option [@jsonaf.option]
      (** env var holding client_secret *)
    }
  [@@deriving jsonaf, sexp, hash, bin_io, compare]

  (* The config element. *)
  type config =
    { max_tokens : int option [@jsonaf.option]
    ; model : string option [@jsonaf.option]
    ; reasoning_effort : string option [@jsonaf.option]
    ; temperature : float option [@jsonaf.option]
    ; show_tool_call : bool
    ; id : string option [@jsonaf.option]
    }
  [@@deriving jsonaf, sexp, hash, bin_io, compare]

  type script_reference =
    { path : string
    ; source_text : string
    }
  [@@deriving jsonaf, sexp, hash, bin_io, compare]

  type script_source =
    | Inline of string
    | Src of script_reference
  [@@deriving jsonaf, sexp, hash, bin_io, compare]

  type script =
    { id : string
    ; language : string
    ; kind : string
    ; source : script_source
    }
  [@@deriving jsonaf, sexp, hash, bin_io, compare]

  type reasoning_summary =
    { text : string
    ; _type : string
    }
  [@@deriving sexp, jsonaf, hash, bin_io, compare]

  type reasoning =
    { summary : reasoning_summary list
    ; id : string
    ; status : string option
    ; _type : string
    ; ochat_history_id : History_entry.Id.t option
    ; source_context : string option
    }
  [@@deriving sexp, jsonaf, hash, bin_io, compare]

  type top_level_elements =
    | Msg of msg
    | Developer of developer_msg
    | System of system_msg
    | User of user_msg
    | Assistant of assistant_msg
    | Tool_call of tool_call_msg
    | Tool_response of tool_response_msg
    | Config of config
    | Reasoning of reasoning
    | Tool of tool
    | Shell_runtime of Chatmd_shell_spec.Shell_spec.t
    | Moderator_runtime of Chatmd_shell_spec.Manifest_compiler.moderator_runtime
    | Script of script
    | Shell_script of Script_spec.t
    | Extension_script of Chatmd_shell_spec.Extension_spec.script
    | Authoring_context of Chatmd_shell_spec.Extension_spec.authoring_context
    | Authoring_help of Chatmd_shell_spec.Extension_spec.authoring_help
  [@@deriving jsonaf, sexp, hash, bin_io, compare]
end

module Chat_markdown = struct
  include Chat_content

  (* The internal chat_element used while building the final messages. *)
  type chat_element =
    | Message of msg
    | Developer_msg of developer_msg
    | System_msg of system_msg
    | User_msg of user_msg
    | Assistant_msg of assistant_msg
    | Tool_call_msg of tool_call_msg
    | Tool_response_msg of tool_response_msg
    | Config of config
    | Tool of tool
    | Shell_runtime of Chatmd_shell_spec.Shell_spec.t
    | Moderator_runtime of Chatmd_shell_spec.Manifest_compiler.moderator_runtime
    | Script of script
    | Shell_script of Script_spec.t
    | Extension_script of Chatmd_shell_spec.Extension_spec.script
    | Authoring_context of Chatmd_shell_spec.Extension_spec.authoring_context
    | Authoring_help of Chatmd_shell_spec.Extension_spec.authoring_help
    | Reasoning of reasoning
    | Summary of reasoning_summary
    | Text of string
    | Image of string * bool
    | Document of string * bool * bool * bool (* url, is_local, cleanup_html, markdown *)
    | Agent of string (* url *) * bool (* is_local *) * chat_element list
  (* Convert a <msg> element’s children into an Items or a single Text. *)

  let rec content_items_of_elements (elts : chat_element list)
    : Chat_content.content_item list
    =
    match elts with
    | [] -> []
    | Text s :: rest ->
      Basic
        { type_ = "text"
        ; text = Some s
        ; image_url = None
        ; document_url = None
        ; is_local = false
        ; cleanup_html = false
        ; markdown = false
        }
      :: content_items_of_elements rest
    | Image (u, loc) :: rest ->
      Basic
        { type_ = "image_url"
        ; text = None
        ; image_url = Some { url = u }
        ; document_url = None
        ; is_local = loc
        ; cleanup_html = false
        ; markdown = false
        }
      :: content_items_of_elements rest
    | Document (u, loc, cln, md) :: rest ->
      Basic
        { type_ = "text"
        ; text = None
        ; image_url = None
        ; document_url = Some u
        ; is_local = loc
        ; cleanup_html = cln
        ; markdown = md
        }
      :: content_items_of_elements rest
    | Agent (u, loc, ch) :: rest ->
      Agent { url = u; is_local = loc; items = content_items_of_elements ch }
      :: content_items_of_elements rest
    | ( Message _
      | Developer_msg _
      | System_msg _
      | User_msg _
      | Assistant_msg _
      | Tool_call_msg _
      | Tool_response_msg _
      | Config _
      | Reasoning _
      | Summary _
      | Tool _
      | Shell_runtime _
      | Moderator_runtime _
      | Script _
      | Shell_script _
      | Extension_script _
      | Authoring_help _
      | Authoring_context _ )
      :: rest -> content_items_of_elements rest
  ;;

  (* Actually parse the child elements to produce a (Text ...) or (Items ...). *)
  (* Converts a list of child chat_elements into either a single text or a list
     of content items, stored in chat_message_content. *)
  let parse_msg_children (children : chat_element list) : chat_message_content option =
    let items = content_items_of_elements children in
    match items with
    | [] -> None
    (* If there is exactly one Basic text item, store as Text. Otherwise use Items. *)
    | [ Basic { type_ = "text"; text = Some txt; _ } ] -> Some (Text txt)
    | _ -> Some (Items items)
  ;;

  (* Build a msg record from the attributes on <msg>. *)
  let attr_to_msg ~source_context attr (content : chat_message_content option) : msg =
    let history_id_attribute = "ochat-history-id" in
    let history_id_values =
      List.filter_map attr ~f:(fun (name, value) ->
        if String.equal name history_id_attribute then Some value else None)
    in
    let ochat_history_id =
      match history_id_values with
      | [] -> None
      | [ encoded ] ->
        (match History_entry.Id.of_string encoded with
         | Ok id -> Some id
         | Error error -> failwith (sprintf "Invalid %s: %s" history_id_attribute error))
      | _ -> failwith (sprintf "Duplicate %s attribute" history_id_attribute)
    in
    let hash_tbl = Hashtbl.create (module String) in
    List.iter attr ~f:(fun (attr_name, value) ->
      Hashtbl.set hash_tbl ~key:attr_name ~data:value);
    (* deprecated and not used *)
    let function_call, content_opt =
      match Hashtbl.mem hash_tbl "function_call" with
      | false -> None, content
      | true ->
        let name = Hashtbl.find_exn hash_tbl "function_name" in
        let arguments =
          match content with
          | Some (Text t) -> t
          | _ ->
            failwith
              "Expected function_call to be raw text arguments; found structured content."
        in
        Some { name; arguments }, content
    in
    (* new way to handle tool calls *)
    let tool_call, content_opt =
      match function_call with
      | Some _ -> None, content_opt
      | None ->
        if Hashtbl.mem hash_tbl "tool_call"
        then (
          let name = Hashtbl.find_exn hash_tbl "function_name" in
          let id = Hashtbl.find_exn hash_tbl "tool_call_id" in
          let arguments =
            match content_opt with
            | Some (Text t) -> t
            | Some (Items _) -> ""
            | _ ->
              failwith
                "Expected tool_call to be raw text arguments or structured content."
          in
          Some { id; function_ = { name; arguments } }, content_opt)
        else None, content_opt
    in
    { role = Hashtbl.find_exn hash_tbl "role"
    ; type_ = Hashtbl.find hash_tbl "type"
    ; name = Hashtbl.find hash_tbl "name"
    ; id = Hashtbl.find hash_tbl "id" (* NEW *)
    ; status = Hashtbl.find hash_tbl "status" (* NEW *)
    ; phase = Hashtbl.find hash_tbl "phase"
    ; ochat_history_id
    ; source_context =
        Option.first_some
          (Hashtbl.find hash_tbl Chatmd_import_expansion.source_attribute)
          (Some source_context)
    ; function_call
    ; tool_call
    ; content = content_opt
    ; tool_call_id = Hashtbl.find hash_tbl "tool_call_id"
    }
  ;;

  let attr_table attr =
    let tbl = Hashtbl.create (module String) in
    List.iter attr ~f:(fun (name, value) -> Hashtbl.set tbl ~key:name ~data:value);
    tbl
  ;;

  (* Helper to turn a chat_element back to string (for unrecognized markup). *)
  let rec chat_element_to_string = function
    | Summary s -> s.text
    | Reasoning r ->
      let ss = List.map r.summary ~f:(fun s -> s.text) |> String.concat ~sep:" " in
      Printf.sprintf "<reasoning id=\"%s\">%s</reasoning>" r.id ss
    | Agent (url, is_local, children) ->
      let sub_items = List.map children ~f:chat_element_to_string in
      Printf.sprintf
        "<agent src=\"%s\" local=\"%b\">%s</agent>"
        url
        is_local
        (String.concat ~sep:"" sub_items)
    | Text s -> s
    | Image (url, is_local) ->
      if is_local
      then Printf.sprintf "<img src=\"%s\" local=\"true\" />" url
      else Printf.sprintf "<img src=\"%s\" />" url
    | Document (url, local, cleanup, markdown) ->
      let local_attr = if local then " local=\"true\"" else "" in
      let strip_attr = if cleanup then " strip=\"true\"" else "" in
      let md_attr = if markdown then " markdown=\"true\"" else "" in
      Printf.sprintf "<doc src=\"%s\"%s%s%s />" url local_attr strip_attr md_attr
    | Config { max_tokens; model; reasoning_effort; temperature; show_tool_call; id } ->
      let attrs =
        [ Option.map max_tokens ~f:(fun n -> Printf.sprintf "max_tokens=\"%d\"" n)
        ; Option.map model ~f:(fun m -> Printf.sprintf "model=\"%s\"" m)
        ; Option.map reasoning_effort ~f:(fun r ->
            Printf.sprintf "reasoning_effort=\"%s\"" r)
        ; Option.map temperature ~f:(fun t -> Printf.sprintf "temperature=\"%.3f\"" t)
        ; Some (Printf.sprintf "show_tool_call=\"%b\"" show_tool_call)
        ; Option.map id ~f:(fun id -> Printf.sprintf "id=\"%s\"" id)
        ]
        |> List.filter_map ~f:Fun.id
      in
      let attrs_string =
        if List.is_empty attrs then "" else " " ^ String.concat ~sep:" " attrs
      in
      Printf.sprintf "<config%s />" attrs_string
    | Script { id; language; kind; source } ->
      let attrs =
        [ Some (Printf.sprintf "id=\"%s\"" id)
        ; Some (Printf.sprintf "language=\"%s\"" language)
        ; Some (Printf.sprintf "kind=\"%s\"" kind)
        ]
        |> List.filter_map ~f:Fun.id
      in
      let attrs_string = String.concat ~sep:" " attrs in
      (match source with
       | Inline body -> Printf.sprintf "<script %s>%s</script>" attrs_string body
       | Src { path; _ } -> Printf.sprintf "<script %s src=\"%s\" />" attrs_string path)
    | Shell_script script -> Chatmd_script_declaration.serialize script
    | Extension_script script -> Chatmd_extension_declaration.serialize_script script
    | Authoring_context config -> Chatmd_extension_declaration.serialize_authoring config
    | Authoring_help help -> Chatmd_extension_declaration.serialize_help help
    | Tool t ->
      (match t with
       | Extension tool -> Chatmd_extension_declaration.serialize_tool tool
       | Inherited name -> Printf.sprintf "<tool type=\"inherited\" name=\"%s\"/>" name
       | Builtin name -> Printf.sprintf "<tool name=\"%s\" />" name
       | Read_file specification -> Chatmd_read_file_declaration.serialize specification
       | Custom { name; description; command; source = _ } ->
         let desc_attr =
           match description with
           | Some d -> Printf.sprintf " description=\"%s\"" d
           | None -> ""
         in
         Printf.sprintf "<tool name=\"%s\"%s command=\"%s\" />" name desc_attr command
       | Shell tool -> Chatmd_shell_serialization.tool tool
       | Agent { name; description; agent; is_local } ->
         let desc_attr =
           Option.value_map description ~default:"" ~f:(fun d ->
             Printf.sprintf " description=\"%s\"" d)
         in
         let local_attr = if is_local then " local" else "" in
         Printf.sprintf
           "<tool name=\"%s\"%s agent=\"%s\"%s />"
           name
           desc_attr
           agent
           local_attr
       | Mcp { names; description; mcp_server; strict; _ } ->
         let strict_attr = if strict then " strict" else "" in
         (* If the description is present, add it as an attribute. *)
         let desc_attr =
           Option.value_map description ~default:"" ~f:(fun d ->
             Printf.sprintf " description=\"%s\"" d)
         in
         let names_str =
           match names with
           | Some names ->
             "includes=\"" ^ (String.concat ~sep:", " names |> String.escaped) ^ "\""
           | None -> ""
         in
         Printf.sprintf
           "<tool %s%s mcp_server=\"%s\"%s />"
           names_str
           desc_attr
           mcp_server
           strict_attr)
    | Shell_runtime runtime -> Chatmd_shell_serialization.runtime runtime
    | Moderator_runtime moderator ->
      Printf.sprintf
        "<moderator_runtime shell_runtime=\"%s\" />"
        moderator.Chatmd_shell_spec.Manifest_compiler.runtime
    | Developer_msg m
    | System_msg m
    | Message m
    | User_msg m
    | Assistant_msg m
    | Tool_call_msg m
    | Tool_response_msg m ->
      (match m.content with
       | Some (Text t) -> t
       | Some (Items items) ->
         let rec aux it =
           match it with
           | Basic it ->
             (match it.type_ with
              | "text" -> Option.value it.text ~default:""
              | "image_url" ->
                (match it.image_url with
                 | Some { url } -> Printf.sprintf "<img src=\"%s\" />" url
                 | None -> "")
              | _ -> Option.value it.text ~default:"")
           | Agent { url; is_local; items } ->
             let pieces = List.map items ~f:aux in
             Printf.sprintf
               "<agent src=\"%s\" local=\"%b\">%s</agent>"
               url
               is_local
               (String.concat ~sep:"" pieces)
         in
         let pieces = List.map items ~f:aux in
         String.concat ~sep:"" pieces
       | None -> "")

  and script_body_of_children children =
    List.map children ~f:chat_element_to_string |> String.concat ~sep:""
  ;;

  let legacy_script (script : Script_spec.t) =
    let source =
      match script.source with
      | Script_spec.Inline source -> Inline source
      | Script_spec.Src { path; source_text } -> Src { path; source_text }
    in
    { id = script.id
    ; language = script.language
    ; kind = Script_spec.kind_to_string script.kind
    ; source
    }
  ;;

  let script_error diagnostics =
    diagnostics
    |> List.map ~f:Chatmd_shell_spec.Diagnostic.to_string
    |> String.concat ~sep:"\n"
    |> failwith
  ;;

  let parse_script ~dir ~loader ~source_node ~source_ref ~attrs ~children =
    if
      List.exists attrs ~f:(function
        | "api", _ | "kind", Some "tool" -> true
        | _ -> false)
    then (
      match
        Chatmd_extension_declaration.script
          ~dir
          ~loader
          ~source_node
          ~source:source_ref
          ~attributes:attrs
          ~inline_source:(script_body_of_children children)
      with
      | Ok script -> Extension_script script
      | Error diagnostics -> script_error diagnostics)
    else (
      match
        Chatmd_script_declaration.parse
          ~dir
          ~loader
          ~source_node
          ~source:source_ref
          ~attributes:attrs
          ~inline_source:(script_body_of_children children)
      with
      | Error diagnostics -> script_error diagnostics
      | Ok script ->
        (match script.kind with
         | Script_spec.Moderator -> Script (legacy_script script)
         | _ -> Shell_script script))
  ;;

  (*--------------------------------------------------------------------------*)
  (* Fold the expanded source tree. Generated parsing preserves the provenance
     of inline imports; the legacy parser retains its existing parent context. *)
  let rec tree ~preserve_child_sources (sourced : Chatmd_import_expansion.sourced_node) ~f
    =
    let children =
      match preserve_child_sources, sourced.children with
      | true, Some children -> children
      | _ ->
        (match sourced.node with
         | Ast.Text _ -> []
         | Ast.Element (_, _, children) ->
           List.map children ~f:(fun node -> { sourced with node; children = None }))
    in
    let results =
      List.map children ~f:(fun child -> tree ~preserve_child_sources child ~f)
    in
    f ~source_ref:sourced.source ~source_node:sourced.source_node sourced.node results
  ;;

  (* Convert AST nodes into internal chat elements before exposing top-level values. *)
  let parse_chat_element ~dir ~loader ~preserve_child_sources sourced =
    tree ~preserve_child_sources sourced ~f:(fun ~source_ref ~source_node node children ->
      let source_context = source_ref.Chatmd_shell_spec.Source_ref.file in
      match node with
      | Element (Msg, attrs, _) ->
        let attr = List.map attrs ~f:(fun (n, v) -> n, Option.value v ~default:"") in
        let content_opt = parse_msg_children children in
        Message (attr_to_msg ~source_context attr content_opt)
      | Element (Developer, attrs, _) ->
        let attr = List.map attrs ~f:(fun (n, v) -> n, Option.value v ~default:"") in
        let content_opt = parse_msg_children children in
        let role_attr = "role", "developer" in
        let attrs = role_attr :: attr in
        Developer_msg (attr_to_msg ~source_context attrs content_opt)
      | Element (System, attrs, _) ->
        let attr = List.map attrs ~f:(fun (n, v) -> n, Option.value v ~default:"") in
        let content_opt = parse_msg_children children in
        let role_attr = "role", "system" in
        let attrs = role_attr :: attr in
        System_msg (attr_to_msg ~source_context attrs content_opt)
      | Element (User, attrs, _) ->
        let attr = List.map attrs ~f:(fun (n, v) -> n, Option.value v ~default:"") in
        let content_opt = parse_msg_children children in
        let role_attr = "role", "user" in
        let attrs = role_attr :: attr in
        User_msg (attr_to_msg ~source_context attrs content_opt)
      | Element (Assistant, attrs, _) ->
        let attr = List.map attrs ~f:(fun (n, v) -> n, Option.value v ~default:"") in
        let content_opt = parse_msg_children children in
        let role_attr = "role", "assistant" in
        let attrs = role_attr :: attr in
        Assistant_msg (attr_to_msg ~source_context attrs content_opt)
      | Element (Tool_call, attrs, _) ->
        let attr = List.map attrs ~f:(fun (n, v) -> n, Option.value v ~default:"") in
        let content_opt = parse_msg_children children in
        let role_attr = "role", "assistant" in
        let tool_call_attr = "tool_call", "true" in
        let attrs = role_attr :: tool_call_attr :: attr in
        Tool_call_msg (attr_to_msg ~source_context attrs content_opt)
      | Element (Tool_response, attrs, _) ->
        let attr = List.map attrs ~f:(fun (n, v) -> n, Option.value v ~default:"") in
        let content_opt = parse_msg_children children in
        let role_attr = "role", "tool" in
        let attrs = role_attr :: attr in
        Tool_response_msg (attr_to_msg ~source_context attrs content_opt)
      | Element (Img, attrs, _) ->
        let attr = List.map attrs ~f:(fun (n, v) -> n, Option.value v ~default:"") in
        let tbl = Hashtbl.create (module String) in
        List.iter attr ~f:(fun (nm, v) -> Hashtbl.set tbl ~key:nm ~data:v);
        let url = Option.value (Hashtbl.find tbl "src") ~default:"" in
        let is_local = Hashtbl.mem tbl "local" in
        Image (url, is_local)
      | Element (Doc, attrs, _) ->
        let attr = List.map attrs ~f:(fun (n, v) -> n, Option.value v ~default:"") in
        let tbl = Hashtbl.create (module String) in
        List.iter attr ~f:(fun (nm, v) -> Hashtbl.set tbl ~key:nm ~data:v);
        let url = Option.value (Hashtbl.find tbl "src") ~default:"" in
        let local = Hashtbl.mem tbl "local" in
        let strip = Hashtbl.mem tbl "strip" in
        let md = Hashtbl.mem tbl "markdown" in
        Document (url, local, strip, md)
      | Element (Config, attrs, _) ->
        let attr = List.map attrs ~f:(fun (n, v) -> n, Option.value v ~default:"") in
        let tbl = attr_table attr in
        let max_tokens = Option.map (Hashtbl.find tbl "max_tokens") ~f:Int.of_string in
        let model = Hashtbl.find tbl "model" in
        let reasoning_effort = Hashtbl.find tbl "reasoning_effort" in
        let temperature =
          Option.map (Hashtbl.find tbl "temperature") ~f:Float.of_string
        in
        let show_tool_call = Hashtbl.mem tbl "show_tool_call" in
        let id = Hashtbl.find tbl "id" in
        Config { max_tokens; model; reasoning_effort; temperature; show_tool_call; id }
      | Element (Script, attrs, _) ->
        parse_script ~dir ~loader ~source_node ~source_ref ~attrs ~children
      | Element (Summary, attrs, _) ->
        let attr = List.map attrs ~f:(fun (n, v) -> n, Option.value v ~default:"") in
        let typ =
          List.find_map attr ~f:(fun (n, v) ->
            if String.equal n "type" then Some v else None)
          |> Option.value ~default:"summary_text"
        in
        let txt =
          List.map children ~f:chat_element_to_string
          |> String.concat ~sep:""
          |> String.strip
        in
        Summary { text = txt; _type = typ }
      | Element (Reasoning, attrs, _) ->
        let attr = List.map attrs ~f:(fun (n, v) -> n, Option.value v ~default:"") in
        let history_ids =
          List.filter_map attr ~f:(fun (name, value) ->
            if String.equal name "ochat-history-id" then Some value else None)
        in
        let ochat_history_id =
          match history_ids with
          | [] -> None
          | [ encoded ] ->
            (match History_entry.Id.of_string encoded with
             | Ok id -> Some id
             | Error error -> failwith ("Invalid ochat-history-id: " ^ error))
          | _ -> failwith "Duplicate ochat-history-id attribute"
        in
        let source_context =
          Option.first_some
            (List.Assoc.find
               attr
               ~equal:String.equal
               Chatmd_import_expansion.source_attribute)
            (Some source_context)
        in
        let tbl = Hashtbl.create (module String) in
        List.iter attr ~f:(fun (n, v) -> Hashtbl.set tbl ~key:n ~data:v);
        let id = Hashtbl.find_exn tbl "id" in
        let status = Hashtbl.find tbl "status" in
        let summaries =
          List.filter_map children ~f:(function
            | Summary s -> Some s
            | Text t when not (String.is_empty (String.strip t)) ->
              Some { text = String.strip t; _type = "summary_text" }
            | _ -> None)
        in
        Reasoning
          { id
          ; status
          ; _type = "reasoning"
          ; summary = summaries
          ; ochat_history_id
          ; source_context
          }
      | Element (Agent, attrs, __bin_read_content_item__) ->
        let attr = List.map attrs ~f:(fun (n, v) -> n, Option.value v ~default:"") in
        let url_attr =
          List.find_map attr ~f:(fun (nm, v) ->
            if String.equal nm "src" then Some v else None)
        in
        let agent_url = Option.value url_attr ~default:"" in
        let agent_is_local = List.exists attr ~f:(fun (nm, _) -> String.(nm = "local")) in
        let agent_url =
          if agent_is_local
          then
            Source_loader.agent_reference loader ~base:source_node ~reference:agent_url
            |> Result.ok_or_failwith
          else agent_url
        in
        Agent (agent_url, agent_is_local, children)
      | Element (Tool, attrs, _) ->
        let attr = List.map attrs ~f:(fun (n, v) -> n, Option.value v ~default:"") in
        let tbl = Hashtbl.create (module String) in
        List.iter attr ~f:(fun (nm, v) -> Hashtbl.set tbl ~key:nm ~data:v);
        let name = Hashtbl.find tbl "name" |> Option.value ~default:"" |> String.strip in
        let command = Hashtbl.find tbl "command" in
        let agent = Hashtbl.find tbl "agent" in
        let mcp_server = Hashtbl.find tbl "mcp_server" in
        let description = Hashtbl.find tbl "description" in
        let is_local = Hashtbl.mem tbl "local" in
        let is_shell =
          Option.value_map
            (Hashtbl.find tbl "type")
            ~default:false
            ~f:(String.equal "shell")
          || Hashtbl.mem tbl "runtime"
        in
        if
          List.exists attrs ~f:(function
            | "type", Some "inherited" -> true
            | _ -> false)
        then (
          let allowed =
            Chatmd_attributes.create
              ~source:source_ref
              ~path:[ "tool" ]
              ~allowed:[ "name"; "type" ]
              attrs
          in
          let attributes =
            match allowed with
            | Ok value -> value
            | Error d -> script_error [ d ]
          in
          let inherited =
            match Chatmd_attributes.required attributes "name" with
            | Ok value -> value
            | Error d -> script_error [ d ]
          in
          if
            String.is_empty inherited
            || String.length inherited > 256
            || not
                 (String.for_all inherited ~f:(function
                    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' | ':' | '.' -> true
                    | _ -> false))
          then failwith "invalid inherited tool name";
          (match node with
           | Ast.Element (_, _, children)
             when List.for_all children ~f:(function
                    | Ast.Text text -> String.for_all text ~f:Char.is_whitespace
                    | _ -> false) -> ()
           | _ -> failwith "inherited tool references cannot have children");
          Tool (Inherited inherited))
        else if
          List.exists attrs ~f:(function
            | "type", Some ("moderator" | "chatml") -> true
            | _ -> false)
        then (
          match
            Chatmd_extension_declaration.tool ~loader ~source_node ~source:source_ref node
          with
          | Ok tool -> Tool (Extension tool)
          | Error diagnostics -> script_error diagnostics)
        else if is_shell
        then (
          match Chatmd_shell_declaration.parse_tool ~source:source_ref node with
          | Ok tool -> Tool (Shell tool)
          | Error diagnostics ->
            failwith
              (String.concat
                 ~sep:"; "
                 (List.map diagnostics ~f:Chatmd_shell_spec.Diagnostic.to_string)))
        else (
          match command, agent, mcp_server with
          | Some _, Some _, _ | Some _, _, Some _ | _, Some _, Some _ ->
            failwith
              "<tool> cannot combine 'command', 'agent' and 'mcp_server' attributes."
          | Some cmd, None, None ->
            if String.is_empty name then failwith "Tool name cannot be empty.";
            let cmd = String.strip cmd in
            if String.is_empty cmd then failwith "Tool command cannot be empty.";
            let description = Option.map description ~f:String.strip in
            Tool (Custom { name; description; command = cmd; source = source_ref })
          | None, Some agent_url, None ->
            if String.is_empty name then failwith "Tool name cannot be empty.";
            let agent_url = String.strip agent_url in
            if String.is_empty agent_url then failwith "Tool agent URL cannot be empty.";
            let description = Option.map description ~f:String.strip in
            let agent =
              if is_local
              then
                Source_loader.agent_reference
                  loader
                  ~base:source_node
                  ~reference:agent_url
                |> Result.ok_or_failwith
              else agent_url
            in
            Tool (Agent { name; description; agent; is_local })
          | None, None, Some mcp_uri ->
            let mcp_uri = String.strip mcp_uri in
            if String.is_empty mcp_uri
            then failwith "Tool mcp_server URI cannot be empty.";
            let description = Option.map description ~f:String.strip in
            let strict = Hashtbl.mem tbl "strict" in
            let client_id_env = Hashtbl.find tbl "client_id_env" in
            let client_secret_env = Hashtbl.find tbl "client_secret_env" in
            (* Accept both [include] and [includes] as attribute names to avoid
              confusion.  If both are present we prefer the more specific
              [include] spelling. *)
            let include_ =
              match Hashtbl.find tbl "include" with
              | Some v -> String.strip v
              | None ->
                Hashtbl.find tbl "includes" |> Option.value ~default:"" |> String.strip
            in
            let names =
              if not @@ String.is_empty name
              then Some [ name ]
              else if not @@ String.is_empty include_
              then Some (String.split ~on:',' include_ |> List.map ~f:String.strip)
              else None
            in
            Tool
              (Mcp
                 { names
                 ; description
                 ; mcp_server = mcp_uri
                 ; strict
                 ; client_id_env
                 ; client_secret_env
                 })
          | None, None, None ->
            if String.is_empty name
            then failwith "Tool name cannot be empty."
            else if String.equal name "read_file" || String.equal name "get_contents"
            then (
              match Chatmd_read_file_declaration.parse ~source:source_ref node with
              | Ok specification -> Tool (Read_file specification)
              | Error diagnostics ->
                failwith
                  (String.concat
                     ~sep:"; "
                     (List.map diagnostics ~f:Chatmd_shell_spec.Diagnostic.to_string)))
            else Tool (Builtin name))
      | Element (Shell_access, _, _) ->
        (match Chatmd_shell_declaration.parse_runtime ~source:source_ref node with
         | Ok runtime -> Shell_runtime runtime
         | Error diagnostics ->
           failwith
             (String.concat
                ~sep:"; "
                (List.map diagnostics ~f:Chatmd_shell_spec.Diagnostic.to_string)))
      | Element (Moderator_runtime, _, _) ->
        (match Chatmd_moderator_runtime_declaration.parse ~source:source_ref node with
         | Ok moderator -> Moderator_runtime moderator
         | Error diagnostics ->
           failwith
             (String.concat
                ~sep:"; "
                (List.map diagnostics ~f:Chatmd_shell_spec.Diagnostic.to_string)))
      | Element (Authoring_help, _, _) ->
        (match Chatmd_extension_declaration.authoring_help ~source:source_ref node with
         | Ok help -> Authoring_help help
         | Error diagnostics -> script_error diagnostics)
      | Element (Authoring_context, _, _) ->
        (match Chatmd_extension_declaration.authoring_context ~source:source_ref node with
         | Ok config -> Authoring_context config
         | Error diagnostics -> script_error diagnostics)
      | Element (Uses, _, _) | Element (Shell_element _, _, _) -> Text ""
      | Element (Import, attrs, _) ->
        let attr_to_string (n, v) =
          Printf.sprintf "%s=\"%s\"" n (Option.value v ~default:"")
        in
        let attr = List.map attrs ~f:attr_to_string in
        let raw_content =
          Printf.sprintf "<%s %s/>" "import" (String.concat ~sep:" " attr)
        in
        Text raw_content
      | Text t -> Text t)
  ;;

  (* We only want to capture recognised top-level chat elements. *)
  let chat_elements document =
    List.filter document ~f:(fun sourced ->
      match sourced.Chatmd_import_expansion.node with
      | Ast.Element (Msg, _, _)
      | Element (Developer, _, _)
      | Element (System, _, _)
      | Element (User, _, _)
      | Element (Assistant, _, _)
      | Element (Tool_call, _, _)
      | Element (Tool_response, _, _)
      | Element (Config, _, _)
      | Element (Reasoning, _, _)
      | Element (Tool, _, _)
      | Element (Shell_access, _, _)
      | Element (Moderator_runtime, _, _)
      | Element (Authoring_context, _, _)
      | Element (Authoring_help, _, _)
      | Element (Script, _, _) -> true
      | _ -> false)
  ;;

  (* Transform the final “Maybe chat_element” from parse_chat_elements
     into top_level_elements we can store. *)
  let to_top_level = function
    | Message m -> Some (Msg m)
    | User_msg m -> Some (User m)
    | Assistant_msg m -> Some (Assistant m)
    | Tool_call_msg m -> Some (Tool_call m)
    | Tool_response_msg m -> Some (Tool_response m)
    | Config c -> Some (Config c)
    | Reasoning r -> Some (Reasoning r)
    | Tool t -> Some (Tool t)
    | Shell_runtime runtime -> Some (Shell_runtime runtime)
    | Moderator_runtime moderator -> Some (Moderator_runtime moderator)
    | Script s -> Some (Script s)
    | Shell_script script -> Some (Shell_script script)
    | Extension_script script -> Some (Extension_script script)
    | Authoring_context config -> Some (Authoring_context config)
    | Authoring_help help -> Some (Authoring_help help)
    | Developer_msg m -> Some (Developer m)
    | System_msg m -> Some (System m) (* System is a legacy alias for Developer *)
    | _ -> None
  ;;

  let of_chat_elements (elts : chat_element list) : top_level_elements list =
    List.filter_map elts ~f:to_top_level
  ;;

  let validate_scripts (elements : top_level_elements list) =
    let moderators, shell_scripts =
      List.fold elements ~init:([], []) ~f:(fun (moderators, shell_scripts) -> function
        | Script script -> script :: moderators, shell_scripts
        | Shell_script script -> moderators, script :: shell_scripts
        | _ -> moderators, shell_scripts)
    in
    let module X = Chatmd_shell_spec.Extension_spec in
    let extension_scripts =
      List.filter_map elements ~f:(function
        | Extension_script script -> Some script
        | _ -> None)
    in
    let extensions =
      List.filter_map elements ~f:(function
        | Tool (Extension tool) -> Some tool
        | _ -> None)
    in
    let error source code message =
      script_error [ Chatmd_shell_spec.Diagnostic.error ~source ~code message ]
    in
    let extension_moderators =
      List.filter extension_scripts ~f:(fun script ->
        X.equal_script_kind script.kind Moderator_script)
    in
    let moderator_ids =
      List.map moderators ~f:(fun script -> script.id)
      @ List.map extension_moderators ~f:(fun script -> script.id)
    in
    (match
       Chatmd_script_declaration.validate_prompt_registry ~moderator_ids shell_scripts
     with
     | Ok () -> ()
     | Error diagnostics -> script_error diagnostics);
    let all_ids =
      List.map moderators ~f:(fun script -> script.id)
      @ List.map shell_scripts ~f:(fun script -> script.Script_spec.id)
      @ List.map extension_scripts ~f:(fun script -> script.id)
    in
    (match List.find_a_dup all_ids ~compare:String.compare with
     | None -> ()
     | Some id ->
       script_error
         [ Chatmd_shell_spec.Diagnostic.error
             ~code:"chatmd.duplicate_script"
             ("duplicate script id: " ^ id)
         ]);
    let policies =
      List.filter_map elements ~f:(function
        | Authoring_context policy -> Some policy
        | _ -> None)
    in
    if List.length policies > 1
    then
      error
        (List.hd_exn policies).source_ref
        "chatmd.duplicate_authoring_context"
        "only one authoring_context declaration is permitted";
    let help_declarations =
      List.filter_map elements ~f:(function
        | Authoring_help help -> Some help
        | _ -> None)
    in
    (match
       List.find_a_dup help_declarations ~compare:(fun a b ->
         String.compare a.X.tool b.X.tool)
     with
     | None -> ()
     | Some help ->
       error
         help.source_ref
         "chatmd.duplicate_authoring_help"
         "only one help declaration per tool is permitted");
    List.iter extensions ~f:(fun tool ->
      let target, kind =
        match tool.X.implementation with
        | Moderator id -> id, X.Moderator_script
        | Standalone { script; _ } -> script, X.Tool_script
      in
      match
        List.find extension_scripts ~f:(fun script -> String.equal script.id target)
      with
      | Some script when X.equal_script_kind script.kind kind -> ()
      | _ ->
        error
          tool.source_ref
          "chatmd.extension_missing_handler"
          "extension tool must reference a script of the correct kind and v1 surface");
    if not (List.is_empty extensions)
    then (
      let names =
        List.concat_map elements ~f:(function
          | Tool (Builtin name) -> [ name ]
          | Tool (Read_file _) -> [ "read_file" ]
          | Tool (Custom tool) -> [ tool.name ]
          | Tool (Shell tool) -> [ tool.name ]
          | Tool (Agent tool) -> [ tool.name ]
          | Tool (Mcp tool) -> Option.value tool.names ~default:[]
          | Tool (Extension tool) -> [ tool.name ]
          | Tool (Inherited name) -> [ name ]
          | _ -> [])
      in
      List.iter extensions ~f:(fun tool ->
        if List.count names ~f:(String.equal tool.name) > 1
        then
          error
            tool.source_ref
            "chatmd.extension_duplicate_tool"
            "extension tool name conflicts with another declaration");
      let complete = Hash_set.create (module String) in
      let rec visit active (tool : X.tool) =
        if Set.mem active tool.name
        then
          error
            tool.source_ref
            "chatmd.extension_capability_cycle"
            "cyclic extension capability dependencies";
        if not (Hash_set.mem complete tool.name)
        then (
          let active = Set.add active tool.name in
          List.iter tool.uses ~f:(fun name ->
            Option.iter
              (List.find extensions ~f:(fun candidate -> String.equal candidate.name name))
              ~f:(visit active));
          Hash_set.add complete tool.name)
      in
      List.iter extensions ~f:(visit String.Set.empty));
    elements
  ;;

  let validate_declarations = validate_scripts

  let parse_inputs
        ~parse_document
        ~preprocess
        ~canonical_sources
        ?source
        ?source_loader
        ~dir
        (xml_content : string)
    =
    let source_file = Option.value source ~default:"<prompt>" in
    let loader =
      Option.value source_loader ~default:(Source_loader.filesystem ~root:dir)
    in
    let root_source =
      Source_loader.root loader ~file:source_file |> Result.ok_or_failwith
    in
    let xml_content = preprocess xml_content in
    let document = parse_document xml_content in
    let expanded =
      Chatmd_import_expansion.expand
        ~canonical_sources
        ~parse:parse_document
        ~loader
        ~root_source
        ~dir
        ~file:source_file
        ~source:xml_content
        document
    in
    let chat_elements = chat_elements expanded in
    let parsed_elements =
      List.map chat_elements ~f:(fun sourced ->
        parse_chat_element ~dir ~loader ~preserve_child_sources:canonical_sources sourced)
    in
    of_chat_elements parsed_elements |> validate_scripts
  ;;

  let parse_chat_inputs_without_preprocessing ?source ~source_loader ~dir content =
    parse_inputs
      ~parse_document:parse
      ~preprocess:Fn.id
      ~canonical_sources:false
      ?source
      ~source_loader
      ~dir
      content
  ;;

  let parse_chat_inputs ?source ?source_loader ~dir content =
    parse_inputs
      ~parse_document:parse
      ~preprocess:Meta_prompting.Preprocessor.preprocess
      ~canonical_sources:false
      ?source
      ?source_loader
      ~dir
      content
  ;;

  type parsed_bundle =
    { root : top_level_elements list
    ; agents : (string * top_level_elements list) list
    }

  let parse_source_bundle ~dir bundle =
    let reads = ref 0
    and bytes = ref 0
    and tokens = ref 0 in
    let queued = Queue.create ()
    and seen = Hash_set.create (module String) in
    let loader =
      Chatmd_source_bundle.loader bundle ~root:dir
      |> Source_loader.with_observer ~f:(fun _ text ->
        incr reads;
        if !reads > 1024 || String.length text > (8 * 1024 * 1024) - !bytes
        then failwith "generated source expansion limit exceeded";
        bytes := !bytes + String.length text)
      |> Source_loader.with_agent_observer ~f:(fun source ->
        Queue.enqueue queued (Source_loader.relative_path source))
    in
    let parse_document content =
      if not (Stdlib.String.is_valid_utf_8 content)
      then failwith "generated ChatMD source must be valid UTF-8";
      Meta_prompting.Preprocessor.validate_inert content |> Result.ok_or_failwith;
      let depth = ref 0
      and next = Chatmd_lexer.create () in
      let token lexbuf =
        incr tokens;
        if !tokens > 100_000 then failwith "generated source token limit exceeded";
        let token = next lexbuf in
        (match token with
         | Chatmd_parser.START _ -> incr depth
         | END _ -> decr depth
         | _ -> ());
        if !depth > 128 then failwith "generated markup nesting limit exceeded";
        token
      in
      let document = Chatmd_parser.document token (Lexing.from_string content) in
      let rec check = function
        | Ast.Text _ -> ()
        | Ast.Element (tag, attrs, children) ->
          let has name = List.exists attrs ~f:(fun (key, _) -> String.equal key name) in
          if
            (Ast.tag_equal tag Ast.Agent || (Ast.tag_equal tag Ast.Tool && has "agent"))
            && not (has "local")
          then failwith "generated agent definitions must use bundled local sources";
          List.iter children ~f:check
      in
      List.iter document ~f:check;
      document
    in
    let parse_file file =
      Hash_set.add seen file;
      let source = Source_loader.root loader ~file |> Result.ok_or_failwith in
      let content =
        Source_loader.read_bounded
          ~max_bytes:(Chatmd_source_bundle.limits bundle).max_source_bytes
          loader
          source
        |> Result.ok_or_failwith
      in
      parse_inputs
        ~parse_document
        ~preprocess:Fn.id
        ~canonical_sources:true
        ~source:file
        ~source_loader:loader
        ~dir
        content
    in
    let root = parse_file (Chatmd_source_bundle.root_file bundle) in
    let agents = ref [] in
    while not (Queue.is_empty queued) do
      let file = Queue.dequeue_exn queued in
      if not (Hash_set.mem seen file)
      then (
        let elements = parse_file file in
        agents := (file, elements) :: !agents)
    done;
    { root; agents = List.rev !agents }
  ;;
end

(** ------------------------------------------------------------------ *)

(** {1 Metadata helpers}  *)

module Metadata = struct
  open Core

  module CM = struct
    type t = Chat_markdown.top_level_elements

    let hash = Chat_markdown.hash_top_level_elements
    let compare = Chat_markdown.compare_top_level_elements
    let sexp_of_t = Chat_markdown.sexp_of_top_level_elements
    let t_of_sexp = Chat_markdown.top_level_elements_of_sexp
  end

  module Table = Hashtbl.Make (CM)

  let store : (string * string) list Table.t = Table.create ~size:16 ()

  let add element ~key ~value =
    let existing = Hashtbl.find store element |> Option.value ~default:[] in
    Hashtbl.set store ~key:element ~data:((key, value) :: existing)
  ;;

  let get element = Hashtbl.find store element
  let set element kvs = Hashtbl.set store ~key:element ~data:kvs
  let clear () = Hashtbl.clear store
end
