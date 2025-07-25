open Core
open Jsonaf.Export
module Ast = Chatmd_ast

let parse str =
  let lexbuf = Lexing.from_string str in
  Chatmd_parser.document Chatmd_lexer.token lexbuf
;;

module Import_expansion = struct
  open Ast

  let can_have_imports tag =
    match tag with
    | User | Agent | System | Developer -> true
    | _ -> false
  ;;

  (* A node is either a text node or an element with attributes and children. *)
  let rec expand_imports ~dir (nodes : document) : document =
    List.concat_map nodes ~f:(function
      | Element (Import, attrs, _) ->
        let src = List.Assoc.find attrs ~equal:String.equal "src" in
        (match src with
         | Some (Some src) ->
           let imported = parse @@ Io.load_doc ~dir src in
           expand_imports ~dir imported
         | _ -> [])
      | Element (Msg, attrs, children) ->
        let role = List.Assoc.find attrs ~equal:String.equal "role" in
        (match role with
         | Some (Some "user") | Some (Some "system") | Some (Some "developer") ->
           let expanded = expand_imports ~dir children in
           [ Element (Msg, attrs, expanded) ]
         | _ -> [ Element (Msg, attrs, children) ])
      | Element (tag, attrs, children) ->
        (match can_have_imports tag with
         | true ->
           let expanded_children = expand_imports ~dir children in
           [ Element (tag, attrs, expanded_children) ]
         | false -> [ Element (tag, attrs, children) ])
      | Text _ as txt -> [ txt ])
  ;;
end

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
    ; content : chat_message_content option
          [@jsonaf.option]
          [@jsonaf.of chat_message_content_of_jsonaf]
          [@jsonaf.to jsonaf_of_chat_message_content]
    ; name : string option [@jsonaf.option]
    ; id : string option [@jsonaf.option] (* NEW *)
    ; status : string option [@jsonaf.option] (* NEW *)
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
    | Custom of custom_tool
    | Agent of agent_tool
    (* A tool exposed by a remote MCP server. *)
    | Mcp of mcp_tool
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
      | Tool _ )
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
  let attr_to_msg attr (content : chat_message_content option) : msg =
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
    ; name = Hashtbl.find hash_tbl "name"
    ; id = Hashtbl.find hash_tbl "id" (* NEW *)
    ; status = Hashtbl.find hash_tbl "status" (* NEW *)
    ; function_call
    ; tool_call
    ; content = content_opt
    ; tool_call_id = Hashtbl.find hash_tbl "tool_call_id"
    }
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
    | Tool t ->
      (match t with
       | Builtin name -> Printf.sprintf "<tool name=\"%s\" />" name
       | Custom { name; description; command } ->
         let desc_attr =
           match description with
           | Some d -> Printf.sprintf " description=\"%s\"" d
           | None -> ""
         in
         Printf.sprintf "<tool name=\"%s\"%s command=\"%s\" />" name desc_attr command
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
  ;;

  (*--------------------------------------------------------------------------*)
  (* Generic tree fold                                                        *)
  (*--------------------------------------------------------------------------*)

  (** [tree node ~f] traverses [node] depth-first and applies the combining
    function [f] to each node together with the list of results that were
    produced for its direct children.  This is analogous to a fold over the
    tree structure.

    For example, to collect all nodes in a tree one can write

    {[ let all_nodes = tree root ~f:(fun n children -> n :: List.concat children) ]}

    The traversal is depth-first and children are processed from left to right,
    mirroring their order in the underlying list. *)
  let rec tree (node : Ast.node) ~(f : Ast.node -> 'a list -> 'a) : 'a =
    match node with
    | Text _ -> f node []
    | Element (_, _, children) ->
      let child_results = List.map children ~f:(fun child -> tree child ~f) in
      f node child_results
  ;;

  (* The Markup.ml “tree” transformation that identifies <msg> or <config> elements
     and returns them as Chat_parser.chat_element variants. *)
  let parse_chat_element node =
    tree node ~f:(fun node children ->
      match node with
      | Element (Msg, attrs, _) ->
        let attr = List.map attrs ~f:(fun (n, v) -> n, Option.value v ~default:"") in
        let content_opt = parse_msg_children children in
        Message (attr_to_msg attr content_opt)
      | Element (Developer, attrs, _) ->
        let attr = List.map attrs ~f:(fun (n, v) -> n, Option.value v ~default:"") in
        let content_opt = parse_msg_children children in
        let role_attr = "role", "developer" in
        let attrs = role_attr :: attr in
        Developer_msg (attr_to_msg attrs content_opt)
      | Element (System, attrs, _) ->
        let attr = List.map attrs ~f:(fun (n, v) -> n, Option.value v ~default:"") in
        let content_opt = parse_msg_children children in
        let role_attr = "role", "system" in
        let attrs = role_attr :: attr in
        System_msg (attr_to_msg attrs content_opt)
      | Element (User, attrs, _) ->
        let attr = List.map attrs ~f:(fun (n, v) -> n, Option.value v ~default:"") in
        let content_opt = parse_msg_children children in
        let role_attr = "role", "user" in
        let attrs = role_attr :: attr in
        User_msg (attr_to_msg attrs content_opt)
      | Element (Assistant, attrs, _) ->
        let attr = List.map attrs ~f:(fun (n, v) -> n, Option.value v ~default:"") in
        let content_opt = parse_msg_children children in
        let role_attr = "role", "assistant" in
        let attrs = role_attr :: attr in
        Assistant_msg (attr_to_msg attrs content_opt)
      | Element (Tool_call, attrs, _) ->
        let attr = List.map attrs ~f:(fun (n, v) -> n, Option.value v ~default:"") in
        let content_opt = parse_msg_children children in
        let role_attr = "role", "assistant" in
        let tool_call_attr = "tool_call", "true" in
        let attrs = role_attr :: tool_call_attr :: attr in
        Tool_call_msg (attr_to_msg attrs content_opt)
      | Element (Tool_response, attrs, _) ->
        let attr = List.map attrs ~f:(fun (n, v) -> n, Option.value v ~default:"") in
        let content_opt = parse_msg_children children in
        let role_attr = "role", "tool" in
        let attrs = role_attr :: attr in
        Tool_response_msg (attr_to_msg attrs content_opt)
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
        let tbl = Hashtbl.create (module String) in
        List.iter attr ~f:(fun (nm, v) -> Hashtbl.set tbl ~key:nm ~data:v);
        let max_tokens = Option.map (Hashtbl.find tbl "max_tokens") ~f:Int.of_string in
        let model = Hashtbl.find tbl "model" in
        let reasoning_effort = Hashtbl.find tbl "reasoning_effort" in
        let temperature =
          Option.map (Hashtbl.find tbl "temperature") ~f:Float.of_string
        in
        let show_tool_call = Hashtbl.mem tbl "show_tool_call" in
        let id = Hashtbl.find tbl "id" in
        Config { max_tokens; model; reasoning_effort; temperature; show_tool_call; id }
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
        Reasoning { id; status; _type = "reasoning"; summary = summaries }
      | Element (Agent, attrs, __bin_read_content_item__) ->
        let attr = List.map attrs ~f:(fun (n, v) -> n, Option.value v ~default:"") in
        let url_attr =
          List.find_map attr ~f:(fun (nm, v) ->
            if String.equal nm "src" then Some v else None)
        in
        let agent_url = Option.value url_attr ~default:"" in
        let agent_is_local = List.exists attr ~f:(fun (nm, _) -> String.(nm = "local")) in
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
        (match command, agent, mcp_server with
         | Some _, Some _, _ | Some _, _, Some _ | _, Some _, Some _ ->
           failwith
             "<tool> cannot combine 'command', 'agent' and 'mcp_server' attributes."
         | Some cmd, None, None ->
           if String.is_empty name then failwith "Tool name cannot be empty.";
           let cmd = String.strip cmd in
           if String.is_empty cmd then failwith "Tool command cannot be empty.";
           let description = Option.map description ~f:String.strip in
           Tool (Custom { name; description; command = cmd })
         | None, Some agent_url, None ->
           if String.is_empty name then failwith "Tool name cannot be empty.";
           let agent_url = String.strip agent_url in
           if String.is_empty agent_url then failwith "Tool agent URL cannot be empty.";
           let description = Option.map description ~f:String.strip in
           Tool (Agent { name; description; agent = agent_url; is_local })
         | None, None, Some mcp_uri ->
           let mcp_uri = String.strip mcp_uri in
           if String.is_empty mcp_uri then failwith "Tool mcp_server URI cannot be empty.";
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
           else Tool (Builtin name))
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

  (* We only want to capture top‐level <msg> or <config>. So we scan the stream
     for those elements, parse them with parse_chat_elements, then flatten. *)
  let chat_elements document =
    List.filter document ~f:(function
      | Ast.Element (Msg, _, _)
      | Element (Developer, _, _)
      | Element (System, _, _)
      | Element (User, _, _)
      | Element (Assistant, _, _)
      | Element (Tool_call, _, _)
      | Element (Tool_response, _, _)
      | Element (Config, _, _)
      | Element (Reasoning, _, _)
      | Element (Tool, _, _) -> true
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
    | Developer_msg m -> Some (Developer m)
    | System_msg m -> Some (System m) (* System is a legacy alias for Developer *)
    | _ -> None
  ;;

  let of_chat_elements (elts : chat_element list) : top_level_elements list =
    List.filter_map elts ~f:to_top_level
  ;;

  let parse_chat_inputs ~dir (xml_content : string) : top_level_elements list =
    let document = parse xml_content in
    let expanded = Import_expansion.expand_imports ~dir document in
    let chat_elements = chat_elements expanded in
    let parsed_elements = List.map ~f:parse_chat_element chat_elements in
    of_chat_elements parsed_elements
  ;;
end
