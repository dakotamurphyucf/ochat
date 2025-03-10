open Core
open Jsonaf.Export
module C = Openai_chat_completion

module Dom = struct
  open Markup

  (* A minimal DOM‐like type for Markup.ml signals. *)
  type node =
    | Element of name * (name * string) list * node list
    | Text of string
    | Comment of string
    | PI of string * string
    | Doctype of doctype
    | XmlDecl of xml_declaration

  (* Convert a node into a Markup “tree” node, so we can feed it back into
     Markup.from_tree. *)
  let from_node (n : node) : node Markup.node =
    match n with
    | Text s -> `Text s
    | Comment c -> `Comment c
    | PI (target, data) -> `PI (target, data)
    | Doctype d -> `Doctype d
    | XmlDecl xd -> `Xml xd
    | Element (tag_name, attrs, children) -> `Element (tag_name, attrs, children)
  ;;

  (* Construct a (signal, sync) stream from one node. *)
  let node_to_signal_stream (root : node) = Markup.from_tree from_node root

  (* Parse an XML string into a list of top‐level DOM nodes. *)
  let parse_xml_to_dom (xml : string) : node list =
    let stream =
      Markup.string xml
      |> Markup.parse_xml ~context:`Document
      |> Markup.signals
      |> Markup.trees
           ~text:(fun ss -> Text (String.concat ~sep:"" ss))
           ~comment:(fun c -> Comment c)
           ~pi:(fun target data -> PI (target, data))
           ~xml:(fun xd -> XmlDecl xd)
           ~doctype:(fun d -> Doctype d)
           ~element:(fun name attrs children -> Element (name, attrs, children))
    in
    Markup.to_list stream
  ;;

  (* Convert a list of DOM nodes back to an XML string. *)
  let dom_to_string (dom : node list) : string =
    let signals =
      dom
      |> List.map ~f:(fun one_node -> node_to_signal_stream one_node |> Markup.to_list)
      |> List.concat
      |> Markup.of_list
    in
    signals |> Markup.write_xml |> Markup.to_string
  ;;
end

module Raw_blocks = struct
  let escape_cdata (raw_text : string) : string =
    let parts = Str.split (Str.regexp_string "]]>") raw_text in
    match parts with
    | [] -> ""
    | first :: rest ->
      List.fold_left
        ~f:(fun acc piece -> acc ^ "]]]]><![CDATA[>" ^ piece)
        ~init:first
        rest
  ;;

  (* Replace <raw>...</raw> with <![CDATA[...]...]]> blocks,
     automatically splitting “]]>” inside. *)
  let replace_raw_with_splitting_cdata (input : string) : string =
    let open Str in
    let cdata_open = "<![CDATA[" in
    let cdata_close = "]]>" in
    let raw_open = "<raw>" in
    let raw_close = "</raw>" in
    let len_open = String.length raw_open
    and len_close = String.length raw_close in
    let buff = Buffer.create (String.length input) in
    let find_substring str pattern start_pos =
      try Some (search_forward (regexp_string pattern) str start_pos) with
      | Stdlib.Not_found -> None
    in
    let rec loop start_index =
      match find_substring input raw_open start_index with
      | None ->
        (* No more <raw> blocks, copy remainder verbatim. *)
        Buffer.add_substring
          buff
          input
          ~pos:start_index
          ~len:(String.length input - start_index)
      | Some idx_open ->
        (* Copy everything prior to <raw>... *)
        Buffer.add_substring buff input ~pos:start_index ~len:(idx_open - start_index);
        let content_start = idx_open + len_open in
        (match find_substring input raw_close content_start with
         | None ->
           (* No matching </raw>, treat the remainder as a single raw block. *)
           let raw_text =
             String.sub input ~pos:content_start ~len:(String.length input - content_start)
           in
           let escaped = escape_cdata raw_text in
           Buffer.add_string buff cdata_open;
           Buffer.add_string buff escaped;
           Buffer.add_string buff cdata_close
         | Some idx_close ->
           (* Extract the text between <raw> and </raw>, escape "]]>", wrap. *)
           let raw_length = idx_close - content_start in
           let raw_text = String.sub input ~pos:content_start ~len:raw_length in
           let escaped = escape_cdata raw_text in
           Buffer.add_string buff cdata_open;
           Buffer.add_string buff escaped;
           Buffer.add_string buff cdata_close;
           loop (idx_close + len_close))
    in
    loop 0;
    Buffer.contents buff
  ;;
end

module Import_expansion = struct
  open Dom

  let rec expand_imports ~dir (nodes : node list) : node list =
    List.concat_map nodes ~f:(function
      | Element (((_, "msg") as name), attrs, children) ->
        let role =
          List.find_map attrs ~f:(fun ((_, attr_name), value) ->
            if String.equal attr_name "role" then Some value else None)
        in
        (* If <msg role="assistant">, we ignore nested <import>. *)
        (match role with
         | Some "assistant" -> [ Element (name, attrs, children) ]
         | _ ->
           let expanded = expand_imports ~dir children in
           [ Element (name, attrs, expanded) ])
      | Element ((_, "import"), attrs, _children) ->
        let maybe_file =
          List.find_map attrs ~f:(fun ((_, attr_name), value) ->
            if String.equal attr_name "file" then Some value else None)
        in
        (match maybe_file with
         | None -> []
         | Some filename ->
           let imported_text = Io.load_doc ~dir filename in
           let replaced = Raw_blocks.replace_raw_with_splitting_cdata imported_text in
           let imported_dom = Dom.parse_xml_to_dom replaced in
           let imported_expanded = expand_imports ~dir imported_dom in
           imported_expanded)
      | Element (name, attrs, children) ->
        let expanded = expand_imports ~dir children in
        [ Element (name, attrs, expanded) ]
      | other -> [ other ])
  ;;

  (* Utility that does: parse → expand imports → re‐stringify. *)
  let parse_with_imports ~dir (xml : string) : string =
    let dom = Dom.parse_xml_to_dom xml in
    let expanded_dom = expand_imports ~dir dom in
    Dom.dom_to_string expanded_dom
  ;;
end

module Chat_content = struct
  (* Minimal “image_url” type, analogous to what you might have in the OpenAI API code. *)
  type image_url = { url : string } [@@deriving sexp, jsonaf]

  (* A single item of content, which can be text or an image or doc. *)
  type basic_content_item =
    { type_ : string [@key "type"]
    ; text : string option [@jsonaf.option]
    ; image_url : image_url option [@jsonaf.option]
    ; document_url : string option [@jsonaf.option]
    ; is_local : bool [@default false]
    ; cleanup_html : bool [@default false]
    }
  [@@deriving sexp, jsonaf]

  (* Agent content: has a url, is_local, and sub-items. *)
  type agent_content =
    { url : string
    ; is_local : bool
    ; items : content_item list [@default []]
    }
  [@@deriving sexp, jsonaf]

  (* content_item can be either a Basic variant or an Agent variant. *)
  and content_item =
    | Basic of basic_content_item
    | Agent of agent_content
  [@@deriving sexp, jsonaf]

  type content_item_list = content_item list [@@deriving sexp, jsonaf]

  type chat_message_content =
    | Text of string
    | Items of content_item list
  [@@deriving sexp]

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
  [@@deriving jsonaf, sexp]

  type tool_call =
    { id : string
    ; function_ : function_call
    }
  [@@deriving jsonaf, sexp]

  type msg =
    { role : string
    ; content : chat_message_content option
         [@jsonaf.option]
         [@jsonaf.of chat_message_content_of_jsonaf]
         [@jsonaf.to jsonaf_of_chat_message_content]
    ; name : string option [@jsonaf.option]
    ; function_call : function_call option [@jsonaf.option]
    ; tool_call : tool_call option [@jsonaf.option]
    ; tool_call_id : string option [@jsonaf.option]
    }
  [@@deriving jsonaf, sexp]

  type config =
    { max_tokens : int option [@jsonaf.option]
    ; model : string option [@jsonaf.option]
    ; reasoning_effort : string option [@jsonaf.option]
    ; temperature : float option [@jsonaf.option]
    }
  [@@deriving jsonaf, sexp]

  type top_level_elements =
    | Msg of msg
    | Config of config
  [@@deriving sexp]
end

module Chat_markdown = struct
  include Chat_content
  include Dom

  (* The internal chat_element used while building the final messages. *)
  type chat_element =
    | Message of msg
    | Config of config
    | Text of string
    | Image of string * bool
    | Document of string * bool * bool
    | Agent of string (* url *) * bool (* is_local *) * chat_element list
  (* Convert a <msg> element’s children into an Items or a single Text. *)

  let rec content_items_of_elements (elts : chat_element list) : content_item list =
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
        }
      :: content_items_of_elements rest
    | Image (url, is_local) :: rest ->
      Basic
        { type_ = "image_url"
        ; text = None
        ; image_url = Some { url }
        ; document_url = None
        ; is_local
        ; cleanup_html = false
        }
      :: content_items_of_elements rest
    | Document (url, local, clean) :: rest ->
      Basic
        { type_ = "text"
        ; text = None
        ; image_url = None
        ; document_url = Some url
        ; is_local = local
        ; cleanup_html = clean
        }
      :: content_items_of_elements rest
    | Agent (url, is_local, children) :: rest ->
      let sub_items = content_items_of_elements children in
      Agent { url; is_local; items = sub_items } :: content_items_of_elements rest
    | Message _ :: rest ->
      (* By design, skip top-level messages if found in "content" context. *)
      content_items_of_elements rest
    | Config _ :: rest ->
      (* Similarly skip any config in content. *)
      content_items_of_elements rest
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
    List.iter attr ~f:(fun ((_, attr_name), value) ->
      Hashtbl.set hash_tbl ~key:attr_name ~data:value);
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
        Some { name; arguments }, None
    in
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
            | _ ->
              failwith
                "Expected tool_call to be raw text arguments; found structured content."
          in
          Some { id; function_ = { name; arguments } }, None)
        else None, content_opt
    in
    { role = Hashtbl.find_exn hash_tbl "role"
    ; name = Hashtbl.find hash_tbl "name"
    ; function_call
    ; tool_call
    ; content = content_opt
    ; tool_call_id = Hashtbl.find hash_tbl "tool_call_id"
    }
  ;;

  (* Helper to turn a chat_element back to string (for unrecognized markup). *)
  let rec chat_element_to_string = function
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
    | Document (url, local, cleanup) ->
      let local_attr = if local then " local=\"true\"" else "" in
      let strip_attr = if cleanup then " strip=\"true\"" else "" in
      Printf.sprintf "<doc src=\"%s\"%s%s />" url local_attr strip_attr
    | Config { max_tokens; model; reasoning_effort; temperature } ->
      let attrs =
        [ Option.map max_tokens ~f:(fun n -> Printf.sprintf "max_tokens=\"%d\"" n)
        ; Option.map model ~f:(fun m -> Printf.sprintf "model=\"%s\"" m)
        ; Option.map reasoning_effort ~f:(fun r ->
            Printf.sprintf "reasoning_effort=\"%s\"" r)
        ; Option.map temperature ~f:(fun t -> Printf.sprintf "temperature=\"%.3f\"" t)
        ]
        |> List.filter_map ~f:Fun.id
      in
      let attrs_string =
        if List.is_empty attrs then "" else " " ^ String.concat ~sep:" " attrs
      in
      Printf.sprintf "<config%s />" attrs_string
    | Message m ->
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

  (* The Markup.ml “tree” transformation that identifies <msg> or <config> elements
     and returns them as Chat_parser.chat_element variants. *)
  let parse_chat_elements =
    Markup.tree
      ~text:(fun ss -> Text (String.concat ~sep:"" ss))
      ~element:(fun (_, name) attr children ->
        match name with
        | "msg" ->
          (* let role_attr =
             List.find_map attr ~f:(fun ((_, nm), _) ->
             if String.equal nm "role" then Some nm else None)
             in
             (match role_attr with
             | Some "assistant" ->
             let verbatim_text =
             String.concat ~sep:"" (List.map children ~f:chat_element_to_string)
             in
             Message (attr_to_msg attr (Some (Text verbatim_text)))
             | _ ->
             let content_opt = parse_msg_children children in
             Message (attr_to_msg attr content_opt)) *)
          let content_opt = parse_msg_children children in
          Message (attr_to_msg attr content_opt)
        | "img" ->
          let tbl = Hashtbl.create (module String) in
          List.iter attr ~f:(fun ((_, nm), v) -> Hashtbl.set tbl ~key:nm ~data:v);
          let url = Option.value (Hashtbl.find tbl "src") ~default:"" in
          let is_local = Hashtbl.mem tbl "local" in
          Image (url, is_local)
        | "doc" ->
          let tbl = Hashtbl.create (module String) in
          List.iter attr ~f:(fun ((_, nm), v) -> Hashtbl.set tbl ~key:nm ~data:v);
          let url = Option.value (Hashtbl.find tbl "src") ~default:"" in
          let local = Hashtbl.mem tbl "local" in
          let strip = Hashtbl.mem tbl "strip" in
          Document (url, local, strip)
        | "config" ->
          let tbl = Hashtbl.create (module String) in
          List.iter attr ~f:(fun ((_, nm), v) -> Hashtbl.set tbl ~key:nm ~data:v);
          let max_tokens = Option.map (Hashtbl.find tbl "max_tokens") ~f:Int.of_string in
          let model = Hashtbl.find tbl "model" in
          let reasoning_effort = Hashtbl.find tbl "reasoning_effort" in
          let temperature =
            Option.map (Hashtbl.find tbl "temperature") ~f:Float.of_string
          in
          Config { max_tokens; model; reasoning_effort; temperature }
        | "agent" ->
          let url_attr =
            List.find_map attr ~f:(fun ((_, nm), v) ->
              if String.equal nm "src" then Some v else None)
          in
          let agent_url = Option.value url_attr ~default:"" in
          let agent_is_local =
            List.exists attr ~f:(fun ((_, nm), _) -> String.(nm = "local"))
          in
          Agent (agent_url, agent_is_local, children)
        | _ ->
          let raw_content =
            match children with
            | [] -> Printf.sprintf "<%s/>" name
            | _ ->
              Printf.sprintf
                "<%s>%s</%s>"
                name
                (String.concat ~sep:"" (List.map children ~f:chat_element_to_string))
                name
          in
          Text raw_content)
  ;;

  (* We only want to capture top‐level <msg> or <config>. So we scan the stream
     for those elements, parse them with parse_chat_elements, then flatten. *)
  let chat_elements_stream =
    Markup.elements (fun (_, name) _attrs ->
      match name with
      | "msg" | "config" -> true
      | _ -> false)
  ;;

  (* Transform the final “Maybe chat_element” from parse_chat_elements
     into top_level_elements we can store. *)
  let to_top_level = function
    | None -> None
    | Some (Message m) -> Some (Msg m)
    | Some (Config c) -> Some (Chat_content.Config c)
    | Some (Text _) | Some (Image _) | Some (Document _) | Some (Agent _) ->
      (* We ignore these at the top level. *)
      None
  ;;

  (* Parse an entire file of chat markup into either <msg> or <config> elements,
     returning them in a list. *)
  let parse_chat_inputs ~dir (xml_content : string) : top_level_elements list =
    let replaced = Raw_blocks.replace_raw_with_splitting_cdata xml_content in
    let with_imports = Import_expansion.parse_with_imports ~dir replaced in
    Markup.string with_imports
    |> Markup.parse_xml ~context:`Document
    |> Markup.signals
    |> chat_elements_stream
    |> Markup.map parse_chat_elements
    |> Markup.to_list
    |> List.filter_map ~f:to_top_level
  ;;
end
