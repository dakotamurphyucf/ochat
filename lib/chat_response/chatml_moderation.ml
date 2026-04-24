open! Core

module Lang = Chatml.Chatml_lang
module Runtime = Chatml_runtime
module Res = Openai.Responses

let field (json : Jsonaf.t) (name : string) : Jsonaf.t option =
  match json with
  | `Object (fields : (string * Jsonaf.t) list) ->
    List.Assoc.find fields name ~equal:String.equal
  | _ -> None
;;

let string_field (json : Jsonaf.t) (name : string) : string option =
  match field json name with
  | Some (`String value) -> Some value
  | _ -> None
;;

let text_values_of_parts (json : Jsonaf.t) : string list =
  match json with
  | `Array parts -> List.filter_map parts ~f:(fun part -> string_field part "text")
  | _ -> []
;;

let output_text_values (json : Jsonaf.t) : string list =
  match json with
  | `String text -> [ text ]
  | _ ->
    (match field json "parts" with
     | Some parts -> text_values_of_parts parts
     | None -> [])
;;

let text_item_json ~role ~text : Jsonaf.t =
  `Object
    [ "type", `String "message"
    ; "role", `String role
    ; ( "content"
      , `Array [ `Object [ "type", `String "input_text"; "text", `String text ] ] )
    ]
;;

module Phase = Moderation.Phase

module Item = struct
  type t = Moderation.Item.t =
    { id : string
    ; value : Jsonaf.t
    }
  [@@deriving sexp]

  let create = Moderation.Item.create
  let of_value = Moderation.Item.of_value
  let to_value = Moderation.Item.to_value
  let of_response_item = Moderation.Item.of_response_item
  let to_response_item = Moderation.Item.to_response_item

  let kind t = string_field t.value "type"
  let role t = string_field t.value "role"

  let text_parts t =
    match field t.value "content" with
    | Some content -> text_values_of_parts content
    | None ->
      (match field t.value "output" with
       | Some output -> output_text_values output
       | None -> [])
  ;;

  let text t = List.hd (text_parts t)

  let input_text_message ~id ~role ~text =
    let value =
      match String.lowercase role with
      | "user" ->
        let role = Res.Input_message.User in
        Moderation.Item.text_input_message ~id ~role ~text |> fun item -> item.value
      | "assistant" ->
        let role = Res.Input_message.Assistant in
        Moderation.Item.text_input_message ~id ~role ~text |> fun item -> item.value
      | "system" ->
        let role = Res.Input_message.System in
        Moderation.Item.text_input_message ~id ~role ~text |> fun item -> item.value
      | "developer" ->
        let role = Res.Input_message.Developer in
        Moderation.Item.text_input_message ~id ~role ~text |> fun item -> item.value
      | _ -> text_item_json ~role ~text
    in
    create ~id ~value
  ;;

  let output_text_message ~id ~text =
    let item =
      Res.Item.Output_message
        { role = Res.Output_message.Assistant
        ; id
        ; content = [ { Res.Output_message.annotations = []; text; _type = "output_text" } ]
        ; status = "completed"
        ; _type = "message"
        }
    in
    of_response_item ~id item
  ;;

  let user_text ~id text = input_text_message ~id ~role:"user" ~text
  let assistant_text ~id text = output_text_message ~id ~text
  let system_text ~id text = input_text_message ~id ~role:"system" ~text
  let notice ~id ~text = system_text ~id text

  let is_user t = String.equal (Option.value (role t) ~default:"") "user"
  let is_assistant t = String.equal (Option.value (role t) ~default:"") "assistant"
  let is_system t = String.equal (Option.value (role t) ~default:"") "system"

  let is_tool_call t =
    match kind t with
    | Some "function_call" | Some "custom_tool_call" -> true
    | _ -> false
  ;;

  let is_tool_result t =
    match kind t with
    | Some "function_call_output" | Some "custom_tool_call_output" -> true
    | _ -> false
  ;;
end

module Tool_desc = Moderation.Tool_desc

module Tool_call = struct
  type kind = Moderation.Tool_call.kind =
    | Function
    | Custom
  [@@deriving sexp, compare]

  type t = Moderation.Tool_call.t =
    { id : string
    ; name : string
    ; args : Jsonaf.t
    ; kind : kind
    ; payload_text : string
    ; meta : Jsonaf.t
    }
  [@@deriving sexp]

  let of_response_item = Moderation.Tool_call.of_response_item
  let to_value = Moderation.Tool_call.to_value

  let arg (t : t) (name : string) : Jsonaf.t option = field t.args name

  let arg_string t name =
    match arg t name with
    | Some (`String value) -> Some value
    | _ -> None
  ;;

  let arg_bool t name =
    match arg t name with
    | Some `True -> Some true
    | Some `False -> Some false
    | _ -> None
  ;;

  let arg_array t name =
    match arg t name with
    | Some (`Array values) -> Some values
    | _ -> None
  ;;

  let is_named t name = String.equal t.name name
  let is_one_of t names = List.mem names t.name ~equal:String.equal
end

module Tool_result = Moderation.Tool_result

let last_matching items ~f = List.find (List.rev items) ~f

let items_since_last_matching items ~f =
  let rec loop acc = function
    | [] -> items
    | item :: rest ->
      let acc = item :: acc in
      if f item then acc else loop acc rest
  in
  loop [] (List.rev items)
;;

module Context = struct
  type t = Moderation.Context.t =
    { session_id : string
    ; now_ms : int
    ; phase : Phase.t
    ; items : Item.t list
    ; available_tools : Tool_desc.t list
    ; session_meta : Jsonaf.t
    }
  [@@deriving sexp]

  let to_value = Moderation.Context.to_value
  let last_item t = List.last t.items
  let last_user_item t = last_matching t.items ~f:Item.is_user
  let last_assistant_item t = last_matching t.items ~f:Item.is_assistant
  let last_system_item t = last_matching t.items ~f:Item.is_system
  let last_tool_call t = last_matching t.items ~f:Item.is_tool_call
  let last_tool_result t = last_matching t.items ~f:Item.is_tool_result
  let find_item t ~id = List.find t.items ~f:(fun item -> String.equal item.id id)

  let items_since_last_user_turn t =
    items_since_last_matching t.items ~f:Item.is_user
  ;;

  let items_since_last_assistant_turn t =
    items_since_last_matching t.items ~f:Item.is_assistant
  ;;

  let items_by_role t ~role =
    List.filter t.items ~f:(fun item ->
      String.equal (Option.value (Item.role item) ~default:"") role)
  ;;

  let find_tool t ~name =
    List.find t.available_tools ~f:(fun tool -> String.equal tool.name name)
  ;;

  let has_tool t ~name = Option.is_some (find_tool t ~name)
end

module Event = Moderation.Event
module Projection = Moderation.Projection

module Overlay = struct
  type replacement = Moderation.Overlay.replacement =
    { target_id : string
    ; item : Item.t
    }
  [@@deriving sexp]

  type op = Moderation.Overlay.op =
    | Prepend_system of string
    | Append_item of Item.t
    | Replace_item of replacement
    | Delete_item of string
    | Halt of string
  [@@deriving sexp]

  type t = Moderation.Overlay.t =
    { prepended_system_items : Item.t list
    ; appended_items : Item.t list
    ; replacements : replacement list
    ; deleted_item_ids : string list
    ; halted_reason : string option [@jsonaf.option]
    }
  [@@deriving sexp]

  let empty = Moderation.Overlay.empty
  let of_turn_effect = Moderation.Overlay.of_runtime_turn_effect
  let apply = Moderation.Overlay.apply
end

module Tool_moderation = Moderation.Tool_moderation
module Runtime_request = Moderation.Runtime_request
module Outcome = Moderation.Outcome
module Capabilities = Moderation.Capabilities
