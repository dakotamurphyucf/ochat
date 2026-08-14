open Core

module Alias = struct
  type t =
    | Output_index of int * string option * int
    | Provider_item_id of int * string option * string
    | Call_id of int * string option * string
  [@@deriving compare, equal, hash, sexp]
end

module Registry = struct
  module Alias_table = Hashtbl.Make (Alias)

  type t =
    { allocator : History_entry.Allocator.t
    ; aliases : History_entry.Id.t Alias_table.t
    ; mutable next_scope : int
    }

  let create ~allocator = { allocator; aliases = Alias_table.create (); next_scope = 0 }

  let create_scope t =
    let scope = t.next_scope in
    t.next_scope <- scope + 1;
    scope
  ;;

  let register_alias t alias id =
    match Hashtbl.find t.aliases alias with
    | None -> Hashtbl.set t.aliases ~key:alias ~data:id
    | Some existing when History_entry.Id.equal existing id -> ()
    | Some existing ->
      failwithf
        "Conflicting streamed history aliases %s and %s for %s"
        (History_entry.Id.to_string existing)
        (History_entry.Id.to_string id)
        (Sexp.to_string_hum ([%sexp_of: Alias.t] alias))
        ()
  ;;

  let find_alias t aliases =
    List.filter_map aliases ~f:(Hashtbl.find t.aliases)
    |> List.dedup_and_sort ~compare:History_entry.Id.compare
    |> function
    | [] -> None
    | [ id ] -> Some id
    | ids ->
      failwithf
        "Conflicting streamed history aliases: %s"
        (Sexp.to_string_hum ([%sexp_of: History_entry.Id.t list] ids))
        ()
  ;;

  let ensure t aliases =
    let id =
      match find_alias t aliases with
      | Some id -> id
      | None -> History_entry.Allocator.allocate t.allocator |> Result.ok_or_failwith
    in
    List.iter aliases ~f:(fun alias -> register_alias t alias id);
    id
  ;;

  let item_aliases ~scope ~source ~output_index = function
    | Openai.Responses.Response_stream.Item.Output_message item ->
      [ Alias.Output_index (scope, source, output_index)
      ; Provider_item_id (scope, source, item.id)
      ]
    | Reasoning item ->
      [ Alias.Output_index (scope, source, output_index)
      ; Alias.Provider_item_id (scope, source, item.id)
      ]
    | Function_call item ->
      [ Alias.Output_index (scope, source, output_index)
      ; Alias.Call_id (scope, source, item.call_id)
      ]
      @ Option.to_list
          (Option.map item.id ~f:(fun id -> Alias.Provider_item_id (scope, source, id)))
    | Custom_function item ->
      [ Alias.Output_index (scope, source, output_index)
      ; Alias.Call_id (scope, source, item.call_id)
      ]
      @ Option.to_list
          (Option.map item.id ~f:(fun id -> Alias.Provider_item_id (scope, source, id)))
    | Input_message _ -> [ Alias.Output_index (scope, source, output_index) ]
  ;;

  let aliases_of_event ~scope ~source = function
    | Openai.Responses.Response_stream.Output_item_added { item; output_index; _ }
    | Output_item_done { item; output_index; _ } ->
      item_aliases ~scope ~source ~output_index item
    | Output_text_delta { item_id; output_index; _ }
    | Output_text_done { item_id; output_index; _ }
    | Reasoning_summary_text_delta { item_id; output_index; _ }
    | Function_call_arguments_delta { item_id; output_index; _ }
    | Function_call_arguments_done { item_id; output_index; _ }
    | Custom_tool_call_input_delta { item_id; output_index; _ }
    | Custom_tool_call_input_done { item_id; output_index; _ }
    | Content_part_added { item_id; output_index; _ }
    | Content_part_done { item_id; output_index; _ }
    | Response_refusal_delta { item_id; output_index; _ }
    | Response_refusal_done { item_id; output_index; _ }
    | Annotation_added { item_id; output_index; _ } ->
      [ Alias.Output_index (scope, source, output_index)
      ; Alias.Provider_item_id (scope, source, item_id)
      ]
    | _ -> []
  ;;

  let observe t ~scope ~source event =
    match aliases_of_event ~scope ~source event with
    | [] -> None
    | aliases -> Some (ensure t aliases)
  ;;

  let tool_output t ~scope ~source ~call_id =
    ensure t [ Alias.Call_id (scope, source, "output:" ^ call_id) ]
  ;;

  let find t alias = Hashtbl.find t.aliases alias

  let find_item t ~scope ~source item =
    match item with
    | Openai.Responses.Item.Output_message item ->
      find t (Alias.Provider_item_id (scope, source, item.id))
    | Reasoning item -> find t (Alias.Provider_item_id (scope, source, item.id))
    | Function_call item ->
      Option.first_some
        (Option.bind item.id ~f:(fun id ->
           find t (Alias.Provider_item_id (scope, source, id))))
        (find t (Alias.Call_id (scope, source, item.call_id)))
    | Custom_tool_call item ->
      Option.first_some
        (Option.bind item.id ~f:(fun id ->
           find t (Alias.Provider_item_id (scope, source, id))))
        (find t (Alias.Call_id (scope, source, item.call_id)))
    | Function_call_output item ->
      find t (Alias.Call_id (scope, source, "output:" ^ item.call_id))
    | Custom_tool_call_output item ->
      find t (Alias.Call_id (scope, source, "output:" ^ item.call_id))
    | Input_message _ | Web_search_call _ | File_search_call _ -> None
  ;;
end

type t =
  { entry_id : History_entry.Id.t
  ; source : string option
  ; event : Openai.Responses.Response_stream.t
  }

let observe registry ~scope ~source event =
  Option.map (Registry.observe registry ~scope ~source event) ~f:(fun entry_id ->
    { entry_id; source; event })
;;
