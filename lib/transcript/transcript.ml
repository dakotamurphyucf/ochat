open! Core
module D = Document_schema
module Payload = History_entry.Payload

module Admission = struct
  let limits ~max_bytes =
    D.Limits.create ~max_bytes ~max_depth:160 ~max_fields:1_000_000 ~max_nodes:2_000_000
  ;;

  let default =
    limits ~max_bytes:(16 * 1024 * 1024)
    |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
    |> Result.ok_or_failwith
  ;;
end

let ( >>= ) value f = Result.bind value ~f
let error message = Error message
let document_error error = Sexp.to_string_hum (D.Error.sexp_of_t error)

let measure ~limits json =
  D.Json.validate_and_measure ~limits json |> Result.map_error ~f:document_error
;;

let field json name =
  match D.Json.field json ~name with
  | Absent -> error ("missing field: " ^ name)
  | Null -> Ok `Null
  | Value value -> Ok value
;;

let string = function
  | `String value -> Ok value
  | _ -> error "string required"
;;

let int = function
  | `Number value ->
    (match Int.of_string_opt value with
     | Some value when value >= 0 -> Ok value
     | None | Some _ -> error "nonnegative host integer required")
  | _ -> error "number required"
;;

let optional decode = function
  | `Null -> Ok None
  | json -> Result.map (decode json) ~f:Option.some
;;

let optional_field json name decode =
  match D.Json.field json ~name with
  | Absent | Null -> Ok None
  | Value value -> Result.map (decode value) ~f:Option.some
;;

let option_json encode = function
  | None -> `Null
  | Some value -> encode value
;;

let valid_string value =
  if String.is_empty value
  then error "nonempty identity required"
  else
    D.Json.validate ~limits:D.Limits.default (`String value)
    |> Result.map_error ~f:document_error
;;

module Header = struct
  type t =
    | Message of Payload.Role.t
    | Call of Payload.Call_kind.t
    | Result of Payload.Call_kind.t
    | Reasoning
    | Unknown of string
  [@@deriving equal, sexp_of]

  let of_semantic semantic =
    match Payload.Semantic.view semantic with
    | Message { role; _ } -> Message role
    | Call { kind; _ } -> Call kind
    | Result { kind; _ } -> Result kind
    | Reasoning _ -> Reasoning
    | Unknown { provider_kind } -> Unknown provider_kind
  ;;

  let role_text = function
    | Payload.Role.System -> "system"
    | Developer -> "developer"
    | User -> "user"
    | Assistant -> "assistant"
    | Tool -> "tool"
  ;;

  let role = function
    | "system" -> Ok Payload.Role.System
    | "developer" -> Ok Developer
    | "user" -> Ok User
    | "assistant" -> Ok Assistant
    | "tool" -> Ok Tool
    | _ -> error "unknown message role"
  ;;

  let kind_text = function
    | Payload.Call_kind.Function -> "function"
    | Custom -> "custom"
  ;;

  let kind = function
    | "function" -> Ok Payload.Call_kind.Function
    | "custom" -> Ok Custom
    | _ -> error "unknown call kind"
  ;;

  let to_json = function
    | Message role ->
      `Object [ "kind", `String "message"; "role", `String (role_text role) ]
    | Call kind ->
      `Object [ "kind", `String "call"; "call_kind", `String (kind_text kind) ]
    | Result kind ->
      `Object [ "kind", `String "result"; "call_kind", `String (kind_text kind) ]
    | Reasoning -> `Object [ "kind", `String "reasoning" ]
    | Unknown provider_kind ->
      `Object [ "kind", `String "unknown"; "provider_kind", `String provider_kind ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind _ = measure ~limits:D.Limits.default json in
    let%bind name = field json "kind" >>= string in
    match name with
    | "message" ->
      let%map role = field json "role" >>= string >>= role in
      Message role
    | "call" ->
      let%map kind = field json "call_kind" >>= string >>= kind in
      Call kind
    | "result" ->
      let%map kind = field json "call_kind" >>= string >>= kind in
      Result kind
    | "reasoning" -> Ok Reasoning
    | "unknown" ->
      let%bind name = field json "provider_kind" >>= string in
      let%map () = valid_string name in
      Unknown name
    | _ -> error "unknown transcript header"
  ;;
end

module Identifier () = struct
  module T = struct
    type t = string [@@deriving compare, equal, hash, sexp_of]
  end

  include T
  include Comparator.Make (T)

  let of_string value = Result.map (valid_string value) ~f:(fun () -> value)
  let to_string t = t
end

module Source_id = Identifier ()
module Attempt_id = Identifier ()
module Item_id = Identifier ()
module Part_id = Identifier ()

module Scope = struct
  module Key = struct
    module T = struct
      type t =
        { source : Source_id.t
        ; attempt : Attempt_id.t
        }
      [@@deriving compare, equal, hash, sexp_of]
    end

    include T
    include Comparator.Make (T)
  end

  type parent =
    { scope : Key.t
    ; call_entry_id : History_entry.Id.t option
    ; call_alias : string option
    }

  type relation =
    | Root
    | Nested of parent

  type t =
    { key : Key.t
    ; relation : relation
    }

  let create ~source ~attempt ~relation =
    let key = Key.{ source; attempt } in
    match relation with
    | Root -> Ok { key; relation }
    | Nested parent ->
      if Key.equal key parent.scope
      then error "source cannot parent itself"
      else
        let open Result.Let_syntax in
        let%map () =
          match parent.call_alias with
          | None -> Ok ()
          | Some alias -> valid_string alias
        in
        { key; relation }
  ;;

  let key t = t.key

  let key_json key =
    `Object
      [ "source", `String (Source_id.to_string key.Key.source)
      ; "attempt", `String (Attempt_id.to_string key.attempt)
      ]
  ;;

  let key_of_json json =
    let open Result.Let_syntax in
    let%bind source = field json "source" >>= string >>= Source_id.of_string in
    let%map attempt = field json "attempt" >>= string >>= Attempt_id.of_string in
    Key.{ source; attempt }
  ;;

  let id_json id = `String (History_entry.Id.to_string id)
  let id_of_json json = string json >>= History_entry.Id.of_string

  let to_json t =
    let parent =
      match t.relation with
      | Root -> `Null
      | Nested parent ->
        `Object
          [ "scope", key_json parent.scope
          ; "call_entry_id", option_json id_json parent.call_entry_id
          ; "call_alias", option_json (fun s -> `String s) parent.call_alias
          ]
    in
    `Object [ "key", key_json t.key; "parent", parent ]
  ;;

  let decode json =
    let open Result.Let_syntax in
    let%bind key = field json "key" >>= key_of_json in
    let%bind parent = field json "parent" in
    let%bind relation =
      match parent with
      | `Null -> Ok Root
      | json ->
        let%bind scope = field json "scope" >>= key_of_json in
        let%bind call_entry_id = optional_field json "call_entry_id" id_of_json in
        let%map call_alias = optional_field json "call_alias" string in
        Nested { scope; call_entry_id; call_alias }
    in
    create ~source:key.source ~attempt:key.attempt ~relation
  ;;

  let of_json json ~limits =
    let open Result.Let_syntax in
    let%bind (_ : int) = measure ~limits json in
    decode json
  ;;

  let equal left right =
    Key.equal left.key right.key
    &&
    match left.relation, right.relation with
    | Root, Root -> true
    | Nested a, Nested b ->
      Key.equal a.scope b.scope
      && Option.equal History_entry.Id.equal a.call_entry_id b.call_entry_id
      && Option.equal String.equal a.call_alias b.call_alias
    | Root, Nested _ | Nested _, Root -> false
  ;;
end

module Item = struct
  module Key = struct
    module T = struct
      type t =
        { scope : Scope.Key.t
        ; item : Item_id.t
        }
      [@@deriving compare, equal, hash, sexp_of]
    end

    include T
    include Comparator.Make (T)
  end

  type t =
    { scope : Scope.t
    ; id : Item_id.t
    ; entry_id : History_entry.Id.t option
    ; header : Header.t option
    ; call_name : string option
    }

  let key (t : t) = Key.{ scope = Scope.key t.scope; item = t.id }

  let create ~scope ~id ~entry_id ~header ~call_name =
    let open Result.Let_syntax in
    let%bind () =
      match header with
      | Some (Header.Unknown kind) -> valid_string kind
      | None | Some (Message _ | Call _ | Result _ | Reasoning) -> Ok ()
    in
    let%map () =
      match call_name, header with
      | None, _ -> Ok ()
      | Some name, (None | Some (Header.Call _)) -> valid_string name
      | Some _, Some (Header.Message _ | Result _ | Reasoning | Unknown _) ->
        error "call name requires call header"
    in
    { scope; id; entry_id; header; call_name }
  ;;

  let refine_optional equal left right =
    match left, right with
    | None, value | value, None -> Ok value
    | Some a, Some b -> if equal a b then Ok left else error "conflicting descriptor"
  ;;

  let refine left right =
    if not (Key.equal (key left) (key right) && Scope.equal left.scope right.scope)
    then error "conflicting item scope/key"
    else
      let open Result.Let_syntax in
      let%bind entry_id =
        refine_optional History_entry.Id.equal left.entry_id right.entry_id
      in
      let%bind header = refine_optional Header.equal left.header right.header in
      let%bind call_name = refine_optional String.equal left.call_name right.call_name in
      create ~scope:left.scope ~id:left.id ~entry_id ~header ~call_name
  ;;

  let to_json t =
    `Object
      [ "scope", Scope.to_json t.scope
      ; "id", `String (Item_id.to_string t.id)
      ; "entry_id", option_json Scope.id_json t.entry_id
      ; "header", option_json Header.to_json t.header
      ; "call_name", option_json (fun s -> `String s) t.call_name
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind scope = field json "scope" >>= Scope.decode in
    let%bind id = field json "id" >>= string >>= Item_id.of_string in
    let%bind entry_id = optional_field json "entry_id" Scope.id_of_json in
    let%bind header = optional_field json "header" Header.of_json in
    let%bind call_name = optional_field json "call_name" string in
    create ~scope ~id ~entry_id ~header ~call_name
  ;;
end

module Part = struct
  module Key = struct
    module T = struct
      type t =
        { item : Item.Key.t
        ; part : Part_id.t
        }
      [@@deriving compare, equal, hash, sexp_of]
    end

    include T
    include Comparator.Make (T)
  end

  type kind =
    | Text
    | Refusal
    | Reasoning_summary
    | Reasoning_text
    | Image
    | Unknown of string
  [@@deriving equal, sexp_of]

  type t =
    { item : Item.t
    ; id : Part_id.t
    ; index : int option
    ; kind : kind
    }

  let key (t : t) = Key.{ item = Item.key t.item; part = t.id }

  let create ~item ~id ~index ~kind =
    if Option.exists index ~f:(fun i -> i < 0)
    then error "negative content index"
    else
      let open Result.Let_syntax in
      let%map () =
        match kind with
        | Unknown name -> valid_string name
        | Text | Refusal | Reasoning_summary | Reasoning_text | Image -> Ok ()
      in
      { item; id; index; kind }
  ;;

  let kind_json = function
    | Text -> `String "text"
    | Refusal -> `String "refusal"
    | Reasoning_summary -> `String "reasoning_summary"
    | Reasoning_text -> `String "reasoning_text"
    | Image -> `String "image"
    | Unknown name -> `Object [ "unknown", `String name ]
  ;;

  let kind_of_json = function
    | `String "text" -> Ok Text
    | `String "refusal" -> Ok Refusal
    | `String "reasoning_summary" -> Ok Reasoning_summary
    | `String "reasoning_text" -> Ok Reasoning_text
    | `String "image" -> Ok Image
    | json -> Result.map (field json "unknown" >>= string) ~f:(fun name -> Unknown name)
  ;;

  let to_json t =
    `Object
      [ "item", Item.to_json t.item
      ; "id", `String (Part_id.to_string t.id)
      ; "index", option_json (fun n -> `Number (Int.to_string n)) t.index
      ; "kind", kind_json t.kind
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind item = field json "item" >>= Item.of_json in
    let%bind id = field json "id" >>= string >>= Part_id.of_string in
    let%bind index = optional_field json "index" int in
    let%bind kind = field json "kind" >>= kind_of_json in
    create ~item ~id ~index ~kind
  ;;
end

module Stream = struct
  module Target = struct
    type t =
      | Content of Part.t
      | Call_input of Item.t
  end

  type change =
    | Append of string
    | Replace of string

  type completion =
    | Complete
    | Incomplete
    | Failed
    | Cancelled

  type view =
    | Source_started of
        { scope : Scope.t
        ; origin : Payload.Origin.t
        }
    | Item_announced of Item.t
    | Part_announced of Part.t
    | Changed of
        { target : Target.t
        ; change : change
        }
    | Item_finalized of
        { item : Item.t
        ; entry : History_entry.t
        }
    | Source_finished of
        { scope : Scope.t
        ; completion : completion
        }
    | Unknown_event of
        { scope : Scope.t
        ; provider_kind : string
        ; raw : Jsonaf.t
        }

  type t =
    { view : view
    ; encoded_bytes : int
    ; json : Jsonaf.t
    }

  let view t = t.view
  let encoded_bytes t = t.encoded_bytes

  let scope t =
    match t.view with
    | Source_started { scope; _ }
    | Source_finished { scope; _ }
    | Unknown_event { scope; _ } -> scope
    | Item_announced item | Item_finalized { item; _ } -> item.scope
    | Part_announced part -> part.item.scope
    | Changed { target = Content part; _ } -> part.item.scope
    | Changed { target = Call_input item; _ } -> item.scope
  ;;

  let target_json = function
    | Target.Content part ->
      `Object [ "kind", `String "content"; "part", Part.to_json part ]
    | Call_input item ->
      `Object [ "kind", `String "call_input"; "item", Item.to_json item ]
  ;;

  let change_json = function
    | Append text -> `Object [ "kind", `String "append"; "text", `String text ]
    | Replace text -> `Object [ "kind", `String "replace"; "text", `String text ]
  ;;

  let completion_json = function
    | Complete -> `String "complete"
    | Incomplete -> `String "incomplete"
    | Failed -> `String "failed"
    | Cancelled -> `String "cancelled"
  ;;

  let completion_of_json = function
    | `String "complete" -> Ok Complete
    | `String "incomplete" -> Ok Incomplete
    | `String "failed" -> Ok Failed
    | `String "cancelled" -> Ok Cancelled
    | _ -> error "unknown source completion"
  ;;

  let entry_json entry =
    `Object
      [ "id", Scope.id_json (History_entry.id entry)
      ; "payload", Payload.to_json (History_entry.payload entry)
      ]
  ;;

  let view_json = function
    | Source_started { scope; origin } ->
      `Object
        [ "type", `String "source_started"
        ; "scope", Scope.to_json scope
        ; "origin", Payload.Origin.to_json origin
        ]
    | Item_announced item ->
      `Object [ "type", `String "item_announced"; "item", Item.to_json item ]
    | Part_announced part ->
      `Object [ "type", `String "part_announced"; "part", Part.to_json part ]
    | Changed { target; change } ->
      `Object
        [ "type", `String "changed"
        ; "target", target_json target
        ; "change", change_json change
        ]
    | Item_finalized { item; entry } ->
      `Object
        [ "type", `String "item_finalized"
        ; "item", Item.to_json item
        ; "entry", entry_json entry
        ]
    | Source_finished { scope; completion } ->
      `Object
        [ "type", `String "source_finished"
        ; "scope", Scope.to_json scope
        ; "completion", completion_json completion
        ]
    | Unknown_event { scope; provider_kind; raw } ->
      `Object
        [ "type", `String "unknown_event"
        ; "scope", Scope.to_json scope
        ; "provider_kind", `String provider_kind
        ; "raw", raw
        ]
  ;;

  let to_json t = t.json
  let sexp_of_t t = Jsonaf.sexp_of_t t.json

  let call_name semantic =
    match Payload.Semantic.view semantic with
    | Call { name; _ } -> Some name
    | Message _ | Result _ | Reasoning _ | Unknown _ -> None
  ;;

  let validate_part part =
    match part.Part.kind, part.item.header with
    | (Text | Refusal | Image), (None | Some (Header.Message _))
    | (Reasoning_summary | Reasoning_text), (None | Some Header.Reasoning)
    | Unknown _, _ -> Ok ()
    | (Text | Refusal | Image | Reasoning_summary | Reasoning_text), Some _ ->
      error "content kind contradicts item header"
  ;;

  let validate_view = function
    | Source_started _ | Source_finished _ | Item_announced _ -> Ok ()
    | Part_announced part -> validate_part part
    | Unknown_event { provider_kind; _ } -> valid_string provider_kind
    | Changed { target = Content part; _ } ->
      let open Result.Let_syntax in
      let%bind () = validate_part part in
      (match part.kind with
       | Text | Refusal | Reasoning_summary | Reasoning_text -> Ok ()
       | Image | Unknown _ -> error "only textual content accepts string changes")
    | Changed { target = Call_input item; _ } ->
      (match item.header with
       | None | Some (Header.Call _) -> Ok ()
       | Some (Message _ | Result _ | Reasoning | Unknown _) ->
         error "input bytes require call header")
    | Item_finalized { item; entry } ->
      let open Result.Let_syntax in
      let%bind () = Payload.validate (History_entry.payload entry) in
      let semantic = Payload.semantic (History_entry.payload entry) in
      if
        not
          (Option.equal
             History_entry.Id.equal
             item.entry_id
             (Some (History_entry.id entry)))
      then error "finalization does not bind actual host entry ID"
      else if
        not (Option.equal Header.equal item.header (Some (Header.of_semantic semantic)))
      then error "finalization header differs from actual payload"
      else if
        Option.exists item.call_name ~f:(fun name ->
          not (Option.equal String.equal (Some name) (call_name semantic)))
      then error "finalization call name differs from actual payload"
      else Ok ()
  ;;

  let create view ~limits =
    let open Result.Let_syntax in
    let%bind () = validate_view view in
    let json = view_json view in
    let%map encoded_bytes = measure ~limits json in
    { view; encoded_bytes; json }
  ;;

  let target_of_json json =
    let open Result.Let_syntax in
    let%bind kind = field json "kind" >>= string in
    match kind with
    | "content" ->
      let%map part = field json "part" >>= Part.of_json in
      Target.Content part
    | "call_input" ->
      let%map item = field json "item" >>= Item.of_json in
      Target.Call_input item
    | _ -> error "unknown transcript target"
  ;;

  let change_of_json json =
    let open Result.Let_syntax in
    let%bind kind = field json "kind" >>= string in
    let%bind text = field json "text" >>= string in
    match kind with
    | "append" -> Ok (Append text)
    | "replace" -> Ok (Replace text)
    | _ -> error "unknown transcript change"
  ;;

  let entry_of_json json =
    let open Result.Let_syntax in
    let%bind id = field json "id" >>= Scope.id_of_json in
    let%map payload = field json "payload" >>= Payload.of_json in
    History_entry.create_with_id ~id payload
  ;;

  let of_json json ~limits =
    let open Result.Let_syntax in
    let%bind encoded_bytes = measure ~limits json in
    let%bind kind = field json "type" >>= string in
    let%bind view =
      match kind with
      | "source_started" ->
        let%bind scope = field json "scope" >>= Scope.decode in
        let%map origin = field json "origin" >>= Payload.Origin.of_json in
        Source_started { scope; origin }
      | "item_announced" ->
        let%map item = field json "item" >>= Item.of_json in
        Item_announced item
      | "part_announced" ->
        let%map part = field json "part" >>= Part.of_json in
        Part_announced part
      | "changed" ->
        let%bind target = field json "target" >>= target_of_json in
        let%map change = field json "change" >>= change_of_json in
        Changed { target; change }
      | "item_finalized" ->
        let%bind item = field json "item" >>= Item.of_json in
        let%map entry = field json "entry" >>= entry_of_json in
        Item_finalized { item; entry }
      | "source_finished" ->
        let%bind scope = field json "scope" >>= Scope.decode in
        let%map completion = field json "completion" >>= completion_of_json in
        Source_finished { scope; completion }
      | "unknown_event" ->
        let%bind scope = field json "scope" >>= Scope.decode in
        let%bind provider_kind = field json "provider_kind" >>= string in
        let%map raw = field json "raw" in
        Unknown_event { scope; provider_kind; raw }
      | _ -> error "unknown transcript event"
    in
    let%map () = validate_view view in
    { view; encoded_bytes; json }
  ;;
end

module Draft = struct
  module Limits = struct
    type t =
      { max_scopes : int
      ; max_items : int
      ; max_parts : int
      ; max_unknown_events : int
      ; max_retained_bytes : int
      ; document_limits : D.Limits.t
      }

    let create
          ~max_scopes
          ~max_items
          ~max_parts
          ~max_unknown_events
          ~max_retained_bytes
          ~document_limits
      =
      if
        List.exists
          [ max_scopes; max_items; max_parts; max_unknown_events; max_retained_bytes ]
          ~f:(fun value -> value <= 0)
      then error "draft limits must be positive"
      else
        Ok
          { max_scopes
          ; max_items
          ; max_parts
          ; max_unknown_events
          ; max_retained_bytes
          ; document_limits
          }
    ;;
  end

  type completeness =
    | Prefix_observed
    | Missing_prefix

  type text =
    { value : string
    ; completeness : completeness
    }

  type part_view =
    { descriptor : Part.t
    ; text : text option
    }

  type partial =
    { parts : part_view list
    ; call_input : text option
    ; completeness : completeness
    }

  type state =
    | Partial of partial
    | Finalized of History_entry.t

  type item_view =
    { descriptor : Item.t
    ; state : state
    }

  type unknown_view =
    { scope : Scope.t
    ; provider_kind : string
    ; raw : Jsonaf.t
    }

  type source_view =
    { scope : Scope.t
    ; origin : Payload.Origin.t
    ; completion : Stream.completion option
    }

  type change =
    | Item_changed of item_view
    | Source_changed of source_view
    | Unknown_observed of unknown_view
    | Scope_cleared of Scope.Key.t

  type retained_item =
    { view : item_view
    ; bytes : int
    ; text_bytes : int Map.M(Part.Key).t
    ; call_bytes : int option
    }

  type retained_source =
    { view : source_view
    ; bytes : int
    }

  type retained_unknown =
    { view : unknown_view
    ; bytes : int
    }

  type retained_scope =
    { descriptor : Scope.t
    ; bytes : int
    ; gap : bool
    ; finished : bool
    }

  type t =
    { limits : Limits.t
    ; scopes : retained_scope Map.M(Scope.Key).t
    ; item_map : retained_item Map.M(Item.Key).t
    ; item_order_reversed : Item.Key.t list
    ; source_map : retained_source Map.M(Scope.Key).t
    ; unknown : retained_unknown list
    ; bytes : int
    ; part_count : int
    ; global_gap : bool
    }

  let create ~limits =
    { limits
    ; scopes = Map.empty (module Scope.Key)
    ; item_map = Map.empty (module Item.Key)
    ; item_order_reversed = []
    ; source_map = Map.empty (module Scope.Key)
    ; unknown = []
    ; bytes = 0
    ; part_count = 0
    ; global_gap = false
    }
  ;;

  let retained_bytes t = t.bytes

  let items t =
    List.rev_map t.item_order_reversed ~f:(fun key -> (Map.find_exn t.item_map key).view)
  ;;

  let sources t =
    Map.data t.source_map |> List.map ~f:(fun (source : retained_source) -> source.view)
  ;;

  let unknown_events t =
    List.rev_map t.unknown ~f:(fun (unknown : retained_unknown) -> unknown.view)
  ;;

  let count_parts = function
    | Partial partial -> List.length partial.parts
    | Finalized _ -> 0
  ;;

  let add_charge old remove add ~cap =
    let remaining = old - remove in
    if add > cap || remaining > cap - add
    then error "draft retained byte limit exceeded"
    else Ok (remaining + add)
  ;;

  let component_bytes t json = measure ~limits:t.limits.document_limits json

  (* Missing_prefix is the longer completeness spelling, so flags have a fixed
     conservative charge. Text placeholders permit admission before concatenation.
     Their admitted UTF8 bytes are charged separately, including JSON escaping. *)
  let text_skeleton = function
    | None -> `Null
    | Some _ -> `Object [ "value", `String ""; "completeness", `String "missing_prefix" ]
  ;;

  let item_skeleton (view : item_view) =
    let state =
      match view.state with
      | Finalized entry -> `Object [ "finalized", Stream.entry_json entry ]
      | Partial partial ->
        `Object
          [ ( "partial"
            , `Object
                [ ( "parts"
                  , `Array
                      (List.map partial.parts ~f:(fun part ->
                         `Object
                           [ "descriptor", Part.to_json part.descriptor
                           ; "text", text_skeleton part.text
                           ])) )
                ; "call_input", text_skeleton partial.call_input
                ; "completeness", `String "missing_prefix"
                ] )
          ]
    in
    `Object [ "descriptor", Item.to_json view.descriptor; "state", state ]
  ;;

  let item_charge t (item : retained_item) =
    let open Result.Let_syntax in
    let%bind base = component_bytes t (item_skeleton item.view) in
    let additions = Map.data item.text_bytes @ Option.to_list item.call_bytes in
    List.fold_result additions ~init:base ~f:(fun bytes quoted ->
      add_charge bytes 0 (quoted - 2) ~cap:(D.Limits.max_bytes t.limits.document_limits))
  ;;

  let scope_charge t scope = component_bytes t (Scope.to_json scope)

  let source_charge t (source : source_view) =
    component_bytes
      t
      (`Object
          [ "scope", Scope.to_json source.scope
          ; "origin", Payload.Origin.to_json source.origin
          ; "completion", option_json Stream.completion_json source.completion
          ])
  ;;

  let unknown_charge t (unknown : unknown_view) =
    component_bytes
      t
      (`Object
          [ "scope", Scope.to_json unknown.scope
          ; "provider_kind", `String unknown.provider_kind
          ; "raw", unknown.raw
          ])
  ;;

  let empty_item descriptor ~gap =
    { view =
        { descriptor
        ; state =
            Partial
              { parts = []
              ; call_input = None
              ; completeness = (if gap then Missing_prefix else Prefix_observed)
              }
        }
    ; bytes = 0
    ; text_bytes = Map.empty (module Part.Key)
    ; call_bytes = None
    }
  ;;

  let admit_scope t scope ~cap =
    let key = Scope.key scope in
    match Map.find t.scopes key with
    | Some known ->
      if not (Scope.equal known.descriptor scope)
      then error "conflicting scope relation"
      else if known.finished
      then error "event after source completion"
      else Ok (t, known.gap)
    | None ->
      if Map.length t.scopes >= t.limits.max_scopes
      then error "draft scope count exceeded"
      else
        let open Result.Let_syntax in
        let%bind charge = scope_charge t scope in
        let%map bytes = add_charge t.bytes 0 charge ~cap in
        let retained =
          { descriptor = scope; bytes = charge; gap = t.global_gap; finished = false }
        in
        { t with scopes = Map.set t.scopes ~key ~data:retained; bytes }, t.global_gap
  ;;

  let refine_item t descriptor ~gap =
    match Map.find t.item_map (Item.key descriptor) with
    | None ->
      if Map.length t.item_map >= t.limits.max_items
      then error "draft item count exceeded"
      else Ok (empty_item descriptor ~gap)
    | Some existing ->
      let open Result.Let_syntax in
      let%map descriptor = Item.refine existing.view.descriptor descriptor in
      let state =
        match existing.view.state with
        | Finalized entry -> Finalized entry
        | Partial partial ->
          Partial
            { partial with
              parts =
                List.map partial.parts ~f:(fun part ->
                  { part with descriptor = { part.descriptor with item = descriptor } })
            }
      in
      { existing with view = { descriptor; state } }
  ;;

  let store_item t (item : retained_item) ~cap =
    let key = Item.key item.view.descriptor in
    let previous = Map.find t.item_map key in
    let old_charge =
      Option.value_map previous ~default:0 ~f:(fun (item : retained_item) -> item.bytes)
    in
    let old_parts =
      Option.value_map previous ~default:0 ~f:(fun item -> count_parts item.view.state)
    in
    let part_count = t.part_count - old_parts + count_parts item.view.state in
    if part_count > t.limits.max_parts
    then error "draft part count exceeded"
    else
      let open Result.Let_syntax in
      let%bind () =
        match item.view.state with
        | Finalized _ -> Ok ()
        | Partial partial ->
          let%bind () =
            List.fold_result partial.parts ~init:() ~f:(fun () part ->
              Stream.validate_part part.descriptor)
          in
          (match partial.call_input, item.view.descriptor.header with
           | None, _ | Some _, (None | Some (Header.Call _)) -> Ok ()
           | Some _, Some (Header.Message _ | Result _ | Reasoning | Unknown _) ->
             error "input bytes contradict refined header")
      in
      let%bind charge = item_charge t item in
      let%map bytes = add_charge t.bytes old_charge charge ~cap in
      let item = { item with bytes = charge } in
      let item_order_reversed =
        match previous with
        | Some _ -> t.item_order_reversed
        | None -> key :: t.item_order_reversed
      in
      ( { t with
          item_map = Map.set t.item_map ~key ~data:item
        ; item_order_reversed
        ; bytes
        ; part_count
        }
      , item )
  ;;

  let refine_part (existing : Part.t) (incoming : Part.t) =
    if
      not
        (Part.Key.equal (Part.key existing) (Part.key incoming)
         && Part.equal_kind existing.kind incoming.kind)
    then error "conflicting content part"
    else
      let open Result.Let_syntax in
      let%bind index = Item.refine_optional Int.equal existing.index incoming.index in
      let%map item = Item.refine existing.item incoming.item in
      { existing with item; index }
  ;;

  let prepare_part (partial : partial) (descriptor : Part.t) =
    let key = Part.key descriptor in
    match
      List.find partial.parts ~f:(fun part ->
        Part.Key.equal (Part.key part.descriptor) key)
    with
    | None -> Ok ({ descriptor; text = None }, false)
    | Some existing ->
      Result.map (refine_part existing.descriptor descriptor) ~f:(fun descriptor ->
        { existing with descriptor }, true)
  ;;

  let replace_part (partial : partial) (part : part_view) =
    let key = Part.key part.descriptor in
    let parts =
      List.filter partial.parts ~f:(fun (existing : part_view) ->
        not (Part.Key.equal (Part.key existing.descriptor) key))
    in
    let parts =
      part :: parts
      |> List.sort ~compare:(fun left right ->
        let by_index =
          match left.descriptor.index, right.descriptor.index with
          | Some left, Some right -> Int.compare left right
          | Some _, None -> -1
          | None, Some _ -> 1
          | None, None -> 0
        in
        if by_index <> 0
        then by_index
        else Part.Key.compare (Part.key left.descriptor) (Part.key right.descriptor))
    in
    { partial with parts }
  ;;

  let text_change t (old : text option) old_bytes change ~prefix =
    let value, append =
      match change with
      | Stream.Append value -> value, true
      | Replace value -> value, false
    in
    let open Result.Let_syntax in
    let%bind new_bytes = component_bytes t (`String value) in
    let%bind bytes =
      if append
      then
        add_charge
          (Option.value old_bytes ~default:2)
          2
          new_bytes
          ~cap:(D.Limits.max_bytes t.limits.document_limits)
      else Ok new_bytes
    in
    let completeness =
      if not append
      then Prefix_observed
      else (
        match old with
        | Some text -> text.completeness
        | None -> if prefix then Prefix_observed else Missing_prefix)
    in
    let pending = { value = ""; completeness } in
    let realize () =
      let value =
        if append
        then Option.value_map old ~default:"" ~f:(fun old -> old.value) ^ value
        else value
      in
      { pending with value }
    in
    Ok (pending, bytes, realize)
  ;;

  let apply_at_limit t ~cap event =
    if cap < 0
    then error "negative draft byte allowance"
    else
      let open Result.Let_syntax in
      let scope = Stream.scope event in
      let%bind t, gap = admit_scope t scope ~cap in
      match Stream.view event with
      | Item_announced descriptor ->
        let%bind item = refine_item t descriptor ~gap in
        let%map t, item = store_item t item ~cap in
        t, [ Item_changed item.view ]
      | Part_announced descriptor ->
        let%bind item = refine_item t descriptor.item ~gap in
        (match item.view.state with
         | Finalized _ -> error "part after item finalization"
         | Partial partial ->
           let descriptor = { descriptor with item = item.view.descriptor } in
           let%bind part, _ = prepare_part partial descriptor in
           let partial = replace_part partial part in
           let%map t, item =
             store_item
               t
               { item with view = { item.view with state = Partial partial } }
               ~cap
           in
           t, [ Item_changed item.view ])
      | Changed { target; change } ->
        let descriptor =
          match target with
          | Content part -> part.item
          | Call_input item -> item
        in
        let was_announced = Map.mem t.item_map (Item.key descriptor) in
        let%bind item = refine_item t descriptor ~gap in
        (match item.view.state with
         | Finalized _ -> error "change after item finalization"
         | Partial partial ->
           (match target with
            | Content descriptor ->
              let descriptor = { descriptor with item = item.view.descriptor } in
              let%bind part, was_part = prepare_part partial descriptor in
              let key = Part.key descriptor in
              let%bind pending, quoted, realize =
                text_change
                  t
                  part.text
                  (Map.find item.text_bytes key)
                  change
                  ~prefix:(was_part && not gap)
              in
              let part = { part with text = Some pending } in
              let partial = replace_part partial part in
              let partial =
                match pending.completeness with
                | Missing_prefix -> { partial with completeness = Missing_prefix }
                | Prefix_observed -> partial
              in
              let candidate =
                { item with
                  view = { item.view with state = Partial partial }
                ; text_bytes = Map.set item.text_bytes ~key ~data:quoted
                }
              in
              let%map t, candidate = store_item t candidate ~cap in
              let part = { part with text = Some (realize ()) } in
              let view =
                { candidate.view with state = Partial (replace_part partial part) }
              in
              let candidate = { candidate with view } in
              ( { t with
                  item_map =
                    Map.set t.item_map ~key:(Item.key view.descriptor) ~data:candidate
                }
              , [ Item_changed view ] )
            | Call_input _ ->
              let%bind pending, quoted, realize =
                text_change
                  t
                  partial.call_input
                  item.call_bytes
                  change
                  ~prefix:(was_announced && not gap)
              in
              let partial =
                { partial with
                  call_input = Some pending
                ; completeness =
                    (match pending.completeness with
                     | Missing_prefix -> Missing_prefix
                     | Prefix_observed -> partial.completeness)
                }
              in
              let candidate =
                { item with
                  view = { item.view with state = Partial partial }
                ; call_bytes = Some quoted
                }
              in
              let%map t, candidate = store_item t candidate ~cap in
              let view =
                { candidate.view with
                  state = Partial { partial with call_input = Some (realize ()) }
                }
              in
              let candidate = { candidate with view } in
              ( { t with
                  item_map =
                    Map.set t.item_map ~key:(Item.key view.descriptor) ~data:candidate
                }
              , [ Item_changed view ] )))
      | Item_finalized { item = descriptor; entry } ->
        let%bind item =
          match Map.find t.item_map (Item.key descriptor) with
          | None -> refine_item t descriptor ~gap
          | Some previous ->
            let%bind _ =
              Item.refine_optional
                History_entry.Id.equal
                previous.view.descriptor.entry_id
                descriptor.entry_id
            in
            if not (Scope.equal previous.view.descriptor.scope descriptor.scope)
            then error "conflicting finalization scope"
            else Ok { previous with view = { previous.view with descriptor } }
        in
        (match item.view.state with
         | Finalized existing ->
           if
             History_entry.Id.equal (History_entry.id existing) (History_entry.id entry)
             && Jsonaf.exactly_equal
                  (Payload.to_json (History_entry.payload existing))
                  (Payload.to_json (History_entry.payload entry))
           then Ok (t, [])
           else error "conflicting item finalization"
         | Partial _ ->
           let candidate =
             { item with
               view = { descriptor = item.view.descriptor; state = Finalized entry }
             ; text_bytes = Map.empty (module Part.Key)
             ; call_bytes = None
             }
           in
           let%map t, candidate = store_item t candidate ~cap in
           t, [ Item_changed candidate.view ])
      | Source_started { scope; origin } ->
        let key = Scope.key scope in
        (match Map.find t.source_map key with
         | Some existing ->
           if
             Jsonaf.exactly_equal
               (Payload.Origin.to_json existing.view.origin)
               (Payload.Origin.to_json origin)
           then Ok (t, [])
           else error "conflicting source origin"
         | None ->
           let view = { scope; origin; completion = None } in
           let%bind charge = source_charge t view in
           let%map bytes = add_charge t.bytes 0 charge ~cap in
           ( { t with
               bytes
             ; source_map = Map.set t.source_map ~key ~data:{ view; bytes = charge }
             }
           , [ Source_changed view ] ))
      | Source_finished { scope; completion } ->
        let key = Scope.key scope in
        let origin =
          Option.value_map
            (Map.find t.source_map key)
            ~default:Payload.Origin.unavailable
            ~f:(fun (source : retained_source) -> source.view.origin)
        in
        let view = { scope; origin; completion = Some completion } in
        let%bind charge = source_charge t view in
        let old =
          Option.value_map (Map.find t.source_map key) ~default:0 ~f:(fun source ->
            source.bytes)
        in
        let%map bytes = add_charge t.bytes old charge ~cap in
        ( { t with
            bytes
          ; source_map = Map.set t.source_map ~key ~data:{ view; bytes = charge }
          ; scopes =
              Map.update t.scopes key ~f:(function
                | Some scope -> { scope with finished = true }
                | None -> assert false)
          }
        , [ Source_changed view ] )
      | Unknown_event { scope; provider_kind; raw } ->
        if List.length t.unknown >= t.limits.max_unknown_events
        then error "draft unknown event count exceeded"
        else (
          let view = { scope; provider_kind; raw } in
          let%bind charge = unknown_charge t view in
          let%map bytes = add_charge t.bytes 0 charge ~cap in
          ( { t with bytes; unknown = { view; bytes = charge } :: t.unknown }
          , [ Unknown_observed view ] ))
  ;;

  let apply t ?max_retained_bytes event =
    let cap =
      Int.min
        t.limits.max_retained_bytes
        (Option.value max_retained_bytes ~default:t.limits.max_retained_bytes)
    in
    let open Result.Let_syntax in
    let%bind candidate, changes = apply_at_limit t ~cap event in
    if candidate.bytes > cap
    then error "draft retained byte limit exceeded"
    else Ok (candidate, changes)
  ;;

  let mark_text_gap =
    Option.map ~f:(fun (text : text) -> { text with completeness = Missing_prefix })
  ;;

  let mark_gap t ~scope =
    let matches key = Option.value_map scope ~default:true ~f:(Scope.Key.equal key) in
    let item_map =
      Map.mapi t.item_map ~f:(fun ~key ~data:item ->
        if not (matches key.scope)
        then item
        else (
          match item.view.state with
          | Finalized _ -> item
          | Partial partial ->
            let partial =
              { parts =
                  List.map partial.parts ~f:(fun part ->
                    { part with text = mark_text_gap part.text })
              ; call_input = mark_text_gap partial.call_input
              ; completeness = Missing_prefix
              }
            in
            { item with view = { item.view with state = Partial partial } }))
    in
    let scopes =
      Map.mapi t.scopes ~f:(fun ~key ~data ->
        if matches key then { data with gap = true } else data)
    in
    { t with
      item_map
    ; scopes
    ; global_gap =
        t.global_gap
        || Option.value_map scope ~default:true ~f:(fun key -> not (Map.mem t.scopes key))
    }
  ;;

  let remove_item t key =
    match Map.find t.item_map key with
    | None -> t
    | Some item ->
      { t with
        item_map = Map.remove t.item_map key
      ; item_order_reversed =
          List.filter t.item_order_reversed ~f:(fun retained ->
            not (Item.Key.equal retained key))
      ; bytes = t.bytes - item.bytes
      ; part_count = t.part_count - count_parts item.view.state
      }
  ;;

  let clear_scope t key =
    let t =
      Map.keys t.item_map
      |> List.fold ~init:t ~f:(fun t item ->
        if Scope.Key.equal item.scope key then remove_item t item else t)
    in
    let removed_scope =
      Option.value_map (Map.find t.scopes key) ~default:0 ~f:(fun scope -> scope.bytes)
    in
    let removed_source =
      Option.value_map (Map.find t.source_map key) ~default:0 ~f:(fun source ->
        source.bytes)
    in
    let removed_unknown, unknown =
      List.partition_tf t.unknown ~f:(fun unknown ->
        Scope.Key.equal unknown.view.scope.key key)
    in
    let removed_unknown =
      List.sum
        (module Int)
        removed_unknown
        ~f:(fun (unknown : retained_unknown) -> unknown.bytes)
    in
    { t with
      scopes = Map.remove t.scopes key
    ; source_map = Map.remove t.source_map key
    ; unknown
    ; bytes = t.bytes - removed_scope - removed_source - removed_unknown
    }
  ;;
end
