open Core

module Presence = struct
  type 'a t =
    | Absent
    | Null
    | Value of 'a
  [@@deriving equal, sexp]
end

module Decode_error = struct
  type reason =
    | Missing_field
    | Wrong_type of string
    | Invalid_value of string
    | Duplicate_field of string
    | Limit_exceeded
  [@@deriving equal, sexp]

  type t =
    { path : string
    ; reason : reason
    }
  [@@deriving equal, sexp]
end

(* Only this private validation exception is caught at the pure decode boundary.
   Unexpected failures are not converted into malformed-input errors. *)
exception Invalid_wire of Decode_error.t

let invalid path reason = raise (Invalid_wire { path; reason })
let field_path path key = path ^ "." ^ key

let object_fields path = function
  | `Object fields -> fields
  | _ -> invalid path (Wrong_type "object")
;;

let string path = function
  | `String value -> value
  | _ -> invalid path (Wrong_type "string")
;;

let nonempty_string path json =
  let value = string path json in
  if String.is_empty value then invalid path (Invalid_value "nonempty string required");
  value
;;

let boolean path = function
  | `True -> true
  | `False -> false
  | _ -> invalid path (Wrong_type "boolean")
;;

let nonnegative_int64 path = function
  | `Number text when (not (String.is_empty text)) && String.for_all text ~f:Char.is_digit
    ->
    (match Int64.of_string_opt text with
     | Some value when Int64.(value >= zero) -> value
     | Some _ | None -> invalid path (Invalid_value "nonnegative int64 required"))
  | _ -> invalid path (Wrong_type "nonnegative integer")
;;

let index path json =
  let value = nonnegative_int64 path json in
  match Int64.to_int value with
  | Some value -> value
  | None -> invalid path (Invalid_value "index exceeds OCaml int range")
;;

let array path parse = function
  | `Array values ->
    List.mapi values ~f:(fun i value -> parse (path ^ "[" ^ Int.to_string i ^ "]") value)
  | _ -> invalid path (Wrong_type "array")
;;

let required path fields key parse =
  let path = field_path path key in
  match List.Assoc.find fields key ~equal:String.equal with
  | Some value -> parse path value
  | None -> invalid path Missing_field
;;

let presence ?(nullable = false) path fields key parse =
  let path = field_path path key in
  match List.Assoc.find fields key ~equal:String.equal with
  | None -> Presence.Absent
  | Some `Null when nullable -> Presence.Null
  | Some value -> Presence.Value (parse path value)
;;

let required_nullable path fields key parse =
  match List.Assoc.find fields key ~equal:String.equal with
  | None -> invalid (field_path path key) Missing_field
  | Some `Null -> Presence.Null
  | Some value -> Presence.Value (parse (field_path path key) value)
;;

let valid_json_number text =
  let length = String.length text in
  let rec digits position =
    if position < length && Char.is_digit text.[position]
    then digits (position + 1)
    else position
  in
  let start = if length > 0 && Char.equal text.[0] '-' then 1 else 0 in
  if start >= length
  then false
  else (
    let integer_end =
      if Char.equal text.[start] '0'
      then start + 1
      else if Char.(text.[start] >= '1' && text.[start] <= '9')
      then digits (start + 1)
      else start
    in
    if integer_end = start
    then false
    else (
      let fraction_end =
        if integer_end < length && Char.equal text.[integer_end] '.'
        then (
          let after = digits (integer_end + 1) in
          if after = integer_end + 1 then -1 else after)
        else integer_end
      in
      if fraction_end < 0
      then false
      else (
        let exponent_end =
          if
            fraction_end < length
            && (Char.equal text.[fraction_end] 'e' || Char.equal text.[fraction_end] 'E')
          then (
            let start = fraction_end + 1 in
            let start =
              if
                start < length
                && (Char.equal text.[start] '+' || Char.equal text.[start] '-')
              then start + 1
              else start
            in
            let after = digits start in
            if after = start then -1 else after)
          else fraction_end
        in
        exponent_end = length)))
;;

(* Validated objects have unique keys. Compare object meaning independent of key
   order while preserving array order and exact string/number representations. *)
let rec equal_json left right =
  match left, right with
  | `Object left, `Object right ->
    let sort =
      List.sort ~compare:(fun (left, _) (right, _) -> String.compare left right)
    in
    List.equal
      (fun (lk, lv) (rk, rv) -> String.equal lk rk && equal_json lv rv)
      (sort left)
      (sort right)
  | `Array left, `Array right -> List.equal equal_json left right
  | `String left, `String right | `Number left, `Number right -> String.equal left right
  | `Null, `Null | `True, `True | `False, `False -> true
  | (`Object _ | `Array _ | `String _ | `Number _ | `Null | `True | `False), _ -> false
;;

let validate_json raw =
  (* Bounded, stack-safe traversal before narrowing, including opaque extras.
     Duplicate keys are rejected rather than choosing an ambiguous projection. *)
  let rec loop pending nodes =
    match pending with
    | [] -> ()
    | (depth, path, json) :: rest ->
      if depth > 128 || nodes >= 100_000 then invalid path Limit_exceeded;
      let children =
        match json with
        | `Object fields ->
          let keys = Hash_set.create (module String) in
          List.map fields ~f:(fun (key, value) ->
            if Hash_set.mem keys key then invalid path (Duplicate_field key);
            Hash_set.add keys key;
            depth + 1, field_path path key, value)
        | `Array values ->
          List.mapi values ~f:(fun i value ->
            depth + 1, path ^ "[" ^ Int.to_string i ^ "]", value)
        | `Number text ->
          if not (valid_json_number text)
          then invalid path (Invalid_value "invalid JSON number");
          []
        | `String _ | `True | `False | `Null -> []
      in
      loop (List.rev_append children rest) (nodes + 1)
  in
  loop [ 0, "$", raw ] 0
;;

let decode raw ~f =
  try
    validate_json raw;
    Ok (f "$" raw)
  with
  | Invalid_wire error -> Error error
;;

module Origin = struct
  type t =
    { provider : string
    ; account : string option
    ; endpoint : string
    }
  [@@deriving equal]

  let create ~provider ~account ~endpoint =
    let validate key value =
      if String.is_empty value
      then invalid key (Invalid_value "nonempty identity required")
    in
    try
      validate "provider" provider;
      Option.iter account ~f:(validate "account");
      validate "endpoint" endpoint;
      Ok { provider; account; endpoint }
    with
    | Invalid_wire error -> Error error
  ;;

  let provider t = t.provider
  let account t = t.account
  let endpoint t = t.endpoint
end

module Status = struct
  type t =
    | In_progress
    | Completed
    | Incomplete
    | Failed
    | Cancelled
    | Queued
    | Other of string
  [@@deriving equal, sexp]

  let parse path json =
    match nonempty_string path json with
    | "in_progress" -> In_progress
    | "completed" -> Completed
    | "incomplete" -> Incomplete
    | "failed" -> Failed
    | "cancelled" -> Cancelled
    | "queued" -> Queued
    | value -> Other value
  ;;
end

module Phase = struct
  type t =
    | Commentary
    | Final_answer
    | Other of string
  [@@deriving equal, sexp]

  let parse path json =
    match nonempty_string path json with
    | "commentary" -> Commentary
    | "final_answer" -> Final_answer
    | value -> Other value
  ;;
end

module Part = struct
  type view =
    | Output_text of
        { text : string
        ; annotations : Jsonaf.t list
        ; logprobs : Jsonaf.t Presence.t
        }
    | Refusal of string
    | Summary_text of string
    | Reasoning_text of string
    | Unknown of string

  type t =
    { raw : Jsonaf.t
    ; origin : Origin.t
    ; view : view
    }

  let parse ~origin path raw =
    let fields = object_fields path raw in
    let name = required path fields "type" nonempty_string in
    let view =
      match name with
      | "output_text" ->
        let text = required path fields "text" string in
        let annotations =
          required path fields "annotations" (fun path value ->
            array path (fun _ value -> value) value)
        in
        let logprobs =
          presence path fields "logprobs" (fun path value ->
            ignore (array path (fun _ value -> value) value : Jsonaf.t list);
            value)
        in
        Output_text { text; annotations; logprobs }
      | "refusal" -> Refusal (required path fields "refusal" string)
      | "summary_text" -> Summary_text (required path fields "text" string)
      | "reasoning_text" -> Reasoning_text (required path fields "text" string)
      | name -> Unknown name
    in
    { raw; origin; view }
  ;;

  let parse_in_context ~origin ~context path raw =
    let part = parse ~origin path raw in
    let valid =
      match context, part.view with
      | `Message, (Output_text _ | Refusal _ | Unknown _)
      | `Summary, (Summary_text _ | Unknown _)
      | `Reasoning, (Reasoning_text _ | Unknown _)
      | `Content_event, (Output_text _ | Refusal _ | Reasoning_text _ | Unknown _) -> true
      | (`Message | `Summary | `Reasoning | `Content_event), _ -> false
    in
    if not valid
    then invalid (field_path path "type") (Invalid_value "known part in wrong context");
    part
  ;;

  let decode raw ~origin = decode raw ~f:(parse ~origin)
  let view t = t.view
  let raw t = t.raw
  let origin t = t.origin
end

module Item = struct
  type call =
    | Function of
        { name : string
        ; namespace : string Presence.t
        ; call_id : string
        ; arguments : string
        ; async : bool Presence.t
        }
    | Custom of
        { name : string
        ; namespace : string Presence.t
        ; call_id : string
        ; input : string
        ; async : bool Presence.t
        }

  type view =
    | Message of
        { content : Part.t list
        ; phase : Phase.t Presence.t
        }
    | Call of call
    | Reasoning of
        { summary : Part.t list
        ; content : Part.t list Presence.t
        ; encrypted_content : string Presence.t
        }
    | Unknown of string

  type t =
    { raw : Jsonaf.t
    ; origin : Origin.t
    ; view : view
    ; id : string Presence.t
    ; status : Status.t Presence.t
    ; caller_supported : bool
    }

  let parse ~origin path raw =
    let fields = object_fields path raw in
    let name = required path fields "type" nonempty_string in
    let known =
      List.mem
        [ "message"; "function_call"; "custom_tool_call"; "reasoning" ]
        name
        ~equal:String.equal
    in
    let id =
      if String.equal name "message" || String.equal name "reasoning"
      then Presence.Value (required path fields "id" nonempty_string)
      else if known
      then presence path fields "id" nonempty_string
      else (
        match List.Assoc.find fields "id" ~equal:String.equal with
        | Some (`String id) -> Presence.Value id
        | Some `Null -> Presence.Null
        | Some _ | None -> Presence.Absent)
    in
    let status =
      if String.equal name "message"
      then (
        let status = required path fields "status" Status.parse in
        match status with
        | In_progress | Completed | Incomplete -> Presence.Value status
        | Failed | Cancelled | Queued | Other _ ->
          invalid (field_path path "status") (Invalid_value "output message status"))
      else if known
      then presence path fields "status" Status.parse
      else Presence.Absent
    in
    let namespace () = presence path fields "namespace" nonempty_string in
    let async () = presence path fields "async" boolean in
    let caller_supported =
      if String.equal name "function_call" || String.equal name "custom_tool_call"
      then (
        match
          presence ~nullable:true path fields "caller" (fun path value ->
            let fields = object_fields path value in
            let type_name = required path fields "type" nonempty_string in
            if String.equal type_name "program"
            then ignore (required path fields "caller_id" nonempty_string : string);
            type_name)
        with
        | Presence.Absent | Null | Value "direct" -> true
        | Value _ -> false)
      else true
    in
    let view =
      match name with
      | "message" ->
        let role = required path fields "role" nonempty_string in
        if not (String.equal role "assistant")
        then
          invalid (field_path path "role") (Invalid_value "output role must be assistant");
        let content =
          required path fields "content" (fun path value ->
            array path (Part.parse_in_context ~origin ~context:`Message) value)
        in
        let phase = presence ~nullable:true path fields "phase" Phase.parse in
        Message { content; phase }
      | "function_call" ->
        let name = required path fields "name" nonempty_string in
        let call_id = required path fields "call_id" nonempty_string in
        let arguments = required path fields "arguments" string in
        Call
          (Function
             { name; namespace = namespace (); call_id; arguments; async = async () })
      | "custom_tool_call" ->
        let name = required path fields "name" nonempty_string in
        let call_id = required path fields "call_id" nonempty_string in
        let input = required path fields "input" string in
        Call (Custom { name; namespace = namespace (); call_id; input; async = async () })
      | "reasoning" ->
        let summary =
          required path fields "summary" (fun path value ->
            array path (Part.parse_in_context ~origin ~context:`Summary) value)
        in
        let content =
          presence path fields "content" (fun path value ->
            array path (Part.parse_in_context ~origin ~context:`Reasoning) value)
        in
        let encrypted_content =
          presence ~nullable:true path fields "encrypted_content" string
        in
        Reasoning { summary; content; encrypted_content }
      | name -> Unknown name
    in
    { raw; origin; view; id; status; caller_supported }
  ;;

  let decode raw ~origin = decode raw ~f:(parse ~origin)
  let view t = t.view
  let id t = t.id
  let status t = t.status
  let raw t = t.raw
  let origin t = t.origin

  let local_call t =
    let namespace_and_async_supported namespace async =
      (match namespace with
       | Presence.Absent -> true
       | Null | Value _ -> false)
      &&
      match async with
      | Presence.Absent | Value false -> true
      | Null | Value true -> false
    in
    let complete =
      match t.status with
      | Presence.Absent | Value Completed -> true
      | Null | Value (In_progress | Incomplete | Failed | Cancelled | Queued | Other _) ->
        false
    in
    if not (complete && t.caller_supported)
    then None
    else (
      match t.view with
      | Call (Function { namespace; async; _ } as call)
      | Call (Custom { namespace; async; _ } as call) ->
        if namespace_and_async_supported namespace async then Some call else None
      | Message _ | Reasoning _ | Unknown _ -> None)
  ;;
end

module Usage = struct
  type t =
    { input_tokens : int64
    ; output_tokens : int64
    ; total_tokens : int64
    ; cached_tokens : int64 Presence.t
    ; cache_write_tokens : int64 Presence.t
    ; reasoning_tokens : int64 Presence.t
    ; raw : Jsonaf.t
    }

  let parse path raw =
    let fields = object_fields path raw in
    let input_tokens = required path fields "input_tokens" nonnegative_int64 in
    let output_tokens = required path fields "output_tokens" nonnegative_int64 in
    let total_tokens = required path fields "total_tokens" nonnegative_int64 in
    let detail object_key count_key =
      match presence ~nullable:true path fields object_key object_fields with
      | Presence.Absent -> Presence.Absent
      | Null -> Null
      | Value detail_fields ->
        presence
          ~nullable:true
          (field_path path object_key)
          detail_fields
          count_key
          nonnegative_int64
    in
    let cached_tokens = detail "input_tokens_details" "cached_tokens" in
    let cache_write_tokens = detail "input_tokens_details" "cache_write_tokens" in
    let reasoning_tokens = detail "output_tokens_details" "reasoning_tokens" in
    { input_tokens
    ; output_tokens
    ; total_tokens
    ; cached_tokens
    ; cache_write_tokens
    ; reasoning_tokens
    ; raw
    }
  ;;

  let input_tokens t = t.input_tokens
  let output_tokens t = t.output_tokens
  let total_tokens t = t.total_tokens
  let cached_tokens t = t.cached_tokens
  let cache_write_tokens t = t.cache_write_tokens
  let reasoning_tokens t = t.reasoning_tokens
  let raw t = t.raw
end

module Provider_error = struct
  type t =
    { code : string Presence.t
    ; message : string
    ; param : string Presence.t
    }

  let response_parse path raw =
    let fields = object_fields path raw in
    let code = Presence.Value (required path fields "code" nonempty_string) in
    let message = required path fields "message" string in
    { code; message; param = Presence.Absent }
  ;;

  let event_parse path fields =
    let code = required_nullable path fields "code" string in
    let message = required path fields "message" string in
    let param = required_nullable path fields "param" string in
    { code; message; param }
  ;;
end

module Response = struct
  type t =
    { raw : Jsonaf.t
    ; origin : Origin.t
    ; id : string
    ; status : Status.t Presence.t
    ; output : Item.t list
    ; usage : Usage.t Presence.t
    ; error : Provider_error.t Presence.t
    ; incomplete_reason : string Presence.t
    }

  type outcome =
    | Completed
    | Refused
    | Incomplete of { reason : string Presence.t }
    | Failed of Provider_error.t Presence.t
    | Nonterminal of Status.t Presence.t

  let parse ~origin path raw =
    let fields = object_fields path raw in
    let id = required path fields "id" nonempty_string in
    let object_name = required path fields "object" string in
    if not (String.equal object_name "response")
    then invalid (field_path path "object") (Invalid_value "expected response object");
    let status = presence path fields "status" Status.parse in
    let output =
      required path fields "output" (fun path value ->
        array path (Item.parse ~origin) value)
    in
    let usage = presence ~nullable:true path fields "usage" Usage.parse in
    let error =
      presence ~nullable:true path fields "error" Provider_error.response_parse
    in
    let incomplete_reason =
      match presence ~nullable:true path fields "incomplete_details" object_fields with
      | Presence.Absent -> Presence.Absent
      | Null -> Null
      | Value fields ->
        presence (field_path path "incomplete_details") fields "reason" nonempty_string
    in
    { raw; origin; id; status; output; usage; error; incomplete_reason }
  ;;

  let decode raw ~origin = decode raw ~f:(parse ~origin)
  let id t = t.id
  let status t = t.status
  let output t = t.output
  let usage t = t.usage
  let error t = t.error
  let incomplete_reason t = t.incomplete_reason

  let outcome t =
    match t.status with
    | Presence.Value Completed ->
      if
        List.exists t.output ~f:(fun item ->
          match Item.view item with
          | Message { content; _ } ->
            List.exists content ~f:(fun part ->
              match Part.view part with
              | Refusal _ -> true
              | Output_text _ | Summary_text _ | Reasoning_text _ | Unknown _ -> false)
          | Call _ | Reasoning _ | Unknown _ -> false)
      then Refused
      else Completed
    | Value Incomplete -> Incomplete { reason = t.incomplete_reason }
    | Value Failed -> Failed t.error
    | (Absent | Null | Value (In_progress | Cancelled | Queued | Other _)) as status ->
      Nonterminal status
  ;;

  let raw t = t.raw
  let origin t = t.origin
end

module Event = struct
  module Delta_kind = struct
    type t =
      | Text
      | Refusal
      | Reasoning_summary
      | Reasoning_text
      | Function_arguments
      | Custom_input
    [@@deriving equal, compare, sexp]
  end

  module Part_space = struct
    type t =
      | Content
      | Summary
    [@@deriving equal, compare, sexp]
  end

  type location =
    { item_id : string
    ; output_index : int
    ; part_index : int option
    }

  type lifecycle =
    | Created
    | In_progress
  [@@deriving equal, sexp]

  type terminal =
    | Completed
    | Incomplete
    | Failed
  [@@deriving equal, sexp]

  type view =
    | Response of
        { lifecycle : lifecycle
        ; response : Response.t
        }
    | Item_added of
        { output_index : int
        ; item : Item.t
        }
    | Item_done of
        { output_index : int
        ; item : Item.t
        }
    | Part_added of
        { location : location
        ; part_space : Part_space.t
        ; part : Part.t
        }
    | Part_done of
        { location : location
        ; part_space : Part_space.t
        ; part : Part.t
        }
    | Delta of
        { location : location
        ; kind : Delta_kind.t
        ; delta : string
        }
    | Text_done of
        { location : location
        ; kind : Delta_kind.t
        ; text : string
        }
    | Annotation_added of
        { location : location
        ; annotation_index : int
        ; annotation : Jsonaf.t
        }
    | Terminal of
        { terminal : terminal
        ; response : Response.t
        }
    | Error of Provider_error.t
    | Unknown of string

  type t =
    { raw : Jsonaf.t
    ; origin : Origin.t
    ; sequence_number : int64 Presence.t
    ; view : view
    }

  let parse ~origin path raw =
    let fields = object_fields path raw in
    let name = required path fields "type" nonempty_string in
    let output_index () = required path fields "output_index" index in
    let location ?part () =
      let item_id = required path fields "item_id" nonempty_string in
      let output_index = output_index () in
      let part_index = Option.map part ~f:(fun key -> required path fields key index) in
      { item_id; output_index; part_index }
    in
    let item () = required path fields "item" (Item.parse ~origin) in
    let part context =
      required path fields "part" (Part.parse_in_context ~origin ~context)
    in
    let response () = required path fields "response" (Response.parse ~origin) in
    let delta ?part kind =
      Delta
        { location = location ?part ()
        ; kind
        ; delta = required path fields "delta" string
        }
    in
    let text_done ?part kind key =
      Text_done
        { location = location ?part (); kind; text = required path fields key string }
    in
    let view =
      match name with
      | "response.created" -> Response { lifecycle = Created; response = response () }
      | "response.in_progress" ->
        Response { lifecycle = In_progress; response = response () }
      | "response.output_item.added" ->
        Item_added { output_index = output_index (); item = item () }
      | "response.output_item.done" ->
        Item_done { output_index = output_index (); item = item () }
      | "response.content_part.added" ->
        Part_added
          { location = location ~part:"content_index" ()
          ; part_space = Content
          ; part = part `Content_event
          }
      | "response.content_part.done" ->
        Part_done
          { location = location ~part:"content_index" ()
          ; part_space = Content
          ; part = part `Content_event
          }
      | "response.reasoning_summary_part.added" ->
        Part_added
          { location = location ~part:"summary_index" ()
          ; part_space = Summary
          ; part = part `Summary
          }
      | "response.reasoning_summary_part.done" ->
        Part_done
          { location = location ~part:"summary_index" ()
          ; part_space = Summary
          ; part = part `Summary
          }
      | "response.output_text.delta" -> delta ~part:"content_index" Text
      | "response.refusal.delta" -> delta ~part:"content_index" Refusal
      | "response.reasoning_summary_text.delta" ->
        delta ~part:"summary_index" Reasoning_summary
      | "response.reasoning_text.delta" -> delta ~part:"content_index" Reasoning_text
      | "response.function_call_arguments.delta" -> delta Function_arguments
      | "response.custom_tool_call_input.delta" -> delta Custom_input
      | "response.output_text.done" -> text_done ~part:"content_index" Text "text"
      | "response.refusal.done" -> text_done ~part:"content_index" Refusal "refusal"
      | "response.reasoning_summary_text.done" ->
        text_done ~part:"summary_index" Reasoning_summary "text"
      | "response.reasoning_text.done" ->
        text_done ~part:"content_index" Reasoning_text "text"
      | "response.function_call_arguments.done" ->
        text_done Function_arguments "arguments"
      | "response.custom_tool_call_input.done" -> text_done Custom_input "input"
      | "response.output_text.annotation.added" ->
        let annotation =
          required_nullable path fields "annotation" (fun path raw ->
            ignore (object_fields path raw : (string * Jsonaf.t) list);
            raw)
        in
        let annotation =
          match annotation with
          | Presence.Null -> `Null
          | Value raw -> raw
          | Absent -> assert false
        in
        Annotation_added
          { location = location ~part:"content_index" ()
          ; annotation_index = required path fields "annotation_index" index
          ; annotation
          }
      | "response.completed" -> Terminal { terminal = Completed; response = response () }
      | "response.incomplete" ->
        Terminal { terminal = Incomplete; response = response () }
      | "response.failed" -> Terminal { terminal = Failed; response = response () }
      | "error" -> Error (Provider_error.event_parse path fields)
      | name -> Unknown name
    in
    let sequence_number =
      match view with
      | Unknown _ -> presence path fields "sequence_number" nonnegative_int64
      | Response _
      | Item_added _
      | Item_done _
      | Part_added _
      | Part_done _
      | Delta _
      | Text_done _
      | Annotation_added _
      | Terminal _
      | Error _ ->
        Presence.Value (required path fields "sequence_number" nonnegative_int64)
    in
    { raw; origin; sequence_number; view }
  ;;

  let decode raw ~origin = decode raw ~f:(parse ~origin)
  let view t = t.view
  let sequence_number t = t.sequence_number
  let raw t = t.raw
  let origin t = t.origin
end

module Tracker = struct
  type error =
    | Origin_mismatch
    | Sequence_conflict of int64
    | Sequence_regression of
        { previous : int64
        ; received : int64
        }
    | Item_conflict of int
    | Part_conflict of int
    | Response_conflict
    | Event_after_terminal
    | Terminal_mismatch
    | Truncated
  [@@deriving equal, sexp]

  type disposition =
    | Accepted
    | Duplicate
  [@@deriving equal, sexp]

  type completion =
    | Response of
        { terminal : Event.terminal
        ; response : Response.t
        }
    | Error of Provider_error.t

  module Text_key = struct
    module T = struct
      type t =
        { output_index : int
        ; part_index : int option
        ; kind : Event.Delta_kind.t
        }
      [@@deriving compare, sexp]
    end

    include T
    include Comparator.Make (T)
  end

  module Part_key = struct
    module T = struct
      type t =
        { output_index : int
        ; part_index : int
        ; space : Event.Part_space.t
        }
      [@@deriving compare, sexp]
    end

    include T
    include Comparator.Make (T)
  end

  module Annotation_key = struct
    module T = struct
      type t =
        { output_index : int
        ; content_index : int
        ; annotation_index : int
        }
      [@@deriving compare, sexp]
    end

    include T
    include Comparator.Make (T)
  end

  type slot =
    { item_id : string option
    ; added : Item.t option
    ; final : Item.t option
    }

  type t =
    { origin : Origin.t
    ; slots : slot Int.Map.t
    ; observed_text : (Text_key.t, unit, Text_key.comparator_witness) Map.t
    ; text_finals : (Text_key.t, string, Text_key.comparator_witness) Map.t
    ; observed_parts : (Part_key.t, string, Part_key.comparator_witness) Map.t
    ; part_finals : (Part_key.t, Part.t, Part_key.comparator_witness) Map.t
    ; annotations : (Annotation_key.t, Jsonaf.t, Annotation_key.comparator_witness) Map.t
    ; response_id : string option
    ; last_sequence : (int64 * Jsonaf.t) option
    ; terminal : (completion * Jsonaf.t) option
    }

  type transition =
    { tracker : t
    ; disposition : disposition
    ; newly_finalized : (int * Item.t) list
    }

  let create origin =
    { origin
    ; slots = Int.Map.empty
    ; observed_text = Map.empty (module Text_key)
    ; text_finals = Map.empty (module Text_key)
    ; observed_parts = Map.empty (module Part_key)
    ; part_finals = Map.empty (module Part_key)
    ; annotations = Map.empty (module Annotation_key)
    ; response_id = None
    ; last_sequence = None
    ; terminal = None
    }
  ;;

  let empty_slot = { item_id = None; added = None; final = None }

  let item_id item =
    match Item.id item with
    | Presence.Value value -> Some value
    | Absent | Null -> None
  ;;

  let same_item left right = equal_json (Item.raw left) (Item.raw right)

  let same_descriptor left right =
    let fields item = object_fields "$" (Item.raw item) in
    let field item key =
      presence ~nullable:true "$" (fields item) key (fun _ value -> value)
    in
    List.for_all [ "type"; "name"; "call_id" ] ~f:(fun key ->
      Presence.equal equal_json (field left key) (field right key))
    && List.for_all [ "namespace"; "async"; "caller"; "phase" ] ~f:(fun key ->
      (* Final snapshots may enrich omitted metadata. An explicit earlier
         restriction must never disappear or change during finalization. *)
      match field left key with
      | Presence.Absent -> true
      | (Null | Value _) as previous ->
        Presence.equal equal_json previous (field right key))
  ;;

  let reconcile_id t output_index received =
    let slot = Option.value (Map.find t.slots output_index) ~default:empty_slot in
    match slot.item_id, received with
    | Some expected, Some received when not (String.equal expected received) ->
      Result.Error (Item_conflict output_index)
    | _, Some received
      when Map.existsi t.slots ~f:(fun ~key ~data ->
             (not (Int.equal key output_index))
             && Option.value_map data.item_id ~default:false ~f:(String.equal received))
      -> Result.Error (Item_conflict output_index)
    | None, Some item_id -> Ok { slot with item_id = Some item_id }
    | Some _, Some _ | Some _, None | None, None -> Ok slot
  ;;

  let correlate t (location : Event.location) =
    let%map.Result slot = reconcile_id t location.output_index (Some location.item_id) in
    { t with slots = Map.set t.slots ~key:location.output_index ~data:slot }
  ;;

  let text_key (location : Event.location) kind =
    { Text_key.output_index = location.output_index
    ; part_index = location.part_index
    ; kind
    }
  ;;

  let part_key (location : Event.location) space =
    match location.part_index with
    | Some part_index ->
      { Part_key.output_index = location.output_index; part_index; space }
    | None -> assert false (* Part events always decode a required index. *)
  ;;

  let part_type part =
    required "$" (object_fields "$" (Part.raw part)) "type" nonempty_string
  ;;

  let part_text part =
    match Part.view part with
    | Output_text { text; _ } -> Some (Event.Delta_kind.Text, text)
    | Refusal text -> Some (Refusal, text)
    | Summary_text text -> Some (Reasoning_summary, text)
    | Reasoning_text text -> Some (Reasoning_text, text)
    | Unknown _ -> None
  ;;

  let kind_supported item kind =
    match Item.view item, kind with
    | Message _, (Event.Delta_kind.Text | Refusal)
    | Reasoning _, (Reasoning_summary | Reasoning_text)
    | Call (Function _), Function_arguments
    | Call (Custom _), Custom_input -> true
    | Unknown _, _ -> true
    | Message _, (Reasoning_summary | Reasoning_text | Function_arguments | Custom_input)
    | Reasoning _, (Text | Refusal | Function_arguments | Custom_input)
    | ( Call (Function _)
      , (Text | Refusal | Reasoning_summary | Reasoning_text | Custom_input) )
    | ( Call (Custom _)
      , (Text | Refusal | Reasoning_summary | Reasoning_text | Function_arguments) ) ->
      false
  ;;

  let validate_kind t (location : Event.location) kind =
    match Map.find t.slots location.output_index with
    | Some { added = Some item; _ } when not (kind_supported item kind) ->
      Result.Error (Part_conflict location.output_index)
    | Some _ | None -> Ok ()
  ;;

  let snapshot_part item space part_index =
    let parts =
      match Item.view item, space with
      | Message { content; _ }, Event.Part_space.Content -> content
      | Reasoning { summary; _ }, Summary -> summary
      | Reasoning { content = Presence.Value content; _ }, Content -> content
      | Reasoning { content = Absent | Null; _ }, Content -> []
      | (Message _ | Call _ | Unknown _), Summary | (Call _ | Unknown _), Content -> []
    in
    List.nth parts part_index
  ;;

  let snapshot_text item (key : Text_key.t) =
    match key.kind, key.part_index, Item.view item with
    | Event.Delta_kind.Function_arguments, None, Call (Function { arguments; _ }) ->
      Some arguments
    | Custom_input, None, Call (Custom { input; _ }) -> Some input
    | ((Text | Refusal | Reasoning_summary | Reasoning_text) as kind), Some part_index, _
      ->
      let space =
        match kind with
        | Reasoning_summary -> Event.Part_space.Summary
        | Text | Refusal | Reasoning_text -> Content
        | Function_arguments | Custom_input -> assert false
      in
      Option.bind (snapshot_part item space part_index) ~f:(fun part ->
        match part_text part with
        | Some (actual_kind, text) when Event.Delta_kind.equal kind actual_kind ->
          Some text
        | Some _ | None -> None)
    | (Function_arguments | Custom_input), Some _, _
    | (Text | Refusal | Reasoning_summary | Reasoning_text), None, _
    | Function_arguments, None, (Message _ | Reasoning _ | Unknown _ | Call (Custom _))
    | Custom_input, None, (Message _ | Reasoning _ | Unknown _ | Call (Function _)) ->
      None
  ;;

  let verify_annotations t output_index item =
    Map.fold t.annotations ~init:(Ok ()) ~f:(fun ~key ~data:expected result ->
      let%bind.Result () = result in
      if not (Int.equal key.output_index output_index)
      then Ok ()
      else (
        match snapshot_part item Event.Part_space.Content key.content_index with
        | Some part ->
          (match Part.view part with
           | Output_text { annotations; _ } ->
             (match List.nth annotations key.annotation_index with
              | Some _ when Jsonaf.exactly_equal expected `Null -> Ok ()
              | Some actual when equal_json expected actual -> Ok ()
              | Some _ | None -> Result.Error (Part_conflict output_index))
           | Refusal _ | Summary_text _ | Reasoning_text _ | Unknown _ ->
             Result.Error (Part_conflict output_index))
        | None -> Result.Error (Part_conflict output_index)))
  ;;

  let verify_item_parts t output_index item =
    let%bind.Result () = verify_annotations t output_index item in
    let%bind.Result () =
      Map.fold t.observed_text ~init:(Ok ()) ~f:(fun ~key ~data:() result ->
        let%bind.Result () = result in
        if not (Int.equal key.output_index output_index)
        then Ok ()
        else (
          match snapshot_text item key with
          | None -> Result.Error (Part_conflict output_index)
          | Some text ->
            (match Map.find t.text_finals key with
             | Some expected when not (String.equal expected text) ->
               Result.Error (Part_conflict output_index)
             | Some _ | None -> Ok ())))
    in
    Map.fold t.observed_parts ~init:(Ok ()) ~f:(fun ~key ~data:expected_type result ->
      let%bind.Result () = result in
      if not (Int.equal key.output_index output_index)
      then Ok ()
      else (
        match snapshot_part item key.space key.part_index with
        | None -> Result.Error (Part_conflict output_index)
        | Some part when not (String.equal expected_type (part_type part)) ->
          Result.Error (Part_conflict output_index)
        | Some part ->
          (match Map.find t.part_finals key with
           | Some expected when not (equal_json (Part.raw expected) (Part.raw part)) ->
             Result.Error (Part_conflict output_index)
           | Some _ | None -> Ok ())))
  ;;

  let finalize ?(emit = true) t output_index item =
    let%bind.Result slot = reconcile_id t output_index (item_id item) in
    let%bind.Result () =
      match slot.added with
      | Some previous when not (same_descriptor previous item) ->
        Result.Error (Item_conflict output_index)
      | Some _ | None -> Ok ()
    in
    let%bind.Result () = verify_item_parts t output_index item in
    match slot.final with
    | Some previous when same_item previous item -> Ok (t, [], Duplicate)
    | Some _ -> Result.Error (Item_conflict output_index)
    | None ->
      let t =
        { t with
          slots = Map.set t.slots ~key:output_index ~data:{ slot with final = Some item }
        }
      in
      let newly_finalized =
        match emit, Item.local_call item with
        | true, Some _ -> [ output_index, item ]
        | false, _ | true, None -> []
      in
      Ok (t, newly_finalized, Accepted)
  ;;

  let with_response_id t response =
    match t.response_id with
    | Some expected when not (String.equal expected (Response.id response)) ->
      Result.Error Response_conflict
    | Some _ | None -> Ok { t with response_id = Some (Response.id response) }
  ;;

  let terminal_matches terminal response =
    match terminal, Response.status response with
    | _, Presence.Absent -> true
    | Event.Completed, Value Completed
    | Incomplete, Value Incomplete
    | Failed, Value Failed -> true
    | (Completed | Incomplete | Failed), (Null | Value _) -> false
  ;;

  let check_unfinalized t (location : Event.location) =
    match Map.find t.slots location.output_index with
    | Some { final = Some _; _ } -> Result.Error (Part_conflict location.output_index)
    | Some _ | None -> Ok ()
  ;;

  let apply_part t location space part ~done_ =
    let%bind.Result t = correlate t location in
    let%bind.Result () = check_unfinalized t location in
    let key = part_key location space in
    let%bind.Result () =
      match Map.find t.observed_parts key with
      | Some expected when not (String.equal expected (part_type part)) ->
        Result.Error (Part_conflict location.output_index)
      | Some _ | None -> Ok ()
    in
    let%bind.Result t =
      match part_text part with
      | None -> Ok t
      | Some (kind, text) ->
        let%bind.Result () = validate_kind t location kind in
        let key = text_key location kind in
        if done_
        then (
          match Map.find t.text_finals key with
          | Some expected when not (String.equal expected text) ->
            Result.Error (Part_conflict location.output_index)
          | Some _ | None ->
            Ok
              { t with
                observed_text = Map.set t.observed_text ~key ~data:()
              ; text_finals = Map.set t.text_finals ~key ~data:text
              })
        else Ok { t with observed_text = Map.set t.observed_text ~key ~data:() }
    in
    let t =
      { t with observed_parts = Map.set t.observed_parts ~key ~data:(part_type part) }
    in
    match Map.find t.part_finals key with
    | Some previous when done_ && equal_json (Part.raw previous) (Part.raw part) ->
      Ok (t, [], Duplicate)
    | Some _ -> Result.Error (Part_conflict location.output_index)
    | None ->
      Ok
        ( (if done_
           then { t with part_finals = Map.set t.part_finals ~key ~data:part }
           else t)
        , []
        , Accepted )
  ;;

  let apply_text t location kind final_text =
    let%bind.Result t = correlate t location in
    let%bind.Result () = validate_kind t location kind in
    let key = text_key location kind in
    match final_text, Map.find t.text_finals key with
    | Some text, Some expected when String.equal text expected -> Ok (t, [], Duplicate)
    | _, Some _ -> Result.Error (Part_conflict location.output_index)
    | _, None ->
      let%bind.Result () = check_unfinalized t location in
      let t = { t with observed_text = Map.set t.observed_text ~key ~data:() } in
      let t =
        match final_text with
        | Some text -> { t with text_finals = Map.set t.text_finals ~key ~data:text }
        | None -> t
      in
      Ok (t, [], Accepted)
  ;;

  let apply t event =
    match Event.view event with
    | Event.Item_added { output_index; item } ->
      let%bind.Result slot = reconcile_id t output_index (item_id item) in
      (match slot.added, slot.final with
       | Some previous, _ when same_item previous item -> Ok (t, [], Duplicate)
       | Some _, _ | None, Some _ -> Result.Error (Item_conflict output_index)
       | None, None ->
         Ok
           ( { t with
               slots =
                 Map.set t.slots ~key:output_index ~data:{ slot with added = Some item }
             }
           , []
           , Accepted ))
    | Item_done { output_index; item } -> finalize t output_index item
    | Response { response; _ } ->
      let%map.Result t = with_response_id t response in
      t, [], Accepted
    | Part_added { location; part_space; part } ->
      apply_part t location part_space part ~done_:false
    | Part_done { location; part_space; part } ->
      apply_part t location part_space part ~done_:true
    | Delta { location; kind; _ } -> apply_text t location kind None
    | Text_done { location; kind; text } -> apply_text t location kind (Some text)
    | Annotation_added { location; annotation_index; annotation } ->
      let%bind.Result t = correlate t location in
      let%bind.Result () = check_unfinalized t location in
      let%bind.Result () = validate_kind t location Text in
      let content_index =
        match location.part_index with
        | Some index -> index
        | None -> assert false
      in
      let key =
        { Annotation_key.output_index = location.output_index
        ; content_index
        ; annotation_index
        }
      in
      (match Map.find t.annotations key with
       | Some previous when equal_json previous annotation -> Ok (t, [], Duplicate)
       | Some _ -> Result.Error (Part_conflict location.output_index)
       | None ->
         Ok
           ( { t with
               annotations = Map.set t.annotations ~key ~data:annotation
             ; observed_text =
                 Map.set t.observed_text ~key:(text_key location Text) ~data:()
             }
           , []
           , Accepted ))
    | Terminal { terminal; response } ->
      if not (terminal_matches terminal response)
      then Result.Error Terminal_mismatch
      else (
        let%bind.Result t = with_response_id t response in
        let output = Response.output response in
        let output_count = List.length output in
        let%bind.Result () =
          if
            Map.existsi t.slots ~f:(fun ~key ~data ->
              key >= output_count
              && (Event.equal_terminal terminal Completed || Option.is_some data.final))
          then Result.Error Response_conflict
          else Ok ()
        in
        let%map.Result t, reversed_new =
          List.foldi
            output
            ~init:(Ok (t, []))
            ~f:(fun index result item ->
              let%bind.Result t, reversed_new = result in
              let%map.Result t, new_items, _ =
                finalize ~emit:(Event.equal_terminal terminal Completed) t index item
              in
              t, List.rev_append new_items reversed_new)
        in
        ( { t with terminal = Some (Response { terminal; response }, Event.raw event) }
        , List.rev reversed_new
        , Accepted ))
    | Error error ->
      Ok ({ t with terminal = Some (Error error, Event.raw event) }, [], Accepted)
    | Unknown _ -> Ok (t, [], Accepted)
  ;;

  let add t event =
    if not (Origin.equal t.origin (Event.origin event))
    then Result.Error Origin_mismatch
    else (
      let%bind.Result sequence_state =
        match t.last_sequence, Event.sequence_number event with
        | Some (previous, raw), Presence.Value received when Int64.equal previous received
          ->
          if equal_json raw (Event.raw event)
          then Ok `Duplicate
          else Result.Error (Sequence_conflict received)
        | Some (previous, _), Value received when Int64.(received < previous) ->
          Result.Error (Sequence_regression { previous; received })
        | _, Value received -> Ok (`Next (Some (received, Event.raw event)))
        | _, (Absent | Null) -> Ok (`Next t.last_sequence)
      in
      match sequence_state with
      | `Duplicate -> Ok { tracker = t; disposition = Duplicate; newly_finalized = [] }
      | `Next last_sequence ->
        (match t.terminal with
         | Some (_, raw) when equal_json raw (Event.raw event) ->
           Ok
             { tracker = { t with last_sequence }
             ; disposition = Duplicate
             ; newly_finalized = []
             }
         | Some _ -> Result.Error Event_after_terminal
         | None ->
           let%map.Result tracker, newly_finalized, disposition = apply t event in
           { tracker = { tracker with last_sequence }; disposition; newly_finalized }))
  ;;

  let finish t =
    match t.terminal with
    | Some (completion, _) -> Ok completion
    | None -> Result.Error Truncated
  ;;
end
