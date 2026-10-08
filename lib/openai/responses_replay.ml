open! Core
module Origin = History_entry.Payload.Origin

module Item_class = struct
  type t =
    | Assistant_text
    | Function_call
    | Custom_call
    | Reasoning
  [@@deriving equal, compare, sexp_of]
end

type t = (string * string * Item_class.t list) list

let exact_origin_only = []

let create ~transitions =
  let valid_name name =
    (not (String.is_empty (String.strip name))) && String.length name <= 512
  in
  if
    List.length transitions > 256
    || List.exists transitions ~f:(fun (source, destination, classes) ->
      (not (valid_name source && valid_name destination))
      || String.equal source destination
      || List.is_empty classes
      || List.contains_dup classes ~compare:Item_class.compare)
    || List.contains_dup
         (List.map transitions ~f:(fun (source, destination, _) -> source, destination))
         ~compare:(Tuple2.compare ~cmp1:String.compare ~cmp2:String.compare)
  then Or_error.error_string "invalid directed replay declarations"
  else Ok transitions
;;

let object_fields raw ~allowed =
  match raw with
  | `Object fields ->
    if
      List.exists fields ~f:(fun (key, _) ->
        not (List.mem allowed key ~equal:String.equal))
      || List.contains_dup (List.map fields ~f:fst) ~compare:String.compare
    then None
    else Some fields
  | _ -> None
;;

let member fields key = List.Assoc.find fields key ~equal:String.equal

let text = function
  | `String _ -> true
  | _ -> false
;;

let optional fields key check =
  match member fields key with
  | None | Some `Null -> true
  | Some value -> check value
;;

let required fields key check = Option.exists (member fields key) ~f:check

let equals expected = function
  | `String actual -> String.equal expected actual
  | _ -> false
;;

let empty_array = function
  | `Array [] -> true
  | _ -> false
;;

let strings choices = function
  | `String value -> List.mem choices value ~equal:String.equal
  | _ -> false
;;

let array check = function
  | `Array values -> List.for_all values ~f:check
  | _ -> false
;;

let summary raw =
  match object_fields raw ~allowed:[ "type"; "text" ] with
  | None -> false
  | Some fields ->
    required fields "type" (equals "summary_text") && required fields "text" text
;;

let output_text raw =
  match object_fields raw ~allowed:[ "type"; "text"; "annotations"; "logprobs" ] with
  | None -> false
  | Some fields ->
    required fields "type" (equals "output_text")
    && required fields "text" text
    && optional fields "annotations" empty_array
    && optional fields "logprobs" empty_array
;;

let classify raw =
  let classify ~allowed ~kind ~check (item_class : Item_class.t) =
    match object_fields raw ~allowed with
    | Some fields
      when required fields "type" (equals kind)
           && optional fields "id" text
           && optional fields "status" (strings [ "completed" ])
           && check fields -> Some item_class
    | Some _ | None -> None
  in
  match raw with
  | `Object fields ->
    (match member fields "type" with
     | Some (`String "message") ->
       classify
         ~allowed:[ "type"; "id"; "status"; "role"; "content"; "phase" ]
         ~kind:"message"
         ~check:(fun fields ->
           required fields "role" (equals "assistant")
           && required fields "content" (array output_text)
           && optional fields "phase" (strings [ "commentary"; "final_answer" ]))
         Assistant_text
     | Some (`String "function_call") ->
       classify
         ~allowed:[ "type"; "id"; "status"; "name"; "arguments"; "call_id" ]
         ~kind:"function_call"
         ~check:(fun fields ->
           List.for_all [ "name"; "arguments"; "call_id" ] ~f:(fun key ->
             required fields key text))
         Function_call
     | Some (`String "custom_tool_call") ->
       classify
         ~allowed:[ "type"; "id"; "status"; "name"; "input"; "call_id" ]
         ~kind:"custom_tool_call"
         ~check:(fun fields ->
           List.for_all [ "name"; "input"; "call_id" ] ~f:(fun key ->
             required fields key text))
         Custom_call
     | Some (`String "reasoning") ->
       classify
         ~allowed:[ "type"; "id"; "status"; "summary"; "encrypted_content" ]
         ~kind:"reasoning"
         ~check:(fun fields ->
           required fields "summary" (array summary)
           && optional fields "encrypted_content" text)
         Reasoning
     | _ -> None)
  | _ -> None
;;

let permits t ~actual ~expected ~raw =
  if not (Origin.same_replay_context actual expected)
  then false
  else if Option.equal String.equal (Origin.model actual) (Origin.model expected)
  then true
  else (
    match Origin.model actual, Origin.model expected, classify raw with
    | Some source, Some destination, Some item_class ->
      List.exists t ~f:(fun (from, to_, classes) ->
        String.equal source from
        && String.equal destination to_
        && List.mem classes item_class ~equal:Item_class.equal)
    | _ -> false)
;;
