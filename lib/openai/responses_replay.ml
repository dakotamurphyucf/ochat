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

type t =
  { transitions : (string * string * Item_class.t list) list
  ; compatible_profiles : String.Set.t option
  }

let exact_origin_only = { transitions = []; compatible_profiles = None }

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
  else Ok { transitions; compatible_profiles = None }
;;

let with_compatible_profiles t ~canonical_profile ~profiles =
  let valid_id id =
    (not (String.is_empty (String.strip id)))
    && String.length id <= 256
    && (not (String.exists id ~f:(fun c -> Char.to_int c < 32 || Char.to_int c = 127)))
    && Result.is_ok
         (Document_schema.Json.validate
            (`String id)
            ~limits:Document_schema.Limits.default)
  in
  if
    List.is_empty profiles
    || List.length profiles > 128
    || (not (List.for_all profiles ~f:valid_id))
    || List.contains_dup profiles ~compare:String.compare
    || not (List.mem profiles canonical_profile ~equal:String.equal)
  then Or_error.error_string "invalid compatible replay profile group"
  else Ok { t with compatible_profiles = Some (String.Set.of_list profiles) }
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

let compatible_profile_context t ~actual ~expected =
  match t.compatible_profiles, Origin.profile actual, Origin.profile expected with
  | Some profiles, Some source, Some destination ->
    Set.mem profiles source
    && Set.mem profiles destination
    && Option.equal String.equal (Origin.provider actual) (Some source)
    && Option.equal String.equal (Origin.provider expected) (Some destination)
    && Origin.same_replay_transport_context actual expected
    &&
      (match Origin.model actual, Origin.model expected with
      | Some source, Some destination -> String.equal source destination
      | Some _, None | None, Some _ | None, None -> false)
  | None, _, _ | Some _, None, _ | Some _, Some _, None -> false
;;

let permits t ~actual ~expected ~raw =
  if Origin.same_replay_context actual expected
  then
    if Option.equal String.equal (Origin.model actual) (Origin.model expected)
    then true
    else (
      match Origin.model actual, Origin.model expected, classify raw with
      | Some source, Some destination, Some item_class ->
        List.exists t.transitions ~f:(fun (from, to_, classes) ->
          String.equal source from
          && String.equal destination to_
          && List.mem classes item_class ~equal:Item_class.equal)
      | _ -> false)
  else if compatible_profile_context t ~actual ~expected
  then (
    match classify raw with
    | Some (Assistant_text | Function_call | Custom_call) -> true
    | Some Reasoning | None -> false)
  else false
;;
