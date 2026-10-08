open! Core
module P = History_entry.Payload

let semantic entry = P.semantic (History_entry.payload entry)
let view entry = P.Semantic.view (semantic entry)

let is_policy entry =
  match view entry with
  | Message { form = Input; role = System | Developer; _ } -> true
  | Message _ | Call _ | Result _ | Reasoning _ | Unknown _ -> false
;;

let is_reminder entry =
  match view entry with
  | Message { form = Input; role = User; content = Text { text; _ } :: _; _ } ->
    String.is_prefix (String.strip text) ~prefix:"<system-reminder>"
  | Message _ | Call _ | Result _ | Reasoning _ | Unknown _ -> false
;;

let is_shared entry = is_policy entry || is_reminder entry

let message ~role text =
  P.Semantic.create
    (Message
       { form = Input
       ; role
       ; content = [ Text { text; annotations = []; logprobs = Absent } ]
       ; phase = Absent
       })
    ~metadata:P.Metadata.empty
  |> Result.ok_or_failwith
  |> P.authored
;;

let content = function
  | P.Content.Text { text; _ } -> text
  | Refusal text -> "<refusal>" ^ text ^ "</refusal>"
  | Image { uri; _ } -> Printf.sprintf "<image src=%S />" uri
  | Unknown { kind; _ } -> Printf.sprintf "<unknown-content kind=%S />" kind
;;

let parts values = List.map values ~f:content |> String.concat ~sep:"\n"

let role = function
  | P.Role.System -> "system"
  | Developer -> "developer"
  | User -> "user"
  | Assistant -> "Assistant"
  | Tool -> "tool"
;;

let alias metadata =
  match metadata.P.Metadata.call_id with
  | Value id -> id
  | Absent | Null -> "unavailable"
;;

let family = function
  | P.Call_kind.Function -> "Function"
  | Custom -> "Custom tool"
;;

let render entries =
  List.filter_map entries ~f:(fun entry ->
    let semantic = semantic entry in
    let metadata = P.Semantic.metadata semantic in
    match P.Semantic.view semantic with
    | Message { role = owner; content; _ } ->
      if List.is_empty content
      then None
      else Some (sprintf "%s: %s" (role owner) (parts content))
    | Call { kind; name; input_bytes; _ } ->
      Some
        (sprintf "%s call (%s): %s(%s)" (family kind) (alias metadata) name input_bytes)
    | Result { kind; output; _ } ->
      let text =
        match output with
        | P.Output.Text text -> text
        | Content values -> parts values
      in
      Some (sprintf "%s call output (%s): %s" (family kind) (alias metadata) text)
    | Reasoning { readable_summary } ->
      if List.is_empty readable_summary
      then None
      else Some ("Reasoning summary: " ^ String.concat ~sep:"\n" readable_summary)
    | Unknown { provider_kind } -> Some (sprintf "<unknown-item kind=%S />" provider_kind))
  |> String.concat ~sep:"\n"
;;

let grouped entries =
  let entries = Array.of_list entries in
  let count = Array.length entries in
  let ending = Array.init count ~f:Fn.id in
  let calls = ref String.Map.empty in
  let aliases = ref String.Map.empty in
  let alias_key kind metadata =
    match metadata.P.Metadata.call_id with
    | Absent | Null -> None
    | Value id -> Some (family kind ^ ":" ^ id)
  in
  Array.iteri entries ~f:(fun index entry ->
    let semantic = semantic entry in
    let metadata = P.Semantic.metadata semantic in
    match P.Semantic.view semantic with
    | Call { kind; _ } ->
      ending.(index) <- count - 1;
      calls
      := Map.set
           !calls
           ~key:(History_entry.Id.to_string (History_entry.id entry))
           ~data:(index, kind);
      Option.iter (alias_key kind metadata) ~f:(fun key ->
        aliases := Map.set !aliases ~key ~data:index)
    | Result { relation; kind; _ } ->
      let call =
        match relation with
        | Bound id ->
          Map.find !calls (History_entry.Id.to_string id)
          |> Option.bind ~f:(fun (index, owner) ->
            if P.Call_kind.equal kind owner then Some index else None)
        | Unresolved -> Option.bind (alias_key kind metadata) ~f:(Map.find !aliases)
      in
      Option.iter call ~f:(fun call ->
        (* More than one output for the same call extends its contiguous group. *)
        if ending.(call) = count - 1
        then ending.(call) <- index
        else ending.(call) <- Int.max ending.(call) index)
    | Message _ | Reasoning _ | Unknown _ -> ());
  let rec consume index last acc =
    if index >= count || index > last
    then List.rev acc, index
    else consume (index + 1) (Int.max last ending.(index)) (entries.(index) :: acc)
  in
  let rec loop index acc =
    if index >= count
    then List.rev acc
    else (
      let group, next = consume index index [] in
      loop next (group :: acc))
  in
  loop 0 []
;;
