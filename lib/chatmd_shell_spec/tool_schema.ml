open Core

let dialect = "ochat.tool-schema.v1"

type diagnostic =
  { code : string
  ; path : string list
  ; message : string
  }
[@@deriving sexp]

exception Invalid of diagnostic

let fail ?(code = "schema.invalid") path message =
  raise_notrace (Invalid { code; path; message })
;;

let protect f =
  try Ok (f ()) with
  | Invalid diagnostic -> Error [ diagnostic ]
;;

let max_bytes = 1024 * 1024
let max_depth = 128
let max_nodes = 100_000
let max_steps = 1_000_000
let limit path message = fail ~code:"schema.resource_limit" path message

let charge remaining path count =
  remaining := !remaining - count;
  if !remaining < 0 then limit path "schema work limit exceeded"
;;

let tick remaining path = charge remaining path 1

(* Normalized decimal significand times 10^exponent. Comparing virtual trailing
   zeros avoids both floating-point rounding and huge exponent allocations. *)
type decimal =
  { negative : bool
  ; digits : string
  ; exponent : int
  }

let decimal path text =
  let valid =
    let length = String.length text in
    let index = ref 0 in
    let at char = !index < length && Char.equal text.[!index] char in
    let digits () =
      let first = !index in
      while !index < length && Char.is_digit text.[!index] do
        Int.incr index
      done;
      !index > first
    in
    if at '-' then Int.incr index;
    let whole =
      if at '0'
      then (
        Int.incr index;
        true)
      else if !index < length && Char.(text.[!index] >= '1' && text.[!index] <= '9')
      then digits ()
      else false
    in
    let fraction =
      if at '.'
      then (
        Int.incr index;
        digits ())
      else true
    in
    let power =
      if at 'e' || at 'E'
      then (
        Int.incr index;
        if at '+' || at '-' then Int.incr index;
        digits ())
      else true
    in
    whole && fraction && power && !index = length
  in
  if not valid then fail path "invalid JSON number";
  let negative = String.is_prefix text ~prefix:"-" in
  let unsigned = if negative then String.drop_prefix text 1 else text in
  let mantissa, power =
    match String.lsplit2 (String.lowercase unsigned) ~on:'e' with
    | None -> unsigned, 0
    | Some (mantissa, power) ->
      (match Int.of_string_opt power with
       | Some power when power >= -1_000_000 && power <= 1_000_000 -> mantissa, power
       | _ -> limit path "numeric exponent exceeds supported bound")
  in
  let whole, fraction =
    match String.lsplit2 mantissa ~on:'.' with
    | None -> mantissa, ""
    | Some parts -> parts
  in
  let digits = String.lstrip (whole ^ fraction) ~drop:(Char.equal '0') in
  if String.is_empty digits
  then { negative = false; digits = "0"; exponent = 0 }
  else (
    let trimmed = String.rstrip digits ~drop:(Char.equal '0') in
    { negative
    ; digits = trimmed
    ; exponent =
        power - String.length fraction + String.length digits - String.length trimmed
    })
;;

let compare_decimal a b =
  let zero value = String.equal value.digits "0" in
  let magnitude a b =
    let order =
      Int.compare
        (String.length a.digits + a.exponent)
        (String.length b.digits + b.exponent)
    in
    if order <> 0
    then order
    else (
      let digit value i =
        if i < String.length value.digits then value.digits.[i] else '0'
      in
      let rec loop i =
        if i = Int.max (String.length a.digits) (String.length b.digits)
        then 0
        else (
          let order = Char.compare (digit a i) (digit b i) in
          if order = 0 then loop (i + 1) else order)
      in
      loop 0)
  in
  if zero a && zero b
  then 0
  else if not (Bool.equal a.negative b.negative)
  then if a.negative then -1 else 1
  else (
    let order = if zero a then -1 else if zero b then 1 else magnitude a b in
    if a.negative then -order else order)
;;

let scalar_length path value =
  Uutf.String.fold_utf_8
    (fun count _ -> function
       | `Uchar _ -> count + 1
       | `Malformed _ -> fail path "string is not valid UTF-8")
    0
    value
;;

let bounded_json json =
  let bytes = ref max_bytes in
  let nodes = ref max_nodes in
  let charge path n =
    bytes := !bytes - n;
    if !bytes < 0 then limit path "JSON byte limit exceeded"
  in
  let rec walk depth path = function
    | _ when depth > max_depth -> limit path "JSON nesting limit exceeded"
    | json ->
      tick nodes path;
      charge path 1;
      (match json with
       | `String text ->
         charge path (String.length text + 2);
         ignore (scalar_length path text : int)
       | `Number text ->
         charge path (String.length text);
         ignore (decimal path text : decimal)
       | `Array values ->
         List.iteri values ~f:(fun i value ->
           walk (depth + 1) (path @ [ Int.to_string i ]) value)
       | `Object fields ->
         let names = Hash_set.create (module String) in
         List.iter fields ~f:(fun (name, value) ->
           charge path (String.length name + 3);
           ignore (scalar_length path name : int);
           if Hash_set.mem names name
           then fail (path @ [ name ]) "duplicate JSON property";
           Hash_set.add names name;
           walk (depth + 1) (path @ [ name ]) value)
       | `True | `False | `Null -> ())
  in
  walk 1 [] json;
  if String.length (Jsonaf.to_string json) > max_bytes
  then limit [] "encoded JSON byte limit exceeded"
;;

type value_type =
  | Null
  | Boolean
  | Object
  | Array
  | Number
  | Integer
  | String

type node =
  | Accept
  | Reject
  | Rules of rules

and rules =
  { types : value_type list option
  ; properties : node String.Map.t
  ; required : string list
  ; additional : node
  ; items : node
  ; enum : Jsonaf.t list option
  ; const : Jsonaf.t option
  ; any_of : node list option
  ; min_items : int option
  ; max_items : int option
  ; min_length : int option
  ; max_length : int option
  ; minimum : decimal option
  ; maximum : decimal option
  }

type t =
  { schema : Jsonaf.t
  ; node : node
  }

let to_json t = t.schema
let field fields name = List.Assoc.find fields name ~equal:String.equal

let type_name path = function
  | `String "null" -> Null
  | `String "boolean" -> Boolean
  | `String "object" -> Object
  | `String "array" -> Array
  | `String "number" -> Number
  | `String "integer" -> Integer
  | `String "string" -> String
  | _ -> fail path "unknown JSON-schema type"
;;

let natural path = function
  | `Number text ->
    let number = decimal path text in
    if number.negative || number.exponent < 0
    then fail path "expected nonnegative integer";
    if
      number.exponent > String.length (Int.to_string Int.max_value)
      || String.length number.digits + number.exponent
         > String.length (Int.to_string Int.max_value)
    then limit path "integer schema bound exceeds host range";
    (match Int.of_string_opt (number.digits ^ String.make number.exponent '0') with
     | Some value -> value
     | None -> limit path "integer schema bound exceeds host range")
  | _ -> fail path "expected nonnegative integer"
;;

let rec json_equal steps path a b =
  tick steps path;
  match a, b with
  | `Number a, `Number b ->
    charge steps path (String.length a + String.length b);
    compare_decimal (decimal path a) (decimal path b) = 0
  | `Null, `Null | `True, `True | `False, `False -> true
  | `String a, `String b ->
    charge steps path (String.length a + String.length b);
    String.equal a b
  | `Array a, `Array b -> List.equal (json_equal steps path) a b
  | `Object a, `Object b ->
    charge steps path (List.length a + List.length b);
    let sort fields =
      List.sort fields ~compare:(fun (a, _) (b, _) -> String.compare a b)
    in
    List.equal
      (fun (ka, va) (kb, vb) ->
         String.equal ka kb && json_equal steps (path @ [ ka ]) va vb)
      (sort a)
      (sort b)
  | _ -> false
;;

let compile schema =
  protect (fun () ->
    bounded_json schema;
    let steps = ref max_steps in
    let rec node path = function
      | `True -> Accept
      | `False -> Reject
      | `Object fields ->
        let allowed =
          [ "type"
          ; "properties"
          ; "required"
          ; "additionalProperties"
          ; "items"
          ; "enum"
          ; "const"
          ; "anyOf"
          ; "minItems"
          ; "maxItems"
          ; "minLength"
          ; "maxLength"
          ; "minimum"
          ; "maximum"
          ; "title"
          ; "description"
          ; "$comment"
          ]
        in
        List.iter fields ~f:(fun (name, value) ->
          tick steps path;
          if not (List.mem allowed name ~equal:String.equal)
          then fail (path @ [ name ]) "unsupported JSON-schema keyword";
          if List.mem [ "title"; "description"; "$comment" ] name ~equal:String.equal
          then (
            match value with
            | `String _ -> ()
            | _ -> fail (path @ [ name ]) "schema metadata must be a string"));
        let optional name decode =
          Option.map (field fields name) ~f:(decode (path @ [ name ]))
        in
        let types =
          optional "type" (fun path -> function
            | `Array [] -> fail path "type union must not be empty"
            | `Array values ->
              let values = List.map values ~f:(type_name path) in
              if Option.is_some (List.find_a_dup values ~compare:Poly.compare)
              then fail path "duplicate type in union";
              values
            | value -> [ type_name path value ])
        in
        let properties =
          match field fields "properties" with
          | None -> String.Map.empty
          | Some (`Object fields) ->
            String.Map.of_alist_exn
              (List.map fields ~f:(fun (key, value) ->
                 key, node (path @ [ "properties"; key ]) value))
          | Some _ -> fail (path @ [ "properties" ]) "properties must be an object"
        in
        let required =
          match field fields "required" with
          | None -> []
          | Some (`Array values) ->
            let values =
              List.map values ~f:(function
                | `String value -> value
                | _ -> fail (path @ [ "required" ]) "required names must be strings")
            in
            if Option.is_some (List.find_a_dup values ~compare:String.compare)
            then fail (path @ [ "required" ]) "duplicate required property";
            values
          | Some _ -> fail (path @ [ "required" ]) "required must be an array"
        in
        let default_node name = Option.value (optional name node) ~default:Accept in
        let enum =
          optional "enum" (fun path -> function
            | `Array (_ :: _ as values) ->
              let rec unique = function
                | [] -> ()
                | value :: rest ->
                  if List.exists rest ~f:(json_equal steps path value)
                  then fail path "duplicate enum value";
                  unique rest
              in
              unique values;
              values
            | _ -> fail path "enum must be a nonempty array")
        in
        let any_of =
          optional "anyOf" (fun path -> function
            | `Array (_ :: _ as values) ->
              List.mapi values ~f:(fun i value -> node (path @ [ Int.to_string i ]) value)
            | _ -> fail path "anyOf must be a nonempty schema array")
        in
        let number path = function
          | `Number text -> decimal path text
          | _ -> fail path "expected numeric bound"
        in
        Rules
          { types
          ; properties
          ; required
          ; additional = default_node "additionalProperties"
          ; items = default_node "items"
          ; enum
          ; const = field fields "const"
          ; any_of
          ; min_items = optional "minItems" natural
          ; max_items = optional "maxItems" natural
          ; min_length = optional "minLength" natural
          ; max_length = optional "maxLength" natural
          ; minimum = optional "minimum" number
          ; maximum = optional "maximum" number
          }
      | _ -> fail path "schema must be a boolean or object"
    in
    { schema; node = node [] schema })
;;

let parse_json text =
  protect (fun () ->
    if String.length text > max_bytes then limit [] "schema source byte limit exceeded";
    let depth = ref 0
    and quoted = ref false
    and escaped = ref false in
    String.iter text ~f:(fun char ->
      if !quoted
      then (
        if !escaped
        then escaped := false
        else if Char.equal char '\\'
        then escaped := true
        else if Char.equal char '"'
        then quoted := false)
      else if Char.equal char '"'
      then quoted := true
      else if Char.equal char '{' || Char.equal char '['
      then (
        Int.incr depth;
        if !depth > max_depth then limit [] "schema source nesting limit exceeded")
      else if Char.equal char '}' || Char.equal char ']'
      then Int.decr depth);
    try Jsonaf.of_string text with
    | _ -> fail [] "invalid schema JSON")
;;

let of_string text =
  match parse_json text with
  | Error _ as error -> error
  | Ok schema -> compile schema
;;

let validate t value =
  protect (fun () ->
    bounded_json value;
    let steps = ref max_steps in
    let mismatch path message = fail ~code:"schema.mismatch" path message in
    let matches path kind value =
      match kind, value with
      | Null, `Null
      | Boolean, (`True | `False)
      | Object, `Object _
      | Array, `Array _
      | Number, `Number _
      | String, `String _ -> true
      | Integer, `Number text ->
        charge steps path (String.length text);
        (decimal path text).exponent >= 0
      | _ -> false
    in
    let rec check path schema value =
      tick steps path;
      match schema with
      | Accept -> ()
      | Reject -> mismatch path "boolean schema rejects this value"
      | Rules rules ->
        Option.iter rules.types ~f:(fun types ->
          if not (List.exists types ~f:(fun kind -> matches path kind value))
          then mismatch path "value has the wrong type");
        Option.iter rules.enum ~f:(fun values ->
          if not (List.exists values ~f:(json_equal steps path value))
          then mismatch path "value is not in enum");
        Option.iter rules.const ~f:(fun expected ->
          if not (json_equal steps path expected value)
          then mismatch path "value differs from const");
        Option.iter rules.any_of ~f:(fun alternatives ->
          if
            not
              (List.exists alternatives ~f:(fun schema ->
                 try
                   check path schema value;
                   true
                 with
                 | Invalid diagnostic when String.equal diagnostic.code "schema.mismatch"
                   -> false))
          then mismatch path "no anyOf branch matches");
        let bounds lower upper size label =
          Option.iter lower ~f:(fun bound ->
            if size < bound then mismatch path (label ^ " below minimum"));
          Option.iter upper ~f:(fun bound ->
            if size > bound then mismatch path (label ^ " above maximum"))
        in
        (match value with
         | `Object fields ->
           charge steps path (List.length fields + List.length rules.required);
           let values = String.Map.of_alist_exn fields in
           List.iter rules.required ~f:(fun name ->
             if not (Map.mem values name)
             then mismatch (path @ [ name ]) "required property is missing");
           List.iter fields ~f:(fun (name, value) ->
             check
               (path @ [ name ])
               (Option.value (Map.find rules.properties name) ~default:rules.additional)
               value)
         | `Array values ->
           bounds rules.min_items rules.max_items (List.length values) "array length";
           List.iteri values ~f:(fun i value ->
             check (path @ [ Int.to_string i ]) rules.items value)
         | `String text ->
           charge steps path (String.length text);
           bounds
             rules.min_length
             rules.max_length
             (scalar_length path text)
             "string length"
         | `Number text ->
           charge steps path (String.length text);
           let number = decimal path text in
           Option.iter rules.minimum ~f:(fun bound ->
             if compare_decimal number bound < 0 then mismatch path "number below minimum");
           Option.iter rules.maximum ~f:(fun bound ->
             if compare_decimal number bound > 0 then mismatch path "number above maximum")
         | `Null | `True | `False -> ())
    in
    check [] t.node value)
;;
