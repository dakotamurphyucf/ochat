open! Core

module Error = struct
  type t =
    | Invalid_configuration of string
    | Limit_exceeded of string
    | Malformed of string
    | Duplicate_key of string list
    | Unsupported_beta_format
    | Unsupported_format of string
    | Invalid_field of
        { path : string list
        ; reason : string
        }
    | Unsupported_kind of string
    | Unsupported_version of
        { kind : string
        ; version : int
        ; target : int
        }
    | Missing_conversion of
        { kind : string
        ; version : int
        }
    | Wrong_kind of
        { expected : string
        ; actual : string
        }
    | Wrong_version of
        { expected : int
        ; actual : int
        }
    | Required_semantics_unknown of string
    | Extension_conflict of string list
  [@@deriving sexp, equal]
end

let invalid path reason = Error (Error.Invalid_field { path; reason })

module Limits = struct
  type t =
    { max_bytes : int
    ; max_depth : int
    ; max_fields : int
    ; max_nodes : int
    }

  let create ~max_bytes ~max_depth ~max_fields ~max_nodes =
    if
      max_bytes <= 0
      || max_depth <= 0
      || max_depth > 256
      || max_fields <= 0
      || max_nodes <= 0
    then
      Error (Error.Invalid_configuration "positive limits required; depth must be <= 256")
    else Ok { max_bytes; max_depth; max_fields; max_nodes }
  ;;

  let default =
    { max_bytes = 16 * 1024 * 1024
    ; max_depth = 128
    ; max_fields = 100_000
    ; max_nodes = 1_000_000
    }
  ;;

  let max_bytes t = t.max_bytes
end

module Json = struct
  type t = Jsonaf.t

  type presence =
    | Absent
    | Null
    | Value of t

  let field t ~name =
    match t with
    | `Object fields ->
      (match List.Assoc.find fields name ~equal:String.equal with
       | None -> Absent
       | Some `Null -> Null
       | Some value -> Value value)
    | `Null | `True | `False | `String _ | `Number _ | `Array _ -> Absent
  ;;

  (* Iterative lexical guard bounds parser stack use before allocating the tree.
     Full syntax validation is still Jsonaf's responsibility. *)
  let guard_text limits text =
    if String.length text > limits.Limits.max_bytes
    then Error (Error.Limit_exceeded "bytes")
    else (
      let depth = ref 0 in
      let in_string = ref false in
      let escaped = ref false in
      let too_deep = ref false in
      String.iter text ~f:(fun ch ->
        if !in_string
        then (
          if !escaped
          then escaped := false
          else if Char.equal ch '\\'
          then escaped := true
          else if Char.equal ch '"'
          then in_string := false)
        else if Char.equal ch '"'
        then in_string := true
        else if Char.equal ch '{' || Char.equal ch '['
        then (
          incr depth;
          if !depth > limits.max_depth then too_deep := true)
        else if Char.equal ch '}' || Char.equal ch ']'
        then decr depth);
      if !too_deep then Error (Error.Limit_exceeded "depth") else Ok ())
  ;;

  (* Jsonaf accepts some non-JSON number spellings (for example 01). Validate
     the JSON numeric grammar directly, including constructed in-memory values. *)
  let valid_number number =
    let length = String.length number in
    let index = ref 0 in
    let consume_if ~f =
      if !index < length && f number.[!index]
      then (
        incr index;
        true)
      else false
    in
    let consume_digits () =
      let start = !index in
      while !index < length && Char.is_digit number.[!index] do
        incr index
      done;
      !index > start
    in
    ignore (consume_if ~f:(Char.equal '-'));
    let integer = if consume_if ~f:(Char.equal '0') then true else consume_digits () in
    let fraction = if consume_if ~f:(Char.equal '.') then consume_digits () else true in
    let exponent =
      if consume_if ~f:(fun ch -> Char.equal ch 'e' || Char.equal ch 'E')
      then (
        ignore (consume_if ~f:(fun ch -> Char.equal ch '+' || Char.equal ch '-'));
        consume_digits ())
      else true
    in
    integer && fraction && exponent && Int.equal !index length
  ;;

  let validate_and_measure ~limits t =
    let fields_seen = ref 0 in
    let nodes_seen = ref 0 in
    let bytes_seen = ref 0 in
    let charge_bytes count =
      if count > limits.Limits.max_bytes - !bytes_seen
      then Error (Error.Limit_exceeded "bytes")
      else (
        bytes_seen := !bytes_seen + count;
        Ok ())
    in
    (* Match Jsonaf's compact string serializer without allocating escaped text.
       Check before every addition so even rejected authored values stay bounded. *)
    let charge_string text =
      let available = limits.Limits.max_bytes - !bytes_seen in
      let encoded_bytes = ref 2 in
      let index = ref 0 in
      let exceeds_limit = ref (available < 2) in
      let seen_bits = ref 0 in
      while !index < String.length text && not !exceeds_limit do
        let byte = text.[!index] in
        seen_bits := !seen_bits lor Char.to_int byte;
        let width =
          match byte with
          | '"' | '\\' | '\b' | '\012' | '\n' | '\r' | '\t' -> 2
          | '\000' .. '\031' -> 6
          | _ -> 1
        in
        if width > available - !encoded_bytes
        then exceeds_limit := true
        else encoded_bytes := !encoded_bytes + width;
        incr index
      done;
      if !exceeds_limit
      then Error (Error.Limit_exceeded "bytes")
      else
        let open Result.Let_syntax in
        let%map () = charge_bytes !encoded_bytes in
        (* A successful charge visited every byte. If none had its high bit set,
           the same bounded scan also proved that the string is valid UTF-8. *)
        !seen_bits land 0x80 = 0
    in
    let open Result.Let_syntax in
    let rec walk t depth rev_path =
      incr nodes_seen;
      if depth > limits.Limits.max_depth
      then Error (Error.Limit_exceeded "depth")
      else if !nodes_seen > limits.max_nodes
      then Error (Error.Limit_exceeded "nodes")
      else (
        match t with
        | `Object fields ->
          let%bind () = charge_bytes 2 in
          List.fold_result fields ~init:String.Set.empty ~f:(fun seen (key, value) ->
            incr fields_seen;
            let%bind () = charge_bytes (if Set.is_empty seen then 1 else 2) in
            let%bind is_ascii = charge_string key in
            let%bind () =
              if is_ascii || String.Utf8.is_valid key
              then Ok ()
              else invalid (List.rev (key :: rev_path)) "invalid UTF-8 object key"
            in
            if !fields_seen > limits.max_fields
            then Error (Error.Limit_exceeded "fields")
            else if Set.mem seen key
            then Error (Error.Duplicate_key (List.rev (key :: rev_path)))
            else (
              let%map () = walk value (depth + 1) (key :: rev_path) in
              Set.add seen key))
          |> Result.map ~f:(fun _ -> ())
        | `Array values ->
          let%bind () = charge_bytes 2 in
          List.fold_result values ~init:0 ~f:(fun index value ->
            let%bind () = if index = 0 then Ok () else charge_bytes 1 in
            let%map () = walk value (depth + 1) (Int.to_string index :: rev_path) in
            index + 1)
          |> Result.map ~f:(fun _ -> ())
        | `Number number ->
          let%bind () = charge_bytes (String.length number) in
          if valid_number number
          then Ok ()
          else invalid (List.rev rev_path) "invalid JSON number"
        | `String text ->
          let%bind is_ascii = charge_string text in
          if is_ascii || String.Utf8.is_valid text
          then Ok ()
          else invalid (List.rev rev_path) "invalid UTF-8 string"
        | `True | `Null -> charge_bytes 4
        | `False -> charge_bytes 5)
    in
    let%map () = walk t 1 [] in
    !bytes_seen
  ;;

  let validate ~limits t = validate_and_measure ~limits t |> Result.map ~f:ignore

  let rec equal left right =
    match left, right with
    | `Null, `Null | `True, `True | `False, `False -> true
    | `String left, `String right | `Number left, `Number right -> String.equal left right
    | `Array left, `Array right -> List.equal equal left right
    | `Object left, `Object right ->
      (match String.Map.of_alist left, String.Map.of_alist right with
       | `Ok left, `Ok right -> Map.equal equal left right
       | `Duplicate_key _, _ | _, `Duplicate_key _ -> false)
    | _ -> false
  ;;

  let decode ~limits text =
    let open Result.Let_syntax in
    let%bind () = guard_text limits text in
    match Jsonaf.parse text with
    | Error message -> Error (Error.Malformed (Core.Error.to_string_hum message))
    | Ok json ->
      let%map () = validate ~limits json in
      json
  ;;
end

module Document = struct
  type t =
    { json : Json.t
    ; kind : string
    ; version : int
    ; payload : Json.t
    ; required_semantics : string list
    ; admitted_limits : Limits.t
    }

  let format = "ochat.document"
  let kind t = t.kind
  let version t = t.version
  let payload t = t.payload
  let required_semantics t = t.required_semantics
  let json t = t.json
  let to_string t = Jsonaf.to_string t.json

  let validate t ~limits =
    let admitted = t.admitted_limits in
    if
      limits.Limits.max_bytes >= admitted.max_bytes
      && limits.max_depth >= admitted.max_depth
      && limits.max_fields >= admitted.max_fields
      && limits.max_nodes >= admitted.max_nodes
    then Ok ()
    else Json.validate ~limits t.json
  ;;

  let string_field json name =
    match Json.field json ~name with
    | Value (`String value) when not (String.is_empty value) -> Ok value
    | Absent | Null | Value _ -> invalid [ name ] "required nonempty string"
  ;;

  (* Callers must establish full JSON admission under these exact limits. Fresh
     input uses Json.validate; scalar edits may preserve an admitted tree's
     structural totals and prove nonincreasing compact bytes after independently
     validating each replacement. No caller-supplied certificate is accepted. *)
  let inspect_validated ~limits json =
    let open Result.Let_syntax in
    let%bind () =
      match json with
      | `Object _ -> Ok ()
      | _ -> invalid [] "expected envelope object"
    in
    let%bind marker = string_field json "format" in
    let%bind () =
      if String.equal marker format
      then Ok ()
      else Error (Error.Unsupported_format marker)
    in
    let%bind kind = string_field json "kind" in
    let%bind version =
      match Json.field json ~name:"schema_version" with
      | Value (`Number value) ->
        (match Int.of_string_opt value with
         | Some version when version > 0 && String.equal value (Int.to_string version) ->
           Ok version
         | Some _ | None ->
           invalid [ "schema_version" ] "positive canonical integer required")
      | Absent | Null | Value _ ->
        invalid [ "schema_version" ] "positive integer required"
    in
    let%bind payload =
      match Json.field json ~name:"payload" with
      | Value (`Object _ as payload) -> Ok payload
      | Absent | Null | Value _ -> invalid [ "payload" ] "required object"
    in
    let%bind () =
      match Json.field json ~name:"extensions" with
      | Absent | Value (`Object _) -> Ok ()
      | Null | Value _ -> invalid [ "extensions" ] "optional non-null object"
    in
    let%map required_semantics =
      match Json.field json ~name:"required_semantics" with
      | Absent -> Ok []
      | Value (`Array values) ->
        List.fold_result
          values
          ~init:([], String.Set.empty)
          ~f:(fun (names, seen) value ->
            match value with
            | `String name when (not (String.is_empty name)) && not (Set.mem seen name) ->
              Ok (name :: names, Set.add seen name)
            | _ -> invalid [ "required_semantics" ] "unique nonempty strings required")
        |> Result.map ~f:(fun (names, _) -> List.rev names)
      | Null | Value _ -> invalid [ "required_semantics" ] "expected array"
    in
    { json; kind; version; payload; required_semantics; admitted_limits = limits }
  ;;

  let inspect ~limits json =
    let%bind.Result () = Json.validate ~limits json in
    inspect_validated ~limits json
  ;;

  let decode ~limits bytes =
    if String.length bytes > Limits.max_bytes limits
    then Error (Error.Limit_exceeded "bytes")
    else (
      let text = String.lstrip bytes in
      if String.is_empty text
      then Error (Error.Malformed "empty document")
      else (
        match String.get text 0 with
        | '{' | '[' | '"' | 'n' | 't' | 'f' | '-' | '0' .. '9' ->
          Result.bind (Json.decode ~limits bytes) ~f:(inspect_validated ~limits)
        | _ -> Error Error.Unsupported_beta_format))
  ;;

  let create ~limits ~kind ~version ~payload =
    inspect
      ~limits
      (`Object
          [ "format", `String format
          ; "schema_version", `Number (Int.to_string version)
          ; "kind", `String kind
          ; "payload", payload
          ])
  ;;

  let replace_payload t ~limits ~version ~payload =
    let json =
      match t.json with
      | `Object fields ->
        `Object
          (List.map fields ~f:(fun (name, value) ->
             if String.equal name "payload"
             then name, payload
             else if String.equal name "schema_version"
             then name, `Number (Int.to_string version)
             else name, value))
      | _ -> assert false
    in
    inspect ~limits json
  ;;

  let replace_payload_scalars t ~limits ~updates =
    let open Result.Let_syntax in
    let%bind () = validate t ~limits in
    let scalar = function
      | `Null | `True | `False | `String _ | `Number _ -> true
      | `Object _ | `Array _ -> false
    in
    let%bind payload, grew =
      List.fold_result
        updates
        ~init:(t.payload, false)
        ~f:(fun (payload, grew) (path, value) ->
          let invalid reason = invalid ("payload" :: path) reason in
          let%bind () =
            if List.is_empty path
            then invalid "scalar path must be nonempty"
            else if not (scalar value)
            then invalid "replacement must be a scalar"
            else Ok ()
          in
          let%bind () = Json.validate ~limits value in
          let rec replace json = function
            | [] -> invalid "scalar path must be nonempty"
            | name :: rest ->
              (match json with
               | `Object fields ->
                 let%bind previous =
                   List.Assoc.find fields name ~equal:String.equal
                   |> Result.of_option
                        ~error:
                          (Error.Invalid_field
                             { path = "payload" :: path
                             ; reason = "scalar field is absent"
                             })
                 in
                 let%bind value, leaf_grew =
                   match rest with
                   | [] ->
                     if not (scalar previous)
                     then invalid "existing field must be a scalar"
                     else
                       Ok
                         ( value
                         , String.length (Jsonaf.to_string value)
                           > String.length (Jsonaf.to_string previous) )
                   | _ -> replace previous rest
                 in
                 let fields =
                   List.map fields ~f:(fun (key, previous) ->
                     key, if String.equal key name then value else previous)
                 in
                 Ok (`Object fields, leaf_grew)
               | _ -> invalid "scalar path requires existing object members")
          in
          let%map payload, leaf_grew = replace payload path in
          payload, grew || leaf_grew)
    in
    let json =
      match t.json with
      | `Object fields ->
        `Object
          (List.map fields ~f:(fun (name, value) ->
             name, if String.equal name "payload" then payload else value))
      | _ -> assert false
    in
    (* Scalar leaves retain every key, node and depth. If no compact scalar grew,
       the original complete byte bound still holds; otherwise inspect the full
       result. Either inspector refreshes payload and envelope metadata together. *)
    if grew then inspect ~limits json else inspect_validated ~limits json
  ;;
end

module Conversion = struct
  module Operation = struct
    type t =
      | Rename of
          { parent : string list
          ; src : string
          ; dst : string
          }
      | Move of
          { src : string list
          ; dst : string list
          }
      | Default of
          { path : string list
          ; value : Json.t
          }
      | Remove of string list
  end

  module Step = struct
    type program =
      | Operations of Operation.t list
      | Function of (Json.t -> (Json.t, Error.t) Result.t)

    type t =
      { kind : string
      ; from_version : int
      ; program : program
      }

    let create ~kind ~from_version ~operations =
      if String.is_empty kind || from_version <= 0 || from_version = Int.max_value
      then
        Error
          (Error.Invalid_configuration
             "step requires kind and positive incrementable version")
      else Ok { kind; from_version; program = Operations operations }
    ;;

    let of_function ~kind ~from_version ~f =
      Result.map (create ~kind ~from_version ~operations:[]) ~f:(fun step ->
        { step with program = Function f })
    ;;

    let operation_count t =
      match t.program with
      | Operations operations -> List.length operations
      | Function _ -> 1
    ;;
  end

  type t =
    { limits : Limits.t
    ; targets : int String.Map.t
    ; max_steps : int
    ; steps : Step.t list
    }

  let create ~limits ~targets ~max_steps ~max_operations ~steps =
    let open Result.Let_syntax in
    if max_steps <= 0 || max_operations <= 0
    then Error (Error.Invalid_configuration "positive conversion bounds required")
    else (
      let%bind targets =
        List.fold_result targets ~init:String.Map.empty ~f:(fun targets (kind, version) ->
          if String.is_empty kind || version <= 0 || Map.mem targets kind
          then
            Error
              (Error.Invalid_configuration
                 "targets require unique kinds and positive versions")
          else Ok (Map.set targets ~key:kind ~data:version))
      in
      let%bind () =
        List.fold_result steps ~init:[] ~f:(fun seen step ->
          match Map.find targets step.Step.kind with
          | None -> Error (Error.Invalid_configuration "step kind has no target")
          | Some target ->
            if
              step.from_version >= target
              || List.exists seen ~f:(fun (kind, version) ->
                String.equal kind step.kind && Int.equal version step.from_version)
              || Step.operation_count step > max_operations
            then
              Error
                (Error.Invalid_configuration
                   "duplicate/out-of-target step or operation bound exceeded")
            else Ok ((step.kind, step.from_version) :: seen))
        |> Result.map ~f:(fun _ -> ())
      in
      let%map () =
        List.fold_result steps ~init:() ~f:(fun () step ->
          let target = Map.find_exn targets step.Step.kind in
          if
            step.from_version + 1 < target
            && not
                 (List.exists steps ~f:(fun next ->
                    String.equal next.Step.kind step.kind
                    && Int.equal next.from_version (step.from_version + 1)))
          then Error (Error.Invalid_configuration "conversion chain has a gap")
          else Ok ())
      in
      { limits; targets; max_steps; steps })
  ;;

  let rec lookup json path =
    match path with
    | [] -> Ok (Some json)
    | key :: rest ->
      (match json with
       | `Object fields ->
         (match List.Assoc.find fields key ~equal:String.equal with
          | None -> Ok None
          | Some value -> lookup value rest)
       | _ -> invalid path "path traverses non-object")
  ;;

  let rec change json path ~f =
    match path, json with
    | [], _ -> invalid [] "operation requires a nonempty field path"
    | key :: rest, `Object fields ->
      let old = List.Assoc.find fields key ~equal:String.equal in
      let open Result.Let_syntax in
      let%map replacement =
        match rest with
        | [] -> f old
        | _ ->
          (match old with
           | None -> invalid path "missing parent object"
           | Some value -> Result.map (change value rest ~f) ~f:Option.some)
      in
      let exists = Option.is_some old in
      let updated =
        List.filter_map fields ~f:(fun (name, value) ->
          if String.equal name key
          then Option.map replacement ~f:(fun value -> name, value)
          else Some (name, value))
      in
      if exists
      then `Object updated
      else
        `Object
          (updated @ Option.to_list (Option.map replacement ~f:(fun value -> key, value)))
    | _ -> invalid path "path traverses non-object"
  ;;

  let move payload ~src ~dst =
    let open Result.Let_syntax in
    let is_prefix prefix path = List.is_prefix path ~prefix ~equal:String.equal in
    let%bind () =
      if List.is_empty src || List.is_empty dst || is_prefix src dst || is_prefix dst src
      then invalid src "move paths must be nonempty and disjoint"
      else Ok ()
    in
    let%bind value = lookup payload src in
    match value with
    | None -> invalid src "required source is absent"
    | Some value ->
      let%bind payload =
        change payload dst ~f:(function
          | None -> Ok (Some value)
          | Some _ -> Error (Error.Extension_conflict dst))
      in
      change payload src ~f:(fun _ -> Ok None)
  ;;

  let apply payload = function
    | Operation.Rename { parent; src; dst } ->
      move payload ~src:(parent @ [ src ]) ~dst:(parent @ [ dst ])
    | Move { src; dst } -> move payload ~src ~dst
    | Default { path; value } ->
      change payload path ~f:(function
        | None -> Ok (Some value)
        | Some existing -> Ok (Some existing))
    | Remove path -> change payload path ~f:(fun _ -> Ok None)
  ;;

  let upgrade t document =
    let open Result.Let_syntax in
    let%bind () = Json.validate ~limits:t.limits (Document.json document) in
    let kind = Document.kind document in
    match Map.find t.targets kind with
    | None -> Error (Error.Unsupported_kind kind)
    | Some target ->
      let version = Document.version document in
      if version > target
      then Error (Error.Unsupported_version { kind; version; target })
      else if target - version > t.max_steps
      then Error (Error.Limit_exceeded "conversion steps")
      else (
        let rec loop document =
          let version = Document.version document in
          if version = target
          then Ok document
          else (
            match
              List.find t.steps ~f:(fun step ->
                String.equal step.Step.kind kind && Int.equal step.from_version version)
            with
            | None -> Error (Error.Missing_conversion { kind; version })
            | Some step ->
              let%bind payload =
                match step.program with
                | Operations operations ->
                  List.fold_result
                    operations
                    ~init:(Document.payload document)
                    ~f:(fun payload operation ->
                      let%bind payload = apply payload operation in
                      let%map () = Json.validate ~limits:t.limits payload in
                      payload)
                | Function f ->
                  let%bind payload = f (Document.payload document) in
                  let%map () = Json.validate ~limits:t.limits payload in
                  payload
              in
              let%bind document =
                Document.replace_payload
                  document
                  ~limits:t.limits
                  ~version:(version + 1)
                  ~payload
              in
              loop document)
        in
        loop document)
  ;;
end

module Shape = struct
  type t =
    | Value
    | Object of t String.Map.t
    | Array of
        { element : t
        ; identity_field : string option
        ; allow_empty_identity : bool
        }
    | Nullable of t
    | Tagged_object of
        { discriminator : string
        ; cases : t String.Map.t
        }

  let value = Value
  let nullable t = Nullable t

  let object_ fields =
    match List.find_a_dup (List.map fields ~f:fst) ~compare:String.compare with
    | Some name -> Error (Error.Invalid_configuration ("duplicate owned field: " ^ name))
    | None -> Ok (Object (String.Map.of_alist_exn fields))
  ;;

  let owns_identity shape key =
    match shape with
    | Object fields ->
      (match Map.find fields key with
       | Some Value -> true
       | _ -> false)
    | Value | Array _ | Nullable _ | Tagged_object _ -> false
  ;;

  let tagged_object ~discriminator cases =
    if
      String.is_empty discriminator
      || List.is_empty cases
      || List.exists cases ~f:(fun (tag, shape) ->
        String.is_empty tag || not (owns_identity shape discriminator))
      || Option.is_some (List.find_a_dup (List.map cases ~f:fst) ~compare:String.compare)
    then
      Error
        (Error.Invalid_configuration
           "tagged object requires unique nonempty tags and object cases owning the \
            discriminator")
    else Ok (Tagged_object { discriminator; cases = String.Map.of_alist_exn cases })
  ;;

  let array ?(allow_empty_identity = false) element ~identity_field =
    match identity_field, element with
    | None, _ -> Ok (Array { element; identity_field; allow_empty_identity })
    | Some key, Object fields ->
      (match Map.find fields key with
       | Some Value -> Ok (Array { element; identity_field; allow_empty_identity })
       | Some (Object _ | Array _ | Nullable _ | Tagged_object _) | None ->
         Error (Error.Invalid_configuration "array identity must be an owned value field"))
    | Some key, Tagged_object { discriminator = _; cases } ->
      if Map.for_all cases ~f:(fun shape -> owns_identity shape key)
      then Ok (Array { element; identity_field; allow_empty_identity })
      else
        Error
          (Error.Invalid_configuration
             "every tagged array case must own the identity field")
    | Some _, (Value | Array _ | Nullable _) ->
      Error (Error.Invalid_configuration "identity array requires object elements")
  ;;
end

(* Retained trees record unknown ownership at decode time, so using a newer
   codec on an old carrier cannot silently promote a formerly unknown field. *)
type retained =
  | Empty
  | Tagged of
      { discriminator : string
      ; tag : string
      ; child : retained
      }
  | Object of (string * retained_field) list
  | Array of
      { original_known : Json.t
      ; identity_field : string option
      ; allow_empty_identity : bool
      ; entries : (string option * retained) list
      }

and retained_field =
  | Unknown of Json.t
  | Known of retained

module Extension_carrier = struct
  type 'a t =
    { value : 'a
    ; template : Document.t option
    ; retained : retained
    }

  let of_authored_value value = { value; template = None; retained = Empty }
  let with_value t value = { t with value }
  let value t = t.value
  let template t = t.template
end

module Domain_codec = struct
  type 'a encoding_validation =
    | Roundtrip
    | Original of ('a -> (unit, Error.t) Result.t)

  type 'a t =
    { limits : Limits.t
    ; kind : string
    ; version : int
    ; shape : Shape.t
    ; supported_semantics : String.Set.t
    ; decode_value : Json.t -> ('a, Error.t) Result.t
    ; encode_value : 'a -> (Json.t, Error.t) Result.t
    ; encoding_validation : 'a encoding_validation
    }

  let create_internal
        ~encoding_validation
        ~limits
        ~kind
        ~version
        ~shape
        ~supported_semantics
        ~decode
        ~encode
    =
    if
      String.is_empty kind
      || version <= 0
      || List.exists supported_semantics ~f:String.is_empty
      || Option.is_some (List.find_a_dup supported_semantics ~compare:String.compare)
    then
      Error
        (Error.Invalid_configuration
           "codec requires kind, positive version and unique semantics")
    else
      Ok
        { limits
        ; kind
        ; version
        ; shape
        ; supported_semantics = String.Set.of_list supported_semantics
        ; decode_value = decode
        ; encode_value = encode
        ; encoding_validation
        }
  ;;

  let create ~limits ~kind ~version ~shape ~supported_semantics ~decode ~encode =
    create_internal
      ~encoding_validation:Roundtrip
      ~limits
      ~kind
      ~version
      ~shape
      ~supported_semantics
      ~decode
      ~encode
  ;;

  let create_validated
        ~limits
        ~kind
        ~version
        ~shape
        ~supported_semantics
        ~validate
        ~decode
        ~encode
    =
    create_internal
      ~encoding_validation:(Original validate)
      ~limits
      ~kind
      ~version
      ~shape
      ~supported_semantics
      ~decode
      ~encode
  ;;

  let check t document =
    if not (String.equal t.kind (Document.kind document))
    then Error (Error.Wrong_kind { expected = t.kind; actual = Document.kind document })
    else if t.version <> Document.version document
    then
      Error
        (Error.Wrong_version { expected = t.version; actual = Document.version document })
    else (
      match
        List.find (Document.required_semantics document) ~f:(fun name ->
          not (Set.mem t.supported_semantics name))
      with
      | Some name -> Error (Error.Required_semantics_unknown name)
      | None -> Document.validate document ~limits:t.limits)
  ;;

  let identity ?(allow_empty = false) json key path =
    match Json.field json ~name:key with
    | Value (`String value) when allow_empty || not (String.is_empty value) -> Ok value
    | Absent | Null | Value _ ->
      invalid (path @ [ key ]) "required nonempty array identity string"
  ;;

  let rec split shape json path =
    let open Result.Let_syntax in
    match shape, json with
    | Shape.Value, _ -> Ok (json, Empty)
    | Nullable _, `Null -> Ok (`Null, Empty)
    | Nullable shape, _ -> split shape json path
    | Tagged_object { discriminator; cases }, _ ->
      let%bind tag = identity json discriminator path in
      let%bind selected =
        Map.find cases tag
        |> Result.of_option
             ~error:
               (Error.Invalid_field
                  { path = path @ [ discriminator ]
                  ; reason = "unsupported discriminator"
                  })
      in
      let%map known, child = split selected json path in
      known, Tagged { discriminator; tag; child }
    | Object owned, `Object fields ->
      let%map known, retained =
        List.fold_result fields ~init:([], []) ~f:(fun (known, retained) (key, value) ->
          match Map.find owned key with
          | None -> Ok (known, (key, Unknown value) :: retained)
          | Some shape ->
            let%map value, child = split shape value (path @ [ key ]) in
            (key, value) :: known, (key, Known child) :: retained)
      in
      `Object (List.rev known), Object (List.rev retained)
    | Array { element; identity_field; allow_empty_identity }, `Array values ->
      let%map known, entries, _, _ =
        List.fold_result
          values
          ~init:([], [], String.Set.empty, 0)
          ~f:(fun (known, entries, seen, index) value ->
            let%bind key =
              match identity_field with
              | None -> Ok None
              | Some key ->
                Result.map
                  (identity ~allow_empty:allow_empty_identity value key path)
                  ~f:Option.some
            in
            let%bind () =
              match key with
              | Some key when Set.mem seen key -> invalid path "duplicate array identity"
              | Some _ | None -> Ok ()
            in
            let%map known_value, retained =
              split
                element
                value
                (path @ [ Option.value key ~default:(Int.to_string index) ])
            in
            ( known_value :: known
            , (key, retained) :: entries
            , Option.value_map key ~default:seen ~f:(Set.add seen)
            , index + 1 ))
      in
      let known = `Array (List.rev known) in
      ( known
      , Array
          { original_known = known
          ; identity_field
          ; allow_empty_identity
          ; entries = List.rev entries
          } )
    | Object _, (`Null | `String _ | `Number _ | `True | `False | `Array _)
    | Array _, (`Null | `String _ | `Number _ | `True | `False | `Object _) ->
      invalid path "value does not match codec shape"
  ;;

  let rec has_unknown = function
    | Empty -> false
    | Tagged { discriminator = _; tag = _; child } -> has_unknown child
    | Object fields ->
      List.exists fields ~f:(function
        | _, Unknown _ -> true
        | _, Known tree -> has_unknown tree)
    | Array { original_known = _; identity_field = _; allow_empty_identity = _; entries }
      -> List.exists entries ~f:(fun (_, tree) -> has_unknown tree)
  ;;

  let decode t document =
    let open Result.Let_syntax in
    let%bind () = check t document in
    let%bind known, retained = split t.shape (Document.payload document) [ "payload" ] in
    let%map value = t.decode_value known in
    { Extension_carrier.value; template = Some document; retained }
  ;;

  let rec merge (shape : Shape.t) (retained : retained) (known : Json.t) path =
    let open Result.Let_syntax in
    match retained, shape, known with
    | Empty, _, _ -> Ok known
    | Tagged previous, Shape.Tagged_object { discriminator; cases }, _ ->
      let%bind tag = identity known discriminator path in
      let%bind selected =
        Map.find cases tag
        |> Result.of_option
             ~error:
               (Error.Invalid_field
                  { path = path @ [ discriminator ]
                  ; reason = "unsupported discriminator"
                  })
      in
      if
        String.equal previous.discriminator discriminator && String.equal previous.tag tag
      then merge selected previous.child known path
      else if has_unknown previous.child
      then Error (Error.Extension_conflict path)
      else Ok known
    | _, Shape.Nullable _, `Null ->
      if has_unknown retained then Error (Error.Extension_conflict path) else Ok known
    | _, Shape.Nullable shape, _ -> merge shape retained known path
    | ( Array
          { original_known = _
          ; identity_field = previous_identity
          ; allow_empty_identity = previous_allow_empty
          ; entries = _
          }
      , Shape.Array { element = _; identity_field; allow_empty_identity }
      , _ )
      when has_unknown retained
           && ((not (Option.equal String.equal previous_identity identity_field))
               || not (Bool.equal previous_allow_empty allow_empty_identity)) ->
      Error (Error.Extension_conflict path)
    | Object previous, Shape.Object owned, `Object fields ->
      (* Both lists originate in duplicate-checked JSON trees. Indexing keeps
         large named-field objects bounded by O(n log n), including templates. *)
      let previous_by_name = String.Map.of_alist_exn previous in
      let fields_by_name = String.Map.of_alist_exn fields in
      let%bind () =
        List.fold_result previous ~init:() ~f:(fun () (name, field) ->
          match field with
          | Unknown _ when Map.mem owned name || Map.mem fields_by_name name ->
            Error (Error.Extension_conflict (path @ [ name ]))
          | Unknown _ -> Ok ()
          | Known tree ->
            if has_unknown tree && not (Map.mem fields_by_name name)
            then Error (Error.Extension_conflict (path @ [ name ]))
            else Ok ())
      in
      let%bind fields =
        List.fold_result fields ~init:[] ~f:(fun result (name, value) ->
          let%map value =
            match Map.find previous_by_name name, Map.find owned name with
            | Some (Known tree), Some shape -> merge shape tree value (path @ [ name ])
            | Some (Known tree), None when has_unknown tree ->
              Error (Error.Extension_conflict (path @ [ name ]))
            | Some (Unknown _), _ -> Error (Error.Extension_conflict (path @ [ name ]))
            | Some (Known _), None | None, _ -> Ok value
          in
          (name, value) :: result)
      in
      let fields = List.rev fields in
      let fields_by_name = String.Map.of_alist_exn fields in
      (* Preserve original field ordering, including unknown members. *)
      let original =
        List.filter_map previous ~f:(fun (name, field) ->
          match field with
          | Unknown value -> Some (name, value)
          | Known _ ->
            Option.map (Map.find fields_by_name name) ~f:(fun value -> name, value))
      in
      Ok
        (`Object
            (original
             @ List.filter fields ~f:(fun (name, _) ->
               not (Map.mem previous_by_name name))))
    | ( Array { original_known; identity_field = _; allow_empty_identity = _; entries }
      , Shape.Array { element; identity_field = None; allow_empty_identity = _ }
      , `Array values ) ->
      if has_unknown retained && not (Json.equal original_known known)
      then Error (Error.Extension_conflict path)
      else if not (has_unknown retained)
      then Ok known
      else
        List.map2 entries values ~f:(fun (_, retained) value ->
          merge element retained value path)
        |> (function
         | List.Or_unequal_lengths.Unequal_lengths ->
           Error (Error.Extension_conflict path)
         | Ok results -> Result.map (Result.all results) ~f:(fun values -> `Array values))
    | ( Array { original_known = _; identity_field = _; allow_empty_identity = _; entries }
      , Shape.Array { element; identity_field = Some key; allow_empty_identity }
      , `Array values ) ->
      let%bind identities =
        Result.all
          (List.map values ~f:(fun value ->
             identity ~allow_empty:allow_empty_identity value key path))
      in
      let identities = String.Set.of_list identities in
      let by_identity =
        List.fold entries ~init:String.Map.empty ~f:(fun result (id, tree) ->
          match id with
          | None -> result
          | Some id -> Map.set result ~key:id ~data:tree)
      in
      let%bind () =
        List.fold_result entries ~init:() ~f:(fun () (old_key, tree) ->
          match old_key with
          | Some old_key when has_unknown tree && not (Set.mem identities old_key) ->
            Error (Error.Extension_conflict (path @ [ old_key ]))
          | Some _ | None -> Ok ())
      in
      let%map values =
        Result.all
          (List.map values ~f:(fun value ->
             let%bind id = identity ~allow_empty:allow_empty_identity value key path in
             match Map.find by_identity id with
             | None -> Ok value
             | Some retained -> merge element retained value (path @ [ id ])))
      in
      `Array values
    | (Object _ | Array _ | Tagged _), _, _ ->
      if has_unknown retained then Error (Error.Extension_conflict path) else Ok known
  ;;

  let encode t carrier =
    let open Result.Let_syntax in
    let%bind () =
      match carrier.Extension_carrier.template with
      | None -> Ok ()
      | Some document -> check t document
    in
    let%bind () =
      match t.encoding_validation with
      | Roundtrip -> Ok ()
      | Original validate -> validate carrier.value
    in
    let%bind known = t.encode_value carrier.value in
    let%bind () = Json.validate ~limits:t.limits known in
    let%bind projection, unexpected = split t.shape known [ "payload" ] in
    let%bind () =
      if has_unknown unexpected
      then Error (Error.Extension_conflict [ "payload" ])
      else Ok ()
    in
    let%bind () =
      match t.encoding_validation with
      | Roundtrip -> t.decode_value projection |> Result.map ~f:(fun _ -> ())
      | Original _ -> Ok ()
    in
    let%bind payload = merge t.shape carrier.retained known [ "payload" ] in
    match carrier.template with
    | None -> Document.create ~limits:t.limits ~kind:t.kind ~version:t.version ~payload
    | Some template ->
      Document.replace_payload template ~limits:t.limits ~version:t.version ~payload
  ;;

  let combine_fields previous incoming ~f =
    let open Result.Let_syntax in
    let previous_by_name = String.Map.of_alist_exn previous in
    let incoming_by_name = String.Map.of_alist_exn incoming in
    let%map fields =
      List.map previous ~f:(fun (name, value) ->
        match Map.find incoming_by_name name with
        | None -> Ok (name, value)
        | Some incoming ->
          Result.map (f name value incoming) ~f:(fun value -> name, value))
      |> Result.all
    in
    fields
    @ List.filter incoming ~f:(fun (name, _) -> not (Map.mem previous_by_name name))
  ;;

  let same_unknown previous incoming path =
    if Json.equal previous incoming
    then Ok previous
    else Error (Error.Extension_conflict path)
  ;;

  let rec combine (shape : Shape.t) (previous : Json.t) (incoming : Json.t) path =
    let open Result.Let_syntax in
    match shape, previous, incoming with
    | Value, _, _ -> same_unknown previous incoming path
    | Nullable _, `Null, `Null -> Ok `Null
    | Nullable shape, _, _ -> combine shape previous incoming path
    | Tagged_object { discriminator; cases }, _, _ ->
      let%bind tag = identity incoming discriminator path in
      let%bind selected =
        Map.find cases tag
        |> Result.of_option ~error:(Error.Extension_conflict (path @ [ discriminator ]))
      in
      combine selected previous incoming path
    | Object owned, `Object previous, `Object incoming ->
      let%map fields =
        combine_fields previous incoming ~f:(fun name previous incoming ->
          match Map.find owned name with
          | None -> same_unknown previous incoming (path @ [ name ])
          | Some shape -> combine shape previous incoming (path @ [ name ]))
      in
      `Object fields
    | ( Array { element; identity_field; allow_empty_identity }
      , `Array previous
      , `Array incoming ) ->
      (match
         List.map2 previous incoming ~f:(fun previous incoming ->
           let%bind path =
             match identity_field with
             | None -> Ok path
             | Some key ->
               let%map id =
                 identity ~allow_empty:allow_empty_identity incoming key path
               in
               path @ [ id ]
           in
           combine element previous incoming path)
       with
       | Unequal_lengths -> Error (Error.Extension_conflict path)
       | Ok results -> Result.map (Result.all results) ~f:(fun values -> `Array values))
    | (Object _ | Array _), _, _ -> Error (Error.Extension_conflict path)
  ;;

  let adopt t ~previous ~incoming =
    let open Result.Let_syntax in
    let%bind previous =
      encode t (Extension_carrier.with_value previous incoming.Extension_carrier.value)
    in
    let%bind incoming = encode t incoming in
    let%bind previous_known, _ =
      split t.shape (Document.payload previous) [ "payload" ]
    in
    let%bind incoming_known, _ =
      split t.shape (Document.payload incoming) [ "payload" ]
    in
    let%bind () =
      if Json.equal previous_known incoming_known
      then Ok ()
      else Error (Error.Extension_conflict [ "payload" ])
    in
    let fields document =
      match Document.json document with
      | `Object fields -> fields
      | _ -> assert false (* Document admission guarantees an object envelope. *)
    in
    let%bind fields =
      combine_fields (fields previous) (fields incoming) ~f:(fun name previous incoming ->
        if String.equal name "payload"
        then combine t.shape previous incoming [ name ]
        else same_unknown previous incoming [ name ])
    in
    let%bind document = Document.inspect ~limits:t.limits (`Object fields) in
    decode t document
  ;;
end
