open Core
module Captured = Chatml_source
module Prepared = Chatml_prepared_program
module Build = Chatml_project_build
module Contract = Chatml.Chatml_compiler_contract
module Schema = Chatmd_shell_spec.Tool_schema
module Context = Chatml_authoring_context

let within_add ~limit left right =
  left >= 0 && right >= 0 && left <= limit && right <= limit - left
;;

exception Json_limit

module Json_bounds = struct
  type t =
    { max_bytes : int
    ; max_depth : int
    ; max_nodes : int
    }

  let create ~max_bytes ~max_depth ~max_nodes =
    if
      max_bytes <= 0
      || max_bytes > 1_048_576
      || max_depth <= 0
      || max_depth > 128
      || max_nodes <= 0
      || max_nodes > 100_000
    then Or_error.error_string "invalid developer JSON bounds"
    else Ok { max_bytes; max_depth; max_nodes }
  ;;

  let valid_number value ~checkpoint =
    let length = String.length value in
    let index = ref 0 in
    let poll () = if !index mod 4096 = 0 then checkpoint () in
    let char () = if !index < length then Some value.[!index] else None in
    let advance () =
      poll ();
      incr index
    in
    let digit = function
      | Some '0' .. '9' -> true
      | _ -> false
    in
    let nonzero = function
      | Some '1' .. '9' -> true
      | _ -> false
    in
    let digits () =
      let start = !index in
      while digit (char ()) do
        advance ()
      done;
      !index > start
    in
    if Char.equal (Option.value (char ()) ~default:'\000') '-' then advance ();
    let integer =
      match char () with
      | Some '0' ->
        advance ();
        not (digit (char ()))
      | value when nonzero value -> digits ()
      | _ -> false
    in
    if not integer
    then false
    else (
      let fraction =
        match char () with
        | Some '.' ->
          advance ();
          digits ()
        | _ -> true
      in
      let exponent =
        match char () with
        | Some ('e' | 'E') ->
          advance ();
          (match char () with
           | Some ('+' | '-') -> advance ()
           | _ -> ());
          digits ()
        | _ -> true
      in
      fraction && exponent && !index = length)
  ;;

  let nonzero_mantissa value =
    let rec loop index =
      if index = String.length value
      then false
      else (
        match value.[index] with
        | 'e' | 'E' -> false
        | '1' .. '9' -> true
        | _ -> loop (index + 1))
    in
    loop 0
  ;;

  let measure bounds json ~checkpoint =
    let count = ref 0 in
    let bytes = ref 0 in
    let add size =
      if within_add ~limit:bounds.max_bytes !bytes size
      then bytes := !bytes + size
      else raise Json_limit
    in
    let string value =
      let length = String.length value in
      (* Printable ASCII is one byte, quotes/backslash need two, controls and
         non-ASCII bytes are charged six. This safely overestimates Jsonaf's
         escaped encoding without constructing it. *)
      if length > bounds.max_bytes - 2 then raise Json_limit;
      add 2;
      String.iteri value ~f:(fun index char ->
        if index mod 4096 = 0 then checkpoint ();
        let code = Char.to_int char in
        add
          (if
             code >= 0x20
             && code < 0x7f
             && not (Char.equal char '"' || Char.equal char '\\')
           then 1
           else if Char.equal char '"' || Char.equal char '\\'
           then 2
           else 6))
    in
    let rec visit depth = function
      | _ when depth > bounds.max_depth -> raise Json_limit
      | json ->
        checkpoint ();
        incr count;
        if !count > bounds.max_nodes then raise Json_limit;
        (match json with
         | `Null -> add 4
         | `True -> add 4
         | `False -> add 5
         | `Number value ->
           let length = String.length value in
           add length;
           if
             length > 4096
             || (not (valid_number value ~checkpoint))
             || not
                  (Option.value_map
                     (Float.of_string_opt value)
                     ~default:false
                     ~f:(fun number ->
                       Float.is_finite number
                       && ((not (Float.equal number 0.)) || not (nonzero_mantissa value))))
           then raise Json_limit
         | `String value -> string value
         | `Array values ->
           add 2;
           List.iter values ~f:(fun value ->
             add 1;
             visit (depth + 1) value)
         | `Object fields ->
           add 2;
           List.iter fields ~f:(fun (key, value) ->
             add 2;
             string key;
             visit (depth + 1) value))
    in
    try
      visit 1 json;
      Ok !bytes
    with
    | Json_limit -> Or_error.error_string "developer JSON exceeds bounds"
  ;;
end

let json_bytes = Json_bounds.measure

let bounded_label ~name value =
  let bytes = String.length value in
  if bytes = 0 || bytes > 256 then Or_error.error_string (name ^ " is invalid") else Ok ()
;;

module Origin = struct
  type kind =
    | Local_saved of string
    | Editor_snapshot of
        { context : Context.Identity.t
        ; generation : Context.Generation.t
        ; versions : Context.Version.t list
        }
    | Uploaded_bundle of string

  type t =
    { graph : Captured.Digest.t
    ; kind : kind
    }

  let local_saved ~graph ~source_revision =
    Or_error.map (bounded_label ~name:"source revision" source_revision) ~f:(fun () ->
      { graph; kind = Local_saved source_revision })
  ;;

  let editor_snapshot ~graph ~context ~generation ~versions ~max_versions ~checkpoint =
    if max_versions <= 0 || max_versions > 4096
    then Or_error.error_string "invalid editor provenance bound"
    else (
      let rec check count seen = function
        | [] -> Ok { graph; kind = Editor_snapshot { context; generation; versions } }
        | _ when count >= max_versions ->
          Or_error.error_string "editor provenance exceeds source bound"
        | version :: rest ->
          checkpoint ();
          let source = Context.Version.source version in
          if
            (not (Context.Identity.equal (Context.Version.identity version) context))
            || (not
                  (Context.Generation.equal
                     (Context.Version.generation version)
                     generation))
            || (not (Context.Version.overlay_open version))
            || Set.mem seen source
          then Or_error.error_string "inconsistent editor provenance"
          else check (count + 1) (Set.add seen source) rest
      in
      check 0 (Set.empty (module Captured.Id)) versions)
  ;;

  let uploaded_bundle ~graph ~receipt_id =
    Or_error.map (bounded_label ~name:"bundle receipt" receipt_id) ~f:(fun () ->
      { graph; kind = Uploaded_bundle receipt_id })
  ;;

  let graph t = t.graph

  let descriptor t ~checkpoint =
    match t.kind with
    | Local_saved revision -> Sexp.List [ Sexp.Atom "saved"; Sexp.Atom revision ]
    | Uploaded_bundle receipt -> Sexp.List [ Sexp.Atom "upload"; Sexp.Atom receipt ]
    | Editor_snapshot { context; generation; versions } ->
      let versions =
        List.map versions ~f:(fun version ->
          checkpoint ();
          Context.Version.sexp_of_t version)
      in
      [%sexp
        ("editor" : string)
      , (context : Context.Identity.t)
      , (generation : Context.Generation.t)
      , (versions : Sexp.t list)]
  ;;
end

module Fixture = struct
  module Response = struct
    type t =
      | Return of Jsonaf.t
      | Fail of string
    [@@deriving sexp_of]
  end

  module Step = struct
    type t =
      { tool_name : string
      ; input_schema : Schema.t
      ; output_schema : Schema.t
      ; expected_input : Jsonaf.t
      ; normalized_input : Jsonaf.t
      ; response : Response.t
      ; encoded : string
      }

    let rec normalize_json json ~checkpoint =
      checkpoint ();
      match json with
      | `Number number ->
        (* Json.t stores numbers as floats. Compare fixtures after the same
           import/export conversion as a Tool.call argument. [json_bytes]
           already rejected malformed and nonfinite numbers. *)
        Jsonaf.Export.jsonaf_of_float (Float.of_string number)
      | `Array values ->
        `Array (List.map values ~f:(fun value -> normalize_json value ~checkpoint))
      | `Object fields ->
        `Object
          (List.map fields ~f:(fun (key, value) -> key, normalize_json value ~checkpoint))
      | (`Null | `True | `False | `String _) as value -> value
    ;;

    let schema json ~json_bounds ~checkpoint =
      let open Or_error.Let_syntax in
      let%bind _ = json_bytes json_bounds json ~checkpoint in
      checkpoint ();
      Schema.compile json
      |> Result.map_error ~f:(fun _ -> Error.of_string "invalid developer fixture schema")
    ;;

    let create
          ~tool_name
          ~input_schema
          ~output_schema
          ~expected_input
          ~response
          ~json_bounds
          ~checkpoint
      =
      let open Or_error.Let_syntax in
      let%bind () = bounded_label ~name:"fixture tool" tool_name in
      let%bind input_schema_compiled = schema input_schema ~json_bounds ~checkpoint in
      let%bind output_schema_compiled = schema output_schema ~json_bounds ~checkpoint in
      let%bind _ = json_bytes json_bounds expected_input ~checkpoint in
      let normalized_input = normalize_json expected_input ~checkpoint in
      let%bind _ = json_bytes json_bounds normalized_input ~checkpoint in
      let%bind () =
        Schema.validate input_schema_compiled expected_input
        |> Result.map_error ~f:(fun _ -> Error.of_string "fixture input violates schema")
      in
      let%bind () =
        Schema.validate input_schema_compiled normalized_input
        |> Result.map_error ~f:(fun _ ->
          Error.of_string "normalized fixture input violates schema")
      in
      let%bind () =
        match response with
        | Response.Return value ->
          let%bind _ = json_bytes json_bounds value ~checkpoint in
          let normalized_output = normalize_json value ~checkpoint in
          let%bind _ = json_bytes json_bounds normalized_output ~checkpoint in
          let%bind () =
            Schema.validate output_schema_compiled value
            |> Result.map_error ~f:(fun _ ->
              Error.of_string "fixture output violates schema")
          in
          Schema.validate output_schema_compiled normalized_output
          |> Result.map_error ~f:(fun _ ->
            Error.of_string "normalized fixture output violates schema")
        | Response.Fail message ->
          if String.length message > 16 * 1024
          then Or_error.error_string "fixture error exceeds bound"
          else Ok ()
      in
      checkpoint ();
      let encoded =
        Sexp.to_string_mach
          [%sexp
            (tool_name : string)
          , (Jsonaf.to_string input_schema : string)
          , (Jsonaf.to_string output_schema : string)
          , (Jsonaf.to_string expected_input : string)
          , (response : Response.t)]
      in
      Ok
        { tool_name
        ; input_schema = input_schema_compiled
        ; output_schema = output_schema_compiled
        ; expected_input
        ; normalized_input
        ; response
        ; encoded
        }
    ;;

    let tool_name t = t.tool_name
    let expected_input t = t.expected_input

    let matches_input t input =
      Result.is_ok (Schema.validate t.input_schema input)
      && Jsonaf.exactly_equal t.normalized_input input
    ;;

    let response t = t.response
  end

  type t =
    { steps : Step.t list
    ; digest : Captured.Digest.t
    }

  let create ~steps ~max_steps ~max_bytes ~checkpoint =
    if
      max_steps <= 0 || max_steps > 4096 || max_bytes <= 0 || max_bytes > 16 * 1024 * 1024
    then Or_error.error_string "invalid fixture suite limits"
    else (
      let rec measure count bytes = function
        | [] -> Ok bytes
        | _ when count >= max_steps -> Or_error.error_string "too many fixture steps"
        | step :: rest ->
          checkpoint ();
          let encoded_size = String.length step.Step.encoded in
          let size = encoded_size + String.length (Int.to_string encoded_size) + 1 in
          if not (within_add ~limit:max_bytes bytes size)
          then Or_error.error_string "fixture suite exceeds byte bound"
          else measure (count + 1) (bytes + size) rest
      in
      let open Or_error.Let_syntax in
      let%bind bytes = measure 0 0 steps in
      let buffer = Buffer.create bytes in
      List.iter steps ~f:(fun step ->
        checkpoint ();
        let encoded = step.Step.encoded in
        Buffer.add_string buffer (Int.to_string (String.length encoded));
        Buffer.add_char buffer ':';
        Buffer.add_string buffer encoded);
      let digest = Captured.Digest.of_contents (Buffer.contents buffer) ~checkpoint in
      Ok { steps; digest })
  ;;

  let steps t = t.steps
  let digest t = t.digest
end

module Error = struct
  type t =
    | Invalid_origin
    | Stale_source of Captured.Id.t
    | Duplicate_source of Captured.Id.t
    | Incompatible_contract
    | Unsupported_target
    | Invalid_entrypoint
    | Input_limit
  [@@deriving sexp_of]
end

type t =
  { prepared : Prepared.t
  ; origin : Origin.t
  ; target : Contract.Target.t
  ; entrypoint : string
  ; input : Jsonaf.t
  ; fixtures : Fixture.t option
  ; graph_fingerprint : Captured.Digest.t
  ; fingerprint : Captured.Digest.t
  }

let create ~build ~origin ~target ~entrypoint ~input ~fixtures ~json_bounds ~checkpoint =
  let open Result.Let_syntax in
  let%bind () =
    match target with
    | Contract.Target.One_off_v1 -> Ok ()
    | Tool_v1 | Moderator_v1 | Delegated_moderator_v1 -> Error Error.Unsupported_target
  in
  let prepared = Build.prepared build in
  let graph_fingerprint = Prepared.fingerprint prepared in
  let%bind () =
    if Captured.Digest.equal graph_fingerprint (Origin.graph origin)
    then Ok ()
    else Error Error.Invalid_origin
  in
  let%bind () =
    match origin.Origin.kind with
    | Origin.Local_saved _ | Uploaded_bundle _ -> Ok ()
    | Editor_snapshot { versions; _ } ->
      List.fold_result
        versions
        ~init:(Set.empty (module Captured.Id))
        ~f:(fun seen version ->
          checkpoint ();
          let source = Context.Version.source version in
          if Set.mem seen source
          then Error (Error.Duplicate_source source)
          else (
            match Prepared.find_source prepared source with
            | Some selected
              when Captured.Digest.equal
                     (Captured.digest selected)
                     (Context.Version.digest version) -> Ok (Set.add seen source)
            | _ -> Error (Error.Stale_source source)))
      |> Result.map ~f:ignore
  in
  let contract = Contract.for_target target ~checkpoint in
  let%bind () =
    if
      Captured.Digest.equal
        (Contract.fingerprint contract ~checkpoint)
        (Prepared.compiler_contract prepared)
    then Ok ()
    else Error Error.Incompatible_contract
  in
  let%bind () =
    if String.length entrypoint = 0 || String.length entrypoint > 256
    then Error Error.Invalid_entrypoint
    else (
      match
        Contract.Invocation.standalone contract ~entrypoint ~argument_count:1 ~checkpoint
      with
      | Ok _ -> Ok ()
      | Error _ -> Error Error.Invalid_entrypoint)
  in
  let%bind _ =
    json_bytes json_bounds input ~checkpoint
    |> Result.map_error ~f:(fun _ -> Error.Input_limit)
  in
  checkpoint ();
  let fingerprint_input =
    Sexp.to_string_mach
      [%sexp
        (Captured.Digest.to_hex graph_fingerprint : string)
      , (Origin.descriptor origin ~checkpoint : Sexp.t)
      , (target : Contract.Target.t)
      , (entrypoint : string)
      , (Jsonaf.to_string input : string)
      , (Option.map fixtures ~f:Fixture.digest : Captured.Digest.t option)]
  in
  let fingerprint = Captured.Digest.of_contents fingerprint_input ~checkpoint in
  Ok
    { prepared
    ; origin
    ; target
    ; entrypoint
    ; input
    ; fixtures
    ; graph_fingerprint
    ; fingerprint
    }
;;

let prepared t = t.prepared
let origin t = t.origin
let target t = t.target
let entrypoint t = t.entrypoint
let input t = t.input
let fixtures t = t.fixtures
let graph_fingerprint t = t.graph_fingerprint
let fingerprint t = t.fingerprint
