open Core
module Captured = Chatml_source
module Prepared = Chatml_prepared_program
module Schema = Document_schema

type mode =
  | Measure
  | Construct

type value =
  { bytes : int
  ; json : Jsonaf.t option
  }

type t =
  { mode : mode
  ; limits : Schema.Limits.t
  ; checkpoint : unit -> unit
  ; nodes : int ref
  ; fields : int ref
  ; depth : int
  }

let exceeded () = Error (Schema.Error.Limit_exceeded "provided-program metadata bytes")

let add t left right =
  let maximum = Schema.Limits.max_bytes t.limits in
  if left < 0 || right < 0 || left > maximum || right > maximum - left
  then exceeded ()
  else Ok (left + right)
;;

let child t = { t with depth = t.depth + 1 }

let admit_value t =
  t.checkpoint ();
  if t.depth > Schema.Limits.max_depth t.limits
  then Error (Schema.Error.Limit_exceeded "depth")
  else if !(t.nodes) >= Schema.Limits.max_nodes t.limits
  then Error (Schema.Error.Limit_exceeded "nodes")
  else (
    Int.incr t.nodes;
    Ok ())
;;

let admit_field t =
  if !(t.fields) >= Schema.Limits.max_fields t.limits
  then Error (Schema.Error.Limit_exceeded "fields")
  else (
    Int.incr t.fields;
    Ok ())
;;

let scalar t json =
  let open Result.Let_syntax in
  let%bind () = admit_value t in
  Result.map (Schema.Json.validate_and_measure ~limits:t.limits json) ~f:(fun bytes ->
    { bytes
    ; json =
        (match t.mode with
         | Measure -> None
         | Construct -> Some json)
    })
;;

let string t value = scalar t (`String value)
let number t value = scalar t (`Number (Int.to_string value))

let hex t value =
  let open Result.Let_syntax in
  let%bind () = admit_value t in
  let maximum = Schema.Limits.max_bytes t.limits in
  let length = String.length value in
  if maximum < 2 || length > (maximum - 2) / 2
  then exceeded ()
  else (
    let bytes = (2 * length) + 2 in
    match t.mode with
    | Measure -> Ok { bytes; json = None }
    | Construct ->
      let output = Bytes.create (2 * length) in
      let digits = "0123456789abcdef" in
      for index = 0 to length - 1 do
        if index % 2048 = 0 then t.checkpoint ();
        let byte = Char.to_int value.[index] in
        Bytes.set output (2 * index) digits.[byte / 16];
        Bytes.set output ((2 * index) + 1) digits.[byte % 16]
      done;
      Ok { bytes; json = Some (`String (Bytes.to_string output)) })
;;

let object_ t fields =
  let open Result.Let_syntax in
  let%bind () = admit_value t in
  let rec loop bytes output first = function
    | [] ->
      let%map bytes = add t bytes 1 in
      { bytes
      ; json =
          (match t.mode with
           | Measure -> None
           | Construct -> Some (`Object (List.rev output)))
      }
    | (name, encode) :: rest ->
      t.checkpoint ();
      let%bind () = admit_field t in
      let%bind key_bytes =
        Schema.Json.validate_and_measure ~limits:t.limits (`String name)
      in
      let%bind value = encode (child t) in
      let%bind bytes = add t bytes (if first then 1 else 2) in
      let%bind bytes = add t bytes key_bytes in
      let%bind bytes = add t bytes value.bytes in
      let output =
        match t.mode with
        | Measure -> output
        | Construct -> (name, Option.value_exn value.json) :: output
      in
      loop bytes output false rest
  in
  loop 1 [] true fields
;;

let array t values ~f =
  let open Result.Let_syntax in
  let%bind () = admit_value t in
  let rec loop bytes output first = function
    | [] ->
      let%map bytes = add t bytes 1 in
      { bytes
      ; json =
          (match t.mode with
           | Measure -> None
           | Construct -> Some (`Array (List.rev output)))
      }
    | value :: rest ->
      t.checkpoint ();
      let%bind value = f (child t) value in
      let%bind bytes = add t bytes (if first then 0 else 1) in
      let%bind bytes = add t bytes value.bytes in
      let output =
        match t.mode with
        | Measure -> output
        | Construct -> Option.value_exn value.json :: output
      in
      loop bytes output false rest
  in
  loop 1 [] true values
;;

let optional name value ~f =
  match value with
  | None -> []
  | Some value -> [ (name, fun t -> f t value) ]
;;

let id t source =
  object_
    t
    [ ("namespace_hex", fun t -> hex t (Captured.Id.package_namespace source))
    ; ("path", fun t -> string t (Captured.Id.relative_path source))
    ]
;;

let digest t value = string t (Captured.Digest.to_hex value)

let location t value =
  let span = Captured.Location.span value in
  object_
    t
    [ ("source", fun t -> id t (Captured.Location.source value))
    ; ("digest", fun t -> digest t (Captured.Location.source_digest value))
    ; ("start_byte", fun t -> number t span.left.offset)
    ; ("end_byte", fun t -> number t span.right.offset)
    ]
;;

let source t value =
  object_
    t
    [ ("id", fun t -> id t (Captured.id value))
    ; ("digest", fun t -> digest t (Captured.digest value))
    ; ("byte_length", fun t -> number t (Captured.byte_length value))
    ]
;;

let definition t value =
  object_
    t
    ([ ("implementation", fun t -> id t (Prepared.Definition.implementation value))
     ; ("declared_at", fun t -> location t (Prepared.Definition.declared_at value))
     ]
     @ optional "interface" (Prepared.Definition.interface value) ~f:id)
;;

let binding t (value : Prepared.Scope.binding) =
  object_
    t
    [ ("name", fun t -> string t value.name)
    ; ("implementation", fun t -> id t (Prepared.Module_id.implementation value.module_id))
    ; ("declared_at", fun t -> location t value.declared_at)
    ]
;;

let scope t value =
  object_
    t
    ([ ("source", fun t -> id t (Prepared.Scope.source value))
     ; ("bindings", fun t -> array t (Prepared.Scope.bindings value) ~f:binding)
     ]
     @ optional "selected_at" (Prepared.Scope.selected_at value) ~f:location)
;;

let environment t value =
  object_
    t
    ([ ("name_hex", fun t -> hex t (Prepared.Environment.name value))
     ; ( "required_contract"
       , fun t -> digest t (Prepared.Environment.required_contract value) )
     ]
     @ optional "declared_at" (Prepared.Environment.declared_at value) ~f:location)
;;

let provenance t = function
  | Prepared.Provenance.Implicit -> object_ t [ ("kind", fun t -> string t "implicit") ]
  | Project { declaration; lock } ->
    object_
      t
      ([ ("kind", fun t -> string t "project")
       ; ("declaration", fun t -> id t declaration)
       ]
       @ optional "lock" lock ~f:id)
;;

let payload t prepared =
  object_
    t
    [ ("fingerprint", fun t -> digest t (Prepared.fingerprint prepared))
    ; ("compiler_contract", fun t -> digest t (Prepared.compiler_contract prepared))
    ; ("entry", fun t -> id t (Prepared.entry prepared))
    ; ("environment", fun t -> environment t (Prepared.environment prepared))
    ; ("provenance", fun t -> provenance t (Prepared.provenance prepared))
    ; ("sources", fun t -> array t (Prepared.sources prepared) ~f:source)
    ; ("definitions", fun t -> array t (Prepared.definitions prepared) ~f:definition)
    ; ("scopes", fun t -> array t (Prepared.scopes prepared) ~f:scope)
    ]
;;

let kind = "chatml.provided-program"

let document prepared ~limits ~checkpoint =
  let open Result.Let_syntax in
  let context mode =
    { mode; limits; checkpoint; nodes = ref 0; fields = ref 0; depth = 1 }
  in
  let measure = context Measure in
  (* Same named-field traversal owns both passes. No projected lists, encoded
     opaque labels or JSON tree are allocated by the first pass. *)
  let%bind _ =
    object_
      measure
      [ ("format", fun t -> string t Schema.Document.format)
      ; ("kind", fun t -> string t kind)
      ; ("schema_version", fun t -> number t 1)
      ; ("payload", fun t -> payload t prepared)
      ]
  in
  let%bind payload = payload { (context Construct) with depth = 2 } prepared in
  checkpoint ();
  let result =
    Schema.Document.create
      ~limits
      ~kind
      ~version:1
      ~payload:(Option.value_exn payload.json)
  in
  checkpoint ();
  result
;;
