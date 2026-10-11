open Core

let iter_result values ~f = List.fold_result values ~init:() ~f:(fun () value -> f value)

module Captured = Chatml_source
module Revision = Program_source_revision
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

let exceeded () = Error (Schema.Error.Limit_exceeded "prompt-program metadata bytes")

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

module Inventory = Program_inventory
module Binding = Program_binding
module Parser = Chatmd_parser_input

let source_key id ~checkpoint =
  let context = ref (Digestif.SHA256.init ()) in
  List.iter
    [ Captured.Id.package_namespace id; Captured.Id.relative_path id ]
    ~f:(fun value ->
      context
      := Digestif.SHA256.feed_string !context (Int.to_string (String.length value) ^ ":");
      let offset = ref 0 in
      while !offset < String.length value do
        checkpoint ();
        let len = Int.min 4096 (String.length value - !offset) in
        context := Digestif.SHA256.feed_string !context ~off:!offset ~len value;
        offset := !offset + len
      done);
  Digestif.SHA256.(get !context |> to_hex)
;;

let key t ~f =
  (* All identity keys have one fixed digest width, admitted before construction. *)
  match t.mode with
  | Measure -> string t (String.make 64 '0')
  | Construct -> string t (f ())
;;

let id t id =
  object_
    t
    [ ("key", fun t -> key t ~f:(fun () -> source_key id ~checkpoint:t.checkpoint))
    ; ("namespace_hex", fun t -> hex t (Captured.Id.package_namespace id))
    ; ("path", fun t -> string t (Captured.Id.relative_path id))
    ]
;;

let revision t revision =
  object_
    t
    [ ("id", fun t -> id t (Revision.id revision))
    ; ("key", fun t -> key t ~f:(fun () -> Revision.key revision ~checkpoint:t.checkpoint))
    ; ("digest", fun t -> string t (Captured.Digest.to_hex (Revision.digest revision)))
    ; ("byte_length", fun t -> number t (Revision.byte_length revision))
    ]
;;

let source t source = revision t (Revision.of_source source)

let input t input =
  object_
    t
    [ ("key", fun t -> string t (Binding.input_key input ~checkpoint:t.checkpoint))
    ; ( "role"
      , fun t ->
          string
            t
            (match Parser.role input with
             | Root -> "root"
             | Import -> "import") )
    ; ("path", fun t -> string t (Parser.path input))
    ; ("authored_digest", fun t -> string t (Parser.authored_sha256 input))
    ; ("authored_bytes", fun t -> number t (Parser.authored_byte_length input))
    ; ("effective_digest", fun t -> string t (Parser.effective_sha256 input))
    ; ("effective_bytes", fun t -> number t (Parser.effective_byte_length input))
    ]
;;

let binding t binding =
  object_
    t
    [ ("key", fun t -> string t (Binding.key binding))
    ; ( "input"
      , fun t ->
          string
            t
            (Binding.input_key (Binding.parser_input binding) ~checkpoint:t.checkpoint) )
    ; ( "root"
      , fun t ->
          string
            t
            (Binding.input_key
               (Binding.registration_input binding)
               ~checkpoint:t.checkpoint) )
    ; ("start_byte", fun t -> number t (Binding.start_byte binding))
    ; ("end_byte", fun t -> number t (Binding.end_byte binding))
    ; ("script_id_hex", fun t -> hex t (Binding.script_id binding))
    ; ( "script_digest"
      , fun t -> string t (Captured.Digest.to_hex (Binding.script_digest binding)) )
    ]
;;

let external_json t json =
  let open Result.Let_syntax in
  let%bind bytes = Schema.Json.validate_and_measure ~limits:t.limits json in
  let rec visit t json =
    let%bind () = admit_value t in
    match json with
    | `Object fields ->
      iter_result fields ~f:(fun (_, value) ->
        let%bind () = admit_field t in
        visit (child t) value)
    | `Array values -> iter_result values ~f:(visit (child t))
    | `String _ | `Number _ | `True | `False | `Null -> Ok ()
  in
  let%map () = visit t json in
  { bytes
  ; json =
      (match t.mode with
       | Measure -> None
       | Construct -> Some json)
  }
;;

let program t (program : Inventory.Program.t) =
  object_
    t
    ([ ("key", fun t -> string t program.key)
     ; ("binding", fun t -> binding t program.binding)
     ; ( "compiler_contract"
       , fun t -> string t (Captured.Digest.to_hex program.compiler_contract) )
     ; ("fingerprint", fun t -> string t (Captured.Digest.to_hex program.fingerprint))
     ; ("input_bytes", fun t -> number t program.input_bytes)
     ; ("sources", fun t -> array t program.sources ~f:revision)
     ]
     @
     match program.kind with
     | Provided document ->
       [ ("kind", fun t -> string t "provided")
       ; ( "graph"
         , fun t ->
             external_json
               t
               (Chatml_program_document.to_document document |> Schema.Document.json) )
       ]
     | Selected lock ->
       [ ("kind", fun t -> string t "selected"); ("lock", fun t -> revision t lock) ])
;;

let selection t (selection : Inventory.Selection.t) =
  object_
    t
    [ ("program", fun t -> string t selection.program)
    ; ("binding", fun t -> binding t selection.binding)
    ]
;;

let encode t (inventory : Inventory.t) =
  object_
    t
    [ ( "root"
      , fun t -> string t (Binding.input_key inventory.root ~checkpoint:t.checkpoint) )
    ; ("inputs", fun t -> array t inventory.inputs ~f:input)
    ; ("sources", fun t -> array t inventory.sources ~f:source)
    ; ("programs", fun t -> array t inventory.programs ~f:program)
    ; ("selections", fun t -> array t inventory.selections ~f:selection)
    ]
;;

let document t inventory =
  object_
    t
    [ ("format", fun t -> string t Schema.Document.format)
    ; ("kind", fun t -> string t "chatml.prompt-programs")
    ; ("version", fun t -> number t 1)
    ; ("payload", fun t -> encode t inventory)
    ]
;;

let payload inventory ~limits ~checkpoint =
  let make mode =
    { mode; limits; checkpoint; nodes = ref 0; fields = ref 0; depth = 1 }
  in
  let open Result.Let_syntax in
  let%bind _ = document (make Measure) inventory in
  let%bind value = document (make Construct) inventory in
  match value.json with
  | Some json ->
    (match Schema.Json.field json ~name:"payload" with
     | Value payload -> Ok payload
     | Absent | Null -> assert false)
  | None -> assert false
;;

let shape () =
  let module S = Schema.Shape in
  let unwrap result =
    Result.map_error result ~f:(fun e -> Sexp.to_string_hum (Schema.Error.sexp_of_t e))
    |> Result.ok_or_failwith
  in
  let object_ fields = S.object_ fields |> unwrap in
  let array shape name = S.array shape ~identity_field:(Some name) |> unwrap in
  let scalar_fields names = List.map names ~f:(fun name -> name, S.value) in
  let id = object_ (scalar_fields [ "key"; "namespace_hex"; "path" ]) in
  let binding =
    object_
      (scalar_fields
         [ "key"
         ; "input"
         ; "root"
         ; "start_byte"
         ; "end_byte"
         ; "script_id_hex"
         ; "script_digest"
         ])
  in
  let input =
    object_
      (scalar_fields
         [ "key"
         ; "role"
         ; "path"
         ; "authored_digest"
         ; "authored_bytes"
         ; "effective_digest"
         ; "effective_bytes"
         ])
  in
  let source = object_ (("id", id) :: scalar_fields [ "key"; "digest"; "byte_length" ]) in
  let common =
    [ "binding", binding; "sources", array source "key" ]
    @ scalar_fields [ "key"; "kind"; "compiler_contract"; "fingerprint"; "input_bytes" ]
  in
  let program =
    S.tagged_object
      ~discriminator:"kind"
      [ "provided", object_ (("graph", S.value) :: common)
      ; "selected", object_ (("lock", source) :: common)
      ]
    |> unwrap
  in
  object_
    [ "root", S.value
    ; "inputs", array input "key"
    ; "sources", array source "key"
    ; "programs", array program "key"
    ; "selections", array (object_ [ "program", S.value; "binding", binding ]) "program"
    ]
;;
