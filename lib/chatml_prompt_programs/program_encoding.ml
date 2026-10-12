open Core
module Captured = Chatml_source
module Revision = Program_source_revision
module Schema = Document_schema

let string = Schema.Encoding.string
let number = Schema.Encoding.number
let hex = Schema.Encoding.hex
let object_ = Schema.Encoding.object_
let array = Schema.Encoding.array

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
  match Schema.Encoding.mode t with
  | Schema.Encoding.Mode.Measure -> string t (String.make 64 '0')
  | Schema.Encoding.Mode.Construct -> string t (f ())
;;

let id t id =
  object_
    t
    [ ( "key"
      , fun t ->
          key t ~f:(fun () ->
            source_key id ~checkpoint:(fun () -> Schema.Encoding.checkpoint t)) )
    ; ("namespace_hex", fun t -> hex t (Captured.Id.package_namespace id))
    ; ("path", fun t -> string t (Captured.Id.relative_path id))
    ]
;;

let revision t revision =
  object_
    t
    [ ("id", fun t -> id t (Revision.id revision))
    ; ( "key"
      , fun t ->
          key t ~f:(fun () ->
            Revision.key revision ~checkpoint:(fun () -> Schema.Encoding.checkpoint t)) )
    ; ("digest", fun t -> string t (Captured.Digest.to_hex (Revision.digest revision)))
    ; ("byte_length", fun t -> number t (Revision.byte_length revision))
    ]
;;

let source t source = revision t (Revision.of_source source)

let input t input =
  object_
    t
    [ ( "key"
      , fun t ->
          string
            t
            (Binding.input_key input ~checkpoint:(fun () -> Schema.Encoding.checkpoint t))
      )
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
            (Binding.input_key (Binding.parser_input binding) ~checkpoint:(fun () ->
               Schema.Encoding.checkpoint t)) )
    ; ( "root"
      , fun t ->
          string
            t
            (Binding.input_key (Binding.registration_input binding) ~checkpoint:(fun () ->
               Schema.Encoding.checkpoint t)) )
    ; ("start_byte", fun t -> number t (Binding.start_byte binding))
    ; ("end_byte", fun t -> number t (Binding.end_byte binding))
    ; ("script_id_hex", fun t -> hex t (Binding.script_id binding))
    ; ( "script_digest"
      , fun t -> string t (Captured.Digest.to_hex (Binding.script_digest binding)) )
    ]
;;

let external_json = Schema.Encoding.raw

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
      , fun t ->
          string
            t
            (Binding.input_key inventory.root ~checkpoint:(fun () ->
               Schema.Encoding.checkpoint t)) )
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
    ; ("schema_version", fun t -> number t 1)
    ; ("payload", fun t -> encode t inventory)
    ]
;;

let payload inventory ~limits ~checkpoint =
  let open Result.Let_syntax in
  let%bind json =
    Schema.Encoding.render
      ~limits
      ~checkpoint
      ~byte_limit_context:"prompt-program metadata bytes"
      ~f:(fun t -> document t inventory)
  in
  match Schema.Json.field json ~name:"payload" with
  | Value payload -> Ok payload
  | Absent | Null -> assert false
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
