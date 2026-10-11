open Core
module B = Content_budget
module Inventory = Content_inventory
module Record = Content_record
module Input = Chatmd_parser_input
module R = Content_rendered
module Storage = Agent_store.Prompt_program_bundle
module D = Document_schema
module E = D.Encoding

let kind = "ochat.prompt-content"
let invalid reason = Error (B.Error.invalid reason)
let document result = Result.map_error result ~f:B.Error.document
let storage_error result = Result.map_error result ~f:B.Error.storage

let map budget values ~f =
  let open Result.Let_syntax in
  let%bind result =
    List.fold_result values ~init:[] ~f:(fun acc x ->
      let%bind () = B.work budget 2 in
      let%map value = f x in
      value :: acc)
  in
  Ok (List.rev result)
;;

type t =
  { inventory : Inventory.t
  ; carrier : Inventory.t D.Extension_carrier.t
  ; storage : Storage.t
  }

let inventory t = t.inventory
let to_storage t = t.storage

let source t record =
  match Record.kind record with
  | Dynamic -> E.object_ t [ ("kind", fun t -> E.string t "dynamic") ]
  | Captured { provenance; input_digest; input_bytes; output_digest; contents } ->
    let selector =
      match provenance with
      | R.Provenance.Local member ->
        [ ("kind", fun t -> E.string t "local")
        ; ("member", fun t -> E.string t (Source.Portable_path.to_string member))
        ]
      | Builtin id ->
        [ ("kind", fun t -> E.string t "builtin"); ("id", fun t -> E.string t id) ]
    in
    E.object_
      t
      (selector
       @ [ ("input_digest", fun t -> E.string t input_digest)
         ; ("input_bytes", fun t -> E.number t input_bytes)
         ; ("output_digest", fun t -> E.string t output_digest)
         ; ("output_bytes", fun t -> E.number t (String.length contents))
         ])
;;

let record t value ~budget =
  E.object_
    t
    [ ("key", fun t -> E.string t (Record.key value))
    ; ( "root"
      , fun t ->
          E.string
            t
            (Input.identity_digest (Record.root value) ~checkpoint:(fun () ->
               B.checkpoint budget)) )
    ; ( "input"
      , fun t ->
          E.string
            t
            (Input.identity_digest (Record.input value) ~checkpoint:(fun () ->
               B.checkpoint budget)) )
    ; ("start", fun t -> E.number t (Input.Range.start_byte (Record.range value)))
    ; ("end", fun t -> E.number t (Input.Range.end_byte (Record.range value)))
    ; ("ordinal", fun t -> E.number t (Record.ordinal value))
    ; ("src_digest", fun t -> E.string t (Record.src_digest value))
    ; ("src_bytes", fun t -> E.number t (Record.src_bytes value))
    ; ("local", fun t -> E.bool t (Record.local value))
    ; ("cleanup_html", fun t -> E.bool t (Record.cleanup_html value))
    ; ("markdown", fun t -> E.bool t (Record.markdown value))
    ; ("source", fun t -> source t value)
    ]
;;

let encode inventory ~budget =
  let open Result.Let_syntax in
  (* Stage-weight logical units for both passes: fixed record structure/keys and
     decimal/digest metadata <=4096 units; each admitted member byte reserves six
     escaping/traversal units. Root identity encoding includes its path twice.
     These conservative units are not CPU instructions or heap-byte accounting. *)
  let%bind () =
    List.fold_result (Inventory.roots inventory) ~init:() ~f:(fun () root ->
      B.work budget (2048 + (2 * String.length (Input.path root))))
  in
  let%bind () =
    List.fold_result (Inventory.records inventory) ~init:() ~f:(fun () record ->
      let variable =
        match Record.kind record with
        | Dynamic -> 0
        | Captured { provenance = Builtin id; _ } -> String.length id
        | Captured { provenance = Local member; _ } ->
          String.length (Source.Portable_path.to_string member)
      in
      B.work
        budget
        (4096
         + (6 * variable)
         + (2 * String.length (Input.path (Record.root record)))
         + (2 * String.length (Input.path (Record.input record)))))
  in
  let%bind () = B.work budget 512 in
  let%bind json =
    document
      (E.render
         ~limits:(B.Limits.document (B.limits budget))
         ~byte_limit_context:"prompt-content metadata bytes"
         ~checkpoint:(fun () -> B.checkpoint budget)
         ~f:(fun t ->
           E.object_
             t
             [ ("format", fun t -> E.string t D.Document.format)
             ; ("schema_version", fun t -> E.number t 1)
             ; ("kind", fun t -> E.string t kind)
             ; ( "payload"
               , fun t ->
                   E.object_
                     t
                     [ ( "roots"
                       , fun t ->
                           E.array t (Inventory.roots inventory) ~f:(fun t root ->
                             E.object_
                               t
                               [ ( "key"
                                 , fun t ->
                                     E.string
                                       t
                                       (Input.identity_digest root ~checkpoint:(fun () ->
                                          B.checkpoint budget)) )
                               ]) )
                     ; ( "documents"
                       , fun t ->
                           E.array t (Inventory.records inventory) ~f:(fun t value ->
                             record t value ~budget) )
                     ] )
             ]))
  in
  match D.Json.field json ~name:"payload" with
  | Value payload -> Ok payload
  | Absent | Null -> invalid "content encoder omitted payload"
;;

let field json name =
  match D.Json.field json ~name with
  | Value value -> Ok value
  | Absent | Null -> invalid "missing required content metadata field"
;;

let string = function
  | `String value -> Ok value
  | _ -> invalid "expected content metadata string"
;;

let integer = function
  | `Number text ->
    (match Int.of_string_opt text with
     | Some n when n >= 0 && String.equal text (Int.to_string n) -> Ok n
     | Some _ | None -> invalid "expected nonnegative canonical content integer")
  | _ -> invalid "expected content metadata integer"
;;

let boolean = function
  | `True -> Ok true
  | `False -> Ok false
  | _ -> invalid "expected content flag"
;;

let digest json =
  let open Result.Let_syntax in
  let%bind value = string json in
  if Result.is_ok (Chatml_source.Digest.of_hex value)
  then Ok value
  else invalid "invalid content digest"
;;

let get json name f = Result.bind (field json name) ~f

let array json ~maximum ~budget =
  match json with
  | `Array values -> Result.map (B.count budget values ~maximum) ~f:(fun _ -> values)
  | _ -> invalid "expected content metadata array"
;;

let decode json ~inputs ~payloads ~budget =
  let open Result.Let_syntax in
  let limits = B.limits budget in
  let%bind input_table = Inventory.input_table inputs ~budget in
  let find_input key =
    let%bind () = B.map_work budget (Map.length input_table) in
    match Map.find input_table key with
    | Some input -> Ok input
    | None -> invalid "unknown parser-input revision"
  in
  let%bind _ = B.count budget payloads ~maximum:(B.Limits.payloads limits) in
  let%bind pool, _ =
    List.fold_result payloads ~init:(String.Map.empty, 0) ~f:(fun (pool, bytes) p ->
      let%bind () = B.map_work budget (Map.length pool) in
      if
        Storage.Payload.byte_length p > B.Limits.payload_bytes limits - bytes
        || Map.mem pool (Storage.Payload.sha256 p)
      then invalid "invalid content payload inventory"
      else
        Ok
          ( Map.set pool ~key:(Storage.Payload.sha256 p) ~data:p
          , bytes + Storage.Payload.byte_length p ))
  in
  let%bind roots_json =
    get json "roots" (fun j -> array j ~maximum:(B.Limits.parser_inputs limits) ~budget)
  in
  let%bind docs_json =
    get json "documents" (fun j -> array j ~maximum:(B.Limits.occurrences limits) ~budget)
  in
  let%bind roots =
    map budget roots_json ~f:(fun json ->
      let%bind key = get json "key" digest in
      find_input key)
  in
  let%bind records =
    map budget docs_json ~f:(fun json ->
      let%bind key = get json "key" digest in
      let%bind root_key = get json "root" digest in
      let%bind input_key = get json "input" digest in
      let%bind root = find_input root_key in
      let%bind input = find_input input_key in
      let%bind start_byte = get json "start" integer in
      let%bind end_byte = get json "end" integer in
      let%bind () =
        if end_byte > Input.effective_byte_length input || end_byte <= start_byte
        then invalid "invalid content declaration range"
        else Ok ()
      in
      let%bind () = B.work budget (8 + (start_byte mod 4096) + (end_byte mod 4096)) in
      let%bind range =
        Input.Range.create input ~start_byte ~end_byte ~checkpoint:(fun () ->
          B.checkpoint budget)
        |> Result.map_error ~f:(fun _ ->
          B.Error.invalid "invalid content declaration range")
      in
      let%bind ordinal = get json "ordinal" integer in
      let%bind src_digest = get json "src_digest" digest in
      let%bind src_bytes = get json "src_bytes" integer in
      let%bind local = get json "local" boolean in
      let%bind cleanup_html = get json "cleanup_html" boolean in
      let%bind markdown = get json "markdown" boolean in
      let%bind source = field json "source" in
      let%bind tag = get source "kind" string in
      let%bind kind =
        match tag with
        | "dynamic" -> Ok Record.Kind.Dynamic
        | "local" | "builtin" ->
          let%bind provenance =
            if String.equal tag "local"
            then (
              let%bind path = get source "member" string in
              let%map member =
                Source.Portable_path.create path
                |> Result.map_error ~f:(fun _ ->
                  B.Error.invalid ~range "invalid stored local document member")
              in
              R.Provenance.Local member)
            else (
              let%map id = get source "id" string in
              R.Provenance.Builtin id)
          in
          let%bind input_digest = get source "input_digest" digest in
          let%bind input_bytes = get source "input_bytes" integer in
          let%bind output_digest = get source "output_digest" digest in
          let%bind output_bytes = get source "output_bytes" integer in
          let%bind () = B.map_work budget (Map.length pool) in
          (match Map.find pool output_digest with
           | Some p when Storage.Payload.byte_length p = output_bytes ->
             Ok
               (Record.Kind.Captured
                  { provenance
                  ; input_digest
                  ; input_bytes
                  ; output_digest
                  ; contents = Storage.Payload.contents p
                  })
           | Some _ | None -> invalid "missing exact rendered payload")
        | _ -> invalid "unknown content source kind"
      in
      let%bind value =
        Record.create
          ~root
          ~range
          ~ordinal
          ~src_digest
          ~src_bytes
          ~local
          ~cleanup_html
          ~markdown
          ~kind
          ~budget
      in
      if String.equal key (Record.key value)
      then Ok value
      else invalid "content occurrence key mismatch")
  in
  let%bind inventory = Inventory.create ~inputs ~roots ~records ~budget in
  let%bind expected =
    B.count budget (Inventory.payloads inventory) ~maximum:(B.Limits.payloads limits)
  in
  if expected <> Map.length pool then invalid "surplus content payload" else Ok inventory
;;

let shape () =
  let unwrap result =
    Result.map_error result ~f:(fun _ -> "invalid static content shape")
    |> Result.ok_or_failwith
  in
  let obj fields = D.Shape.object_ fields |> unwrap in
  let values fields = List.map fields ~f:(fun name -> name, D.Shape.value) in
  let source =
    D.Shape.tagged_object
      ~discriminator:"kind"
      [ "dynamic", obj (values [ "kind" ])
      ; ( "local"
        , obj
            (values
               [ "kind"
               ; "member"
               ; "input_digest"
               ; "input_bytes"
               ; "output_digest"
               ; "output_bytes"
               ]) )
      ; ( "builtin"
        , obj
            (values
               [ "kind"
               ; "id"
               ; "input_digest"
               ; "input_bytes"
               ; "output_digest"
               ; "output_bytes"
               ]) )
      ]
    |> unwrap
  in
  let record =
    obj
      (values
         [ "key"
         ; "root"
         ; "input"
         ; "start"
         ; "end"
         ; "ordinal"
         ; "src_digest"
         ; "src_bytes"
         ; "local"
         ; "cleanup_html"
         ; "markdown"
         ]
       @ [ "source", source ])
  in
  let array value = D.Shape.array value ~identity_field:(Some "key") |> unwrap in
  obj [ "roots", array (obj [ "key", D.Shape.value ]); "documents", array record ]
;;

let codec ~inputs ~payloads ~budget =
  D.Domain_codec.create_validated
    ~limits:(B.Limits.document (B.limits budget))
    ~kind
    ~version:1
    ~shape:(shape ())
    ~supported_semantics:[]
    ~validate:(fun inventory ->
      Inventory.create
        ~inputs:(Inventory.inputs inventory)
        ~roots:(Inventory.roots inventory)
        ~records:(Inventory.records inventory)
        ~budget
      |> Result.map ~f:ignore
      |> Result.map_error ~f:(fun _ ->
        D.Error.Invalid_field
          { path = [ "payload" ]; reason = "invalid content inventory" }))
    ~decode:(fun json ->
      decode json ~inputs ~payloads ~budget
      |> Result.map_error ~f:(fun _ ->
        D.Error.Invalid_field
          { path = [ "payload" ]; reason = "invalid content metadata" }))
    ~encode:(fun inventory ->
      encode inventory ~budget
      |> Result.map_error ~f:(fun _ ->
        D.Error.Limit_exceeded "content encoding admission"))
;;

(* Fixed schema/extension passes can scan unknown field names and values too.
   Admit a conservative64 units per visited scalar/key byte before domain lookup
   or extension projection, not just per known document. Source storage/document
   admission already bounds JSON depth; this traversal polls every node/key. *)
let admit_metadata metadata ~budget =
  let open Result.Let_syntax in
  let%bind () =
    document (D.Document.validate metadata ~limits:(B.Limits.document (B.limits budget)))
  in
  let rec visit json =
    let%bind () = B.work budget 64 in
    match json with
    | `String text | `Number text -> B.work_product budget 64 (String.length text)
    | `True | `False | `Null -> Ok ()
    | `Array values -> List.fold_result values ~init:() ~f:(fun () value -> visit value)
    | `Object fields ->
      List.fold_result fields ~init:() ~f:(fun () (key, value) ->
        let%bind () = B.work_product budget 64 (String.length key) in
        visit value)
  in
  visit (D.Document.json metadata)
;;

let finish inventory carrier ~budget =
  let open Result.Let_syntax in
  let payloads = Inventory.payloads inventory in
  let%bind codec =
    document (codec ~inputs:(Inventory.inputs inventory) ~payloads ~budget)
  in
  let%bind metadata = document (D.Domain_codec.encode codec carrier) in
  let%bind () = admit_metadata metadata ~budget in
  let%map storage = storage_error (Storage.create ~metadata ~payloads) in
  { inventory; carrier; storage }
;;

let create inventory ~budget =
  finish inventory (D.Extension_carrier.of_authored_value inventory) ~budget
;;

let of_storage storage ~inputs ~budget =
  let open Result.Let_syntax in
  let%bind () =
    admit_metadata (Storage.Manifest.metadata (Storage.manifest storage)) ~budget
  in
  let%bind codec =
    document (codec ~inputs ~payloads:(Storage.payloads storage) ~budget)
  in
  let%map carrier =
    document
      (D.Domain_codec.decode codec (Storage.Manifest.metadata (Storage.manifest storage)))
  in
  { inventory = D.Extension_carrier.value carrier; carrier; storage }
;;

let adopt incoming ~previous ~budget =
  let open Result.Let_syntax in
  let%bind () =
    admit_metadata (Storage.Manifest.metadata (Storage.manifest previous.storage)) ~budget
  in
  let%bind () =
    admit_metadata (Storage.Manifest.metadata (Storage.manifest incoming.storage)) ~budget
  in
  let%bind () =
    Inventory.preserves incoming.inventory ~previous:previous.inventory ~budget
  in
  let%bind () =
    List.fold_result (Inventory.records incoming.inventory) ~init:() ~f:(fun () record ->
      let input_bytes, output_bytes =
        match Record.kind record with
        | Dynamic -> 0, 0
        | Captured { input_bytes; contents; _ } -> input_bytes, String.length contents
      in
      B.add
        budget
        (B.Accounting.create
           ~occurrences:1
           ~input_bytes
           ~output_bytes
           ~work_steps:0
           ~nodes:0))
  in
  let%bind codec =
    document
      (codec
         ~inputs:(Inventory.inputs incoming.inventory)
         ~payloads:(Storage.payloads incoming.storage)
         ~budget)
  in
  let%bind carrier =
    document
      (D.Domain_codec.adopt codec ~previous:previous.carrier ~incoming:incoming.carrier)
  in
  finish incoming.inventory carrier ~budget
;;
