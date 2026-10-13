open Core

let map_result values ~f =
  List.fold_result values ~init:[] ~f:(fun acc value ->
    Result.map (f value) ~f:(fun value -> value :: acc))
  |> Result.map ~f:List.rev
;;

module Inventory = Program_inventory
module Error = Inventory.Error
module Limits = Inventory.Limits
module Captured = Chatml_source
module Revision = Program_source_revision
module Parser = Chatmd_parser_input
module Binding = Chatml_prompt_binding
module D = Document_schema
module F = Program_fields
module Storage = Agent_store.Prompt_program_bundle

type t =
  { inventory : Inventory.t
  ; carrier : Inventory.t D.Extension_carrier.t
  ; storage : Storage.t
  }

let inventory t = t.inventory
let to_storage t = t.storage
let document result = Result.map_error result ~f:(fun e -> Error.Document e)
let storage_error result = Result.map_error result ~f:(fun e -> Error.Storage e)
let invalid reason = Error (Error.Invalid_inventory reason)
let get json name decode = Result.bind (F.field json name) ~f:decode

let source_id json ~checkpoint =
  let open Result.Let_syntax in
  let%bind id = F.id json ~checkpoint in
  let%bind key = get json "key" F.string in
  if String.equal key (Program_encoding.source_key id ~checkpoint)
  then Ok id
  else F.invalid "Logical source key mismatch"
;;

let source_revision json ~checkpoint =
  let open Result.Let_syntax in
  let%bind id = get json "id" (source_id ~checkpoint) in
  let%bind digest = get json "digest" F.digest in
  let%bind byte_length = get json "byte_length" F.int in
  let%bind key = get json "key" F.string in
  let%bind revision =
    Revision.create id ~digest ~byte_length
    |> Result.map_error ~f:(fun _ ->
      D.Error.Invalid_field { path = [ "payload" ]; reason = "Invalid source revision" })
  in
  if String.equal key (Revision.key revision ~checkpoint)
  then Ok revision
  else F.invalid "Source revision key mismatch"
;;

let decode ~authored ~payloads ~limits ~checkpoint json =
  let open Result.Let_syntax in
  let doc result = document result in
  let count json maximum = F.array json ~maximum ~checkpoint in
  let%bind () = Inventory.admit_authored authored ~checkpoint in
  let raw = Chatmd_source_bundle.sources authored in
  let%bind pool =
    List.fold_result raw ~init:String.Map.empty ~f:(fun pool (_, body) ->
      let digest =
        Captured.Digest.of_contents body ~checkpoint |> Captured.Digest.to_hex
      in
      match Map.find pool digest with
      | Some previous when Inventory.bytes_equal previous body ~checkpoint -> Ok pool
      | Some _ -> invalid "Authored payload hash collision"
      | None -> Ok (Map.add_exn pool ~key:digest ~data:body))
  in
  let%bind pool =
    List.fold_result payloads ~init:pool ~f:(fun pool payload ->
      checkpoint ();
      let digest = Storage.Payload.sha256 payload
      and body = Storage.Payload.contents payload in
      match Map.find pool digest with
      | Some previous when Inventory.bytes_equal previous body ~checkpoint -> Ok pool
      | Some _ -> invalid "Private payload hash collision"
      | None -> Ok (Map.add_exn pool ~key:digest ~data:body))
  in
  let body digest length =
    match Map.find pool digest with
    | Some body when String.length body = length -> Ok body
    | Some _ | None -> invalid "Missing exact private or authored payload"
  in
  let%bind source_json =
    doc (get json "sources" (fun x -> count x (Limits.max_logical_sources limits)))
  in
  (* Descriptor bytes are checked before source hashing/indexes and source maps. *)
  let%bind descriptors, _ =
    List.fold_result source_json ~init:([], 0) ~f:(fun (descriptors, used) value ->
      checkpoint ();
      let%bind revision = doc (source_revision value ~checkpoint) in
      let length = Revision.byte_length revision in
      if length > Limits.max_total_input_bytes limits - used
      then Error Error.Limit_exceeded
      else Ok (revision :: descriptors, used + length))
  in
  let%bind sources =
    map_result (List.rev descriptors) ~f:(fun revision ->
      let digest = Revision.digest revision
      and length = Revision.byte_length revision in
      let%bind contents = body (Captured.Digest.to_hex digest) length in
      Captured.create
        (Revision.id revision)
        ~contents
        ~expected_digest:(Some digest)
        ~max_bytes:(Some (Limits.max_total_input_bytes limits))
        ~checkpoint
      |> Result.map_error ~f:(fun _ -> Error.Invalid_inventory "Source digest mismatch"))
  in
  let%bind input_json =
    doc (get json "inputs" (fun x -> count x (Limits.max_parser_inputs limits)))
  in
  let%bind inputs =
    map_result input_json ~f:(fun value ->
      checkpoint ();
      let%bind key = doc (get value "key" F.string) in
      let%bind role = doc (get value "role" F.string) in
      let%bind role =
        match role with
        | "root" -> Ok Parser.Role.Root
        | "import" -> Ok Parser.Role.Import
        | _ -> invalid "Invalid parser role"
      in
      let%bind path = doc (get value "path" F.string) in
      let%bind _ =
        Source.Portable_path.create path
        |> Result.map_error ~f:(fun _ ->
          Error.Invalid_inventory "Nonportable parser path")
      in
      let%bind authored_digest = doc (get value "authored_digest" F.digest) in
      let%bind authored_bytes = doc (get value "authored_bytes" F.int) in
      let%bind effective_digest = doc (get value "effective_digest" F.digest) in
      let%bind effective_bytes = doc (get value "effective_bytes" F.int) in
      let%bind authored =
        match List.Assoc.find raw path ~equal:String.equal with
        | Some bytes
          when String.length bytes = authored_bytes
               && String.equal
                    (Captured.Digest.to_hex
                       (Captured.Digest.of_contents bytes ~checkpoint))
                    (Captured.Digest.to_hex authored_digest) -> Ok bytes
        | Some _ | None -> invalid "Parser authored source mismatch"
      in
      let%bind effective =
        body (Captured.Digest.to_hex effective_digest) effective_bytes
      in
      let%bind input =
        Parser.create
          ~role
          ~path
          ~authored
          ~effective
          ~max_input_bytes:Storage.max_payload_bytes
          ~checkpoint
        |> Result.map_error ~f:(fun _ -> Error.Invalid_inventory "Invalid parser input")
      in
      if String.equal key (Binding.input_key input ~checkpoint)
      then Ok input
      else invalid "Parser input key mismatch")
  in
  let find_input key =
    match
      List.find inputs ~f:(fun input ->
        checkpoint ();
        String.equal key (Binding.input_key input ~checkpoint))
    with
    | Some input -> Ok input
    | None -> invalid "Unknown parser input"
  in
  let decode_binding json =
    let%bind key = doc (get json "key" F.string) in
    let%bind input_key = doc (get json "input" F.string) in
    let%bind root_key = doc (get json "root" F.string) in
    let%bind input = find_input input_key in
    let%bind root = find_input root_key in
    let%bind start_byte = doc (get json "start_byte" F.int) in
    let%bind end_byte = doc (get json "end_byte" F.int) in
    let%bind script_id = doc (get json "script_id_hex" (F.hex ~checkpoint)) in
    let%bind script_digest = doc (get json "script_digest" F.digest) in
    let%bind binding =
      Binding.of_metadata
        input
        ~registration_input:root
        ~start_byte
        ~end_byte
        ~script_id
        ~script_digest
        ~checkpoint
      |> Result.map_error ~f:(fun _ ->
        Error.Invalid_binding "Invalid exact declaration binding")
    in
    if String.equal key (Binding.key binding)
    then Ok binding
    else invalid "Binding key mismatch"
  in
  let%bind program_json =
    doc (get json "programs" (fun x -> count x (Limits.max_programs limits)))
  in
  let%bind programs =
    map_result program_json ~f:(fun value ->
      checkpoint ();
      let%bind key = doc (get value "key" F.string) in
      let%bind raw_binding = doc (F.field value "binding") in
      let%bind binding = decode_binding raw_binding in
      let%bind compiler_contract = doc (get value "compiler_contract" F.digest) in
      let%bind fingerprint = doc (get value "fingerprint" F.digest) in
      let%bind input_bytes = doc (get value "input_bytes" F.int) in
      let%bind source_json =
        doc (get value "sources" (fun x -> count x (Limits.max_logical_sources limits)))
      in
      let%bind source_ids =
        map_result source_json ~f:(fun value -> doc (source_revision value ~checkpoint))
      in
      let%bind kind = doc (get value "kind" F.string) in
      let%bind kind =
        match kind with
        | "provided" ->
          let%bind graph =
            doc (get value "graph" (D.Document.inspect ~limits:(Limits.document limits)))
          in
          let%map graph =
            Chatml_program_document.of_document
              graph
              ~limits:(Limits.document limits)
              ~checkpoint
            |> Result.map_error ~f:(fun e -> Error.Provided e)
          in
          Inventory.Program.Provided graph
        | "selected" ->
          let%map lock = doc (get value "lock" (source_revision ~checkpoint)) in
          Inventory.Program.Selected lock
        | _ -> invalid "Unknown program kind"
      in
      Ok
        { Inventory.Program.key
        ; binding
        ; compiler_contract
        ; fingerprint
        ; input_bytes
        ; sources = source_ids
        ; kind
        })
  in
  let%bind selection_json =
    doc (get json "selections" (fun x -> count x (Limits.max_programs limits)))
  in
  let%bind selections =
    map_result selection_json ~f:(fun value ->
      let%bind program = doc (get value "program" F.string) in
      let%bind raw_binding = doc (F.field value "binding") in
      let%map binding = decode_binding raw_binding in
      Inventory.Selection.{ program; binding })
  in
  let%bind root_key = doc (get json "root" F.string) in
  let%bind root = find_input root_key in
  let inventory = Inventory.{ programs; inputs; sources; selections; root } in
  let%bind () = Inventory.validate inventory ~authored ~limits ~checkpoint in
  let%bind expected = Inventory.payloads inventory ~authored ~checkpoint in
  let actual =
    List.sort payloads ~compare:(fun a b ->
      checkpoint ();
      String.compare (Storage.Payload.sha256 a) (Storage.Payload.sha256 b))
  in
  let expected =
    List.sort expected ~compare:(fun a b ->
      checkpoint ();
      String.compare (Storage.Payload.sha256 a) (Storage.Payload.sha256 b))
  in
  if
    List.equal
      (fun a b ->
         checkpoint ();
         String.equal (Storage.Payload.sha256 a) (Storage.Payload.sha256 b)
         && Inventory.bytes_equal
              (Storage.Payload.contents a)
              (Storage.Payload.contents b)
              ~checkpoint)
      actual
      expected
  then Ok inventory
  else invalid "Surplus private payload"
;;

let codec ~authored ~payloads ~limits ~checkpoint =
  D.Domain_codec.create_validated
    ~limits:(Limits.document limits)
    ~kind:"chatml.prompt-programs"
    ~version:1
    ~shape:(Program_encoding.shape ())
    ~supported_semantics:[]
    ~validate:(fun inventory ->
      Inventory.validate inventory ~authored ~limits ~checkpoint
      |> Result.map_error ~f:(fun _ ->
        D.Error.Invalid_field
          { path = [ "payload" ]; reason = "Invalid custody inventory" }))
    ~decode:(fun json ->
      decode ~authored ~payloads ~limits ~checkpoint json
      |> Result.map_error ~f:(function
        | Error.Document e -> e
        | _ ->
          D.Error.Invalid_field
            { path = [ "payload" ]; reason = "Invalid custody metadata" }))
    ~encode:(fun inventory ->
      Program_encoding.payload inventory ~limits:(Limits.document limits) ~checkpoint)
;;

let finish ~authored ~limits ~checkpoint inventory carrier =
  let open Result.Let_syntax in
  let%bind payloads = Inventory.payloads inventory ~authored ~checkpoint in
  let%bind codec = document (codec ~authored ~payloads ~limits ~checkpoint) in
  let%bind metadata = document (D.Domain_codec.encode codec carrier) in
  let%map storage = storage_error (Storage.create ~metadata ~payloads) in
  { inventory; carrier; storage }
;;

let create inventory ~authored ~limits ~checkpoint =
  finish
    ~authored
    ~limits
    ~checkpoint
    inventory
    (D.Extension_carrier.of_authored_value inventory)
;;

let replace t inventory ~authored ~limits ~checkpoint =
  finish
    ~authored
    ~limits
    ~checkpoint
    inventory
    (D.Extension_carrier.with_value t.carrier inventory)
;;

let of_storage storage ~authored ~limits ~checkpoint =
  let open Result.Let_syntax in
  let%bind codec =
    document (codec ~authored ~payloads:(Storage.payloads storage) ~limits ~checkpoint)
  in
  let%map carrier =
    document
      (D.Domain_codec.decode codec (Storage.Manifest.metadata (Storage.manifest storage)))
  in
  { inventory = D.Extension_carrier.value carrier; carrier; storage }
;;

let adopt incoming ~previous ~authored ~limits ~checkpoint =
  let open Result.Let_syntax in
  let inventory = incoming.inventory in
  let%bind () = Inventory.validate inventory ~authored ~limits ~checkpoint in
  let%bind () =
    Inventory.iter_result previous.inventory.sources ~f:(fun source ->
      match Inventory.find_source inventory (Revision.of_source source) ~checkpoint with
      | Some actual when Inventory.same_source source actual ~checkpoint -> Ok ()
      | Some _ | None -> invalid "Previous logical revision was dropped or changed")
  in
  let%bind () =
    Inventory.iter_result previous.inventory.inputs ~f:(fun input ->
      if
        List.exists inventory.inputs ~f:(fun actual ->
          checkpoint ();
          Inventory.input_equal_exact input actual ~checkpoint)
      then Ok ()
      else invalid "Previous parser input was dropped or changed")
  in
  let%bind () =
    Inventory.iter_result previous.inventory.programs ~f:(fun program ->
      checkpoint ();
      match
        Inventory.find_program inventory program.Inventory.Program.key ~checkpoint
      with
      | None -> invalid "Previous program was dropped"
      | Some actual ->
        let same_kind =
          match program.kind, actual.kind with
          | Selected left, Selected right -> Revision.equal left right
          | Provided left, Provided right ->
            D.Json.equal
              (D.Document.json (Chatml_program_document.to_document left))
              (D.Document.json (Chatml_program_document.to_document right))
          | Provided _, Selected _ | Selected _, Provided _ -> false
        in
        if
          (not same_kind)
          || (not (Chatml_prompt_binding.equal program.binding actual.binding))
          || (not
                (Captured.Digest.equal program.compiler_contract actual.compiler_contract))
          || (not (Captured.Digest.equal program.fingerprint actual.fingerprint))
          || (not (Int.equal program.input_bytes actual.input_bytes))
          || not
               (List.equal
                  (fun left right ->
                     checkpoint ();
                     Revision.equal left right)
                  program.sources
                  actual.sources)
        then invalid "Previous program was changed"
        else Ok ())
  in
  let%bind codec =
    document
      (codec ~authored ~payloads:(Storage.payloads incoming.storage) ~limits ~checkpoint)
  in
  let%bind carrier =
    document
      (D.Domain_codec.adopt codec ~previous:previous.carrier ~incoming:incoming.carrier)
  in
  finish ~authored ~limits ~checkpoint inventory carrier
;;
