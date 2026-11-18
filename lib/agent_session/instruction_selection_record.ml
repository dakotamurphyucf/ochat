open! Core
module P = Agent_protocol
module I = Instruction_content
module C = Instruction_capture
module R = Instruction_command.Receipt
module S = Agent_store.Prompt_program_bundle
module D = Document_schema
module X = Persistence_codec

type reference =
  { capture : string
  ; file : string
  }

type t =
  { revision : int64
  ; project : string option
  ; skills : reference list
  ; captures : C.t list
  ; pool : S.Payload.t String.Map.t
  ; receipts : R.t list
  }

let empty =
  { revision = 0L
  ; project = None
  ; skills = []
  ; captures = []
  ; pool = String.Map.empty
  ; receipts = []
  }
;;

let active t = Option.is_some t.project || not (List.is_empty t.skills)
let find t id = List.find t.captures ~f:(fun capture -> String.equal (C.id capture) id)

let project t =
  Option.bind t.project ~f:(find t) |> Option.value_map ~default:[] ~f:C.loaded
;;

let skill t reference =
  Option.bind (find t reference.capture) ~f:(fun capture ->
    List.find (C.loaded capture) ~f:(fun loaded ->
      String.equal reference.file (C.file_key (I.Observed.id (I.Loaded.observed loaded)))))
;;

let skills t = List.filter_map t.skills ~f:(skill t)
let invalid message = Error (P.Error.invalid_request message)
let full message = Error (P.Error.create Resource_limit ~message ~retryable:false ())
let unique values = List.length values = Set.length (String.Set.of_list values)

let validate t =
  let open Result.Let_syntax in
  let files = List.concat_map t.captures ~f:C.loaded in
  let membership_bytes =
    List.sum (module Int) files ~f:(fun file -> String.length (I.Loaded.contents file))
  in
  let manifest_bytes =
    List.sum
      (module Int)
      t.captures
      ~f:(fun capture -> String.length (Jsonaf.to_string (C.to_json capture)))
  in
  if
    List.length t.skills > 64
    || List.length t.captures > 128
    || (List.length t.receipts > if active t then 127 else 128)
    || List.length files > 512
    || membership_bytes > 8 * 1024 * 1024
    || manifest_bytes > 2 * 1024 * 1024
    || Map.length t.pool > 512
    || Map.fold t.pool ~init:0 ~f:(fun ~key:_ ~data sum ->
         sum + S.Payload.byte_length data)
       > 4 * 1024 * 1024
    || Int64.(t.revision < 0L || (active t && t.revision >= max_value))
  then full "retained instruction selection capacity exhausted"
  else if
    (not (unique (List.map t.captures ~f:C.id)))
    || (not (unique (List.map t.receipts ~f:R.identity)))
    || (not (unique (List.map t.skills ~f:(fun reference -> reference.file))))
    || not (Int64.equal t.revision (Int64.of_int (List.length t.receipts)))
  then invalid "instruction selection identities/frontier disagree"
  else (
    let%bind () =
      match t.project with
      | None -> Ok ()
      | Some id ->
        (match find t id with
         | Some capture when C.Kind.equal (C.kind capture) Project -> Ok ()
         | None | Some _ -> invalid "missing or non-project selected capture")
    in
    let%bind () =
      List.fold_result t.skills ~init:() ~f:(fun () reference ->
        match find t reference.capture, skill t reference with
        | Some capture, Some _ when C.Kind.equal (C.kind capture) Skills -> Ok ()
        | None, _ | Some _, _ -> invalid "missing or non-skill selected member")
    in
    let selected = project t @ skills t in
    if
      List.length selected > 64
      || List.sum
           (module Int)
           selected
           ~f:(fun value -> String.length (I.Loaded.contents value))
         > 256 * 1024
    then full "active instruction selection exceeds its profile"
    else (
      let%bind _ =
        List.fold_result t.receipts ~init:0L ~f:(fun previous receipt ->
          if Int64.equal receipt.R.revision (Int64.succ previous)
          then Ok receipt.revision
          else invalid "instruction receipts must form one ordered frontier")
      in
      let expected =
        List.concat_map t.captures ~f:C.payloads
        |> List.map ~f:S.Payload.sha256
        |> String.Set.of_list
      in
      if not (Set.equal expected (Map.key_set t.pool))
      then invalid "instruction pool membership mismatch"
      else Ok ()))
;;

let to_json t =
  `Object
    [ "revision", X.int64_json t.revision
    ; "project", X.option_json (fun value -> `String value) t.project
    ; ( "skills"
      , `Array
          (List.map t.skills ~f:(fun r -> `Array [ `String r.capture; `String r.file ])) )
    ; "captures", `Array (List.map t.captures ~f:C.to_json)
    ; ( "payloads"
      , `Array
          (Map.data t.pool
           |> List.map ~f:(fun p ->
             `Object
               [ "id", `String (S.Payload.sha256 p)
               ; "contents", `String (S.Payload.contents p)
               ])) )
    ; ( "receipts"
      , `Array
          (List.map t.receipts ~f:(fun receipt ->
             `Object [ "id", `String (R.identity receipt); "receipt", R.to_json receipt ]))
      )
    ]
;;

exception Work_limit

let of_json raw =
  let remaining = ref 10_000_000 in
  let checkpoint () =
    decr remaining;
    if !remaining < 0 then raise Work_limit
  in
  try
    let open Result.Let_syntax in
    let%bind f = X.object_ raw in
    let%bind revision = X.required f "revision" X.nonnegative_int64 in
    let%bind project = X.required f "project" (X.nullable P.Json_codec.string) in
    let%bind skills =
      X.required f "skills" (fun raw ->
        let%bind () =
          match raw with
          | `Array rows when List.length rows <= 64 -> Ok ()
          | _ -> full "selected skills count exceeded"
        in
        X.list
          (function
            | `Array [ `String capture; `String file ] -> Ok { capture; file }
            | _ -> invalid "invalid selected skill reference")
          raw)
    in
    let array name max =
      X.required f name (function
        | `Array values when List.length values <= max -> Ok values
        | _ -> full ("instruction " ^ name ^ " count exceeded"))
    in
    let%bind capture_rows = array "captures" 128 in
    let%bind pool_rows = array "payloads" 512 in
    let%bind receipt_rows = array "receipts" 128 in
    (* Bound declared row memberships before ANY semantic child restoration.
       Snapshot.of_storage independently verifies every declaration and payload. *)
    let%bind _ =
      List.fold_result capture_rows ~init:(0, 0, 0) ~f:(fun (count, bytes, encoded) row ->
        let%bind fields = X.object_ row in
        let%bind raw_manifest = X.required fields "manifest" X.raw in
        let encoded = encoded + String.length (Jsonaf.to_string row) in
        let%bind manifest =
          S.Manifest.of_json raw_manifest
          |> Result.map_error ~f:(fun _ ->
            P.Error.invalid_request "invalid instruction manifest")
        in
        let metadata = S.Manifest.metadata manifest |> D.Document.payload in
        let%bind rows =
          match D.Json.field metadata ~name:"instructions" with
          | Value (`Array rows) when List.length rows <= 64 -> Ok rows
          | _ -> invalid "missing or oversized captured instruction rows"
        in
        let%bind bytes =
          List.fold_result rows ~init:bytes ~f:(fun bytes row ->
            let%bind fields = X.object_ row in
            let%map length =
              X.required
                fields
                "byte_length"
                (P.Json_codec.bounded_int ~min:0 ~max:1048576)
            in
            bytes + length)
        in
        let count = count + List.length rows in
        if count > 512 || bytes > 8 * 1024 * 1024 || encoded > 2 * 1024 * 1024
        then full "retained instruction memberships/manifests exceed limits"
        else Ok (count, bytes, encoded))
    in
    let%bind pool, _ =
      List.fold_result pool_rows ~init:(String.Map.empty, 0) ~f:(fun (pool, bytes) row ->
        let%bind fields = X.object_ row in
        let%bind id = X.required fields "id" P.Json_codec.string in
        let%bind contents = X.required fields "contents" P.Json_codec.string in
        let bytes = bytes + String.length contents in
        if bytes > 4 * 1024 * 1024
        then full "retained instruction pool exceeds limits"
        else (
          let%bind payload =
            S.Payload.create contents
            |> Result.map_error ~f:(fun _ ->
              P.Error.invalid_request "invalid instruction payload")
          in
          if (not (String.equal id (S.Payload.sha256 payload))) || Map.mem pool id
          then invalid "corrupt or duplicate instruction payload"
          else Ok (Map.set pool ~key:id ~data:payload, bytes)))
    in
    let%bind captures =
      List.map capture_rows ~f:(fun row -> C.of_json row ~pool ~checkpoint) |> Result.all
    in
    let%bind receipts =
      List.map receipt_rows ~f:(fun row ->
        let%bind fields = X.object_ row in
        let%bind id = X.required fields "id" P.Json_codec.string in
        let%bind receipt = X.required fields "receipt" R.of_json in
        if String.equal id (R.identity receipt)
        then Ok receipt
        else invalid "instruction receipt identity mismatch")
      |> Result.all
    in
    let value = { revision; project; skills; captures; pool; receipts } in
    let%map () = validate value in
    value
  with
  | Work_limit -> full "aggregate instruction restore work exhausted"
;;

let shape =
  X.shape_exn
    [ "revision", D.Shape.value
    ; "project", D.Shape.value
    ; "skills", D.Shape.value
    ; "captures", X.array_shape_exn ~identity_field:"id" C.shape
    ; ( "payloads"
      , X.array_shape_exn ~identity_field:"id" (X.fields_shape [ "id"; "contents" ]) )
    ; ( "receipts"
      , X.array_shape_exn
          ~identity_field:"id"
          (X.shape_exn [ "id", D.Shape.value; "receipt", R.shape ]) )
    ]
;;
