open! Core
module D = Document_schema
module F = Document_fields
module A = Prompt_artifact_store
module B = Prompt_program_bundle
module M = Prompt_manifest_document
module C = D.Extension_carrier

type t = A.Artifact.t C.t

let value = C.value
let invalid field reason = F.invalid field reason
let limited reason = Error (D.Error.Limit_exceeded reason)

let limits =
  D.Limits.create
    ~max_bytes:(16 * 1024 * 1024)
    ~max_depth:64
    ~max_fields:4096
    ~max_nodes:8192
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let store_result = function
  | Ok value -> Ok value
  | Error (Store_error.Document error | Admission_capacity error) -> Error error
  | Error error -> invalid "artifact" (Sexp.to_string_hum (Store_error.sexp_of_t error))
;;

(* Validate canonical padded RFC4648 text and calculate decoded length without
   allocating a decoded buffer. Aggregate admission precedes EVERY decode/hash. *)
module Bytes = struct
  let digit = function
    | 'A' .. 'Z' as c -> Some (Char.to_int c - Char.to_int 'A')
    | 'a' .. 'z' as c -> Some (26 + Char.to_int c - Char.to_int 'a')
    | '0' .. '9' as c -> Some (52 + Char.to_int c - Char.to_int '0')
    | '+' -> Some 62
    | '/' -> Some 63
    | _ -> None
  ;;

  let length encoded ~maximum =
    let size = String.length encoded in
    if size > (maximum + 2) / 3 * 4
    then limited "captured artifact base64 bytes"
    else if size % 4 <> 0
    then invalid "bytes" "expected canonical padded base64"
    else if size = 0
    then Ok 0
    else (
      let padding =
        if Char.equal encoded.[size - 1] '='
        then if Char.equal encoded.[size - 2] '=' then 2 else 1
        else 0
      in
      let content = size - padding in
      let rec scan index =
        if index >= content
        then Ok ()
        else (
          match digit encoded.[index] with
          | None -> invalid "bytes" "invalid base64 alphabet or padding"
          | Some _ -> scan (index + 1))
      in
      let open Result.Let_syntax in
      let%bind () = scan 0 in
      let%bind () =
        match padding, digit encoded.[content - 1] with
        | 2, Some last when last land 15 <> 0 ->
          invalid "bytes" "noncanonical base64 padding bits"
        | 1, Some last when last land 3 <> 0 ->
          invalid "bytes" "noncanonical base64 padding bits"
        | (0 | 1 | 2), Some _ -> Ok ()
        | _, (None | Some _) -> invalid "bytes" "invalid base64 padding"
      in
      let decoded = (size / 4 * 3) - padding in
      if decoded > maximum then limited "captured artifact decoded bytes" else Ok decoded)
  ;;

  let decode encoded =
    match Base64.decode encoded with
    | Ok decoded -> Ok decoded
    | Error _ -> invalid "bytes" "invalid base64"
  ;;

  let encode value = `String (Base64.encode_exn value)
end

let payload_list json =
  match json with
  | `Null -> Ok None
  | json -> Result.map (F.array json) ~f:Option.some
;;

let decode json =
  let open Result.Let_syntax in
  let%bind publication = F.required json "publication" (fun json -> Ok json) in
  let%bind revision_id =
    F.required publication "revision_id" (fun json ->
      F.protocol (Agent_protocol.Id.Prompt_revision.of_json json))
  in
  let%bind expected_sha256 = F.required publication "sha256" F.digest in
  let%bind manifest = F.required publication "bytes" F.string in
  let%bind _ = Bytes.length manifest ~maximum:M.max_bytes in
  let remaining_bytes = ref B.max_payload_bytes in
  let remaining_count = ref B.max_payload_count in
  let bytes json =
    let%bind encoded = F.string json in
    if !remaining_count <= 0
    then limited "captured artifact inventory count"
    else (
      let%bind length = Bytes.length encoded ~maximum:!remaining_bytes in
      remaining_bytes := !remaining_bytes - length;
      decr remaining_count;
      Ok encoded)
  in
  let%bind root = F.required json "root" bytes in
  let%bind raw_sources = F.required json "sources" F.array in
  let%bind sources =
    List.fold_result raw_sources ~init:[] ~f:(fun accumulated source ->
      let%bind relative_path = F.required source "relative_path" F.string in
      let%map contents = F.required source "bytes" bytes in
      (relative_path, contents) :: accumulated)
    |> Result.map ~f:List.rev
  in
  let pool name =
    let%bind raw = F.required json name payload_list in
    match raw with
    | None -> Ok None
    | Some raw ->
      List.fold_result raw ~init:[] ~f:(fun acc json ->
        let%map encoded = bytes json in
        encoded :: acc)
      |> Result.map ~f:(fun values -> Some (List.rev values))
  in
  let%bind programs = pool "program_payloads" in
  let%bind content = pool "content_payloads" in
  (* All inventory strings are now known to fit ONE budget. No decode, digest or
     manifest parser has run before this point. *)
  let%bind manifest = Bytes.decode manifest in
  let%bind publication =
    M.Publication.of_bytes manifest ~revision_id ~expected_sha256 |> store_result
  in
  let%bind root_chatmd = Bytes.decode root in
  let%bind sources =
    List.map sources ~f:(fun (relative_path, encoded) ->
      let%bind contents = Bytes.decode encoded in
      A.Source.create ~relative_path ~contents |> store_result)
    |> Result.all
  in
  let restore_pool = function
    | None -> Ok None
    | Some encoded ->
      List.map encoded ~f:(fun encoded ->
        let%bind contents = Bytes.decode encoded in
        B.Payload.create contents |> store_result)
      |> Result.all
      |> Result.map ~f:Option.some
  in
  let%bind program_payloads = restore_pool programs in
  let%bind content_payloads = restore_pool content in
  A.Artifact.restore
    ~publication
    ~root_chatmd
    ~sources
    ~program_payloads
    ~content_payloads
  |> store_result
;;

let encode (artifact : A.Artifact.t) =
  let pool = function
    | None -> `Null
    | Some bundle ->
      `Array
        (List.map (B.payloads bundle) ~f:(fun payload ->
           Bytes.encode (B.Payload.contents payload)))
  in
  `Object
    [ ( "publication"
      , `Object
          [ "revision_id", Agent_protocol.Id.Prompt_revision.to_json artifact.revision_id
          ; "sha256", `String artifact.manifest_sha256
          ; "bytes", Bytes.encode (M.Publication.bytes artifact.manifest_publication)
          ] )
    ; "root", Bytes.encode artifact.root_chatmd
    ; ( "sources"
      , `Array
          (List.map artifact.sources ~f:(fun source ->
             `Object
               [ "relative_path", `String source.A.Source.relative_path
               ; "bytes", Bytes.encode source.contents
               ])) )
    ; "program_payloads", pool artifact.program_bundle
    ; "content_payloads", pool artifact.content_bundle
    ]
;;

let shape =
  let v = D.Shape.value in
  let sources =
    D.Shape.array
      (F.shape [ "relative_path", v; "bytes", v ])
      ~identity_field:(Some "relative_path")
    |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
    |> Result.ok_or_failwith
  in
  F.shape
    [ "publication", F.shape [ "revision_id", v; "sha256", v; "bytes", v ]
    ; "root", v
    ; "sources", sources
    ; "program_payloads", v
    ; "content_payloads", v
    ]
;;

let validate (artifact : A.Artifact.t) =
  A.Artifact.restore
    ~publication:artifact.manifest_publication
    ~root_chatmd:artifact.root_chatmd
    ~sources:artifact.sources
    ~program_payloads:(Option.map artifact.program_bundle ~f:B.payloads)
    ~content_payloads:(Option.map artifact.content_bundle ~f:B.payloads)
  |> store_result
  |> Result.map ~f:(fun _ -> ())
;;

let codec =
  D.Domain_codec.create_validated
    ~limits
    ~kind:"ochat.captured_prompt_artifact"
    ~version:1
    ~shape
    ~supported_semantics:[]
    ~validate
    ~decode
    ~encode:(fun artifact -> Ok (encode artifact))
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let to_document t = D.Domain_codec.encode codec t
let of_document document = D.Domain_codec.decode codec document

let create artifact =
  let carrier = C.of_authored_value artifact in
  let%map.Result _ = to_document carrier in
  carrier
;;
