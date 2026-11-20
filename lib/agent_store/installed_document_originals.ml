open Core
module D = Document_schema
module F = Document_fields
module B = Prompt_program_bundle

let digest_bytes text = Digestif.SHA256.(digest_string text |> to_hex)
let kind = "store.installed_document_originals"
let invalid reason = Error (Store_error.Corrupt reason)

module Occurrence = struct
  type t = string

  let of_digest digest = F.store (F.digest (`String digest))
  let digest t = t
end

module Entry = struct
  type t =
    { occurrence : Occurrence.t
    ; sha256 : string
    ; byte_length : int
    ; media_type : string
    }

  let occurrence t = t.occurrence
  let sha256 t = t.sha256
  let byte_length t = t.byte_length
  let media_type t = t.media_type

  let create ~occurrence ~sha256 ~byte_length ~media_type =
    let open Result.Let_syntax in
    let%bind _ = F.store (F.digest (`String sha256)) in
    if
      byte_length < 0
      || byte_length > B.max_payload_bytes
      || not
           (List.mem
              [ "text/html"; "application/octet-stream" ]
              media_type
              ~equal:String.equal)
    then invalid "invalid installed document original descriptor"
    else Ok { occurrence; sha256; byte_length; media_type }
  ;;

  let to_json t =
    `Object
      [ "occurrence", `String (Occurrence.digest t.occurrence)
      ; "sha256", `String t.sha256
      ; "byte_length", F.decimal_json (Int64.of_int t.byte_length)
      ; "media_type", `String t.media_type
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind occurrence = F.store (F.required json "occurrence" F.digest) in
    let%bind occurrence = Occurrence.of_digest occurrence in
    let%bind sha256 = F.store (F.required json "sha256" F.digest) in
    let%bind length = F.store (F.required json "byte_length" F.decimal) in
    let%bind media_type = F.store (F.required json "media_type" F.string) in
    if Int64.(length < 0L || length > of_int B.max_payload_bytes)
    then invalid "invalid installed document original length"
    else create ~occurrence ~sha256 ~byte_length:(Int64.to_int_exn length) ~media_type
  ;;
end

module Manifest = struct
  type t =
    { entries : Entry.t String.Map.t
    ; bytes : string
    ; sha256 : string
    }

  let max_bytes = 262144
  let limits = F.limits ~max_bytes |> Result.ok |> Option.value_exn
  let bytes t = t.bytes
  let sha256 t = t.sha256
  let find t ~occurrence = Map.find t.entries (Occurrence.digest occurrence)

  let index entries =
    let open Result.Let_syntax in
    let%bind entries, _, _ =
      List.fold_result
        entries
        ~init:(String.Map.empty, String.Map.empty, 0)
        ~f:(fun (entries, payloads, bytes) entry ->
          let key = Occurrence.digest (Entry.occurrence entry) in
          let digest = Entry.sha256 entry in
          let length = Entry.byte_length entry in
          if Map.length entries >= B.max_payload_count || Map.mem entries key
          then invalid "excess or duplicate installed document original occurrences"
          else (
            let entries = Map.set entries ~key ~data:entry in
            match Map.find payloads digest with
            | Some previous when Int.equal previous length -> Ok (entries, payloads, bytes)
            | Some _ -> invalid "conflicting installed document original lengths"
            | None when length > B.max_payload_bytes - bytes ->
              invalid "installed document originals exceed the payload limit"
            | None ->
              Ok (entries, Map.set payloads ~key:digest ~data:length, bytes + length)))
    in
    if Map.is_empty entries
    then invalid "empty installed document original inventory"
    else Ok entries
  ;;

  let create values =
    let open Result.Let_syntax in
    let%bind entries = index values in
    let payload =
      `Object [ "entries", `Array (List.map (Map.data entries) ~f:Entry.to_json) ]
    in
    let%map document = F.store (D.Document.create ~limits ~kind ~version:1 ~payload) in
    let bytes = D.Document.to_string document in
    { entries; bytes; sha256 = digest_bytes bytes }
  ;;

  let of_bytes bytes ~expected_sha256 =
    let open Result.Let_syntax in
    if
      String.length bytes > max_bytes
      || not (String.equal (digest_bytes bytes) expected_sha256)
    then invalid "installed document original manifest digest differs"
    else (
      let%bind document = F.store (D.Document.decode ~limits bytes) in
      let%bind () = F.store (F.expect document ~kind ~version:1) in
      let%bind values =
        F.store (F.required (D.Document.payload document) "entries" F.array)
      in
      if Stdlib.List.compare_length_with values B.max_payload_count > 0
      then invalid "installed document original occurrence limit"
      else (
        let%bind values = Result.all (List.map values ~f:Entry.of_json) in
        let%map entries = index values in
        { entries; bytes; sha256 = expected_sha256 }))
  ;;
end

type t =
  { manifest : Manifest.t
  ; payloads : B.Payload.t list
  }

let manifest t = t.manifest
let payloads t = t.payloads

let create originals =
  let open Result.Let_syntax in
  if Stdlib.List.compare_length_with originals B.max_payload_count > 0
  then invalid "installed document original occurrence limit"
  else (
    let%bind manifest = Manifest.create (List.map originals ~f:fst) in
    let%map payloads =
      List.fold_result
        originals
        ~init:String.Map.empty
        ~f:(fun payloads (entry, contents) ->
          let digest = Entry.sha256 entry in
          if not (Int.equal (String.length contents) (Entry.byte_length entry))
          then invalid "installed document original length differs"
          else (
            match Map.find payloads digest with
            | Some stored when String.equal contents (B.Payload.contents stored) ->
              Ok payloads
            | Some _ -> invalid "installed document original digest collision"
            | None ->
              let%bind stored = B.Payload.create contents in
              if String.equal digest (B.Payload.sha256 stored)
              then Ok (Map.set payloads ~key:digest ~data:stored)
              else invalid "installed document original digest differs"))
      |> Result.map ~f:Map.data
    in
    { manifest; payloads })
;;
