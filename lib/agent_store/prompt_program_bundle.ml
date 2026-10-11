open Core
module D = Document_schema
module F = Document_fields

let max_payload_count = 256
let max_payload_bytes = 8 * 1024 * 1024

let metadata_limits =
  F.limits ~max_bytes:262144
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

module Payload = struct
  type t =
    { contents : string
    ; sha256 : string
    }

  let create contents =
    if String.length contents > max_payload_bytes
    then Error (Store_error.Document (D.Error.Limit_exceeded "program payload bytes"))
    else Ok { contents; sha256 = Digestif.SHA256.(digest_string contents |> to_hex) }
  ;;

  let contents t = t.contents
  let sha256 t = t.sha256
  let byte_length t = String.length t.contents
end

module Manifest = struct
  type t =
    { metadata : D.Document.t
    ; payloads : (string * int) list
    ; payload_bytes : int
    }

  let metadata t = t.metadata
  let payloads t = t.payloads
  let payload_bytes t = t.payload_bytes

  let equal a b =
    D.Json.equal (D.Document.json a.metadata) (D.Document.json b.metadata)
    && List.equal [%equal: string * int] a.payloads b.payloads
  ;;

  let sexp_of_t t =
    [%sexp
      { metadata = (D.Document.to_string t.metadata : string)
      ; payloads = (t.payloads : (string * int) list)
      }]
  ;;

  let admit metadata payloads =
    let open Result.Let_syntax in
    let%bind () = D.Document.validate metadata ~limits:metadata_limits in
    let%bind total, _ =
      List.fold_result payloads ~init:(0, 0) ~f:(fun (total, count) (_, bytes) ->
        if count >= max_payload_count || bytes < 0 || bytes > max_payload_bytes - total
        then Error (D.Error.Limit_exceeded "program payload inventory")
        else Ok (total + bytes, count + 1))
    in
    let%bind () =
      List.fold_result payloads ~init:String.Set.empty ~f:(fun seen (sha256, _) ->
        let%bind _ = F.digest (`String sha256) in
        if Set.mem seen sha256
        then F.invalid "payloads" "duplicate program payload"
        else Ok (Set.add seen sha256))
      |> Result.map ~f:(fun _ -> ())
    in
    Ok
      { metadata
      ; payloads = List.sort payloads ~compare:[%compare: string * int]
      ; payload_bytes = total
      }
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind () = D.Json.validate ~limits:metadata_limits json in
    let%bind metadata = F.required json "metadata" (F.document ~limits:metadata_limits) in
    let%bind raw = F.required json "payloads" F.array in
    let rec decode remaining count total acc =
      match remaining with
      | [] -> admit metadata (List.rev acc)
      | value :: tail ->
        if count >= max_payload_count
        then Error (D.Error.Limit_exceeded "program payload count")
        else (
          let%bind sha256 = F.required value "sha256" F.digest in
          let%bind bytes = F.required value "byte_length" F.decimal in
          let remaining = Int64.of_int (max_payload_bytes - total) in
          if Int64.(bytes < 0L || bytes > remaining)
          then Error (D.Error.Limit_exceeded "program payload bytes")
          else (
            let bytes = Int64.to_int_exn bytes in
            decode tail (count + 1) (total + bytes) ((sha256, bytes) :: acc)))
    in
    decode raw 0 0 []
  ;;

  let shape =
    let payload = F.shape [ "sha256", D.Shape.value; "byte_length", D.Shape.value ] in
    let payloads =
      D.Shape.array payload ~identity_field:(Some "sha256")
      |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
      |> Result.ok_or_failwith
    in
    F.shape [ "metadata", D.Shape.value; "payloads", payloads ]
  ;;

  let to_json t =
    `Object
      [ "metadata", D.Document.json t.metadata
      ; ( "payloads"
        , `Array
            (List.map t.payloads ~f:(fun (sha256, byte_length) ->
               `Object
                 [ "sha256", `String sha256
                 ; "byte_length", F.decimal_json (Int64.of_int byte_length)
                 ])) )
      ]
  ;;
end

type t =
  { manifest : Manifest.t
  ; payloads : Payload.t list
  }

let manifest t = t.manifest
let payloads t = t.payloads

let create ~metadata ~payloads =
  let open Result.Let_syntax in
  let%bind () =
    List.fold_result payloads ~init:(0, 0) ~f:(fun (count, bytes) payload ->
      let length = Payload.byte_length payload in
      if count >= max_payload_count || length > max_payload_bytes - bytes
      then
        Error (Store_error.Document (D.Error.Limit_exceeded "program payload inventory"))
      else Ok (count + 1, bytes + length))
    |> Result.map ~f:(fun _ -> ())
  in
  let%bind manifest =
    Manifest.admit
      metadata
      (List.map payloads ~f:(fun p -> Payload.sha256 p, Payload.byte_length p))
    |> F.store
  in
  Ok
    { manifest
    ; payloads =
        List.sort payloads ~compare:(fun a b ->
          String.compare (Payload.sha256 a) (Payload.sha256 b))
    }
;;

let restore expected payloads =
  let open Result.Let_syntax in
  let%bind result = create ~metadata:(Manifest.metadata expected) ~payloads in
  if Manifest.equal expected result.manifest
  then Ok result
  else Error (Store_error.Corrupt "program payloads differ from manifest inventory")
;;
