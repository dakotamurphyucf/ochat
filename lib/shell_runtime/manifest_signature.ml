open! Core
open Jsonaf.Export
module M = Chatmd_shell_spec.Manifest

type payload =
  { canonical_manifest_sha256 : string
  ; encoding_version : string
  ; builtin_versions : (string * string) list
  ; issuer : string
  ; audience : string list
  ; issued_at_unix : int64
  ; expires_at_unix : int64 option
  ; imported_source_sha256 : (string * string) list
  }
[@@deriving sexp, compare, equal, jsonaf]

type t =
  { key_id : string
  ; algorithm : string
  ; payload : payload
  ; signature_base64 : string
  }
[@@deriving sexp, compare, equal, jsonaf]

type public_key =
  { key_id : string
  ; public_key_base64 : string
  }
[@@deriving sexp, compare, equal, jsonaf]

type error =
  { code : string
  ; message : string
  }
[@@deriving sexp, compare, equal]

let builtin_versions manifest =
  List.filter_map manifest.M.payload.runtimes ~f:(fun runtime ->
    Option.map runtime.Chatmd_shell_spec.Shell_spec.resolved_profile ~f:(fun profile ->
      Chatmd_shell_spec.Shell_spec.Runtime_id.to_string runtime.id, profile))
  |> List.dedup_and_sort ~compare:[%compare: string * string]
;;

let imported_sources manifest =
  let runtime_sources =
    List.map manifest.M.payload.runtimes ~f:(fun runtime -> runtime.Chatmd_shell_spec.Shell_spec.source)
  in
  let tool_sources = List.map manifest.payload.tools ~f:(fun tool -> tool.Chatmd_shell_spec.Shell_tool_spec.source) in
  runtime_sources @ tool_sources
  |> List.map ~f:(fun source -> source.Chatmd_shell_spec.Source_ref.file, source.source_sha256)
  |> List.dedup_and_sort ~compare:[%compare: string * string]
;;

let payload ~manifest ~issuer ~audience ~issued_at_unix ~expires_at_unix =
  { canonical_manifest_sha256 = manifest.M.sha256
  ; encoding_version = manifest.payload.encoding_version
  ; builtin_versions = builtin_versions manifest
  ; issuer
  ; audience = List.dedup_and_sort audience ~compare:String.compare
  ; issued_at_unix
  ; expires_at_unix
  ; imported_source_sha256 = imported_sources manifest
  }
;;

let canonical_payload payload =
  payload |> jsonaf_of_payload |> Jsonaf.to_string
;;

let error code message = Error { code; message }

let decode_base64 code value =
  try Ok (Base64.decode_exn value) with
  | exn -> error code (Exn.to_string exn)
;;

let key public_keys key_id =
  List.find public_keys ~f:(fun key -> String.equal key.key_id key_id)
  |> Result.of_option
       ~error:{ code = "shell.signature_key_missing"; message = "unknown public key: " ^ key_id }
;;

let expected_payload manifest signed =
  payload
    ~manifest
    ~issuer:signed.issuer
    ~audience:signed.audience
    ~issued_at_unix:signed.issued_at_unix
    ~expires_at_unix:signed.expires_at_unix
;;

let validate_claims ~now_unix ~audience ~manifest signature =
  if not (String.Caseless.equal signature.algorithm "ed25519")
  then error "shell.signature_algorithm" "only ed25519 signatures are accepted"
  else if not (List.mem signature.payload.audience audience ~equal:String.equal)
  then error "shell.signature_audience" "signature audience does not include this runtime"
  else if Int64.(signature.payload.issued_at_unix > now_unix)
  then error "shell.signature_not_yet_valid" "signature issue time is in the future"
  else if Option.exists signature.payload.expires_at_unix ~f:(fun expiry -> Int64.(expiry < now_unix))
  then error "shell.signature_expired" "signature has expired"
  else if not (equal_payload signature.payload (expected_payload manifest signature.payload))
  then error "shell.signature_payload_mismatch" "manifest, profile version, or imported source digest changed"
  else Ok ()
;;

let verify ~now_unix ~audience ~public_keys ~manifest signature =
  let open Result.Let_syntax in
  let%bind () = validate_claims ~now_unix ~audience ~manifest signature in
  let%bind key = key public_keys signature.key_id in
  let%bind key_octets = decode_base64 "shell.signature_public_key" key.public_key_base64 in
  let%bind signature_octets = decode_base64 "shell.signature_encoding" signature.signature_base64 in
  let%bind public_key =
    Mirage_crypto_ec.Ed25519.pub_of_octets key_octets
    |> Result.map_error ~f:(fun _ ->
      { code = "shell.signature_public_key"; message = "invalid Ed25519 public key" })
  in
  if
    Mirage_crypto_ec.Ed25519.verify
      ~key:public_key
      signature_octets
      ~msg:(canonical_payload signature.payload)
  then Ok ()
  else error "shell.signature_invalid" "Ed25519 signature verification failed"
;;

let load parse ~fs ~path code =
  try Eio.Path.(fs / path) |> Eio.Path.load |> Jsonaf.of_string |> parse |> Result.return with
  | exn -> error code (Exn.to_string exn)
;;

let load_signature ~fs ~path = load t_of_jsonaf ~fs ~path "shell.signature_load_failed"

module Public_keys = struct
  type t = public_key list [@@deriving jsonaf]
end

let load_public_keys ~fs ~path =
  load Public_keys.t_of_jsonaf ~fs ~path "shell.signature_keys_load_failed"
;;
