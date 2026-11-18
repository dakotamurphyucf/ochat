open! Core
module P = Agent_protocol
module D = Document_schema
module X = Persistence_codec

type t =
  { workspace : Workspace_instance.t
  ; permission_profile : string
  ; permission_profile_digest : string
  ; runtime_policy : string option
  ; configuration_revision : int64
  ; inference_selection : Inference.Selection.t
  ; capability_pins : (string * string) list
  }

let workspace t = t.workspace
let permission_profile t = t.permission_profile
let permission_profile_digest t = t.permission_profile_digest
let runtime_policy t = t.runtime_policy
let configuration_revision t = t.configuration_revision
let inference_selection t = t.inference_selection
let capability_pins t = t.capability_pins
let invalid message = Error (P.Error.invalid_request message)

let limits =
  D.Limits.create ~max_bytes:262144 ~max_depth:64 ~max_fields:4096 ~max_nodes:8192
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let bounded json =
  D.Json.validate ~limits json
  |> Result.map_error ~f:(fun error ->
    P.Error.invalid_request (Sexp.to_string_hum (D.Error.sexp_of_t error)))
;;

let digest text =
  Agent_store.Document_fields.digest (`String text)
  |> Result.map_error ~f:(fun error ->
    P.Error.invalid_request (Sexp.to_string_hum (D.Error.sexp_of_t error)))
  |> Result.map ~f:(fun _ -> ())
;;

let raw_json t =
  `Object
    [ "workspace", Workspace_instance.to_jsonaf t.workspace
    ; "permission_profile", `String t.permission_profile
    ; "permission_profile_digest", `String t.permission_profile_digest
    ; "runtime_policy", X.option_json X.text_json t.runtime_policy
    ; "configuration_revision", X.int64_json t.configuration_revision
    ; "inference_selection", Inference.Selection.to_json t.inference_selection
    ; ( "capability_pins"
      , `Array
          (List.map t.capability_pins ~f:(fun (name, fingerprint) ->
             `Object [ "name", `String name; "fingerprint", `String fingerprint ])) )
    ]
;;

let create
      ~workspace
      ~permission_profile
      ~permission_profile_digest
      ~runtime_policy
      ~configuration_revision
      ~inference_selection
      ~capability_pins
  =
  let open Result.Let_syntax in
  let%bind _ =
    List.fold_result
      capability_pins
      ~init:(0, String.Set.empty)
      ~f:(fun (count, seen) (name, fingerprint) ->
        if
          count >= 256
          || String.is_empty name
          || String.length name > 1024
          || Set.mem seen name
        then invalid "invalid or duplicate captured capability pin"
        else (
          let%map () = digest fingerprint in
          count + 1, Set.add seen name))
  in
  let%bind () = digest permission_profile_digest in
  let%bind () =
    Inference.Selection.validate inference_selection ~limits
    |> Result.map_error ~f:(fun error ->
      P.Error.invalid_request
        (Sexp.to_string_hum (Inference.Request.Error.sexp_of_t error)))
  in
  let t =
    { workspace
    ; permission_profile
    ; permission_profile_digest
    ; runtime_policy
    ; configuration_revision
    ; inference_selection
    ; capability_pins
    }
  in
  let%bind () = bounded (raw_json t) in
  let%bind _ = Workspace_instance.of_jsonaf (Workspace_instance.to_jsonaf workspace) in
  if String.is_empty permission_profile || Int64.(configuration_revision < 0L)
  then invalid "invalid captured parent policy or configuration revision"
  else Ok t
;;

let to_jsonaf t =
  let%map.Result _ =
    create
      ~workspace:t.workspace
      ~permission_profile:t.permission_profile
      ~permission_profile_digest:t.permission_profile_digest
      ~runtime_policy:t.runtime_policy
      ~configuration_revision:t.configuration_revision
      ~inference_selection:t.inference_selection
      ~capability_pins:t.capability_pins
  in
  raw_json t
;;

let of_jsonaf json =
  let open Result.Let_syntax in
  let%bind () = bounded json in
  let%bind fields = X.object_ json in
  let%bind workspace = X.required fields "workspace" Workspace_instance.of_jsonaf in
  let%bind permission_profile =
    X.required fields "permission_profile" P.Json_codec.string
  in
  let%bind permission_profile_digest =
    X.required fields "permission_profile_digest" P.Json_codec.string
  in
  let%bind runtime_policy =
    X.required fields "runtime_policy" (X.nullable P.Json_codec.string)
  in
  let%bind configuration_revision =
    X.required fields "configuration_revision" (function
      | `String text when String.length text <= 19 -> X.nonnegative_int64 (`String text)
      | _ -> invalid "invalid captured configuration revision")
  in
  let%bind inference_selection =
    X.required fields "inference_selection" (fun json ->
      Inference.Selection.of_json json ~limits
      |> Result.map_error ~f:(fun error ->
        P.Error.invalid_request
          (Sexp.to_string_hum (Inference.Request.Error.sexp_of_t error))))
  in
  let%bind pins =
    X.required fields "capability_pins" (function
      | `Array values -> Ok values
      | _ -> invalid "expected captured capability pin array")
  in
  let%bind capability_pins =
    List.fold_result pins ~init:(0, []) ~f:(fun (count, acc) json ->
      if count >= 256
      then invalid "too many captured capability pins"
      else (
        let%bind f = X.object_ json in
        let%bind name = X.required f "name" P.Json_codec.string in
        let%map fingerprint = X.required f "fingerprint" P.Json_codec.string in
        count + 1, (name, fingerprint) :: acc))
    |> Result.map ~f:(fun (_, pins) -> List.rev pins)
  in
  create
    ~workspace
    ~permission_profile
    ~permission_profile_digest
    ~runtime_policy
    ~configuration_revision
    ~inference_selection
    ~capability_pins
;;

let shape =
  let v = D.Shape.value in
  let pins =
    X.array_shape_exn ~identity_field:"name" (X.fields_shape [ "name"; "fingerprint" ])
  in
  X.shape_exn
    [ "workspace", Workspace_instance.shape
    ; "permission_profile", v
    ; "permission_profile_digest", v
    ; "runtime_policy", v
    ; "configuration_revision", v
    ; "inference_selection", v
    ; "capability_pins", pins
    ]
;;
