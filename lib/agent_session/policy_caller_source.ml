open! Core
module P = Agent_protocol
module X = Persistence_codec
module D = Document_schema

let invalid message = Error (P.Error.invalid_request message)

module Unmoderated = struct
  type t =
    { generation : int
    ; prompt_revision_id : P.Id.Prompt_revision.t
    ; configuration_revision : int64
    ; artifact_manifest_sha256 : string
    }
  [@@deriving equal, sexp_of]

  let create
        ~generation
        ~prompt_revision_id
        ~configuration_revision
        ~artifact_manifest_sha256
    =
    let valid_digest =
      String.length artifact_manifest_sha256 = 64
      && String.for_all artifact_manifest_sha256 ~f:(function
        | '0' .. '9' | 'a' .. 'f' -> true
        | _ -> false)
    in
    if generation < 0 || Int64.(configuration_revision < 0L) || not valid_digest
    then invalid "invalid unmoderated policy caller source"
    else
      Ok
        { generation
        ; prompt_revision_id
        ; configuration_revision
        ; artifact_manifest_sha256
        }
  ;;
end

type t =
  | Installed of P.Run_source.t
  | Unmoderated of Unmoderated.t
[@@deriving equal, sexp_of]

type view = t =
  | Installed of P.Run_source.t
  | Unmoderated of Unmoderated.t

let installed source =
  let%map.Result () = P.Run_source.validate source in
  Installed source
;;

let unmoderated source = Unmoderated source
let view t = t

let generation = function
  | Installed source -> source.generation
  | Unmoderated source -> source.generation
;;

let matches_installed t source =
  match t with
  | Installed actual -> P.Run_source.equal actual source
  | Unmoderated _ -> false
;;

let to_json = function
  | Installed source ->
    `Object [ "kind", `String "installed"; "source", P.Run_source.to_json source ]
  | Unmoderated source ->
    `Object
      [ "kind", `String "unmoderated"
      ; "generation", X.host_counter_to_json source.generation
      ; "prompt_revision_id", P.Id.Prompt_revision.to_json source.prompt_revision_id
      ; "configuration_revision", X.int64_json source.configuration_revision
      ; "artifact_manifest_sha256", `String source.artifact_manifest_sha256
      ]
;;

let of_json raw =
  let open Result.Let_syntax in
  let%bind fields = X.object_ raw in
  let%bind kind = X.required fields "kind" P.Json_codec.string in
  match kind with
  | "installed" ->
    let%bind source = X.required fields "source" P.Run_source.of_json in
    installed source
  | "unmoderated" ->
    let%bind generation = X.required fields "generation" X.host_counter_of_json in
    let%bind prompt_revision_id =
      X.required fields "prompt_revision_id" P.Id.Prompt_revision.of_json
    in
    let%bind configuration_revision =
      X.required fields "configuration_revision" X.nonnegative_int64
    in
    let%bind artifact_manifest_sha256 =
      X.required fields "artifact_manifest_sha256" P.Json_codec.string
    in
    let%map source =
      Unmoderated.create
        ~generation
        ~prompt_revision_id
        ~configuration_revision
        ~artifact_manifest_sha256
    in
    unmoderated source
  | _ -> invalid "unknown policy caller source kind"
;;

let shape =
  X.tagged_shape_exn
    ~discriminator:"kind"
    [ ( "installed"
      , X.shape_exn [ "kind", D.Shape.value; "source", Run_record_shapes.source ] )
    ; ( "unmoderated"
      , X.fields_shape
          [ "kind"
          ; "generation"
          ; "prompt_revision_id"
          ; "configuration_revision"
          ; "artifact_manifest_sha256"
          ] )
    ]
;;
