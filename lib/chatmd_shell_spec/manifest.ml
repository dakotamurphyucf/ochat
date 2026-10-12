open! Core
open Jsonaf.Export

type extension_script =
  { id : string
  ; kind : Chatmd_script_spec.kind
  ; source_sha256 : string
  ; surface_version : string
  ; entrypoint : string
  ; limits : Chatmd_script_spec.limits
  }
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type edge_kind =
  | Extends
  | Tool_runtime
  | Moderator_runtime
  | Chatml_extension
  | Executable_extension
  | Worker_runtime
  | External_backend
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type dependency =
  { from_id : string
  ; to_id : string
  ; kind : edge_kind
  }
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type payload =
  { encoding_version : string
  ; platform : Shell_spec.platform
  ; runtimes : Shell_spec.t list
  ; tools : Shell_tool_spec.t list
  ; moderator_runtime : string option
  ; extension_scripts : extension_script list
  ; dependencies : dependency list
  ; required_features : Feature.t list
  }
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

module Prepared_hook = Manifest_prepared_hook
module Authorization = Manifest_authorization

type t =
  { payload : payload
  ; canonical_json : string
  ; sha256 : string
  ; authorization : Authorization.t option
  }
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

let rec canonical_jsonaf = function
  | `Object fields ->
    `Object
      (List.map fields ~f:(fun (name, value) -> name, canonical_jsonaf value)
       |> List.sort ~compare:(fun (left, _) (right, _) -> String.compare left right))
  | `Array values -> `Array (List.map values ~f:canonical_jsonaf)
  | (`Null | `True | `False | `Number _ | `String _) as value -> value
;;

let sha256 value = Digestif.SHA256.(to_hex (digest_string value))

let logical_source (source : Source_ref.t) =
  { source with source_dir = Filename.dirname source.file; prompt_dir = "." }
;;

let canonical_payload payload =
  { payload with
    runtimes =
      List.map payload.runtimes ~f:(fun runtime ->
        { runtime with Shell_spec.source = logical_source runtime.source })
  ; tools =
      List.map payload.tools ~f:(fun tool ->
        { tool with Shell_tool_spec.source = logical_source tool.source })
  }
;;

let create payload =
  let canonical_json =
    canonical_payload payload |> jsonaf_of_payload |> canonical_jsonaf |> Jsonaf.to_string
  in
  { payload; canonical_json; sha256 = sha256 canonical_json; authorization = None }
;;

let resource_sha256 t =
  match t.authorization with
  | None -> t.sha256
  | Some authorization -> Authorization.resource_sha256 authorization
;;

let encoding_version t =
  match t.authorization with
  | None -> t.payload.encoding_version
  | Some _ -> "ochat.shell.execution.v2"
;;

let validate_hooks t hooks ~checkpoint =
  let module Key = struct
    module T = struct
      type t = string [@@deriving sexp_of]

      let compare left right = Authorization.compare_string left right ~checkpoint
    end

    include T
    include Comparator.Make (T)
  end
  in
  let open Or_error.Let_syntax in
  let%bind declarations =
    List.fold_result
      t.payload.extension_scripts
      ~init:(Map.empty (module Key))
      ~f:(fun declarations script ->
        checkpoint ();
        match Map.add declarations ~key:script.id ~data:script with
        | `Ok declarations -> Ok declarations
        | `Duplicate -> Or_error.error_string "duplicate script in resource manifest")
  in
  let%bind represented =
    List.fold_result
      hooks
      ~init:(Map.empty (module Key))
      ~f:(fun represented hook ->
        checkpoint ();
        match Map.find declarations (Prepared_hook.id hook) with
        | None -> Or_error.error_string "prepared hook is absent from resource manifest"
        | Some script ->
          if not (Chatmd_script_spec.equal_kind script.kind (Prepared_hook.kind hook))
          then
            Or_error.error_string "prepared hook kind differs from resource declaration"
          else
            Ok
              (Map.update represented script.id ~f:(fun found ->
                 Option.value found ~default:false
                 || Authorization.compare_string
                      (Prepared_hook.entrypoint hook)
                      script.entrypoint
                      ~checkpoint
                    = 0)))
  in
  if
    Map.for_all represented ~f:(fun found ->
      checkpoint ();
      found)
  then Ok ()
  else Or_error.error_string "prepared hook is missing its declared default entrypoint"
;;

let with_prepared_hooks t hooks ~max_bytes ~checkpoint =
  let open Or_error.Let_syntax in
  checkpoint ();
  match t.authorization with
  | Some _ -> Or_error.error_string "shell manifest already has prepared authorization"
  | None ->
    if List.is_empty hooks
    then Ok t
    else (
      let%bind canonical_bytes =
        Authorization.canonical_size
          hooks
          ~resource_json:t.canonical_json
          ~max_bytes
          ~checkpoint
      in
      let%bind () = validate_hooks t hooks ~checkpoint in
      let%bind authorization =
        Authorization.create ~resource_sha256:t.sha256 hooks ~checkpoint
      in
      let canonical_json, sha256 =
        Authorization.encode
          authorization
          ~resource_json:t.canonical_json
          ~canonical_bytes
          ~checkpoint
      in
      Ok { t with canonical_json; sha256; authorization = Some authorization })
;;
