open Core
module P = Agent_protocol
module F = Document_fields

module Source = struct
  type t =
    { relative_path : string
    ; sha256 : string
    }
  [@@deriving equal, sexp_of]
end

type t =
  { version : int
  ; revision_id : P.Id.Prompt_revision.t
  ; prompt_definition_id : P.Id.Prompt_definition.t option
  ; canonical_source : string option
  ; root_relative_path : string
  ; root_sha256 : string
  ; sources : Source.t list
  ; parser_schema_version : int
  ; runtime_schema_version : int
  ; shell_manifest_sha256 : string option
  ; created_at : P.Timestamp.t
  }
[@@deriving equal, sexp_of]

let valid_relative_path path =
  (not (String.is_empty path))
  && (not (String.mem path '\000'))
  && (not (Filename.is_absolute path))
  && List.for_all (String.split path ~on:'/') ~f:(fun component ->
    (not (String.is_empty component))
    && (not (String.equal component "."))
    && not (String.equal component ".."))
;;

let validate t =
  let open Result.Let_syntax in
  let%bind _ =
    F.protocol
      (P.Id.Prompt_revision.of_string (P.Id.Prompt_revision.to_string t.revision_id))
  in
  let%bind _ =
    match t.prompt_definition_id with
    | None -> Ok None
    | Some id ->
      F.protocol (P.Id.Prompt_definition.of_string (P.Id.Prompt_definition.to_string id))
      |> Result.map ~f:Option.some
  in
  let%bind _ = F.protocol (P.Timestamp.of_string (P.Timestamp.to_string t.created_at)) in
  let%bind _ = F.digest (`String t.root_sha256) in
  let%bind _ =
    match t.shell_manifest_sha256 with
    | None -> Ok None
    | Some digest -> F.digest (`String digest) |> Result.map ~f:Option.some
  in
  let%bind () =
    if
      Int.equal t.version 1
      && t.parser_schema_version >= 0
      && t.runtime_schema_version >= 0
      && valid_relative_path t.root_relative_path
      && Option.for_all t.canonical_source ~f:(fun path -> not (String.mem path '\000'))
    then Ok ()
    else F.invalid "manifest" "invalid manifest version, paths or schema counters"
  in
  let%bind () =
    if
      List.contains_dup t.sources ~compare:(fun a b ->
        String.compare a.Source.relative_path b.relative_path)
      || List.exists t.sources ~f:(fun source ->
        String.equal source.Source.relative_path t.root_relative_path)
    then F.invalid "sources" "duplicate or root-colliding source paths"
    else Ok ()
  in
  List.fold_result t.sources ~init:() ~f:(fun () source ->
    let%bind () =
      if valid_relative_path source.Source.relative_path
      then Ok ()
      else F.invalid "relative_path" "unsafe prompt source path"
    in
    Result.map (F.digest (`String source.sha256)) ~f:(fun _ -> ()))
;;
