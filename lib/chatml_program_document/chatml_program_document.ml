open Core
module Prepared = Chatml_prepared_program
module Captured = Chatml_source
module Schema = Document_schema
module Metadata = Program_metadata

module Error = struct
  type t =
    | Document of Schema.Error.t
    | Prepared of Prepared.Error.t
    | Invalid_metadata of string list
    | Missing_source of Captured.Id.t
    | Duplicate_source of Captured.Id.t
    | Unexpected_source of Captured.Id.t
    | Revision_mismatch of Captured.Id.t
    | Fingerprint_mismatch
    | Contract_mismatch
    | Limit_exceeded
end

module Source = struct
  type t = Metadata.source

  let id (t : t) = t.id
  let digest (t : t) = t.digest
  let byte_length (t : t) = t.byte_length
end

type t =
  { document : Schema.Document.t
  ; metadata : Metadata.t
  }

let to_document t = t.document
let fingerprint t = t.metadata.fingerprint
let sources t = t.metadata.sources
let document_error result = Result.map_error result ~f:(fun error -> Error.Document error)
let kind = "chatml.provided-program"

let of_document document ~limits ~checkpoint =
  let open Result.Let_syntax in
  checkpoint ();
  let%bind () = document_error (Schema.Document.validate document ~limits) in
  let%bind conversion =
    document_error
      (Schema.Conversion.create
         ~limits
         ~targets:[ kind, 1 ]
         ~max_steps:1
         ~max_operations:1
         ~steps:[])
  in
  let%bind document = document_error (Schema.Conversion.upgrade conversion document) in
  let%bind () =
    match Schema.Document.required_semantics document with
    | [] -> Ok ()
    | semantics :: _ ->
      Error (Error.Document (Schema.Error.Required_semantics_unknown semantics))
  in
  checkpoint ();
  let%map metadata =
    document_error (Metadata.decode (Schema.Document.payload document) ~checkpoint)
  in
  { document; metadata }
;;

let capture prepared ~limits ~checkpoint =
  let open Result.Let_syntax in
  let%bind document =
    document_error (Program_encoding.document prepared ~limits ~checkpoint)
  in
  of_document document ~limits ~checkpoint
;;

(* A lower bound on existing Prepared canonical metadata, using only encoded
   scalar payload lengths and one byte per numeric/record unit. It is not a
   competing canonical encoder: exact admission/fingerprinting stays Prepared's.
   Reject before building source maps, decoded-location or binding lists. *)
let preflight metadata sources ~max_input_bytes ~checkpoint =
  if max_input_bytes <= 0
  then Error Error.Limit_exceeded
  else
    let open Result.Let_syntax in
    let add used bytes =
      checkpoint ();
      if bytes < 0 || bytes > max_input_bytes - used
      then Error Error.Limit_exceeded
      else Ok (used + bytes)
    in
    let text used value = add used (String.length value) in
    let digest used value = text used (Captured.Digest.to_hex value) in
    let id used value =
      let%bind used = text used (Captured.Id.package_namespace value) in
      text used (Captured.Id.relative_path value)
    in
    let location used (value : Metadata.location) =
      let%bind used = id used value.source in
      let%bind used = digest used value.digest in
      add used 2
    in
    let optional used value ~f =
      match value with
      | None -> add used 1
      | Some value -> f used value
    in
    let items used values ~f =
      List.fold_result values ~init:used ~f:(fun used value ->
        let%bind used = add used 1 in
        f used value)
    in
    let%bind used = digest 0 metadata.Metadata.compiler_contract in
    let%bind used = text used metadata.environment_name in
    let%bind used = digest used metadata.required_contract in
    let%bind used = optional used metadata.environment_declared_at ~f:location in
    let%bind used = id used metadata.entry in
    let%bind used =
      match metadata.provenance with
      | Prepared.Provenance.Implicit -> add used 1
      | Project { declaration; lock } ->
        let%bind used = id used declaration in
        optional used lock ~f:id
    in
    let%bind used =
      items used metadata.sources ~f:(fun used (source : Metadata.source) ->
        let%bind used = id used source.id in
        let%bind used = digest used source.digest in
        add used 1)
    in
    let%bind used =
      items used metadata.definitions ~f:(fun used (value : Metadata.definition) ->
        let%bind used = id used value.implementation in
        let%bind used = optional used value.interface ~f:id in
        location used value.declared_at)
    in
    let%bind used =
      items used metadata.scopes ~f:(fun used (scope : Metadata.scope) ->
        let%bind used = id used scope.source in
        let%bind used = optional used scope.selected_at ~f:location in
        items used scope.bindings ~f:(fun used (binding : Metadata.binding) ->
          let%bind used = text used binding.name in
          let%bind used = id used binding.implementation in
          location used binding.declared_at))
    in
    let maximum_count = List.length metadata.sources in
    let%map _ =
      List.fold_result
        sources
        ~init:(0, used, 0)
        ~f:(fun (count, used, actual_minimum) source ->
          checkpoint ();
          if count >= maximum_count
          then Error Error.Limit_exceeded
          else (
            let%bind actual_minimum = id actual_minimum (Captured.id source) in
            let%bind actual_minimum = add actual_minimum 65 in
            let%bind actual_minimum = add actual_minimum (Captured.byte_length source) in
            let%map used = add used (Captured.byte_length source) in
            count + 1, used, actual_minimum))
    in
    ()
;;

let inventory metadata sources ~checkpoint =
  let open Result.Let_syntax in
  let compare left right =
    checkpoint ();
    Captured.Id.compare left right
  in
  let expected =
    List.sort metadata.Metadata.sources ~compare:(fun (left : Metadata.source) right ->
      compare left.id right.id)
  in
  let actual =
    List.sort sources ~compare:(fun left right ->
      compare (Captured.id left) (Captured.id right))
  in
  let rec unique values ~id =
    checkpoint ();
    match values with
    | [] | [ _ ] -> Ok ()
    | first :: (second :: _ as rest) ->
      if Captured.Id.equal (id first) (id second)
      then Error (Error.Duplicate_source (id first))
      else unique rest ~id
  in
  let%bind () = unique expected ~id:(fun (source : Metadata.source) -> source.id) in
  let%bind () = unique actual ~id:Captured.id in
  let rec reconcile expected actual =
    checkpoint ();
    match expected, actual with
    | [], [] -> Ok ()
    | [], source :: _ -> Error (Error.Unexpected_source (Captured.id source))
    | source :: _, [] -> Error (Error.Missing_source source.Metadata.id)
    | expected :: expected_rest, source :: rest ->
      let order = compare expected.Metadata.id (Captured.id source) in
      if order < 0
      then Error (Error.Missing_source expected.id)
      else if order > 0
      then Error (Error.Unexpected_source (Captured.id source))
      else if
        expected.byte_length <> Captured.byte_length source
        || not (Captured.Digest.equal expected.digest (Captured.digest source))
      then Error (Error.Revision_mismatch expected.id)
      else reconcile expected_rest rest
  in
  let%bind () = reconcile expected actual in
  (* Maps and reconstructed locations are allocated only after duplicate, surplus,
     missing and revision checks over the already bounded inventory. *)
  Ok
    (List.fold
       actual
       ~init:(Map.empty (module Captured.Id))
       ~f:(fun map source ->
         checkpoint ();
         Map.set map ~key:(Captured.id source) ~data:source))
;;

let restore t ~sources ~compiler_contract ~max_input_bytes ~checkpoint =
  let open Result.Let_syntax in
  checkpoint ();
  if not (Captured.Digest.equal compiler_contract t.metadata.compiler_contract)
  then Error Error.Contract_mismatch
  else (
    let%bind () = preflight t.metadata sources ~max_input_bytes ~checkpoint in
    let%bind source_map = inventory t.metadata sources ~checkpoint in
    let location (value : Metadata.location) =
      checkpoint ();
      match Map.find source_map value.source with
      | None -> Error (Error.Missing_source value.source)
      | Some source ->
        if not (Captured.Digest.equal value.digest (Captured.digest source))
        then Error (Error.Revision_mismatch value.source)
        else (
          match
            Captured.locate
              source
              ~start_byte:value.start_byte
              ~end_byte:value.end_byte
              ~checkpoint
          with
          | Ok location -> Ok location
          | Error _ -> Error (Error.Invalid_metadata [ "location" ]))
    in
    let optional value ~f =
      match value with
      | None -> Ok None
      | Some value -> Result.map (f value) ~f:Option.some
    in
    let invalid path result =
      Result.map_error result ~f:(fun _ -> Error.Invalid_metadata path)
    in
    let%bind declared_at = optional t.metadata.environment_declared_at ~f:location in
    let%bind environment =
      invalid
        [ "environment" ]
        (Prepared.Environment.create
           ~name:t.metadata.environment_name
           ~required_contract:t.metadata.required_contract
           ~declared_at)
    in
    let%bind definitions =
      List.map t.metadata.definitions ~f:(fun (value : Metadata.definition) ->
        let%bind declared_at = location value.declared_at in
        invalid
          [ "definitions" ]
          (Prepared.Definition.create
             ~implementation:value.implementation
             ~interface:value.interface
             ~declared_at))
      |> Result.all
    in
    let%bind scopes =
      List.map t.metadata.scopes ~f:(fun (value : Metadata.scope) ->
        let%bind selected_at = optional value.selected_at ~f:location in
        let%bind bindings =
          List.map value.bindings ~f:(fun (binding : Metadata.binding) ->
            let%map declared_at = location binding.declared_at in
            { Prepared.Scope.name = binding.name
            ; module_id = Prepared.Module_id.of_implementation binding.implementation
            ; declared_at
            })
          |> Result.all
        in
        invalid
          [ "scopes" ]
          (Prepared.Scope.create value.source ~bindings ~selected_at ~checkpoint))
      |> Result.all
    in
    let%bind prepared =
      Prepared.create
        ~max_input_bytes:(Some max_input_bytes)
        ~compiler_contract
        ~environment
        ~provenance:t.metadata.provenance
        ~entry:t.metadata.entry
        ~sources
        ~definitions
        ~scopes
        ~checkpoint
        ()
      |> Result.map_error ~f:(fun error -> Error.Prepared error)
    in
    if Captured.Digest.equal (Prepared.fingerprint prepared) t.metadata.fingerprint
    then Ok prepared
    else Error Error.Fingerprint_mismatch)
;;
