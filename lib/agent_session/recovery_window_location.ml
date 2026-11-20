open! Core
module P = Agent_protocol
module X = Persistence_codec
module D = Document_schema
module J = P.Json_codec
module Scope = P.Recovery_window.Scope
module Citation = P.Recovery_window.Citation
module Window_id = P.Id.Context_window
module Operation = P.Id.Operation
module Digest = Chatml_source.Digest

let invalid message = Error (P.Error.invalid_request message)

module Source = struct
  type basis =
    { owner : P.Session_ref.t
    ; generation : int
    ; revision : int64
    }
  [@@deriving equal, sexp_of]

  type archive_kind =
    | Summary
    | Rollover
  [@@deriving equal, sexp_of]

  type archive =
    { kind : archive_kind
    ; basis : basis
    ; operation : Operation.t
    ; digest : Digest.t
    }
  [@@deriving equal, sexp_of]

  type t =
    | Live of basis
    | Archive of archive
  [@@deriving equal, sexp_of]

  let basis ~owner ~generation ~revision =
    if generation < 0 || Int64.(revision < 0L)
    then invalid "negative recovery source coordinate"
    else Ok { owner; generation; revision }
  ;;

  let archive basis ~kind ~operation ~digest = Archive { kind; basis; operation; digest }

  let coordinates = function
    | Live basis | Archive { basis; _ } -> basis
  ;;

  let digest_of_jsonaf json =
    let open Result.Let_syntax in
    let%bind hex = J.string json in
    Digest.of_hex hex
    |> Result.map_error ~f:(fun _ ->
      P.Error.invalid_request "invalid recovery archive SHA-256")
  ;;

  let to_jsonaf t =
    let basis = coordinates t in
    let location =
      match t with
      | Live _ -> [ "kind", `String "live" ]
      | Archive { kind; operation; digest; _ } ->
        let tag =
          match kind with
          | Summary -> "summary_archive"
          | Rollover -> "rollover_archive"
        in
        [ "kind", `String tag
        ; "operation", Operation.to_json operation
        ; "sha256", `String (Digest.to_hex digest)
        ]
    in
    `Object
      ([ "owner", P.Session_ref.to_json basis.owner
       ; "generation", X.host_counter_to_json basis.generation
       ; "revision", X.int64_json basis.revision
       ]
       @ location)
  ;;

  let of_jsonaf json =
    let open Result.Let_syntax in
    let%bind fields = X.object_ json in
    let%bind owner = X.required fields "owner" P.Session_ref.of_json in
    let%bind generation = X.required fields "generation" X.host_counter_of_json in
    let%bind revision = X.required fields "revision" X.nonnegative_int64 in
    let%bind basis = basis ~owner ~generation ~revision in
    let%bind kind = X.required fields "kind" J.string in
    match kind with
    | "live" -> Ok (Live basis)
    | ("summary_archive" | "rollover_archive") as tag ->
      let%bind operation = X.required fields "operation" Operation.of_json in
      let%map digest = X.required fields "sha256" digest_of_jsonaf in
      let kind = if String.equal tag "summary_archive" then Summary else Rollover in
      archive basis ~kind ~operation ~digest
    | _ -> invalid "unknown recovery source kind"
  ;;

  let owner_shape = X.fields_shape [ "server_id"; "session_id" ]

  let shape =
    let basis =
      [ "owner", owner_shape
      ; "generation", D.Shape.value
      ; "revision", D.Shape.value
      ; "kind", D.Shape.value
      ]
    in
    let archived =
      X.shape_exn (basis @ [ "operation", D.Shape.value; "sha256", D.Shape.value ])
    in
    X.tagged_shape_exn
      ~discriminator:"kind"
      [ "live", X.shape_exn basis
      ; "summary_archive", archived
      ; "rollover_archive", archived
      ]
  ;;
end

module Member = struct
  type provenance =
    | Observed
    | Carried_from of Window_id.t
    | Derived_summary of Operation.t
  [@@deriving sexp_of]

  type t =
    { citation : Citation.t
    ; revision : P.History.Content_revision.t
    ; source : Source.t
    ; source_ordinal : int
    ; provenance : provenance
    ; withdrawn : bool
    }

  let create citation ~source ~source_ordinal ~provenance =
    let scope = Citation.scope citation in
    let basis = Source.coordinates source in
    if
      not
        (P.Session_ref.equal basis.owner (Scope.session scope)
         && Int.equal basis.generation (Scope.generation scope))
    then invalid "foreign recovery member source"
    else if source_ordinal < 0
    then invalid "negative recovery source ordinal"
    else (
      match Citation.version citation with
      | Canonical revision ->
        Ok { citation; revision; source; source_ordinal; provenance; withdrawn = false }
      | Effective _ -> invalid "effective member requires an admitted prepared source")
  ;;

  let withdraw t = { t with withdrawn = true }
  let is_withdrawn t = t.withdrawn
  let citation t = t.citation
  let source t = t.source
  let source_ordinal t = t.source_ordinal
  let provenance t = t.provenance

  let matches t (entry : P.History.entry) =
    P.History.Id.equal entry.id (Citation.occurrence t.citation)
    && P.History.Content_revision.equal entry.content_revision t.revision
  ;;

  let same_version t other =
    P.History.Id.equal
      (Citation.occurrence t.citation)
      (Citation.occurrence other.citation)
    && P.History.Content_revision.equal t.revision other.revision
  ;;

  let key t =
    P.History.Id.to_string (Citation.occurrence t.citation)
    ^ "@"
    ^ Int64.to_string (P.History.Content_revision.to_int64 t.revision)
  ;;

  let provenance_to_jsonaf = function
    | Observed -> `Object [ "kind", `String "observed" ]
    | Carried_from parent ->
      `Object [ "kind", `String "carried"; "previous", Window_id.to_json parent ]
    | Derived_summary operation ->
      `Object [ "kind", `String "summary"; "operation", Operation.to_json operation ]
  ;;

  let provenance_of_jsonaf json =
    let open Result.Let_syntax in
    let%bind fields = X.object_ json in
    let%bind kind = X.required fields "kind" J.string in
    match kind with
    | "observed" -> Ok Observed
    | "carried" ->
      let%map previous = X.required fields "previous" Window_id.of_json in
      Carried_from previous
    | "summary" ->
      let%map operation = X.required fields "operation" Operation.of_json in
      Derived_summary operation
    | _ -> invalid "unknown recovery member provenance"
  ;;

  let to_jsonaf t =
    `Object
      ([ "key", `String (key t)
       ; "occurrence", P.History.Id.to_json (Citation.occurrence t.citation)
       ; "revision", P.History.Content_revision.to_json t.revision
       ; "source", Source.to_jsonaf t.source
       ; "source_ordinal", X.host_counter_to_json t.source_ordinal
       ; "provenance", provenance_to_jsonaf t.provenance
       ]
       @ if t.withdrawn then [ "withdrawn", `True ] else [])
  ;;

  let of_jsonaf scope json =
    let open Result.Let_syntax in
    let%bind fields = X.object_ json in
    let%bind encoded_key = X.required fields "key" J.string in
    let%bind occurrence = X.required fields "occurrence" P.History.Id.of_json in
    let%bind revision = X.required fields "revision" P.History.Content_revision.of_json in
    let%bind source = X.required fields "source" Source.of_jsonaf in
    let%bind source_ordinal = X.required fields "source_ordinal" X.host_counter_of_json in
    let%bind provenance = X.required fields "provenance" provenance_of_jsonaf in
    let%bind citation =
      Citation.create
        scope
        ~occurrence
        ~version:(P.Recovery_item_version.canonical revision)
    in
    let%bind member = create citation ~source ~source_ordinal ~provenance in
    if not (String.equal encoded_key (key member))
    then invalid "recovery member key is not canonical"
    else (
      match J.optional fields "withdrawn" with
      | None -> Ok member
      | Some `True -> Ok (withdraw member)
      | Some _ -> invalid "invalid recovery withdrawal marker")
  ;;

  let shape =
    X.shape_exn
      [ "key", D.Shape.value
      ; "occurrence", D.Shape.value
      ; "revision", D.Shape.value
      ; "withdrawn", D.Shape.value
      ; "source", Source.shape
      ; "source_ordinal", D.Shape.value
      ; ( "provenance"
        , X.tagged_shape_exn
            ~discriminator:"kind"
            [ "observed", X.fields_shape [ "kind" ]
            ; "carried", X.fields_shape [ "kind"; "previous" ]
            ; "summary", X.fields_shape [ "kind"; "operation" ]
            ] )
      ]
  ;;
end
