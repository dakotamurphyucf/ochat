open Core
module P = Agent_protocol
module G = P.Authoring_guidance
module M = Authoring_materialization

type context =
  { version : int
  ; scope : string
  ; identity : string
  ; policy : string
  }
[@@deriving equal, sexp]

let capture materialization =
  { version = 1
  ; scope = M.scope materialization
  ; identity = M.context_identity materialization
  ; policy = M.policy_fingerprint materialization
  }
;;

let validate_context context ~session_id ~generation =
  let hash value =
    String.length value = 64
    && String.for_all value ~f:(function
      | 'a' .. 'f' | '0' .. '9' -> true
      | _ -> false)
  in
  match
    context.version = 1
    && hash context.identity
    && hash context.policy
    && String.equal context.scope (M.session_scope ~session_id ~generation)
  with
  | true -> Ok ()
  | false ->
    Error (P.Error.invalid_request "invalid or foreign authoring publication context")
;;

let reference_guidance reference ~version ~identity ~policy ~payload =
  let fragments =
    List.map reference.P.Authoring_reference.topics ~f:(fun topic ->
      G.
        { topic_id = topic.topic.id
        ; total_parts = topic.total_parts
        ; parts =
            List.map topic.parts ~f:(fun part ->
              { index = part.P.Authoring_reference.index; item_sha256 = part.item_sha256 })
        })
  in
  let create =
    match version with
    | 2 -> G.create_reference
    | _ -> G.create_surface_reference ~surface_id:reference.surface_id
  in
  create
    ~context_identity:identity
    ~policy_fingerprint:policy
    ~topics:
      (List.map reference.topics ~f:(fun topic -> topic.P.Authoring_reference.topic))
    ~fragments
    ~payload
;;

let encode ~context invocation entry =
  let open Result.Let_syntax in
  match
    context, invocation.P.Invocation.authoring_reference, invocation.context.origin
  with
  | Some context, Some _, Model
    when not
           (String.equal
              context.scope
              (M.session_scope
                 ~session_id:invocation.context.session_id
                 ~generation:invocation.context.generation)) -> Ok entry
  | Some context, Some reference, Model ->
    let%bind () =
      validate_context
        context
        ~session_id:invocation.context.session_id
        ~generation:invocation.context.generation
    in
    (match
       M.reference_identity
         reference
         ~session_id:invocation.context.session_id
         ~generation:invocation.context.generation
     with
     | Some identity when String.equal identity context.identity ->
       let%map guidance =
         reference_guidance
           reference
           ~version:3
           ~identity
           ~policy:context.policy
           ~payload:entry.P.History.payload
       in
       { entry with provenance = Runtime_authoring guidance }
     | None | Some _ -> Ok entry)
  | _ -> Ok entry
;;

let validate_output invocation entry =
  let open Result.Let_syntax in
  match entry.P.History.redacted, entry.provenance with
  | false, Canonical -> Ok ()
  | false, Runtime_authoring guidance ->
    (match invocation.P.Invocation.authoring_reference, invocation.context.origin with
     | Some reference, Model ->
       let%bind identity =
         M.reference_identity
           reference
           ~session_id:invocation.context.session_id
           ~generation:invocation.context.generation
         |> Result.of_option
              ~error:
                (P.Error.invalid_request "authoring output has foreign receipt scope")
       in
       let%bind expected =
         reference_guidance
           reference
           ~version:guidance.version
           ~identity
           ~policy:guidance.policy_fingerprint
           ~payload:entry.payload
       in
       (match G.equal expected guidance with
        | true -> Ok ()
        | false ->
          Error
            (P.Error.invalid_request
               "authoring output differs from its invocation receipt"))
     | _ ->
       Error (P.Error.invalid_request "authoring output has no model invocation receipt"))
  | _ ->
    Error
      (P.Error.invalid_request "invocation output provenance is not usable model input")
;;

let context_to_jsonaf t =
  `Object
    [ "version", `Number (Int.to_string t.version)
    ; "scope", `String t.scope
    ; "identity", `String t.identity
    ; "policy", `String t.policy
    ]
;;

let context_of_jsonaf json =
  let module J = P.Json_codec in
  let open Result.Let_syntax in
  let%bind fields = J.fields json in
  let%bind version = J.required_as fields "version" (J.bounded_int ~min:1 ~max:1) in
  let%bind scope = J.required_as fields "scope" J.string in
  let%bind identity = J.required_as fields "identity" J.string in
  let%bind policy = J.required_as fields "policy" J.string in
  let valid_hash text =
    String.length text = 64
    && String.for_all text ~f:(function
      | 'a' .. 'f' | '0' .. '9' -> true
      | _ -> false)
  in
  if String.is_empty scope || not (valid_hash identity && valid_hash policy)
  then Error (P.Error.invalid_request "invalid authoring publication context")
  else Ok { version; scope; identity; policy }
;;

let context_shape =
  match
    Document_schema.Shape.object_
      [ "version", Document_schema.Shape.value
      ; "scope", Document_schema.Shape.value
      ; "identity", Document_schema.Shape.value
      ; "policy", Document_schema.Shape.value
      ]
  with
  | Ok shape -> shape
  | Error error ->
    raise_s [%sexp "invalid authoring context shape", (error : Document_schema.Error.t)]
;;
