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

let reference_guidance reference ~identity ~policy ~payload =
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
  G.create_reference
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
