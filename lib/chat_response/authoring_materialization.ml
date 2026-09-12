open Core
module Q = Authoring_context
module V = Authoring_validation
module P = Authoring_policy
module C = Tool_capability
module M = Chatmd_shell_spec.Authoring_metadata
module Corpus = Authoring_corpus
module G = Agent_protocol.Authoring_guidance
module H = Agent_protocol.History

type message =
  { payload : Jsonaf.t
  ; guidance : G.t
  }

type t =
  { policy : P.t
  ; context_identity : string
  ; scope : string
  ; initial : message list
  ; rediscovery : Authoring_rediscovery.t
  }

let context_identity t = t.context_identity
let scope t = t.scope

let session_scope ~session_id ~generation =
  [%sexp (session_id : Agent_protocol.Id.Session.t), (generation : int)]
  |> Sexp.to_string_mach
;;

let initial t = t.initial
let digest = Chatmd_shell_spec.Source_ref.digest

let task_surfaces host =
  List.filter_map
    [ M.One_off_script
    ; Standalone_tool
    ; Moderator_tool
    ; Child_agent
    ; Background_workflow
    ]
    ~f:(fun task ->
      match Q.task_surface host task with
      | Ok surface -> Some (task, surface)
      | Error _ -> None)
;;

let catalog context ~host = V.catalog_of_corpus host (Q.corpus_for_host context ~host)

let estimated_tokens messages =
  List.sum
    (module Int)
    messages
    ~f:(fun message -> (String.length (Jsonaf.to_string message.payload) + 2) / 3)
;;

let entry (message : message) ~id =
  H.
    { id
    ; role = User
    ; kind = Message
    ; payload = message.payload
    ; provenance = Runtime_authoring message.guidance
    ; redacted = false
    }
;;

let create ?(max_tokens = 32000) ~context ~host ~policy ~capabilities ~scope () =
  let open Result.Let_syntax in
  let corpus = Q.corpus_for_host context ~host in
  let corpus_identity = Corpus.identity corpus in
  let%bind () =
    match
      max_tokens > 0
      && max_tokens <= 1_000_000
      && (not (String.is_empty scope))
      && String.equal (C.fingerprint capabilities) (C.fingerprint (P.capabilities policy))
    with
    | true -> Ok ()
    | false -> Error "invalid authoring materialization scope, budget or capabilities"
  in
  let context_identity =
    [%sexp
      ("ochat.authoring-materialization.v1" : string)
    , (Q.fingerprint context : string)
    , (V.host_fingerprint host : string)
    , (C.fingerprint capabilities : string)
    , (scope : string)]
    |> Sexp.to_string
    |> digest
  in
  let%bind rediscovery = Authoring_rediscovery.create ~context ~host ~policy in
  match P.inject_primer policy with
  | false -> Ok { policy; context_identity; scope; initial = []; rediscovery }
  | true ->
    let%bind () =
      match
        Option.equal String.equal (P.corpus_identity policy) (Some corpus_identity)
      with
      | true -> Ok ()
      | false -> Error "authoring policy refers to a different installed corpus"
    in
    let%bind corpus = Q.scoped_corpus ~host context ~capabilities in
    let available = task_surfaces host in
    let%bind surfaces =
      P.authoring_tools policy
      |> List.concat_map ~f:(fun (_, help) -> help.M.tasks)
      |> List.dedup_and_sort ~compare:M.compare_task
      |> List.map ~f:(fun task ->
        List.Assoc.find available task ~equal:M.equal_task
        |> Result.of_option ~error:("unavailable authoring task: " ^ M.task_id task))
      |> Result.all
      |> Result.map ~f:(List.dedup_and_sort ~compare:String.compare)
    in
    let resolve id =
      match
        List.find_map surfaces ~f:(fun surface_id ->
          match Corpus.assemble corpus ~surface_id ~roots:[ id ] with
          | Ok topics -> Some topics
          | Error _ -> None)
      with
      | Some topics ->
        (match
           List.for_all topics ~f:(fun topic ->
             match topic.Corpus.origin, topic.specification.review with
             | Authored _, _ | Installed, Audited _ -> true
             | Installed, Pending -> false)
         with
         | true -> Ok topics
         | false -> Error ("unaudited preload topic: " ^ id))
      | None -> Error ("unavailable preload topic: " ^ id)
    in
    let make purpose topic =
      let content =
        (match topic.Corpus.origin with
         | Installed -> "[Ochat installed authoring reference; topic="
         | Authored _ ->
           "[Author-supplied conventions; not authoritative runtime semantics; topic=")
        ^ topic.Corpus.specification.id
        ^ "; corpus="
        ^ corpus_identity
        ^ "]\n"
        ^ String.concat ~sep:"\n\n" (List.map topic.fragments ~f:(fun f -> f.Corpus.text))
      in
      let module R = Openai.Responses in
      let payload =
        R.Item.Input_message
          { role = User
          ; content = [ R.Input_message.Text { text = content; _type = "input_text" } ]
          ; _type = "message"
          }
        |> R.Item.jsonaf_of_t
      in
      let%map guidance =
        G.create
          ~context_identity
          ~policy_fingerprint:(P.fingerprint policy)
          ~purpose
          ~payload
          ~topics:
            [ { id = topic.specification.id
              ; document_sha256 = topic.sha256
              ; source =
                  (match topic.origin with
                   | Installed -> Installed corpus_identity
                   | Authored owner -> Authored owner.package_sha256)
              ; complete = true
              }
            ]
        |> Result.map_error ~f:(fun error -> error.Agent_protocol.Error.message)
      in
      { payload; guidance }
    in
    let%bind primer = resolve "authoring.primer" in
    let%bind preload = List.map (P.preload_topics policy) ~f:resolve |> Result.all in
    let _, topics =
      List.map primer ~f:(fun topic -> G.Primer, topic)
      @ List.concat_map preload ~f:(List.map ~f:(fun topic -> G.Preload, topic))
      |> List.fold ~init:(String.Set.empty, []) ~f:(fun (seen, items) (purpose, topic) ->
        let id = topic.Corpus.specification.id in
        match Set.mem seen id with
        | true -> seen, items
        | false -> Set.add seen id, (purpose, topic) :: items)
    in
    let%bind initial =
      List.rev_map topics ~f:(fun (purpose, topic) -> make purpose topic) |> Result.all
    in
    (match estimated_tokens initial <= max_tokens with
     | true -> Ok { policy; context_identity; scope; initial; rediscovery }
     | false ->
       Error
         (sprintf
            "authoring preload needs at least %d estimated tokens; budget is %d"
            (estimated_tokens initial)
            max_tokens))
;;

let refresh t ~known ~effective =
  let open Result.Let_syntax in
  let remembered = known in
  let%bind known = Authoring_presence.remember ~previous:known ~history:effective in
  let%bind report =
    Authoring_presence.inspect_with_topics
      ~expected_topics:
        (List.concat_map t.initial ~f:(fun message -> message.guidance.topics))
      ~policy:t.policy
      ~context_identity:t.context_identity
      ~known
      ~effective
  in
  let present =
    List.filter_map report.observations ~f:(fun observation ->
      match observation.presence with
      | Present ->
        (match observation.receipt.guidance.purpose with
         | Rediscovery -> None
         | Primer | Preload | Reference -> Some observation.receipt.guidance)
      | _ -> None)
  in
  let%bind complete = Authoring_fragment_coverage.complete present in
  let missing =
    List.filter t.initial ~f:(fun message ->
      let available =
        match message.guidance.purpose with
        | Primer ->
          List.concat_map present ~f:(fun guidance ->
            match guidance.G.purpose with
            | Primer -> guidance.topics
            | _ -> [])
        | Preload | Reference | Rediscovery -> complete
      in
      not
        (List.for_all message.guidance.topics ~f:(fun topic ->
           List.mem available topic ~equal:G.equal_topic)))
  in
  let%bind pointer =
    Authoring_rediscovery.render
      t.rediscovery
      ~context_identity:t.context_identity
      ~known:remembered
      ~effective
      ~inserting:(List.concat_map missing ~f:(fun message -> message.guidance.topics))
      ()
  in
  match pointer with
  | None -> Ok missing
  | Some pointer ->
    let module R = Openai.Responses in
    let payload =
      R.Item.Input_message
        { role = User
        ; content = [ R.Input_message.Text { text = pointer.text; _type = "input_text" } ]
        ; _type = "message"
        }
      |> R.Item.jsonaf_of_t
    in
    let%map guidance =
      G.create
        ~context_identity:t.context_identity
        ~policy_fingerprint:(P.fingerprint t.policy)
        ~purpose:Rediscovery
        ~payload
        ~topics:pointer.topics
    in
    missing @ [ { payload; guidance } ]
;;
