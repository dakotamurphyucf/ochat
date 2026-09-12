open Core
module P = Authoring_policy
module Q = Authoring_context
module C = Authoring_corpus
module G = Agent_protocol.Authoring_guidance
module H = Agent_protocol.History
module Presence = Authoring_presence
module M = Chatmd_shell_spec.Authoring_metadata

type t =
  { policy : P.t
  ; topics : G.topic String.Map.t
  ; entrypoints : string list
  }

type pointer =
  { text : string
  ; topics : G.topic list
  }

let create ~context ~host ~policy =
  let open Result.Let_syntax in
  let tools = P.authoring_tools policy in
  let entrypoints =
    List.map tools ~f:(fun (reference, _) -> reference.Tool_capability.name)
  in
  match tools with
  | [] -> Ok { policy; topics = String.Map.empty; entrypoints }
  | _ ->
    let%map available =
      match
        ( Q.scoped_corpus ~host context ~capabilities:(P.capabilities policy)
        , P.policy policy )
      with
      | Ok corpus, _ -> Ok (Some corpus)
      | Error _, Manual -> Ok None
      | Error message, (Auto | Preload _) -> Error message
    in
    let identity = C.identity (Q.corpus_for_host context ~host) in
    let surfaces =
      List.concat_map tools ~f:(fun (_, help) -> help.M.tasks)
      |> List.filter_map ~f:(fun task -> Q.task_surface host task |> Result.ok)
      |> List.dedup_and_sort ~compare:String.compare
    in
    let topics =
      Option.to_list available
      |> List.concat_map ~f:(fun corpus ->
        C.topics corpus
        |> List.filter ~f:(fun topic ->
          List.exists surfaces ~f:(fun surface_id ->
            Result.is_ok (C.assemble corpus ~surface_id ~roots:[ topic.specification.id ]))))
      |> List.map ~f:(fun topic ->
        let source =
          match topic.origin with
          | Installed -> G.Installed identity
          | Authored owner -> G.Authored owner.package_sha256
        in
        ( topic.specification.id
        , G.
            { id = topic.specification.id
            ; document_sha256 = topic.sha256
            ; source
            ; complete = false
            } ))
      |> String.Map.of_alist_exn
    in
    { policy; topics; entrypoints }
;;

let same_version a b =
  G.equal_topic { a with complete = false } { b with complete = false }
;;

let source_json = function
  | G.Installed digest ->
    `Object [ "kind", `String "installed"; "sha256", `String digest ]
  | Authored digest -> `Object [ "kind", `String "authored"; "sha256", `String digest ]
;;

let render
      ?(max_topics = 32)
      ?(max_bytes = 8192)
      t
      ~context_identity
      ~known
      ~effective
      ~inserting
      ()
  =
  let open Result.Let_syntax in
  let%bind () =
    match max_topics > 0 && max_topics <= 128 && max_bytes > 0 with
    | true -> Ok ()
    | false ->
      Error (Agent_protocol.Error.invalid_request "invalid rediscovery pointer limits")
  in
  let%bind report =
    Presence.inspect ~policy:t.policy ~context_identity ~known ~effective
  in
  let observations =
    List.map report.observations ~f:(fun observation ->
      H.Id.to_string observation.Presence.receipt.entry_id, observation.presence)
    |> String.Map.of_alist_exn
  in
  let current_guidance =
    List.filter_map effective ~f:(fun entry ->
      match entry.H.provenance with
      | Runtime_authoring guidance
        when (not entry.redacted)
             && G.matches_payload guidance entry.payload
             && String.equal guidance.context_identity context_identity
             && String.equal guidance.policy_fingerprint (P.fingerprint t.policy) ->
        Some (entry, guidance)
      | _ -> None)
  in
  let pointers =
    List.filter current_guidance ~f:(fun (_, guidance) ->
      G.equal_purpose guidance.purpose Rediscovery)
  in
  let remaining_topics =
    max_topics
    - List.sum (module Int) pointers ~f:(fun (_, guidance) -> List.length guidance.topics)
  in
  let remaining_bytes =
    max_bytes
    - List.sum
        (module Int)
        pointers
        ~f:(fun (entry, _) -> String.length (Jsonaf.to_string entry.H.payload))
  in
  let%bind complete =
    Authoring_fragment_coverage.complete (List.map current_guidance ~f:snd)
  in
  let covered =
    List.concat_map pointers ~f:(fun (_, guidance) -> guidance.G.topics)
    @ complete
    @ inserting
  in
  let seen = String.Hash_set.create () in
  let candidates =
    List.rev known
    |> List.concat_map ~f:(fun receipt ->
      match receipt.Presence.guidance.purpose with
      | Rediscovery -> []
      | Primer | Preload | Reference ->
        (match Map.find observations (H.Id.to_string receipt.entry_id) with
         | Some (Absent | Modified | Stale_context | Stale_policy) ->
           receipt.guidance.topics
         | Some (Present | Redacted) | None -> []))
    |> List.filter_map ~f:(fun remembered ->
      match Map.find t.topics remembered.G.id with
      | None -> None
      | Some current ->
        let same_origin =
          match remembered.source, current.source with
          | Installed _, Installed _ | Authored _, Authored _ -> true
          | _ -> false
        in
        (match
           same_origin
           && (not (Hash_set.mem seen current.id))
           && not (List.exists covered ~f:(same_version current))
         with
         | false -> None
         | true ->
           Hash_set.add seen current.id;
           Some (remembered, current)))
  in
  let render_text pairs ~truncated =
    let policy =
      match P.policy t.policy with
      | Auto -> "auto"
      | Manual -> "manual"
      | Preload _ -> "preload"
    in
    let references =
      List.map pairs ~f:(fun (remembered, current) ->
        `Object
          [ "topic_id", `String current.G.id
          ; "remembered_sha256", `String remembered.G.document_sha256
          ; "current_sha256", `String current.document_sha256
          ; "source", source_json current.source
          ])
    in
    let metadata =
      `Object
        [ "version", `Number "1"
        ; "policy", `String policy
        ; "entrypoints", `Array (List.map t.entrypoints ~f:(fun name -> `String name))
        ; ( "helpers"
          , `Array
              (List.map (P.helper_pointers t.policy) ~f:(fun (_, name) -> `String name)) )
        ; ("truncated", if truncated then `True else `False)
        ; "references", `Array references
        ]
    in
    "[Ochat authoring rediscovery]\nThese are remembered references, not their contents. "
    ^ "Retrieve the needed current topics before relying on their exact contracts. "
    ^ "Authored sources are author conventions, not authoritative runtime semantics.\n"
    ^ Jsonaf.to_string metadata
  in
  match candidates with
  | [] -> Ok None
  | _ when remaining_topics <= 0 || remaining_bytes <= 0 -> Ok None
  | _ ->
    let retained, _, dropped =
      List.fold candidates ~init:([], 0, false) ~f:(fun (retained, count, dropped) pair ->
        let proposed = pair :: retained in
        match
          count < remaining_topics
          && String.length (render_text (List.rev proposed) ~truncated:false)
             <= remaining_bytes
        with
        | true -> proposed, count + 1, dropped
        | false -> retained, count, true)
    in
    (match retained with
     | [] when not (List.is_empty pointers) -> Ok None
     | [] ->
       Error
         (Agent_protocol.Error.invalid_request
            "rediscovery pointer budget cannot fit one reference")
     | _ ->
       let pairs = List.rev retained in
       Ok
         (Some
            { text = render_text pairs ~truncated:dropped
            ; topics = List.map pairs ~f:snd
            }))
;;
