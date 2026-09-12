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
  ; surfaces : (M.task * G.topic String.Map.t) String.Map.t
  }

type pointer =
  { text : string
  ; topics : G.topic list
  ; surface_id : string option
  }

type rendered =
  { pointer : pointer
  ; text_with_truncation : truncated:bool -> string
  }

let create ~context ~host ~policy =
  let open Result.Let_syntax in
  let tools = P.authoring_tools policy in
  let entrypoints =
    List.map tools ~f:(fun (reference, _) -> reference.Tool_capability.name)
  in
  match tools, P.helper_pointers policy with
  | [], [] ->
    Ok { policy; topics = String.Map.empty; entrypoints; surfaces = String.Map.empty }
  | _ ->
    let%bind available =
      match
        ( Q.scoped_corpus ~host context ~capabilities:(P.capabilities policy)
        , P.policy policy )
      with
      | Ok corpus, _ -> Ok (Some corpus)
      | Error _, Manual -> Ok None
      | Error message, (Auto | Preload _) -> Error message
    in
    let identity = C.identity (Q.corpus_for_host context ~host) in
    let tasks =
      match available, tools with
      | None, _ -> []
      | Some _, [] ->
        M.
          [ One_off_script
          ; Standalone_tool
          ; Moderator_tool
          ; Child_agent
          ; Background_workflow
          ]
      | Some _, _ -> List.concat_map tools ~f:(fun (_, help) -> help.M.tasks)
    in
    let surfaces =
      List.fold tasks ~init:String.Map.empty ~f:(fun surfaces task ->
        match Q.task_surface host task with
        | Error _ -> surfaces
        | Ok surface ->
          Map.change surfaces surface ~f:(function
            | None -> Some task
            | Some _ as previous -> previous))
    in
    let%map surfaces =
      Map.to_alist surfaces
      |> List.map ~f:(fun (surface, task) ->
        let%map topics =
          Q.virtual_topics context ~host ~capabilities:(P.capabilities policy) ~task
        in
        ( surface
        , ( task
          , String.Map.of_alist_exn (List.map topics ~f:(fun topic -> topic.G.id, topic))
          ) ))
      |> Result.all
      |> Result.map ~f:String.Map.of_alist_exn
    in
    let topics =
      Option.to_list available
      |> List.concat_map ~f:(fun corpus ->
        C.topics corpus
        |> List.filter ~f:(fun topic ->
          List.exists (Map.keys surfaces) ~f:(fun surface_id ->
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
    { policy; topics; entrypoints; surfaces }
;;

let same_version a b =
  G.equal_topic { a with complete = false } { b with complete = false }
;;

let source_json = function
  | G.Installed digest ->
    `Object [ "kind", `String "installed"; "sha256", `String digest ]
  | Authored digest -> `Object [ "kind", `String "authored"; "sha256", `String digest ]
;;

let validate_limits ~max_topics ~max_bytes =
  match max_topics > 0 && max_topics <= 128 && max_bytes > 0 with
  | true -> Ok ()
  | false ->
    Error (Agent_protocol.Error.invalid_request "invalid rediscovery pointer limits")
;;

let render_for
      ?(max_topics = 32)
      ?(max_bytes = 8192)
      (t : t)
      ~context_identity
      ~known
      ~effective
      ~inserting
      ~surface
      ~already_emitted
      ()
  =
  let open Result.Let_syntax in
  let surface_id = Option.map surface ~f:fst in
  let topics =
    match surface with
    | None -> t.topics
    | Some (_, (_, topics)) -> topics
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
           (match surface_id with
            | None -> receipt.guidance.topics
            | Some id ->
              (match receipt.guidance.surface_id with
               | Some remembered when String.equal remembered id ->
                 receipt.guidance.topics
               | None | Some _ -> []))
         | Some (Present | Redacted) | None -> []))
    |> List.filter_map ~f:(fun remembered ->
      match Map.find topics remembered.G.id with
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
          ([ "topic_id", `String current.G.id
           ; "remembered_sha256", `String remembered.G.document_sha256
           ; "current_sha256", `String current.document_sha256
           ; "source", source_json current.source
           ]
           @
           match surface with
           | None -> []
           | Some (id, (task, _)) ->
             [ "surface_id", `String id; "task", `String (M.task_id task) ]))
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
  | [] -> Ok (None, false)
  | _ when remaining_topics <= 0 || remaining_bytes <= 0 -> Ok (None, true)
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
     | [] when already_emitted || not (List.is_empty pointers) -> Ok (None, true)
     | [] ->
       Error
         (Agent_protocol.Error.invalid_request
            "rediscovery pointer budget cannot fit one reference")
     | _ ->
       let pairs = List.rev retained in
       let text_with_truncation ~truncated =
         render_text pairs ~truncated:(dropped || truncated)
       in
       Ok
         ( Some
             { pointer =
                 { text = text_with_truncation ~truncated:false
                 ; topics = List.map pairs ~f:snd
                 ; surface_id
                 }
             ; text_with_truncation
             }
         , dropped ))
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
  let%bind () = validate_limits ~max_topics ~max_bytes in
  let%map result, _ =
    render_for
      ~max_topics
      ~max_bytes
      t
      ~context_identity
      ~known
      ~effective
      ~inserting
      ~surface:None
      ~already_emitted:false
      ()
  in
  Option.map result ~f:(fun result -> result.pointer)
;;

let render_all
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
  let%bind () = validate_limits ~max_topics ~max_bytes in
  let%map pointers, _, _, truncated =
    List.fold_result
      (None :: List.map (Map.to_alist t.surfaces) ~f:Option.some)
      ~init:([], max_topics, max_bytes, false)
      ~f:(fun (pointers, remaining_topics, remaining_bytes, truncated) surface ->
        let%map pointer, dropped =
          render_for
            ~max_topics:remaining_topics
            ~max_bytes:remaining_bytes
            t
            ~context_identity
            ~known
            ~effective
            ~inserting:
              (inserting
               @ List.concat_map pointers ~f:(fun rendered -> rendered.pointer.topics))
            ~surface
            ~already_emitted:(not (List.is_empty pointers))
            ()
        in
        match pointer with
        | None -> pointers, remaining_topics, remaining_bytes, truncated || dropped
        | Some rendered ->
          ( rendered :: pointers
          , remaining_topics - List.length rendered.pointer.topics
          , remaining_bytes - String.length rendered.pointer.text
          , truncated || dropped ))
  in
  List.rev_map pointers ~f:(fun rendered ->
    { rendered.pointer with text = rendered.text_with_truncation ~truncated })
;;
