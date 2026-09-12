open Core
module Guidance = Agent_protocol.Authoring_guidance
module History = Agent_protocol.History

type receipt =
  { entry_id : History.Id.t
  ; guidance : Guidance.t
  }
[@@deriving equal, sexp]

type presence =
  | Present
  | Absent
  | Modified
  | Redacted
  | Stale_context
  | Stale_policy
[@@deriving compare, equal, sexp]

type observation =
  { receipt : receipt
  ; presence : presence
  }
[@@deriving sexp]

type report =
  { observations : observation list
  ; refresh_primer : bool
  ; missing_preload : string list
  }
[@@deriving sexp]

let invalid message = Error (Agent_protocol.Error.invalid_request message)

let receipt_index receipts =
  List.fold_result receipts ~init:String.Map.empty ~f:(fun index receipt ->
    let open Result.Let_syntax in
    let%bind () = Guidance.validate receipt.guidance in
    let key = History.Id.to_string receipt.entry_id in
    match Map.find index key with
    | None -> Ok (Map.set index ~key ~data:receipt)
    | Some previous when equal_receipt previous receipt -> Ok index
    | Some _ -> invalid "history ID has conflicting authoring guidance identities")
;;

let remember ~previous ~history =
  let open Result.Let_syntax in
  let%bind () =
    match
      List.find_a_dup history ~compare:(fun a b -> History.Id.compare a.History.id b.id)
    with
    | None -> Ok ()
    | Some _ -> invalid "duplicate history identity in authoring context"
  in
  let current =
    List.filter_map history ~f:(fun entry ->
      match entry.History.provenance with
      | Runtime_authoring guidance -> Some { entry_id = entry.id; guidance }
      | _ -> None)
  in
  let%map index = receipt_index (previous @ current) in
  Map.data index
;;

let inspect_internal ~expected_topics ~policy ~context_identity ~known ~effective =
  let open Result.Let_syntax in
  let%bind known = receipt_index known in
  let%bind entries =
    match
      String.Map.of_alist
        (List.map effective ~f:(fun entry -> History.Id.to_string entry.History.id, entry))
    with
    | `Ok entries -> Ok entries
    | `Duplicate_key _ -> invalid "duplicate effective history identity"
  in
  let observations =
    Map.data known
    |> List.map ~f:(fun receipt ->
      let presence =
        match Map.find entries (History.Id.to_string receipt.entry_id) with
        | None -> Absent
        | Some entry when entry.redacted -> Redacted
        | Some entry ->
          (match entry.provenance with
           | Runtime_authoring guidance
             when Guidance.equal guidance receipt.guidance
                  && Guidance.matches_payload guidance entry.payload ->
             if not (String.equal guidance.context_identity context_identity)
             then Stale_context
             else if
               not
                 (String.equal
                    guidance.policy_fingerprint
                    (Authoring_policy.fingerprint policy))
             then Stale_policy
             else Present
           | _ -> Modified)
      in
      { receipt; presence })
  in
  let complete_installed_topics guidance =
    match guidance.Guidance.purpose with
    | Rediscovery -> []
    | Primer | Preload | Reference ->
      List.filter guidance.topics ~f:(fun topic ->
        topic.complete
        &&
        match topic.source with
        | Installed identity ->
          Option.value_map
            (Authoring_policy.corpus_identity policy)
            ~default:false
            ~f:(String.equal identity)
        | Authored _ -> false)
  in
  let present =
    List.filter_map observations ~f:(fun observation ->
      match observation.presence with
      | Present -> Some observation.receipt.guidance
      | _ -> None)
  in
  let complete_topics guidance =
    match expected_topics, guidance.Guidance.purpose with
    | _, Rediscovery -> []
    | None, _ -> complete_installed_topics guidance
    | Some expected, _ ->
      List.filter guidance.topics ~f:(fun topic ->
        topic.complete && List.mem expected topic ~equal:Guidance.equal_topic)
  in
  let primer =
    List.exists present ~f:(fun guidance ->
      Guidance.equal_purpose guidance.purpose Primer
      && List.length (complete_installed_topics guidance) = List.length guidance.topics
      && List.length (complete_topics guidance) = List.length guidance.topics)
  in
  let complete_topics =
    List.concat_map present ~f:complete_topics
    |> List.map ~f:(fun topic -> topic.Guidance.id)
    |> String.Set.of_list
  in
  Ok
    { observations
    ; refresh_primer = Authoring_policy.inject_primer policy && not primer
    ; missing_preload =
        List.filter (Authoring_policy.preload_topics policy) ~f:(fun id ->
          not (Set.mem complete_topics id))
    }
;;

let inspect ~policy ~context_identity ~known ~effective =
  inspect_internal ~expected_topics:None ~policy ~context_identity ~known ~effective
;;

let inspect_with_topics ~expected_topics ~policy ~context_identity ~known ~effective =
  inspect_internal
    ~expected_topics:(Some expected_topics)
    ~policy
    ~context_identity
    ~known
    ~effective
;;
