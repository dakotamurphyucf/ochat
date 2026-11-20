open! Core
module P = Agent_protocol
module Effective = Chat_response.Moderation.Effective_entry
module Overlay = Chat_response.Moderation.Identity_overlay
module D = Document_schema
module Version = P.Recovery_item_version

let invalid reason = P.Error.invalid_request reason

module Row = struct
  type payload =
    | Stored of P.History.entry
    | Applied of History_entry.t

  type t =
    { payload : payload
    ; effective_ordinal : int
    ; canonical_ordinal : int option
    ; canonical : P.History.entry option
    ; provenance : Effective.provenance
    ; origin : Version.origin
    }

  let occurrence t =
    match t.payload with
    | Stored entry -> entry.id
    | Applied entry -> History_entry.id entry
  ;;

  let effective_ordinal t = t.effective_ordinal
  let canonical_ordinal t = t.canonical_ordinal
  let max_decode_bytes = 64 * 1024

  let projected t ~remaining_bytes =
    match t.payload with
    | Applied entry -> Some entry, remaining_bytes
    | Stored entry ->
      let admitted =
        let open Option.Let_syntax in
        let%bind limits =
          D.Limits.create
            ~max_bytes:(Int.min max_decode_bytes remaining_bytes)
            ~max_depth:24
            ~max_fields:2048
            ~max_nodes:4096
          |> Result.ok
        in
        let%bind bytes = D.Json.validate_and_measure entry.payload ~limits |> Result.ok in
        let%map decoded = History_codec.of_protocol entry |> Result.ok in
        decoded, bytes
      in
      (match admitted with
       | None -> None, Int.max 0 (remaining_bytes - max_decode_bytes)
       | Some (entry, bytes) -> Some entry, remaining_bytes - bytes)
  ;;

  let versioned t ~remaining_bytes =
    if remaining_bytes <= 0 || Option.exists t.canonical ~f:(fun entry -> entry.redacted)
    then None, remaining_bytes
    else (
      match projected t ~remaining_bytes with
      | None, remaining_bytes -> None, remaining_bytes
      | Some entry, remaining_bytes ->
        let digest, remaining_bytes =
          Inference_runtime.Recovery_input.projected_digest entry ~remaining_bytes
        in
        let qualified =
          let open Option.Let_syntax in
          let%bind digest = digest in
          let%map version = Version.effective ~origin:t.origin ~digest |> Result.ok in
          version, entry
        in
        qualified, remaining_bytes)
  ;;

  let version t ~remaining_bytes =
    let matched, remaining_bytes = versioned t ~remaining_bytes in
    Option.map matched ~f:fst, remaining_bytes
  ;;

  let entry t =
    let max_bytes = Inference_runtime.Recovery_input.max_projection_bytes in
    Option.map
      (fst (versioned t ~remaining_bytes:max_bytes))
      ~f:(fun (_, entry) ->
        let provenance : P.History.provenance =
          match t.provenance with
          | Canonical ->
            Option.value_map t.canonical ~default:P.History.Canonical ~f:(fun entry ->
              entry.provenance)
          | Moderator_inserted _ -> Moderator_inserted
          | Moderator_replacement { target_id; _ } -> Moderator_replaced target_id
        in
        let content_revision =
          Option.value_map
            t.canonical
            ~default:P.History.Content_revision.zero
            ~f:(fun entry -> entry.content_revision)
        in
        let module Payload = History_entry.Payload in
        let payload = Payload.authored (Payload.semantic (History_entry.payload entry)) in
        let projected =
          History_codec.to_protocol ~provenance (History_entry.with_payload entry payload)
        in
        { projected with content_revision })
  ;;
end

type t =
  { rows : Row.t array
  ; by_occurrence : (P.History.Id.t, Row.t) Hashtbl.t
  }

let find t = Hashtbl.find t.by_occurrence

let at t ~ordinal =
  if ordinal < 0 || ordinal >= Array.length t.rows then None else Some t.rows.(ordinal)
;;

let origin ~source_revision ~overlay_incarnation canonical provenance =
  let fallback () = Ok (Version.Captured { session_revision = source_revision }) in
  match provenance, canonical with
  | Effective.Canonical, Some (_, (entry : P.History.entry)) ->
    (match entry.provenance with
     | Moderator_inserted | Moderator_replaced _ -> fallback ()
     | Canonical | Runtime_authoring _ | Runtime_notification _ ->
       Ok (Version.Unchanged entry.content_revision))
  | Moderator_inserted { change_id }, None ->
    Ok (Version.Inserted { incarnation = overlay_incarnation; change_id })
  | Moderator_replacement { change_id; _ }, Some (_, entry) ->
    Ok
      (Version.Replaced
         { incarnation = overlay_incarnation; change_id; base = entry.content_revision })
  | Canonical, None | Moderator_inserted _, Some _ | Moderator_replacement _, None ->
    Error (invalid "effective projection origin is inconsistent")
;;

let create ~source_revision ~overlay_incarnation ~history ~moderator =
  let open Result.Let_syntax in
  let max_members = Recovery_window_record.max_members in
  let too_many values = Stdlib.List.compare_length_with values max_members > 0 in
  let oversized =
    too_many history
    || Option.exists
         moderator
         ~f:(fun (snapshot : Session.Moderator_state.Identity_snapshot.t) ->
           too_many snapshot.prepended_items
           || too_many snapshot.appended_items
           || too_many snapshot.replacements
           || too_many snapshot.tombstones)
  in
  if
    Int64.(
      source_revision < 0L
      || overlay_incarnation < 0L
      || overlay_incarnation > source_revision)
  then Error (invalid "effective projection revision is out of bounds")
  else if oversized
  then Ok None
  else (
    let seen = Hash_set.create (module P.History.Id) in
    let%bind history =
      List.mapi history ~f:(fun ordinal (entry : P.History.entry) ->
        if Hash_set.mem seen entry.id
        then Error (invalid "duplicate canonical projection occurrence")
        else (
          Hash_set.add seen entry.id;
          Ok (ordinal, entry)))
      |> Result.all
    in
    let%bind effective =
      match moderator with
      | None -> Ok (List.map history ~f:(fun row -> Overlay.Unchanged row))
      | Some snapshot ->
        Chat_response.Moderator_manager.effective_sources_of_snapshot
          snapshot
          history
          ~occurrence:(fun (_, entry) -> entry.id)
        |> Result.map_error ~f:(fun _ -> invalid "invalid committed moderator projection")
    in
    if too_many effective
    then Ok None
    else (
      let by_occurrence = Hashtbl.create (module P.History.Id) in
      let%map rows =
        List.mapi effective ~f:(fun effective_ordinal source ->
          let payload, canonical, provenance =
            match source with
            | Unchanged ((_, entry) as canonical) ->
              Row.Stored entry, Some canonical, Effective.Canonical
            | Inserted { entry; change_id; _ } ->
              Row.Applied entry, None, Effective.Moderator_inserted { change_id }
            | Replaced (canonical, replacement) ->
              let { Overlay.target_id; item; change_id; _ } = replacement in
              let entry = History_entry.create_with_id ~id:target_id item in
              ( Row.Applied entry
              , Some canonical
              , Effective.Moderator_replacement { target_id; change_id } )
          in
          let%bind origin =
            origin ~source_revision ~overlay_incarnation canonical provenance
          in
          let row : Row.t =
            { payload
            ; effective_ordinal
            ; canonical_ordinal = Option.map canonical ~f:fst
            ; canonical = Option.map canonical ~f:snd
            ; provenance
            ; origin
            }
          in
          match Hashtbl.add by_occurrence ~key:(Row.occurrence row) ~data:row with
          | `Ok -> Ok row
          | `Duplicate -> Error (invalid "duplicate effective projection occurrence"))
        |> Result.all
      in
      Some { rows = Array.of_list rows; by_occurrence }))
;;
