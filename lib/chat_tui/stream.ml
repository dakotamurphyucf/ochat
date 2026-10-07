open! Core

type t = Transcript.Draft.t

let create () =
  let document_limits = Transcript.Admission.default in
  let limits =
    Transcript.Draft.Limits.create
      ~max_scopes:256
      ~max_items:4096
      ~max_parts:16384
      ~max_unknown_events:256
      ~max_retained_bytes:(64 * 1024 * 1024)
      ~document_limits
    |> Result.ok_or_failwith
  in
  Transcript.Draft.create ~limits
;;

let apply t event = Transcript.Draft.apply t event |> Result.map ~f:fst

let rows_of_draft t =
  let root scope =
    match scope.Transcript.Scope.relation with
    | Root -> true
    | Nested _ -> false
  in
  let items =
    Transcript.Draft.items t
    |> List.filter ~f:(fun item -> root item.descriptor.scope)
    |> List.map ~f:Conversation.draft_row
  in
  let unknowns =
    Transcript.Draft.unknown_events t
    |> List.filter_mapi ~f:(fun index value ->
      if not (root value.scope)
      then None
      else (
        let key =
          Transcript.Scope.key value.scope
          |> Transcript.Scope.Key.sexp_of_t
          |> Sexp.to_string_mach
        in
        let local_id = Printf.sprintf "%d:%s:%d" (String.length key) key index in
        let id =
          Projected_message.Id.local ~namespace:"unknown-live-observation" ~local_id
          |> Result.ok_or_failwith
        in
        Some
          Projected_message.
            { id
            ; entry_id = None
            ; message =
                ( "unknown"
                , Util.sanitize
                    ~strip:false
                    (Printf.sprintf
                       "[Unknown live event: %s]\n%s"
                       value.provider_kind
                       (Jsonaf.to_string value.raw)) )
            ; provenance = Streaming
            ; source = Draft { key = local_id }
            ; editing_text = None
            ; revision = 0
            }))
  in
  items @ unknowns
;;

let remove_committed t entry_id =
  List.fold (Transcript.Draft.items t) ~init:t ~f:(fun t item ->
    match item.descriptor.scope.relation, item.descriptor.entry_id with
    | Root, Some id when History_entry.Id.equal id entry_id ->
      Transcript.Draft.remove_item t (Transcript.Item.key item.descriptor)
    | Root, Some _ | Root, None | Nested _, _ -> t)
;;

let rows = rows_of_draft
