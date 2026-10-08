open! Core

module Id = struct
  type t =
    | Canonical of History_entry.Id.t
    | Local of
        { namespace : string
        ; local_id : string
        }
  [@@deriving compare, equal, hash, sexp]

  let canonical id = Canonical id

  let local ~namespace ~local_id =
    if String.is_empty namespace
    then Error "Projected row local namespace must be nonempty."
    else if String.is_empty local_id
    then Error "Projected row local ID must be nonempty."
    else Ok (Local { namespace; local_id })
  ;;

  let to_string = function
    | Canonical id -> History_entry.Id.to_string id
    | Local { namespace; local_id } ->
      Printf.sprintf "local:%d:%s:%s" (String.length namespace) namespace local_id
  ;;
end

type provenance =
  | Public of Agent_protocol.History.provenance
  | Canonical
  | Moderator_inserted of { change_id : int }
  | Moderator_replacement of
      { target_id : History_entry.Id.t
      ; change_id : int
      }
  | Streaming
  | Pending_approval
  | Placeholder
[@@deriving equal, sexp]

type disclosure =
  | Full
  | Visible
  | Redacted
[@@deriving equal, sexp]

type source =
  | Canonical of { entry_id : History_entry.Id.t }
  | Moderator_inserted of
      { entry_id : History_entry.Id.t
      ; change_id : int
      }
  | Moderator_replacement of
      { target_id : History_entry.Id.t
      ; change_id : int
      }
  | Streaming of
      { entry_id : History_entry.Id.t
      ; provider_item_id : string option
      ; call_id : string option
      }
  | Public_history of
      { entry_id : History_entry.Id.t
      ; provenance : Agent_protocol.History.provenance
      ; disclosure : disclosure
      }
  | Draft of { key : string }
  | Pending_approval of { local_id : string }
  | Placeholder of
      { local_id : string
      ; kind : string
      }
[@@deriving equal, sexp]

type t =
  { id : Id.t
  ; entry_id : History_entry.Id.t option
  ; message : Types.message
  ; provenance : provenance
  ; source : source
  ; editing_text : string option
  ; revision : int
  }

let render_equal a b =
  Id.equal a.id b.id
  && String.equal (fst a.message) (fst b.message)
  && String.equal (snd a.message) (snd b.message)
  && equal_provenance a.provenance b.provenance
  && equal_source a.source b.source
  && Option.equal String.equal a.editing_text b.editing_text
;;

let reconcile ~previous row =
  match previous with
  | None -> { row with revision = 0 }
  | Some previous when render_equal previous row ->
    { row with revision = previous.revision }
  | Some previous -> { row with revision = previous.revision + 1 }
;;

let canonical_row ?editing_text ~entry_id message =
  { id = Id.canonical entry_id
  ; entry_id = Some entry_id
  ; message
  ; provenance = Canonical
  ; source = Canonical { entry_id }
  ; editing_text
  ; revision = 0
  }
;;

let deletion_target t =
  match t.source with
  | Canonical { entry_id } | Public_history { entry_id; provenance = Canonical; _ } ->
    Some entry_id
  | Public_history
      { provenance = Runtime_notification _ | Runtime_authoring _; entry_id; _ } ->
    Some entry_id
  | Public_history { provenance = Moderator_inserted | Moderator_replaced _; _ }
  | Moderator_inserted _
  | Moderator_replacement _
  | Streaming _
  | Draft _
  | Pending_approval _
  | Placeholder _ -> None
;;

let editing_text t =
  match t.source with
  | Canonical _ | Public_history { disclosure = Full; provenance = Canonical; _ } ->
    t.editing_text
  | Public_history _
  | Moderator_inserted _
  | Moderator_replacement _
  | Streaming _
  | Draft _
  | Pending_approval _
  | Placeholder _ -> None
;;
