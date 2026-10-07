open Core

module History = struct
  type t = History_entry.t list [@@deriving bin_io, sexp]
end

module Snapshot = struct
  include Chatml.Chatml_value_codec.Snapshot

  let rec validate = function
    | Float value when not (Float.is_finite value) ->
      Error "nonfinite ChatML snapshot float"
    | Int _ | Float _ | Bool _ | String _ | Unit -> Ok ()
    | Array values | Variant (_, values) ->
      List.fold_result values ~init:() ~f:(fun () value -> validate value)
    | Record fields ->
      Result.bind
        (Session_json.unique ~allow_empty:true (List.map fields ~f:fst))
        ~f:(fun () ->
          List.fold_result fields ~init:() ~f:(fun () (_, value) -> validate value))
  ;;

  let of_jsonaf json =
    Session_json.bounded
      (fun json ->
         Result.bind
           (Chatml.Chatml_value_codec.Snapshot.of_jsonaf json)
           ~f:(fun snapshot -> Result.map (validate snapshot) ~f:(fun () -> snapshot)))
      json
  ;;

  (* A bounded DAG shares the child shape between array/record/variant cases.
     The JSON admission depth limit is 256, so no admitted child lies beyond
     this ownership description. Unknown nested fields remain unowned. *)
  let shape =
    let open Session_json in
    let rec build depth =
      let scalar = object_ [ "kind", value; "value", value ] in
      let cases =
        [ "int", scalar
        ; "float", scalar
        ; "bool", scalar
        ; "string", scalar
        ; "unit", object_ [ "kind", value ]
        ]
      in
      let cases =
        if depth = 0
        then cases
        else (
          let child = build (depth - 1) in
          cases
          @ [ "array", object_ [ "kind", value; "values", array child ]
            ; "record", object_ [ "kind", value; "fields", named_values_shape child ]
            ; "variant", object_ [ "kind", value; "tag", value; "payload", array child ]
            ])
      in
      match Document_schema.Shape.tagged_object ~discriminator:"kind" cases with
      | Ok shape -> shape
      | Error error -> raise_s [%sexp (error : Document_schema.Error.t)]
    in
    build 256
  ;;
end

(* ----------------------------------------------------------------------- *)
(*  Versioning                                                             *)
(* ----------------------------------------------------------------------- *)

(** Runtime marker retained for callers. Durable document versions are owned
    independently by the codec below, not by the OCaml record layout. *)
let current_version = 1

module Task = struct
  type state =
    | Pending
    | In_progress
    | Done
  [@@deriving bin_io, sexp]

  let state_to_jsonaf = function
    | Pending -> `String "pending"
    | In_progress -> `String "in_progress"
    | Done -> `String "done"
  ;;

  let state_of_jsonaf = function
    | `String "pending" -> Ok Pending
    | `String "in_progress" -> Ok In_progress
    | `String "done" -> Ok Done
    | _ -> Error "unknown task state"
  ;;

  type t =
    { id : string
    ; title : string
    ; state : state
    }
  [@@deriving bin_io, sexp]

  let to_jsonaf (t : t) =
    `Object
      [ "id", Session_json.encode_string t.id
      ; "title", Session_json.encode_string t.title
      ; "state", state_to_jsonaf t.state
      ]
  ;;

  let of_jsonaf json =
    let open Result.Let_syntax in
    let%bind fields = Session_json.fields json in
    let%bind id = Session_json.field fields "id" Session_json.string in
    let%bind title = Session_json.field fields "title" Session_json.string in
    let%bind state = Session_json.field fields "state" state_of_jsonaf in
    Ok { id; title; state }
  ;;

  let shape =
    Session_json.object_
      [ "id", Session_json.value
      ; "title", Session_json.value
      ; "state", Session_json.value
      ]
  ;;

  let create ?id ~title ?(state = Pending) () : t =
    let default_id () =
      let open Core in
      let data =
        let time_ns =
          Time_ns.to_int63_ns_since_epoch (Time_ns.now ()) |> Int63.to_string
        in
        time_ns ^ Int.to_string (Random.bits ())
      in
      Md5.digest_string data |> Md5.to_hex
    in
    let id = Option.value_or_thunk id ~default:default_id in
    { id; title; state }
  ;;
end

module Moderator_snapshot = struct
  module Item = struct
    type t =
      { id : string
      ; value : Snapshot.t
      }
    [@@deriving bin_io, sexp]

    let to_jsonaf (t : t) =
      `Object
        [ "id", Session_json.encode_string t.id; "value", Snapshot.to_jsonaf t.value ]
    ;;

    let of_jsonaf json =
      let open Result.Let_syntax in
      let%bind fields = Session_json.fields json in
      let%bind id = Session_json.field fields "id" Session_json.string in
      let%bind value = Session_json.field fields "value" Snapshot.of_jsonaf in
      Ok { id; value }
    ;;

    let shape = Session_json.object_ [ "id", Session_json.value; "value", Snapshot.shape ]
  end

  module Overlay = struct
    type replacement =
      { target_id : string
      ; item : Item.t
      }
    [@@deriving bin_io, sexp]

    let replacement_to_jsonaf (t : replacement) =
      `Object
        [ "target_id", Session_json.encode_string t.target_id
        ; "item", Item.to_jsonaf t.item
        ]
    ;;

    let replacement_of_jsonaf json =
      let open Result.Let_syntax in
      let%bind fields = Session_json.fields json in
      let%bind target_id = Session_json.field fields "target_id" Session_json.string in
      let%bind item = Session_json.field fields "item" Item.of_jsonaf in
      Ok { target_id; item }
    ;;

    let replacement_shape =
      Session_json.object_ [ "target_id", Session_json.value; "item", Item.shape ]
    ;;

    type t =
      { prepended_system_items : Item.t list
      ; appended_items : Item.t list
      ; replacements : replacement list
      ; deleted_item_ids : string list
      ; halted_reason : string option
      }
    [@@deriving bin_io, sexp]

    let to_jsonaf (t : t) =
      `Object
        [ ( "prepended_system_items"
          , (Session_json.encode_list Item.to_jsonaf) t.prepended_system_items )
        ; "appended_items", (Session_json.encode_list Item.to_jsonaf) t.appended_items
        ; "replacements", (Session_json.encode_list replacement_to_jsonaf) t.replacements
        ; ( "deleted_item_ids"
          , (Session_json.encode_list Session_json.encode_string) t.deleted_item_ids )
        ; ( "halted_reason"
          , (Session_json.encode_option Session_json.encode_string) t.halted_reason )
        ]
    ;;

    let of_jsonaf json =
      let open Result.Let_syntax in
      let%bind fields = Session_json.fields json in
      let%bind prepended_system_items =
        Session_json.field
          fields
          "prepended_system_items"
          (Session_json.list Item.of_jsonaf)
      in
      let%bind appended_items =
        Session_json.field fields "appended_items" (Session_json.list Item.of_jsonaf)
      in
      let%bind replacements =
        Session_json.field fields "replacements" (Session_json.list replacement_of_jsonaf)
      in
      let%bind deleted_item_ids =
        Session_json.field
          fields
          "deleted_item_ids"
          (Session_json.list Session_json.string)
      in
      let%bind halted_reason =
        Session_json.field
          fields
          "halted_reason"
          (Session_json.option Session_json.string)
      in
      Ok
        { prepended_system_items
        ; appended_items
        ; replacements
        ; deleted_item_ids
        ; halted_reason
        }
    ;;

    let shape =
      Session_json.object_
        [ "prepended_system_items", Session_json.array ~identity:"id" Item.shape
        ; "appended_items", Session_json.array ~identity:"id" Item.shape
        ; "replacements", Session_json.array ~identity:"target_id" replacement_shape
        ; "deleted_item_ids", Session_json.array Session_json.value
        ; "halted_reason", Session_json.nullable Session_json.value
        ]
    ;;

    let empty =
      { prepended_system_items = []
      ; appended_items = []
      ; replacements = []
      ; deleted_item_ids = []
      ; halted_reason = None
      }
    ;;
  end

  type t =
    { script_id : string
    ; script_source_hash : string
    ; current_state : Snapshot.t
    ; queued_internal_events : Snapshot.t list
    ; halted : bool
    ; overlay : Overlay.t
    }
  [@@deriving bin_io, sexp]

  let to_jsonaf (t : t) =
    `Object
      [ "script_id", Session_json.encode_string t.script_id
      ; "script_source_hash", Session_json.encode_string t.script_source_hash
      ; "current_state", Snapshot.to_jsonaf t.current_state
      ; ( "queued_internal_events"
        , (Session_json.encode_list Snapshot.to_jsonaf) t.queued_internal_events )
      ; "halted", Session_json.encode_bool t.halted
      ; "overlay", Overlay.to_jsonaf t.overlay
      ]
  ;;

  let of_jsonaf json =
    let open Result.Let_syntax in
    let%bind fields = Session_json.fields json in
    let%bind script_id = Session_json.field fields "script_id" Session_json.string in
    let%bind script_source_hash =
      Session_json.field fields "script_source_hash" Session_json.string
    in
    let%bind current_state =
      Session_json.field fields "current_state" Snapshot.of_jsonaf
    in
    let%bind queued_internal_events =
      Session_json.field
        fields
        "queued_internal_events"
        (Session_json.list Snapshot.of_jsonaf)
    in
    let%bind halted = Session_json.field fields "halted" Session_json.bool in
    let%bind overlay = Session_json.field fields "overlay" Overlay.of_jsonaf in
    Ok
      { script_id
      ; script_source_hash
      ; current_state
      ; queued_internal_events
      ; halted
      ; overlay
      }
  ;;

  let shape =
    Session_json.object_
      [ "script_id", Session_json.value
      ; "script_source_hash", Session_json.value
      ; "current_state", Snapshot.shape
      ; "queued_internal_events", Session_json.array Snapshot.shape
      ; "halted", Session_json.value
      ; "overlay", Overlay.shape
      ]
  ;;

  let create
        ~script_id
        ~script_source_hash
        ?(current_state = Snapshot.Unit)
        ?(queued_internal_events = [])
        ?(halted = false)
        ?(overlay = Overlay.empty)
        ()
    =
    { script_id
    ; script_source_hash
    ; current_state
    ; queued_internal_events
    ; halted
    ; overlay
    }
  ;;
end

module Moderator_state = struct
  module Identity_snapshot = struct
    module Inserted = struct
      type t =
        { entry_id : History_entry.Id.t
        ; change_id : int
        ; value : History_entry.Payload.t
        ; script_label : string option
        }
      [@@deriving bin_io, sexp]

      let to_jsonaf (t : t) =
        `Object
          [ "entry_id", (fun id -> `String (History_entry.Id.to_string id)) t.entry_id
          ; "change_id", Session_json.encode_int t.change_id
          ; "value", History_entry.Payload.to_json t.value
          ; ( "script_label"
            , (Session_json.encode_option Session_json.encode_string) t.script_label )
          ]
      ;;

      let of_jsonaf json =
        let open Result.Let_syntax in
        let%bind fields = Session_json.fields json in
        let%bind entry_id =
          Session_json.field fields "entry_id" (fun json ->
            Result.bind (Session_json.string json) ~f:History_entry.Id.of_string)
        in
        let%bind change_id = Session_json.field fields "change_id" Session_json.int in
        let%bind value =
          Session_json.field fields "value" History_entry.Payload.of_json
        in
        let%bind script_label =
          Session_json.field
            fields
            "script_label"
            (Session_json.option Session_json.string)
        in
        Ok { entry_id; change_id; value; script_label }
      ;;

      let shape =
        Session_json.object_
          [ "entry_id", Session_json.value
          ; "change_id", Session_json.value
          ; "value", Session_json.value
          ; "script_label", Session_json.nullable Session_json.value
          ]
      ;;
    end

    module Replacement = struct
      type t =
        { target_id : History_entry.Id.t
        ; change_id : int
        ; value : History_entry.Payload.t
        ; script_label : string option
        }
      [@@deriving bin_io, sexp]

      let to_jsonaf (t : t) =
        `Object
          [ "target_id", (fun id -> `String (History_entry.Id.to_string id)) t.target_id
          ; "change_id", Session_json.encode_int t.change_id
          ; "value", History_entry.Payload.to_json t.value
          ; ( "script_label"
            , (Session_json.encode_option Session_json.encode_string) t.script_label )
          ]
      ;;

      let of_jsonaf json =
        let open Result.Let_syntax in
        let%bind fields = Session_json.fields json in
        let%bind target_id =
          Session_json.field fields "target_id" (fun json ->
            Result.bind (Session_json.string json) ~f:History_entry.Id.of_string)
        in
        let%bind change_id = Session_json.field fields "change_id" Session_json.int in
        let%bind value =
          Session_json.field fields "value" History_entry.Payload.of_json
        in
        let%bind script_label =
          Session_json.field
            fields
            "script_label"
            (Session_json.option Session_json.string)
        in
        Ok { target_id; change_id; value; script_label }
      ;;

      let shape =
        Session_json.object_
          [ "target_id", Session_json.value
          ; "change_id", Session_json.value
          ; "value", Session_json.value
          ; "script_label", Session_json.nullable Session_json.value
          ]
      ;;
    end

    module Tombstone = struct
      type t =
        { target_id : History_entry.Id.t
        ; change_id : int
        }
      [@@deriving bin_io, sexp]

      let to_jsonaf (t : t) =
        `Object
          [ "target_id", (fun id -> `String (History_entry.Id.to_string id)) t.target_id
          ; "change_id", Session_json.encode_int t.change_id
          ]
      ;;

      let of_jsonaf json =
        let open Result.Let_syntax in
        let%bind fields = Session_json.fields json in
        let%bind target_id =
          Session_json.field fields "target_id" (fun json ->
            Result.bind (Session_json.string json) ~f:History_entry.Id.of_string)
        in
        let%bind change_id = Session_json.field fields "change_id" Session_json.int in
        Ok { target_id; change_id }
      ;;

      let shape =
        Session_json.object_
          [ "target_id", Session_json.value; "change_id", Session_json.value ]
      ;;
    end

    type t =
      { script_id : string
      ; script_source_hash : string
      ; current_state : Snapshot.t
      ; queued_internal_events : Snapshot.t list
      ; halted : bool
      ; revision : int
      ; next_change_id : int
      ; prepended_items : Inserted.t list
      ; appended_items : Inserted.t list
      ; replacements : Replacement.t list
      ; tombstones : Tombstone.t list
      ; halted_reason : string option
      }
    [@@deriving bin_io, sexp]

    let to_jsonaf (t : t) =
      `Object
        [ "script_id", Session_json.encode_string t.script_id
        ; "script_source_hash", Session_json.encode_string t.script_source_hash
        ; "current_state", Snapshot.to_jsonaf t.current_state
        ; ( "queued_internal_events"
          , (Session_json.encode_list Snapshot.to_jsonaf) t.queued_internal_events )
        ; "halted", Session_json.encode_bool t.halted
        ; "revision", Session_json.encode_int t.revision
        ; "next_change_id", Session_json.encode_int t.next_change_id
        ; ( "prepended_items"
          , (Session_json.encode_list Inserted.to_jsonaf) t.prepended_items )
        ; "appended_items", (Session_json.encode_list Inserted.to_jsonaf) t.appended_items
        ; "replacements", (Session_json.encode_list Replacement.to_jsonaf) t.replacements
        ; "tombstones", (Session_json.encode_list Tombstone.to_jsonaf) t.tombstones
        ; ( "halted_reason"
          , (Session_json.encode_option Session_json.encode_string) t.halted_reason )
        ]
    ;;

    let of_jsonaf json =
      let open Result.Let_syntax in
      let%bind fields = Session_json.fields json in
      let%bind script_id = Session_json.field fields "script_id" Session_json.string in
      let%bind script_source_hash =
        Session_json.field fields "script_source_hash" Session_json.string
      in
      let%bind current_state =
        Session_json.field fields "current_state" Snapshot.of_jsonaf
      in
      let%bind queued_internal_events =
        Session_json.field
          fields
          "queued_internal_events"
          (Session_json.list Snapshot.of_jsonaf)
      in
      let%bind halted = Session_json.field fields "halted" Session_json.bool in
      let%bind revision = Session_json.field fields "revision" Session_json.int in
      let%bind next_change_id =
        Session_json.field fields "next_change_id" Session_json.int
      in
      let%bind prepended_items =
        Session_json.field fields "prepended_items" (Session_json.list Inserted.of_jsonaf)
      in
      let%bind appended_items =
        Session_json.field fields "appended_items" (Session_json.list Inserted.of_jsonaf)
      in
      let%bind replacements =
        Session_json.field fields "replacements" (Session_json.list Replacement.of_jsonaf)
      in
      let%bind tombstones =
        Session_json.field fields "tombstones" (Session_json.list Tombstone.of_jsonaf)
      in
      let%bind halted_reason =
        Session_json.field
          fields
          "halted_reason"
          (Session_json.option Session_json.string)
      in
      Ok
        { script_id
        ; script_source_hash
        ; current_state
        ; queued_internal_events
        ; halted
        ; revision
        ; next_change_id
        ; prepended_items
        ; appended_items
        ; replacements
        ; tombstones
        ; halted_reason
        }
    ;;

    let shape =
      Session_json.object_
        [ "script_id", Session_json.value
        ; "script_source_hash", Session_json.value
        ; "current_state", Snapshot.shape
        ; "queued_internal_events", Session_json.array Snapshot.shape
        ; "halted", Session_json.value
        ; "revision", Session_json.value
        ; "next_change_id", Session_json.value
        ; "prepended_items", Session_json.array ~identity:"entry_id" Inserted.shape
        ; "appended_items", Session_json.array ~identity:"entry_id" Inserted.shape
        ; "replacements", Session_json.array ~identity:"target_id" Replacement.shape
        ; "tombstones", Session_json.array ~identity:"target_id" Tombstone.shape
        ; "halted_reason", Session_json.nullable Session_json.value
        ]
    ;;

    let validate (t : t) =
      let open Result.Let_syntax in
      let inserted = t.prepended_items @ t.appended_items in
      let changes =
        List.map inserted ~f:(fun x -> x.Inserted.change_id)
        @ List.map t.replacements ~f:(fun x -> x.Replacement.change_id)
        @ List.map t.tombstones ~f:(fun x -> x.Tombstone.change_id)
      in
      let%bind () =
        if
          t.revision < 0
          || t.next_change_id < 0
          || List.exists changes ~f:(fun id -> id < 0 || id >= t.next_change_id)
        then
          Error
            "moderator counters must be nonnegative and changes below the next change ID"
        else Ok ()
      in
      let%bind () =
        match List.find_a_dup changes ~compare:Int.compare with
        | Some _ -> Error "duplicate moderator change ID"
        | None -> Ok ()
      in
      let%bind () =
        Session_json.unique
          (List.map inserted ~f:(fun x -> History_entry.Id.to_string x.Inserted.entry_id))
      in
      let%bind () =
        Session_json.unique
          (List.map t.replacements ~f:(fun x ->
             History_entry.Id.to_string x.Replacement.target_id))
      in
      let%bind () =
        Session_json.unique
          (List.map t.tombstones ~f:(fun x ->
             History_entry.Id.to_string x.Tombstone.target_id))
      in
      let payloads =
        List.map inserted ~f:(fun x -> x.Inserted.value)
        @ List.map t.replacements ~f:(fun x -> x.Replacement.value)
      in
      let%bind () =
        List.fold_result payloads ~init:() ~f:(fun () payload ->
          History_entry.Payload.validate payload)
      in
      List.fold_result
        (t.current_state :: t.queued_internal_events)
        ~init:()
        ~f:(fun () value -> Snapshot.validate value)
    ;;

    let of_jsonaf_unchecked = of_jsonaf

    let validate_history_ids t ~history_ids =
      let history_ids = Hash_set.of_list (module History_entry.Id) history_ids in
      if
        List.exists (t.prepended_items @ t.appended_items) ~f:(fun item ->
          Hash_set.mem history_ids item.Inserted.entry_id)
      then Error "moderator insertion collides with an existing history identity"
      else Ok ()
    ;;

    let of_jsonaf json =
      Session_json.bounded
        (fun json ->
           Result.bind (of_jsonaf_unchecked json) ~f:(fun value ->
             Result.map (validate value) ~f:(fun () -> value)))
        json
    ;;
  end

  type t =
    { legacy_snapshot : Moderator_snapshot.t option
    ; identity_snapshot : Identity_snapshot.t option
    ; extensions : (string * Snapshot.t) list
    }
  [@@deriving bin_io, sexp]

  let validate (t : t) =
    let open Result.Let_syntax in
    let%bind () = Session_json.unique ~allow_empty:true (List.map t.extensions ~f:fst) in
    let%bind () =
      List.fold_result t.extensions ~init:() ~f:(fun () (_, value) ->
        Snapshot.validate value)
    in
    let%bind () =
      match t.identity_snapshot with
      | None -> Ok ()
      | Some snapshot -> Identity_snapshot.validate snapshot
    in
    match t.legacy_snapshot with
    | None -> Ok ()
    | Some snapshot ->
      let inserted =
        snapshot.overlay.prepended_system_items @ snapshot.overlay.appended_items
      in
      let%bind () =
        Session_json.unique
          (List.map inserted ~f:(fun item -> item.Moderator_snapshot.Item.id))
      in
      let%bind () =
        Session_json.unique
          (List.map snapshot.overlay.replacements ~f:(fun item ->
             item.Moderator_snapshot.Overlay.target_id))
      in
      let%bind () = Session_json.unique snapshot.overlay.deleted_item_ids in
      let values =
        (snapshot.current_state :: snapshot.queued_internal_events)
        @ List.map inserted ~f:(fun item -> item.Moderator_snapshot.Item.value)
        @ List.map snapshot.overlay.replacements ~f:(fun replacement ->
          replacement.Moderator_snapshot.Overlay.item.value)
      in
      List.fold_result values ~init:() ~f:(fun () value -> Snapshot.validate value)
  ;;

  let to_jsonaf (t : t) =
    `Object
      [ ( "legacy_snapshot"
        , (Session_json.encode_option Moderator_snapshot.to_jsonaf) t.legacy_snapshot )
      ; ( "identity_snapshot"
        , (Session_json.encode_option Identity_snapshot.to_jsonaf) t.identity_snapshot )
      ; "extensions", (Session_json.encode_named_values Snapshot.to_jsonaf) t.extensions
      ]
  ;;

  let of_jsonaf json =
    let open Result.Let_syntax in
    let%bind fields = Session_json.fields json in
    let%bind legacy_snapshot =
      Session_json.field
        fields
        "legacy_snapshot"
        (Session_json.option Moderator_snapshot.of_jsonaf)
    in
    let%bind identity_snapshot =
      Session_json.field
        fields
        "identity_snapshot"
        (Session_json.option Identity_snapshot.of_jsonaf)
    in
    let%bind extensions =
      Session_json.field
        fields
        "extensions"
        (Session_json.named_values Snapshot.of_jsonaf)
    in
    let value = { legacy_snapshot; identity_snapshot; extensions } in
    let%map () = validate value in
    value
  ;;

  let shape =
    Session_json.object_
      [ "legacy_snapshot", Session_json.nullable Moderator_snapshot.shape
      ; "identity_snapshot", Session_json.nullable Identity_snapshot.shape
      ; "extensions", Session_json.named_values_shape Snapshot.shape
      ]
  ;;

  let of_legacy legacy_snapshot =
    { legacy_snapshot; identity_snapshot = None; extensions = [] }
  ;;
end

module Shell_state = struct
  module Request_kind = struct
    type t =
      | Structured
      | Script_file
      | Raw_shell
    [@@deriving bin_io, sexp]

    let to_jsonaf = function
      | Structured -> `String "structured"
      | Script_file -> `String "script_file"
      | Raw_shell -> `String "raw_shell"
    ;;

    let of_jsonaf = function
      | `String "structured" -> Ok Structured
      | `String "script_file" -> Ok Script_file
      | `String "raw_shell" -> Ok Raw_shell
      | _ -> Error "unknown shell request kind"
    ;;

    let shape = Session_json.value
  end

  module Approval_scope = struct
    type t =
      | Exact_session
      | Prefix_session of { prefix : string list }
      | Durable_exact
    [@@deriving bin_io, sexp]

    let to_jsonaf = function
      | Exact_session -> `Object [ "kind", `String "exact_session" ]
      | Durable_exact -> `Object [ "kind", `String "durable_exact" ]
      | Prefix_session { prefix } ->
        `Object
          [ "kind", `String "prefix_session"
          ; "prefix", Session_json.encode_list Session_json.encode_string prefix
          ]
    ;;

    let of_jsonaf json =
      let open Result.Let_syntax in
      let%bind fields = Session_json.fields json in
      let%bind kind = Session_json.field fields "kind" Session_json.string in
      match kind with
      | "exact_session" -> Ok Exact_session
      | "durable_exact" -> Ok Durable_exact
      | "prefix_session" ->
        let%map prefix =
          Session_json.field fields "prefix" (Session_json.list Session_json.string)
        in
        Prefix_session { prefix }
      | _ -> Error "unknown shell approval scope"
    ;;

    let shape =
      let open Session_json in
      match
        Document_schema.Shape.tagged_object
          ~discriminator:"kind"
          [ "exact_session", object_ [ "kind", value ]
          ; "durable_exact", object_ [ "kind", value ]
          ; "prefix_session", object_ [ "kind", value; "prefix", array value ]
          ]
      with
      | Ok shape -> shape
      | Error error -> raise_s [%sexp (error : Document_schema.Error.t)]
    ;;
  end

  module Reviewer = struct
    type t =
      { source : string
      ; reviewer_id : string option
      }
    [@@deriving bin_io, sexp]

    let to_jsonaf (t : t) =
      `Object
        [ "source", Session_json.encode_string t.source
        ; ( "reviewer_id"
          , (Session_json.encode_option Session_json.encode_string) t.reviewer_id )
        ]
    ;;

    let of_jsonaf json =
      let open Result.Let_syntax in
      let%bind fields = Session_json.fields json in
      let%bind source = Session_json.field fields "source" Session_json.string in
      let%bind reviewer_id =
        Session_json.field fields "reviewer_id" (Session_json.option Session_json.string)
      in
      Ok { source; reviewer_id }
    ;;

    let shape =
      Session_json.object_
        [ "source", Session_json.value
        ; "reviewer_id", Session_json.nullable Session_json.value
        ]
    ;;
  end

  module Approval_grant = struct
    type persisted =
      { grant_id : string
      ; manifest_sha256 : string
      ; runtime_id : string
      ; request_kind : Request_kind.t
      ; command_sha256 : string
      ; executable_sha256 : string
      ; argv : string list
      ; argv_prefix : string list option
      ; cwd_sha256 : string
      ; environment_sha256 : string
      ; stdin_sha256 : string option
      ; stdin_bytes : int
      ; script_sha256 : string option
      ; scope : Approval_scope.t
      ; session_id : string option
      ; user_id : string option
      ; host_id : string option
      ; created_at_ns : int64
      ; expires_at_ns : int64 option
      ; last_used_at_ns : int64 option
      ; reviewer : Reviewer.t
      ; revoked_at_ns : int64 option
      ; revocation_reason : string option
      }
    [@@deriving bin_io, sexp]

    let to_jsonaf (t : persisted) =
      `Object
        [ "grant_id", Session_json.encode_string t.grant_id
        ; "manifest_sha256", Session_json.encode_string t.manifest_sha256
        ; "runtime_id", Session_json.encode_string t.runtime_id
        ; "request_kind", Request_kind.to_jsonaf t.request_kind
        ; "command_sha256", Session_json.encode_string t.command_sha256
        ; "executable_sha256", Session_json.encode_string t.executable_sha256
        ; "argv", (Session_json.encode_list Session_json.encode_string) t.argv
        ; ( "argv_prefix"
          , (Session_json.encode_option
               (Session_json.encode_list Session_json.encode_string))
              t.argv_prefix )
        ; "cwd_sha256", Session_json.encode_string t.cwd_sha256
        ; "environment_sha256", Session_json.encode_string t.environment_sha256
        ; ( "stdin_sha256"
          , (Session_json.encode_option Session_json.encode_string) t.stdin_sha256 )
        ; "stdin_bytes", Session_json.encode_int t.stdin_bytes
        ; ( "script_sha256"
          , (Session_json.encode_option Session_json.encode_string) t.script_sha256 )
        ; "scope", Approval_scope.to_jsonaf t.scope
        ; ( "session_id"
          , (Session_json.encode_option Session_json.encode_string) t.session_id )
        ; "user_id", (Session_json.encode_option Session_json.encode_string) t.user_id
        ; "host_id", (Session_json.encode_option Session_json.encode_string) t.host_id
        ; "created_at_ns", Session_json.encode_int64 t.created_at_ns
        ; ( "expires_at_ns"
          , (Session_json.encode_option Session_json.encode_int64) t.expires_at_ns )
        ; ( "last_used_at_ns"
          , (Session_json.encode_option Session_json.encode_int64) t.last_used_at_ns )
        ; "reviewer", Reviewer.to_jsonaf t.reviewer
        ; ( "revoked_at_ns"
          , (Session_json.encode_option Session_json.encode_int64) t.revoked_at_ns )
        ; ( "revocation_reason"
          , (Session_json.encode_option Session_json.encode_string) t.revocation_reason )
        ]
    ;;

    let of_jsonaf json =
      let open Result.Let_syntax in
      let%bind fields = Session_json.fields json in
      let%bind grant_id = Session_json.field fields "grant_id" Session_json.string in
      let%bind manifest_sha256 =
        Session_json.field fields "manifest_sha256" Session_json.string
      in
      let%bind runtime_id = Session_json.field fields "runtime_id" Session_json.string in
      let%bind request_kind =
        Session_json.field fields "request_kind" Request_kind.of_jsonaf
      in
      let%bind command_sha256 =
        Session_json.field fields "command_sha256" Session_json.string
      in
      let%bind executable_sha256 =
        Session_json.field fields "executable_sha256" Session_json.string
      in
      let%bind argv =
        Session_json.field fields "argv" (Session_json.list Session_json.string)
      in
      let%bind argv_prefix =
        Session_json.field
          fields
          "argv_prefix"
          (Session_json.option (Session_json.list Session_json.string))
      in
      let%bind cwd_sha256 = Session_json.field fields "cwd_sha256" Session_json.string in
      let%bind environment_sha256 =
        Session_json.field fields "environment_sha256" Session_json.string
      in
      let%bind stdin_sha256 =
        Session_json.field fields "stdin_sha256" (Session_json.option Session_json.string)
      in
      let%bind stdin_bytes = Session_json.field fields "stdin_bytes" Session_json.int in
      let%bind script_sha256 =
        Session_json.field
          fields
          "script_sha256"
          (Session_json.option Session_json.string)
      in
      let%bind scope = Session_json.field fields "scope" Approval_scope.of_jsonaf in
      let%bind session_id =
        Session_json.field fields "session_id" (Session_json.option Session_json.string)
      in
      let%bind user_id =
        Session_json.field fields "user_id" (Session_json.option Session_json.string)
      in
      let%bind host_id =
        Session_json.field fields "host_id" (Session_json.option Session_json.string)
      in
      let%bind created_at_ns =
        Session_json.field fields "created_at_ns" Session_json.int64
      in
      let%bind expires_at_ns =
        Session_json.field fields "expires_at_ns" (Session_json.option Session_json.int64)
      in
      let%bind last_used_at_ns =
        Session_json.field
          fields
          "last_used_at_ns"
          (Session_json.option Session_json.int64)
      in
      let%bind reviewer = Session_json.field fields "reviewer" Reviewer.of_jsonaf in
      let%bind revoked_at_ns =
        Session_json.field fields "revoked_at_ns" (Session_json.option Session_json.int64)
      in
      let%bind revocation_reason =
        Session_json.field
          fields
          "revocation_reason"
          (Session_json.option Session_json.string)
      in
      Ok
        { grant_id
        ; manifest_sha256
        ; runtime_id
        ; request_kind
        ; command_sha256
        ; executable_sha256
        ; argv
        ; argv_prefix
        ; cwd_sha256
        ; environment_sha256
        ; stdin_sha256
        ; stdin_bytes
        ; script_sha256
        ; scope
        ; session_id
        ; user_id
        ; host_id
        ; created_at_ns
        ; expires_at_ns
        ; last_used_at_ns
        ; reviewer
        ; revoked_at_ns
        ; revocation_reason
        }
    ;;

    let shape =
      Session_json.object_
        [ "grant_id", Session_json.value
        ; "manifest_sha256", Session_json.value
        ; "runtime_id", Session_json.value
        ; "request_kind", Request_kind.shape
        ; "command_sha256", Session_json.value
        ; "executable_sha256", Session_json.value
        ; "argv", Session_json.array Session_json.value
        ; "argv_prefix", Session_json.nullable (Session_json.array Session_json.value)
        ; "cwd_sha256", Session_json.value
        ; "environment_sha256", Session_json.value
        ; "stdin_sha256", Session_json.nullable Session_json.value
        ; "stdin_bytes", Session_json.value
        ; "script_sha256", Session_json.nullable Session_json.value
        ; "scope", Approval_scope.shape
        ; "session_id", Session_json.nullable Session_json.value
        ; "user_id", Session_json.nullable Session_json.value
        ; "host_id", Session_json.nullable Session_json.value
        ; "created_at_ns", Session_json.value
        ; "expires_at_ns", Session_json.nullable Session_json.value
        ; "last_used_at_ns", Session_json.nullable Session_json.value
        ; "reviewer", Reviewer.shape
        ; "revoked_at_ns", Session_json.nullable Session_json.value
        ; "revocation_reason", Session_json.nullable Session_json.value
        ]
    ;;
  end

  module Manifest_grant = struct
    type persisted =
      { grant_id : string
      ; manifest_sha256 : string
      ; canonical_source_root : string
      ; repository_identity : string option
      ; source_sha256 : string
      ; signer : string option
      ; issuer : string option
      ; audience : string list
      ; schema_version : int
      ; builtin_versions : (string * string) list
      ; imported_source_sha256 : (string * string) list
      ; session_id : string option
      ; user_id : string option
      ; host_id : string option
      ; created_at_ns : int64
      ; expires_at_ns : int64 option
      ; revoked_at_ns : int64 option
      ; revocation_reason : string option
      }
    [@@deriving bin_io, sexp]

    let to_jsonaf (t : persisted) =
      `Object
        [ "grant_id", Session_json.encode_string t.grant_id
        ; "manifest_sha256", Session_json.encode_string t.manifest_sha256
        ; "canonical_source_root", Session_json.encode_string t.canonical_source_root
        ; ( "repository_identity"
          , (Session_json.encode_option Session_json.encode_string) t.repository_identity
          )
        ; "source_sha256", Session_json.encode_string t.source_sha256
        ; "signer", (Session_json.encode_option Session_json.encode_string) t.signer
        ; "issuer", (Session_json.encode_option Session_json.encode_string) t.issuer
        ; "audience", (Session_json.encode_list Session_json.encode_string) t.audience
        ; "schema_version", Session_json.encode_int t.schema_version
        ; ( "builtin_versions"
          , (Session_json.encode_named_values Session_json.encode_string)
              t.builtin_versions )
        ; ( "imported_source_sha256"
          , (Session_json.encode_named_values Session_json.encode_string)
              t.imported_source_sha256 )
        ; ( "session_id"
          , (Session_json.encode_option Session_json.encode_string) t.session_id )
        ; "user_id", (Session_json.encode_option Session_json.encode_string) t.user_id
        ; "host_id", (Session_json.encode_option Session_json.encode_string) t.host_id
        ; "created_at_ns", Session_json.encode_int64 t.created_at_ns
        ; ( "expires_at_ns"
          , (Session_json.encode_option Session_json.encode_int64) t.expires_at_ns )
        ; ( "revoked_at_ns"
          , (Session_json.encode_option Session_json.encode_int64) t.revoked_at_ns )
        ; ( "revocation_reason"
          , (Session_json.encode_option Session_json.encode_string) t.revocation_reason )
        ]
    ;;

    let of_jsonaf json =
      let open Result.Let_syntax in
      let%bind fields = Session_json.fields json in
      let%bind grant_id = Session_json.field fields "grant_id" Session_json.string in
      let%bind manifest_sha256 =
        Session_json.field fields "manifest_sha256" Session_json.string
      in
      let%bind canonical_source_root =
        Session_json.field fields "canonical_source_root" Session_json.string
      in
      let%bind repository_identity =
        Session_json.field
          fields
          "repository_identity"
          (Session_json.option Session_json.string)
      in
      let%bind source_sha256 =
        Session_json.field fields "source_sha256" Session_json.string
      in
      let%bind signer =
        Session_json.field fields "signer" (Session_json.option Session_json.string)
      in
      let%bind issuer =
        Session_json.field fields "issuer" (Session_json.option Session_json.string)
      in
      let%bind audience =
        Session_json.field fields "audience" (Session_json.list Session_json.string)
      in
      let%bind schema_version =
        Session_json.field fields "schema_version" Session_json.int
      in
      let%bind builtin_versions =
        Session_json.field
          fields
          "builtin_versions"
          (Session_json.named_values Session_json.string)
      in
      let%bind imported_source_sha256 =
        Session_json.field
          fields
          "imported_source_sha256"
          (Session_json.named_values Session_json.string)
      in
      let%bind session_id =
        Session_json.field fields "session_id" (Session_json.option Session_json.string)
      in
      let%bind user_id =
        Session_json.field fields "user_id" (Session_json.option Session_json.string)
      in
      let%bind host_id =
        Session_json.field fields "host_id" (Session_json.option Session_json.string)
      in
      let%bind created_at_ns =
        Session_json.field fields "created_at_ns" Session_json.int64
      in
      let%bind expires_at_ns =
        Session_json.field fields "expires_at_ns" (Session_json.option Session_json.int64)
      in
      let%bind revoked_at_ns =
        Session_json.field fields "revoked_at_ns" (Session_json.option Session_json.int64)
      in
      let%bind revocation_reason =
        Session_json.field
          fields
          "revocation_reason"
          (Session_json.option Session_json.string)
      in
      Ok
        { grant_id
        ; manifest_sha256
        ; canonical_source_root
        ; repository_identity
        ; source_sha256
        ; signer
        ; issuer
        ; audience
        ; schema_version
        ; builtin_versions
        ; imported_source_sha256
        ; session_id
        ; user_id
        ; host_id
        ; created_at_ns
        ; expires_at_ns
        ; revoked_at_ns
        ; revocation_reason
        }
    ;;

    let shape =
      Session_json.object_
        [ "grant_id", Session_json.value
        ; "manifest_sha256", Session_json.value
        ; "canonical_source_root", Session_json.value
        ; "repository_identity", Session_json.nullable Session_json.value
        ; "source_sha256", Session_json.value
        ; "signer", Session_json.nullable Session_json.value
        ; "issuer", Session_json.nullable Session_json.value
        ; "audience", Session_json.array Session_json.value
        ; "schema_version", Session_json.value
        ; "builtin_versions", Session_json.named_values_shape Session_json.value
        ; "imported_source_sha256", Session_json.named_values_shape Session_json.value
        ; "session_id", Session_json.nullable Session_json.value
        ; "user_id", Session_json.nullable Session_json.value
        ; "host_id", Session_json.nullable Session_json.value
        ; "created_at_ns", Session_json.value
        ; "expires_at_ns", Session_json.nullable Session_json.value
        ; "revoked_at_ns", Session_json.nullable Session_json.value
        ; "revocation_reason", Session_json.nullable Session_json.value
        ]
    ;;
  end

  module Extension_snapshot = struct
    type t =
      { extension_id : string
      ; extension_kind : string
      ; runtime_id : string
      ; manifest_sha256 : string
      ; source_sha256 : string
      ; state : Snapshot.t
      ; captured_at_ns : int64
      }
    [@@deriving bin_io, sexp]

    let to_jsonaf (t : t) =
      `Object
        [ "extension_id", Session_json.encode_string t.extension_id
        ; "extension_kind", Session_json.encode_string t.extension_kind
        ; "runtime_id", Session_json.encode_string t.runtime_id
        ; "manifest_sha256", Session_json.encode_string t.manifest_sha256
        ; "source_sha256", Session_json.encode_string t.source_sha256
        ; "state", Snapshot.to_jsonaf t.state
        ; "captured_at_ns", Session_json.encode_int64 t.captured_at_ns
        ]
    ;;

    let of_jsonaf json =
      let open Result.Let_syntax in
      let%bind fields = Session_json.fields json in
      let%bind extension_id =
        Session_json.field fields "extension_id" Session_json.string
      in
      let%bind extension_kind =
        Session_json.field fields "extension_kind" Session_json.string
      in
      let%bind runtime_id = Session_json.field fields "runtime_id" Session_json.string in
      let%bind manifest_sha256 =
        Session_json.field fields "manifest_sha256" Session_json.string
      in
      let%bind source_sha256 =
        Session_json.field fields "source_sha256" Session_json.string
      in
      let%bind state = Session_json.field fields "state" Snapshot.of_jsonaf in
      let%bind captured_at_ns =
        Session_json.field fields "captured_at_ns" Session_json.int64
      in
      Ok
        { extension_id
        ; extension_kind
        ; runtime_id
        ; manifest_sha256
        ; source_sha256
        ; state
        ; captured_at_ns
        }
    ;;

    let shape =
      Session_json.object_
        [ "extension_id", Session_json.value
        ; "extension_kind", Session_json.value
        ; "runtime_id", Session_json.value
        ; "manifest_sha256", Session_json.value
        ; "source_sha256", Session_json.value
        ; "state", Snapshot.shape
        ; "captured_at_ns", Session_json.value
        ]
    ;;
  end

  module Interrupted_request = struct
    type t =
      { request_id : string
      ; runtime_id : string
      ; manifest_sha256 : string
      ; request_kind : Request_kind.t
      ; command_sha256 : string
      ; redacted_command : string
      ; cwd_sha256 : string
      ; effects : string list
      ; interrupted_at_ns : int64
      ; reason : string
      ; audit_sequence : int64 option
      ; retryable : bool
      }
    [@@deriving bin_io, sexp]

    let to_jsonaf (t : t) =
      `Object
        [ "request_id", Session_json.encode_string t.request_id
        ; "runtime_id", Session_json.encode_string t.runtime_id
        ; "manifest_sha256", Session_json.encode_string t.manifest_sha256
        ; "request_kind", Request_kind.to_jsonaf t.request_kind
        ; "command_sha256", Session_json.encode_string t.command_sha256
        ; "redacted_command", Session_json.encode_string t.redacted_command
        ; "cwd_sha256", Session_json.encode_string t.cwd_sha256
        ; "effects", (Session_json.encode_list Session_json.encode_string) t.effects
        ; "interrupted_at_ns", Session_json.encode_int64 t.interrupted_at_ns
        ; "reason", Session_json.encode_string t.reason
        ; ( "audit_sequence"
          , (Session_json.encode_option Session_json.encode_int64) t.audit_sequence )
        ; "retryable", Session_json.encode_bool t.retryable
        ]
    ;;

    let of_jsonaf json =
      let open Result.Let_syntax in
      let%bind fields = Session_json.fields json in
      let%bind request_id = Session_json.field fields "request_id" Session_json.string in
      let%bind runtime_id = Session_json.field fields "runtime_id" Session_json.string in
      let%bind manifest_sha256 =
        Session_json.field fields "manifest_sha256" Session_json.string
      in
      let%bind request_kind =
        Session_json.field fields "request_kind" Request_kind.of_jsonaf
      in
      let%bind command_sha256 =
        Session_json.field fields "command_sha256" Session_json.string
      in
      let%bind redacted_command =
        Session_json.field fields "redacted_command" Session_json.string
      in
      let%bind cwd_sha256 = Session_json.field fields "cwd_sha256" Session_json.string in
      let%bind effects =
        Session_json.field fields "effects" (Session_json.list Session_json.string)
      in
      let%bind interrupted_at_ns =
        Session_json.field fields "interrupted_at_ns" Session_json.int64
      in
      let%bind reason = Session_json.field fields "reason" Session_json.string in
      let%bind audit_sequence =
        Session_json.field
          fields
          "audit_sequence"
          (Session_json.option Session_json.int64)
      in
      let%bind retryable = Session_json.field fields "retryable" Session_json.bool in
      Ok
        { request_id
        ; runtime_id
        ; manifest_sha256
        ; request_kind
        ; command_sha256
        ; redacted_command
        ; cwd_sha256
        ; effects
        ; interrupted_at_ns
        ; reason
        ; audit_sequence
        ; retryable
        }
    ;;

    let shape =
      Session_json.object_
        [ "request_id", Session_json.value
        ; "runtime_id", Session_json.value
        ; "manifest_sha256", Session_json.value
        ; "request_kind", Request_kind.shape
        ; "command_sha256", Session_json.value
        ; "redacted_command", Session_json.value
        ; "cwd_sha256", Session_json.value
        ; "effects", Session_json.array Session_json.value
        ; "interrupted_at_ns", Session_json.value
        ; "reason", Session_json.value
        ; "audit_sequence", Session_json.nullable Session_json.value
        ; "retryable", Session_json.value
        ]
    ;;
  end

  type t =
    { manifest_grants : Manifest_grant.persisted list
    ; approval_grants : Approval_grant.persisted list
    ; extension_snapshots : Extension_snapshot.t list
    ; last_audit_sequence : int64 option
    ; interrupted_requests : Interrupted_request.t list
    }
  [@@deriving bin_io, sexp]

  let to_jsonaf (t : t) =
    `Object
      [ ( "manifest_grants"
        , (Session_json.encode_list Manifest_grant.to_jsonaf) t.manifest_grants )
      ; ( "approval_grants"
        , (Session_json.encode_list Approval_grant.to_jsonaf) t.approval_grants )
      ; ( "extension_snapshots"
        , (Session_json.encode_list Extension_snapshot.to_jsonaf) t.extension_snapshots )
      ; ( "last_audit_sequence"
        , (Session_json.encode_option Session_json.encode_int64) t.last_audit_sequence )
      ; ( "interrupted_requests"
        , (Session_json.encode_list Interrupted_request.to_jsonaf) t.interrupted_requests
        )
      ]
  ;;

  let of_jsonaf json =
    let open Result.Let_syntax in
    let%bind fields = Session_json.fields json in
    let%bind manifest_grants =
      Session_json.field
        fields
        "manifest_grants"
        (Session_json.list Manifest_grant.of_jsonaf)
    in
    let%bind approval_grants =
      Session_json.field
        fields
        "approval_grants"
        (Session_json.list Approval_grant.of_jsonaf)
    in
    let%bind extension_snapshots =
      Session_json.field
        fields
        "extension_snapshots"
        (Session_json.list Extension_snapshot.of_jsonaf)
    in
    let%bind last_audit_sequence =
      Session_json.field
        fields
        "last_audit_sequence"
        (Session_json.option Session_json.int64)
    in
    let%bind interrupted_requests =
      Session_json.field
        fields
        "interrupted_requests"
        (Session_json.list Interrupted_request.of_jsonaf)
    in
    Ok
      { manifest_grants
      ; approval_grants
      ; extension_snapshots
      ; last_audit_sequence
      ; interrupted_requests
      }
  ;;

  let shape =
    Session_json.object_
      [ "manifest_grants", Session_json.array ~identity:"grant_id" Manifest_grant.shape
      ; "approval_grants", Session_json.array ~identity:"grant_id" Approval_grant.shape
      ; ( "extension_snapshots"
        , Session_json.array ~identity:"extension_id" Extension_snapshot.shape )
      ; "last_audit_sequence", Session_json.nullable Session_json.value
      ; ( "interrupted_requests"
        , Session_json.array ~identity:"request_id" Interrupted_request.shape )
      ]
  ;;

  let validate (t : t) =
    let open Result.Let_syntax in
    let%bind () =
      Session_json.unique
        (List.map t.manifest_grants ~f:(fun x -> x.Manifest_grant.grant_id))
    in
    let%bind () =
      Session_json.unique
        (List.map t.approval_grants ~f:(fun x -> x.Approval_grant.grant_id))
    in
    let%bind () =
      Session_json.unique
        (List.map t.extension_snapshots ~f:(fun x -> x.Extension_snapshot.extension_id))
    in
    let%bind () =
      Session_json.unique
        (List.map t.interrupted_requests ~f:(fun x -> x.Interrupted_request.request_id))
    in
    let invalid_sequence value =
      Option.exists value ~f:(fun value -> Int64.(value < 0L))
    in
    let%bind () =
      if
        List.exists t.approval_grants ~f:(fun x -> x.Approval_grant.stdin_bytes < 0)
        || invalid_sequence t.last_audit_sequence
        || List.exists t.interrupted_requests ~f:(fun x ->
          invalid_sequence x.Interrupted_request.audit_sequence)
      then Error "shell byte counts and audit sequences must be nonnegative"
      else Ok ()
    in
    let%bind () =
      if List.exists t.manifest_grants ~f:(fun x -> x.Manifest_grant.schema_version <= 0)
      then Error "manifest grant schema versions must be positive"
      else Ok ()
    in
    List.fold_result t.extension_snapshots ~init:() ~f:(fun () x ->
      Snapshot.validate x.Extension_snapshot.state)
  ;;

  let of_jsonaf_unchecked = of_jsonaf

  let of_jsonaf json =
    Session_json.bounded
      (fun json ->
         Result.bind (of_jsonaf_unchecked json) ~f:(fun value ->
           Result.map (validate value) ~f:(fun () -> value)))
      json
  ;;

  let empty =
    { manifest_grants = []
    ; approval_grants = []
    ; extension_snapshots = []
    ; last_audit_sequence = None
    ; interrupted_requests = []
    }
  ;;
end

(* ----------------------------------------------------------------------- *)
(*  Latest schema                                                           *)
(* ----------------------------------------------------------------------- *)

(* Make the latest schema directly available at the top-level. *)
type t =
  { version : int
  ; id : string
  ; prompt_file : string
  ; local_prompt_copy : string option
  ; history : History.t
  ; next_history_sequence : int
  ; tasks : Task.t list
  ; moderator_state : Moderator_state.t
  ; shell_state : Shell_state.t
  ; kv_store : (string * string) list
  ; vfs_root : string
  ; storage : unit Document_schema.Extension_carrier.t
  }

let create
      ?id
      ~prompt_file
      ?local_prompt_copy
      ?(history = [])
      ?(next_history_sequence = 0)
      ?(tasks = [])
      ?moderator_snapshot
      ?moderator_state
      ?(shell_state = Shell_state.empty)
      ?(kv_store = [])
      ?(vfs_root = "vfs")
      ()
  : t
  =
  let default_id () =
    let data =
      let time_ns = Time_ns.to_int63_ns_since_epoch (Time_ns.now ()) |> Int63.to_string in
      time_ns ^ Int.to_string (Random.bits ())
    in
    Md5.digest_string data |> Md5.to_hex
  in
  let id = Option.value_or_thunk id ~default:default_id in
  { version = current_version
  ; id
  ; prompt_file
  ; local_prompt_copy
  ; history
  ; next_history_sequence
  ; tasks
  ; moderator_state =
      Option.value moderator_state ~default:(Moderator_state.of_legacy moderator_snapshot)
  ; shell_state
  ; kv_store
  ; vfs_root
  ; storage = Document_schema.Extension_carrier.of_authored_value ()
  }
;;

(* ------------------------------------------------------------------------- *)
(* IO helpers                                                                *)
(* ------------------------------------------------------------------------- *)

let reset ?prompt_file (t : t) : t =
  let prompt_file = Option.value prompt_file ~default:t.prompt_file in
  { t with
    prompt_file
  ; history = []
  ; moderator_state = Moderator_state.of_legacy None
  ; shell_state = Shell_state.empty
  }
;;

(** Same as {!reset} but preserves the existing conversation history. *)
let reset_keep_history ?prompt_file (t : t) : t =
  let prompt_file = Option.value prompt_file ~default:t.prompt_file in
  { t with
    prompt_file
  ; moderator_state = Moderator_state.of_legacy None
  ; shell_state = Shell_state.empty
  }
;;

let allocator (t : t) =
  History_entry.Allocator.create ~namespace:t.id ~next_sequence:t.next_history_sequence
;;

let validate (t : t) =
  let open Result.Let_syntax in
  let%bind () =
    if t.version = current_version
    then Ok ()
    else Error "unsupported runtime session version"
  in
  let%bind () = Session_json.unique (List.map t.tasks ~f:(fun task -> task.Task.id)) in
  let%bind () = Session_json.unique ~allow_empty:true (List.map t.kv_store ~f:fst) in
  let%bind () = Shell_state.validate t.shell_state in
  let%bind () = Moderator_state.validate t.moderator_state in
  let%bind () =
    match t.moderator_state.identity_snapshot with
    | None -> Ok ()
    | Some snapshot ->
      Moderator_state.Identity_snapshot.validate_history_ids
        snapshot
        ~history_ids:(List.map t.history ~f:History_entry.id)
  in
  let%bind allocator = allocator t in
  History_entry.validate ~allocator t.history
;;

module Document = struct
  module Schema = Document_schema
  module J = Session_json

  let domain_error error =
    Schema.Error.Invalid_field { path = [ "payload" ]; reason = error }
  ;;

  let validated result = Result.map_error result ~f:domain_error

  let history_to_jsonaf entry =
    `Object
      [ "id", `String (History_entry.Id.to_string (History_entry.id entry))
      ; "payload", History_entry.Payload.to_json (History_entry.payload entry)
      ]
  ;;

  let history_of_jsonaf json =
    let open Result.Let_syntax in
    let%bind fields = J.fields json in
    let%bind id =
      J.field fields "id" (fun value ->
        Result.bind (J.string value) ~f:History_entry.Id.of_string)
    in
    let%map payload = J.field fields "payload" History_entry.Payload.of_json in
    History_entry.create_with_id ~id payload
  ;;

  let encode_value t =
    let open Result.Let_syntax in
    let%map () = validated (validate t) in
    `Object
      [ "id", `String t.id
      ; "prompt_file", `String t.prompt_file
      ; "local_prompt_copy", J.encode_option J.encode_string t.local_prompt_copy
      ; "history", J.encode_list history_to_jsonaf t.history
      ; "next_history_sequence", J.encode_int t.next_history_sequence
      ; "tasks", J.encode_list Task.to_jsonaf t.tasks
      ; "moderator_state", Moderator_state.to_jsonaf t.moderator_state
      ; "shell_state", Shell_state.to_jsonaf t.shell_state
      ; "kv_store", J.encode_named_values J.encode_string t.kv_store
      ; "vfs_root", `String t.vfs_root
      ]
  ;;

  let decode_value json =
    let open Result.Let_syntax in
    validated
      (let%bind fields = J.fields json in
       let%bind id = J.field fields "id" J.string in
       let%bind prompt_file = J.field fields "prompt_file" J.string in
       let%bind local_prompt_copy =
         J.field fields "local_prompt_copy" (J.option J.string)
       in
       let%bind history = J.field fields "history" (J.list history_of_jsonaf) in
       let%bind next_history_sequence = J.field fields "next_history_sequence" J.int in
       let%bind tasks = J.field fields "tasks" (J.list Task.of_jsonaf) in
       let%bind moderator_state =
         J.field fields "moderator_state" Moderator_state.of_jsonaf
       in
       let%bind shell_state = J.field fields "shell_state" Shell_state.of_jsonaf in
       let%bind kv_store = J.field fields "kv_store" (J.named_values J.string) in
       let%bind vfs_root = J.field fields "vfs_root" J.string in
       let t =
         create
           ~id
           ~prompt_file
           ?local_prompt_copy
           ~history
           ~next_history_sequence
           ~tasks
           ~moderator_state
           ~shell_state
           ~kv_store
           ~vfs_root
           ()
       in
       let%map () = validate t in
       t)
  ;;

  let shape =
    J.object_
      [ "id", J.value
      ; "prompt_file", J.value
      ; "local_prompt_copy", J.nullable J.value
      ; ( "history"
        , J.array ~identity:"id" (J.object_ [ "id", J.value; "payload", J.value ]) )
      ; "next_history_sequence", J.value
      ; "tasks", J.array ~identity:"id" Task.shape
      ; "moderator_state", Moderator_state.shape
      ; "shell_state", Shell_state.shape
      ; "kv_store", J.named_values_shape J.value
      ; "vfs_root", J.value
      ]
  ;;

  let configuration_exn = function
    | Ok value -> value
    | Error error -> raise_s [%sexp (error : Schema.Error.t)]
  ;;

  let kind = "standalone.session"
  let limits = Schema.Limits.default

  let conversion =
    Schema.Conversion.create
      ~limits
      ~targets:[ kind, 1 ]
      ~max_steps:1
      ~max_operations:1
      ~steps:[]
    |> configuration_exn
  ;;

  let codec =
    Schema.Domain_codec.create
      ~limits
      ~kind
      ~version:1
      ~shape
      ~supported_semantics:[]
      ~decode:decode_value
      ~encode:encode_value
    |> configuration_exn
  ;;

  let encode t =
    Schema.Domain_codec.encode codec (Schema.Extension_carrier.with_value t.storage t)
  ;;

  let decode document =
    let open Result.Let_syntax in
    let%bind document = Schema.Conversion.upgrade conversion document in
    let%map carrier = Schema.Domain_codec.decode codec document in
    let t = Schema.Extension_carrier.value carrier in
    { t with storage = Schema.Extension_carrier.with_value carrier () }
  ;;

  let to_string t = Result.map (encode t) ~f:Schema.Document.to_string
  let of_string bytes = Result.bind (Schema.Document.decode ~limits bytes) ~f:decode
end

module Io = struct
  module File = struct
    let result_exn = function
      | Ok value -> value
      | Error error -> raise_s [%sexp (error : Document_schema.Error.t)]
    ;;

    let read path =
      Eio.Path.with_open_in path (fun flow ->
        let maximum = Document_schema.Limits.max_bytes Document_schema.Limits.default in
        let reader = Eio.Buf_read.of_flow flow ~max_size:maximum in
        Eio.Buf_read.take_all reader |> Document.of_string |> result_exn)
    ;;

    let write path t =
      let bytes = Document.to_string t |> result_exn in
      Eio.Path.save ~create:(`Or_truncate 0o600) path bytes
    ;;
  end
end
