open! Core

type availability =
  | Ready of Prompt_revision.t
  | Unavailable of Prompt_revision_builder.Diagnostic.t list
  | Disabled

type entry =
  { definition : Prompt_definition.t
  ; availability : availability
  }

type prepared =
  { entries : entry list
  ; revisions : Prompt_revision.t list
  }

type t =
  { env : Eio_unix.Stdenv.base
  ; artifact_store : Agent_store.Prompt_artifact_store.t
  ; mutex : Eio.Mutex.t
  ; mutable entries_by_id : (Agent_protocol.Id.Prompt_definition.t, entry) Map.Poly.t
  ; mutable entries_by_name : (string, entry) Map.Poly.t
  ; mutable revisions :
      (Agent_protocol.Id.Prompt_revision.t, Prompt_revision.t) Map.Poly.t
  }

let create ~env ~artifact_store =
  { env
  ; artifact_store
  ; mutex = Eio.Mutex.create ()
  ; entries_by_id = Map.Poly.empty
  ; entries_by_name = Map.Poly.empty
  ; revisions = Map.Poly.empty
  }
;;

let prepare_entry t ~transaction_id ~created_at definition =
  if not definition.Prompt_definition.enabled
  then { definition; availability = Disabled }, None
  else (
    match
      Prompt_revision_builder.build
        ~env:t.env
        ~artifact_store:t.artifact_store
        ~transaction_id:(transaction_id definition)
        ~created_at
        definition
    with
    | Ok revision -> { definition; availability = Ready revision }, Some revision
    | Error diagnostics -> { definition; availability = Unavailable diagnostics }, None)
;;

let prepare t ~transaction_id ~created_at definitions =
  let entries, revisions =
    List.map definitions ~f:(prepare_entry t ~transaction_id ~created_at) |> List.unzip
  in
  { entries; revisions = List.filter_opt revisions }
;;

let install t prepared =
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    t.entries_by_id
    <- List.fold prepared.entries ~init:Map.Poly.empty ~f:(fun map entry ->
         Map.set map ~key:entry.definition.id ~data:entry);
    t.entries_by_name
    <- List.fold prepared.entries ~init:Map.Poly.empty ~f:(fun map entry ->
         Map.set map ~key:entry.definition.config_name ~data:entry);
    t.revisions
    <- List.fold prepared.revisions ~init:t.revisions ~f:(fun map revision ->
         Map.set map ~key:(Prompt_revision.id revision) ~data:revision))
;;

let entries t = Eio.Mutex.use_ro t.mutex (fun () -> Map.data t.entries_by_id)
let find t id = Eio.Mutex.use_ro t.mutex (fun () -> Map.find t.entries_by_id id)

let find_by_name t name =
  Eio.Mutex.use_ro t.mutex (fun () -> Map.find t.entries_by_name name)
;;

let find_revision t revision_id =
  Eio.Mutex.use_ro t.mutex (fun () -> Map.find t.revisions revision_id)
;;

let prune_unreferenced_artifacts t ~additional =
  let protected =
    Eio.Mutex.use_ro t.mutex (fun () -> Map.keys t.revisions @ additional)
  in
  Agent_store.Prompt_artifact_store.prune_unreferenced t.artifact_store ~protected
;;

let restore_revision t ~definition_id ~revision_id =
  match find_revision t revision_id with
  | Some revision -> Ok revision
  | None ->
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      match Map.find t.revisions revision_id with
      | Some revision -> Ok revision
      | None ->
        (match Map.find t.entries_by_id definition_id with
         | None ->
           Error
             [ Prompt_revision_builder.Diagnostic.
                 { code = "prompt.definition_missing"
                 ; message = "pinned prompt definition is absent from the catalog"
                 ; source = None
                 }
             ]
         | Some entry ->
           (match
              Prompt_revision_builder.restore
                ~artifact_store:t.artifact_store
                entry.definition
                revision_id
            with
            | Error _ as failure -> failure
            | Ok revision ->
              t.revisions <- Map.set t.revisions ~key:revision_id ~data:revision;
              Ok revision)))
;;
