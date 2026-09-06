open! Core

type t =
  { mutex : Eio.Mutex.t
  ; mutable definitions : Workspace_definition.t list
  ; mutable by_id :
      (Agent_protocol.Id.Workspace_definition.t, Workspace_definition.t) Map.Poly.t
  ; mutable by_name : (string, Workspace_definition.t) Map.Poly.t
  }

let create definitions =
  let duplicate_id =
    List.contains_dup definitions ~compare:(fun left right ->
      Agent_protocol.Id.Workspace_definition.compare
        left.Workspace_definition.id
        right.Workspace_definition.id)
  in
  let duplicate_name =
    List.contains_dup definitions ~compare:(fun left right ->
      String.compare
        left.Workspace_definition.config_name
        right.Workspace_definition.config_name)
  in
  if duplicate_id || duplicate_name
  then
    Error
      (Agent_store.Store_error.Corrupt "workspace catalog contains duplicate identities")
  else
    Ok
      { mutex = Eio.Mutex.create ()
      ; definitions
      ; by_id =
          List.fold definitions ~init:Map.Poly.empty ~f:(fun map definition ->
            Map.set map ~key:definition.id ~data:definition)
      ; by_name =
          List.fold definitions ~init:Map.Poly.empty ~f:(fun map definition ->
            Map.set map ~key:definition.config_name ~data:definition)
      }
;;

let definitions t = Eio.Mutex.use_ro t.mutex (fun () -> t.definitions)

let install t ~replacement =
  let definitions, by_id, by_name =
    Eio.Mutex.use_ro replacement.mutex (fun () ->
      replacement.definitions, replacement.by_id, replacement.by_name)
  in
  Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
    t.definitions <- definitions;
    t.by_id <- by_id;
    t.by_name <- by_name)
;;

let find t id = Eio.Mutex.use_ro t.mutex (fun () -> Map.find t.by_id id)
let find_by_name t name = Eio.Mutex.use_ro t.mutex (fun () -> Map.find t.by_name name)
