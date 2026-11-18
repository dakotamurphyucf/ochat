open! Core
module Store = Agent_store

type t =
  { domain_mgr : Eio.Domain_manager.ty Eio.Resource.t
  ; limits : Document_schema.Limits.t
  ; available : Eio.Semaphore.t
  }

let create ~domain_mgr ~limits = { domain_mgr; limits; available = Eio.Semaphore.make 1 }

let run t operation =
  Eio.Semaphore.acquire t.available;
  Exn.protect
    ~finally:(fun () -> Eio.Semaphore.release t.available)
    ~f:(fun () -> Eio.Domain_manager.run t.domain_mgr operation)
;;

let restore t snapshot =
  let limits = t.limits in
  run t (fun () ->
    Session_persistence.restore_snapshot ~limits snapshot
    |> Result.map ~f:(fun restored ->
      Some (Session_persistence.Restored.state_document restored)))
;;

let apply t state transaction =
  let limits = t.limits in
  run t (fun () ->
    match state with
    | None -> Error (Store.Store_error.Corrupt "journal replay has no snapshot state")
    | Some document ->
      Session_persistence.apply_document document ~limits transaction
      |> Result.map ~f:Option.some)
;;

let equivalent t left right =
  let limits = t.limits in
  run t (fun () ->
    let open Result.Let_syntax in
    let encode = function
      | None -> Error (Store.Store_error.Corrupt "journal replay has no snapshot state")
      | Some document ->
        Session_state_document.encode document ~limits
        |> Result.map ~f:(fun document ->
          Document_schema.Document.json document |> Jsonaf.to_string)
        |> Result.map_error ~f:(fun error -> Store.Store_error.Document error)
    in
    let%bind left = encode left in
    let%map right = encode right in
    String.equal left right)
;;

let validate_native t document =
  run t (fun () ->
    match document with
    | None -> Error (Store.Store_error.Corrupt "durable session has no valid snapshot")
    | Some document ->
      Session_state.validate (Session_state_document.value document)
      |> Result.map_error ~f:(fun error ->
        Store.Store_error.Corrupt error.Agent_protocol.Error.message))
;;
