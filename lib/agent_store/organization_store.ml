open Core
module P = Agent_protocol
module D = Document_schema
module S = Organization_state
module Document = Organization_document

type initialization =
  | Create_if_missing
  | Require_existing

type availability =
  | Available of S.t D.Extension_carrier.t
  | Unavailable
  | Closed

type t =
  { mutex : Eio.Mutex.t
  ; mutable availability : availability
  ; replace : string -> (unit, Store_error.t) result
  }

let unavailable () =
  Error (Store_error.Corrupt "organization authority unavailable; reopen the owned store")
;;

let create_ephemeral ~server_id =
  { mutex = Eio.Mutex.create ()
  ; availability = Available (D.Extension_carrier.of_authored_value (S.empty ~server_id))
  ; replace = (fun _ -> Ok ())
  }
;;

let basename = "organization.json"

let load directory =
  Durable_file.load_bounded_in
    ~directory
    ~basename
    ~max_bytes:(D.Limits.max_bytes Document.limits)
;;

let open_owned ~directory ~server_id ~initialization =
  let open Result.Let_syntax in
  let replace contents =
    Durable_file.replace_in
      ~directory
      ~durability:Flush_file_and_directory
      ~basename
      contents
  in
  let%bind carrier =
    match load directory with
    | Ok contents ->
      let%bind doc =
        D.Document.decode ~limits:Document.limits contents |> Document_fields.store
      in
      let%bind carrier = Document.restore doc in
      if P.Id.Server.equal server_id (S.server_id (D.Extension_carrier.value carrier))
      then Ok carrier
      else Error (Store_error.Corrupt "organization document belongs to another host")
    | Error (Store_error.Missing _)
      when match initialization with
           | Create_if_missing -> true
           | Require_existing -> false ->
      let carrier = D.Extension_carrier.of_authored_value (S.empty ~server_id) in
      let%bind document = Document.encode carrier in
      let%map () = replace (D.Document.to_string document) in
      carrier
    | Error error -> Error error
  in
  Ok { mutex = Eio.Mutex.create (); availability = Available carrier; replace }
;;

let close t = Eio.Mutex.use_rw ~protect:true t.mutex (fun () -> t.availability <- Closed)

let snapshot_checked t =
  Eio.Mutex.use_ro t.mutex (fun () ->
    match t.availability with
    | Available carrier -> Ok (D.Extension_carrier.value carrier)
    | Unavailable | Closed -> unavailable ())
;;

let receipt t ~principal ~now ~key ~request_digest =
  Eio.Mutex.use_ro t.mutex (fun () ->
    match t.availability with
    | Available carrier ->
      S.lookup_receipt
        (D.Extension_carrier.value carrier)
        ~principal
        ~now
        ~key
        ~request_digest
    | Unavailable | Closed ->
      unavailable () |> Result.map_error ~f:Store_error.to_protocol_error)
;;

let with_mutation_lock t ~f =
  let outcome =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      try Ok (f ()) with
      | exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ()))
  in
  match outcome with
  | Ok value -> value
  | Error (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
;;

let mutate t ~principal ~audit ~now ~candidate mutation =
  with_mutation_lock t ~f:(fun () ->
    let open Result.Let_syntax in
    match t.availability with
    | Unavailable | Closed ->
      unavailable () |> Result.map_error ~f:Store_error.to_protocol_error
    | Available carrier ->
      let before = D.Extension_carrier.value carrier in
      let%bind state, result =
        S.apply before ~principal ~audit ~now ~candidate mutation
      in
      if Int64.equal (S.revision before) (S.revision state)
      then Ok result
      else (
        let%bind carrier =
          Document.with_state carrier state ~now
          |> Result.map_error ~f:Store_error.to_protocol_error
        in
        let%bind document =
          Document.encode carrier |> Result.map_error ~f:Store_error.to_protocol_error
        in
        let contents = D.Document.to_string document in
        (* Errors can follow successful rename before directory sync acknowledgement.
       Never expose a previous snapshot as current after entering replacement. *)
        t.availability <- Unavailable;
        let persisted = t.replace contents in
        let%map () = persisted |> Result.map_error ~f:Store_error.to_protocol_error in
        t.availability <- Available carrier;
        result))
;;
