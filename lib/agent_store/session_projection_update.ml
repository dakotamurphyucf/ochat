open! Core
module D = Document_schema
module F = Document_fields
module Codec = Session_index_recovery_document

type t =
  { env : Eio_unix.Stdenv.base
  ; marker_path : string
  ; mutex : Eio.Mutex.t
  ; sync_directory :
      env:Eio_unix.Stdenv.base -> path:string -> (unit, Store_error.t) Result.t
  ; mutable live_failure : bool
  ; mutable canonical : canonical_pending list
  ; mutable canonical_inherited : bool
  }

and canonical_pending =
  { owner : t
  ; mutable entry : Session_index.Entry.t
  ; mutable active : bool
  }

(* A recoverable storage owner remains usable after an explicitly propagated
   cancellation. Capture inside the protected section so Eio does not poison
   its mutex; re-raise with the original backtrace only after releasing it. *)
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

let create ?(sync_directory = Durable_file.sync_directory) ~env ~marker_path () =
  { env
  ; marker_path
  ; mutex = Eio.Mutex.create ()
  ; sync_directory
  ; live_failure = false
  ; canonical = []
  ; canonical_inherited = false
  }
;;

let marker ~env ~marker_path =
  try
    match Eio.Path.kind ~follow:false Eio.Path.(Eio.Stdenv.fs env / marker_path) with
    | `Not_found -> Ok None
    | `Regular_file ->
      let open Result.Let_syntax in
      let%bind bytes =
        Durable_file.load_bounded
          ~env
          ~path:marker_path
          ~max_bytes:(D.Limits.max_bytes Codec.limits)
      in
      let%bind document = D.Document.decode ~limits:Codec.limits bytes |> F.store in
      Codec.of_document document
      |> F.store
      |> Result.map ~f:(fun carrier -> Some (carrier, bytes))
    | _ -> Error (Store_error.Corrupt "session projection recovery marker is not regular")
  with
  | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
  | exn ->
    Error
      (Store_error.of_exn
         ~operation:"read projection recovery marker"
         ~path:marker_path
         exn)
;;

let pending ~env ~marker_path = marker ~env ~marker_path |> Result.map ~f:Option.is_some

let require ~env ~marker_path =
  let open Result.Let_syntax in
  let%bind previous = marker ~env ~marker_path in
  match previous with
  | Some _ -> Ok ()
  | None ->
    let%bind document =
      Codec.to_document (D.Extension_carrier.of_authored_value ()) |> F.store
    in
    Durable_file.replace
      ~env
      ~durability:Flush_file_and_directory
      ~path:marker_path
      (D.Document.to_string document)
;;

let clear t =
  let open Result.Let_syntax in
  let%bind previous = marker ~env:t.env ~marker_path:t.marker_path in
  let attempt () =
    try
      Eio.Path.unlink Eio.Path.(Eio.Stdenv.fs t.env / t.marker_path);
      t.sync_directory ~env:t.env ~path:(Filename.dirname t.marker_path)
    with
    | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
    | exn ->
      Error
        (Store_error.of_exn
           ~operation:"clear projection recovery marker"
           ~path:t.marker_path
           exn)
  in
  let retain () =
    (* Unlink may have succeeded before a failed directory sync. Restore the
       intent conservatively; never interpret uncertain clearing as completion. *)
    Eio.Cancel.protect (fun () ->
      let%bind current = marker ~env:t.env ~marker_path:t.marker_path in
      match current, previous with
      | Some _, _ | None, None -> Ok ()
      | None, Some (_, bytes) ->
        Durable_file.replace
          ~env:t.env
          ~durability:Flush_file_and_directory
          ~path:t.marker_path
          bytes)
  in
  match attempt () with
  | Ok () -> Ok ()
  | Error _ as error ->
    t.live_failure <- true;
    let%bind () = retain () in
    error
  | exception exn ->
    let backtrace = Stdlib.Printexc.get_raw_backtrace () in
    t.live_failure <- true;
    (try ignore (retain () : (unit, Store_error.t) Result.t) with
     | _ -> ());
    Exn.raise_with_original_backtrace exn backtrace
;;

let publish t ~f =
  with_mutation_lock t ~f:(fun () ->
    let open Result.Let_syntax in
    let%bind prior = pending ~env:t.env ~marker_path:t.marker_path in
    let began = ref false in
    let authority () =
      let%bind () = require ~env:t.env ~marker_path:t.marker_path in
      began := true;
      Ok ()
    in
    let perform () =
      match f ~require_intent:authority with
      | Ok value ->
        if (not !began) || prior
        then Ok value
        else (
          match clear t with
          | Ok () -> Ok value
          | Error _ as error ->
            t.live_failure <- true;
            error)
      | Error _ as error ->
        if !began then t.live_failure <- true;
        error
      | exception exn ->
        if !began then t.live_failure <- true;
        raise exn
    in
    try perform () with
    | exn ->
      if !began then t.live_failure <- true;
      raise exn)
;;

let complete_recovery t =
  with_mutation_lock t ~f:(fun () ->
    if t.live_failure
    then
      Error
        (Store_error.Corrupt
           "a live projection publication requires restart reconciliation")
    else
      let open Result.Let_syntax in
      let%bind pending = pending ~env:t.env ~marker_path:t.marker_path in
      if not (List.is_empty t.canonical)
      then Ok ()
      else if pending
      then clear t
      else Ok ())
;;

module Pending = struct
  type t = canonical_pending

  let matches t entry = t.active && Session_index.Entry.equal t.entry entry
end

let prepare_canonical t ~previous ~prepare =
  with_mutation_lock t ~f:(fun () ->
    let open Result.Let_syntax in
    let%bind (entry : Session_index.Entry.t) = prepare () in
    let%bind () =
      match previous with
      | None -> Ok ()
      | Some pending ->
        if
          (not (phys_equal pending.owner t))
          || (not pending.active)
          || not
               (Agent_protocol.Id.Session.equal pending.entry.session.id entry.session.id)
        then Error (Store_error.Corrupt "canonical projection token ownership mismatch")
        else if
          Int64.(entry.session.revision < pending.entry.session.revision)
          || (Int64.equal entry.session.revision pending.entry.session.revision
              && not (Session_index.Entry.equal entry pending.entry))
        then
          Error
            (Store_error.Corrupt
               "canonical projection target cannot regress or alter equal-revision hints")
        else Ok ()
    in
    let%bind prior = pending ~env:t.env ~marker_path:t.marker_path in
    let%bind () =
      match require ~env:t.env ~marker_path:t.marker_path with
      | Ok () -> Ok ()
      | Error _ as error ->
        t.live_failure <- true;
        error
      | exception exn ->
        t.live_failure <- true;
        raise exn
    in
    match previous with
    | Some pending ->
      pending.entry <- entry;
      Ok pending
    | None ->
      if List.is_empty t.canonical then t.canonical_inherited <- prior;
      let pending = { owner = t; entry; active = true } in
      t.canonical <- pending :: t.canonical;
      Ok pending)
;;

let finish_canonical t pending ~entry =
  with_mutation_lock t ~f:(fun () ->
    if (not (phys_equal pending.owner t)) || not (Pending.matches pending entry)
    then
      Error
        (Store_error.Corrupt
           "canonical projection completion does not match latest owned target")
    else (
      pending.active <- false;
      t.canonical
      <- List.filter t.canonical ~f:(fun other -> not (phys_equal pending other));
      if List.is_empty t.canonical && (not t.canonical_inherited) && not t.live_failure
      then clear t
      else Ok ()))
;;
