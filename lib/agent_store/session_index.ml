open! Core
module D = Document_schema
module F = Document_fields
module Entry = Session_index_entry

module Id = struct
  module T = struct
    type t = Agent_protocol.Id.Session.t [@@deriving compare, sexp]
  end

  include T
  include Comparator.Make (T)
end

type t =
  { env : Eio_unix.Stdenv.base
  ; path : string
  ; mutex : Eio.Mutex.t
  ; mutable entries : (Id.t, Entry.t, Id.comparator_witness) Map.t
  ; mutable carrier : Entry.t list D.Extension_carrier.t
  ; mutable unavailable : Store_error.t option
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

let map_of_entries entries =
  match
    Map.of_alist
      (module Id)
      (List.map entries ~f:(fun entry -> entry.Entry.session.id, entry))
  with
  | `Ok entries -> Ok entries
  | `Duplicate_key _ -> Error (Store_error.Corrupt "duplicate session index identity")
;;

let entries_of_map map =
  Map.data map
  |> List.sort ~compare:(fun left right ->
    Agent_protocol.Timestamp.compare
      left.Entry.session.created_at
      right.Entry.session.created_at)
;;

(* Explicit projection retirement: only identities removed by the caller's
   remove/rebuild operation lose their extensions. Retained entries preserve
   their complete envelope and nested fields. *)
let retire carrier entries =
  match D.Extension_carrier.template carrier with
  | None -> Ok carrier
  | Some document ->
    let ids =
      List.map (Map.keys entries) ~f:Agent_protocol.Id.Session.to_string
      |> String.Set.of_list
    in
    let payload = D.Document.payload document in
    let open Result.Let_syntax in
    let%bind previous = F.required payload "entries" F.array |> F.store in
    let%bind retained =
      List.filter_map previous ~f:(fun entry ->
        match D.Json.field entry ~name:"session_id" with
        | Value (`String id) when Set.mem ids id -> Some entry
        | Absent | Null | Value _ -> None)
      |> fun entries -> Ok entries
    in
    let replace fields name value =
      List.Assoc.add fields ~equal:String.equal name value
    in
    let%bind json =
      match D.Document.json document, payload with
      | `Object envelope, `Object fields ->
        Ok
          (`Object
              (replace
                 envelope
                 "payload"
                 (`Object (replace fields "entries" (`Array retained)))))
      | _ -> Error (Store_error.Corrupt "invalid index carrier")
    in
    let%bind document =
      D.Document.inspect ~limits:Session_index_document.limits json |> F.store
    in
    Session_index_document.of_document document |> F.store
;;

let prepare t entries =
  let open Result.Let_syntax in
  let%bind carrier = retire t.carrier entries in
  let carrier = D.Extension_carrier.with_value carrier (entries_of_map entries) in
  let%bind document = Session_index_document.to_document carrier |> F.store in
  let%map carrier = Session_index_document.of_document document |> F.store in
  D.Document.to_string document, carrier
;;

let load ~env ~path =
  let open Result.Let_syntax in
  let%bind contents =
    Durable_file.load_bounded
      ~env
      ~path
      ~max_bytes:(D.Limits.max_bytes Session_index_document.limits)
  in
  let%bind document =
    D.Document.decode ~limits:Session_index_document.limits contents |> F.store
  in
  let%bind carrier = Session_index_document.of_document document |> F.store in
  let%map entries = map_of_entries (D.Extension_carrier.value carrier) in
  entries, carrier
;;

let refresh t =
  match load ~env:t.env ~path:t.path with
  | Ok (entries, carrier) ->
    t.entries <- entries;
    t.carrier <- carrier;
    t.unavailable <- None
  | Error error -> t.unavailable <- Some error
  | exception exn ->
    t.unavailable
    <- Some (Store_error.Corrupt "index refresh failed during uncertain publication");
    raise exn
;;

let availability t =
  Eio.Mutex.use_ro t.mutex (fun () ->
    match t.unavailable with
    | None -> Ok ()
    | Some error -> Error error)
;;

let publish t entries (bytes, carrier) =
  match
    Durable_file.replace
      ~env:t.env
      ~durability:Flush_file_and_directory
      ~path:t.path
      bytes
  with
  | Ok () ->
    t.entries <- entries;
    t.carrier <- carrier;
    t.unavailable <- None;
    Ok ()
  | Error _ as error ->
    Eio.Cancel.protect (fun () ->
      try refresh t with
      | _ -> ());
    error
  | exception exn ->
    let backtrace = Stdlib.Printexc.get_raw_backtrace () in
    Eio.Cancel.protect (fun () ->
      try refresh t with
      | _ -> ());
    Exn.raise_with_original_backtrace exn backtrace
;;

let make ~env ~path (entries, carrier) =
  { env; path; entries; carrier; unavailable = None; mutex = Eio.Mutex.create () }
;;

let rebuild_missing ~env ~path ~rebuild =
  let open Result.Let_syntax in
  let%bind values = rebuild () in
  let%bind entries = map_of_entries values in
  let t = make ~env ~path (entries, D.Extension_carrier.of_authored_value values) in
  let%bind prepared = prepare t entries in
  let%map () = publish t entries prepared in
  t
;;

let open_or_rebuild ~env ~path ~rebuild =
  if not (Filename.is_absolute path)
  then Error (Store_error.Corrupt "session index path must be absolute")
  else (
    try
      match Eio.Path.kind ~follow:false Eio.Path.(Eio.Stdenv.fs env / path) with
      | `Not_found -> rebuild_missing ~env ~path ~rebuild
      | `Regular_file -> load ~env ~path |> Result.map ~f:(make ~env ~path)
      | _ -> Error (Store_error.Corrupt "session index is not a regular file")
    with
    | (Eio.Cancel.Cancelled _ | Eio.Time.Timeout) as exn -> raise exn
    | exn -> Error (Store_error.of_exn ~operation:"open session index" ~path exn))
;;

let open_or_create ~env ~path = open_or_rebuild ~env ~path ~rebuild:(fun () -> Ok [])
let list t = Eio.Mutex.use_ro t.mutex (fun () -> entries_of_map t.entries)
let find t id = Eio.Mutex.use_ro t.mutex (fun () -> Map.find t.entries id)

let list_checked t =
  Eio.Mutex.use_ro t.mutex (fun () ->
    match t.unavailable with
    | None -> Ok (entries_of_map t.entries)
    | Some error -> Error error)
;;

let find_checked t id =
  Eio.Mutex.use_ro t.mutex (fun () ->
    match t.unavailable with
    | None -> Ok (Map.find t.entries id)
    | Some error -> Error error)
;;

let update t f =
  with_mutation_lock t ~f:(fun () ->
    let open Result.Let_syntax in
    let%bind () =
      match t.unavailable with
      | None -> Ok ()
      | Some error -> Error error
    in
    let%bind entries = f t.entries in
    let%bind prepared = prepare t entries in
    publish t entries prepared)
;;

let unavailable_after_authority t =
  t.unavailable
  <- Some
       (Store_error.Corrupt
          "metadata/index projection requires reconciliation after authoritative \
           publication began")
;;

let confirm_requested_publication t bytes =
  if Option.is_none t.unavailable
  then
    Eio.Cancel.protect (fun () ->
      match
        Durable_file.load_bounded
          ~env:t.env
          ~path:t.path
          ~max_bytes:(D.Limits.max_bytes Session_index_document.limits)
      with
      | Ok current when String.equal current bytes -> ()
      | Ok _ | Error _ -> unavailable_after_authority t
      | exception exn ->
        unavailable_after_authority t;
        raise exn)
;;

let with_prepared_replacement
      t
      ~session_id
      ~expected_entry
      ~replacement
      ~publish_authority
  =
  with_mutation_lock t ~f:(fun () ->
    let open Result.Let_syntax in
    let%bind () =
      match t.unavailable with
      | None -> Ok ()
      | Some error -> Error error
    in
    let%bind () =
      match expected_entry with
      | None -> Ok ()
      | Some expected ->
        if Option.equal Entry.equal expected (Map.find t.entries session_id)
        then Ok ()
        else Error (Store_error.Corrupt "lifecycle index observation changed")
    in
    let entries =
      match replacement with
      | Some entry -> Map.set t.entries ~key:session_id ~data:entry
      | None -> Map.remove t.entries session_id
    in
    let%bind ((bytes, _) as prepared) = prepare t entries in
    match publish_authority () with
    | Error _ as error ->
      unavailable_after_authority t;
      error
    | exception exn ->
      let backtrace = Stdlib.Printexc.get_raw_backtrace () in
      unavailable_after_authority t;
      Exn.raise_with_original_backtrace exn backtrace
    | Ok value ->
      (match publish t entries prepared with
       | Ok () -> Ok value
       | Error _ as error ->
         (try confirm_requested_publication t bytes with
          | _ -> ());
         error
       | exception exn ->
         let backtrace = Stdlib.Printexc.get_raw_backtrace () in
         (try confirm_requested_publication t bytes with
          | _ -> ());
         Exn.raise_with_original_backtrace exn backtrace))
;;

let with_prepared_upsert ?expected_entry t entry ~publish_authority =
  with_prepared_replacement
    t
    ~session_id:entry.Entry.session.id
    ~expected_entry
    ~replacement:(Some entry)
    ~publish_authority
;;

let with_prepared_remove ?expected_entry t session_id ~publish_authority =
  with_prepared_replacement
    t
    ~session_id
    ~expected_entry
    ~replacement:None
    ~publish_authority
;;

let upsert t entry =
  update t (fun entries -> Ok (Map.set entries ~key:entry.Entry.session.id ~data:entry))
;;

let remove t id = update t (fun entries -> Ok (Map.remove entries id))
let replace_all t entries = update t (fun _ -> map_of_entries entries)

let validate_upsert t entry =
  with_mutation_lock t ~f:(fun () ->
    let open Result.Let_syntax in
    let%bind () =
      match t.unavailable with
      | None -> Ok ()
      | Some error -> Error error
    in
    prepare t (Map.set t.entries ~key:entry.Entry.session.id ~data:entry)
    |> Result.map ~f:ignore)
;;

let validate_remove t session_id =
  with_mutation_lock t ~f:(fun () ->
    match t.unavailable with
    | Some error -> Error error
    | None -> prepare t (Map.remove t.entries session_id) |> Result.map ~f:(fun _ -> ()))
;;
