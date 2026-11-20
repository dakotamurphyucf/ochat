open! Core
module Store = Agent_store.Session_store
module Reader = Agent_store.Retention_reader
module D = Document_schema

type t =
  { store : Store.t
  ; handle : Store.Handle.t
  ; reader : Reader.t
  ; check_host : unit -> unit
  ; owner_domain : int
  ; mutable active : bool
  ; mutable borrowed : bool
  ; mutable remaining_selected_bytes : int
  }

type open_error =
  | Denied
  | Invalid_limits

exception Exhausted
exception Inactive

let in_owner_domain t = Int.equal t.owner_domain (Stdlib.Domain.self () :> int)

let is_active t =
  in_owner_domain t && t.active && Store.retained_lifecycle_available t.store t.handle
;;

let checkpoint t =
  Eio.Fiber.check ();
  if not (is_active t && t.borrowed) then raise Inactive;
  t.check_host ();
  Eio.Fiber.check ()
;;

let with_scope
      ?(max_source_opens = 8)
      ?(max_source_bytes = 64 * 1024 * 1024)
      ?(max_text_bytes = 4 * 1024 * 1024)
      ?(checkpoint = fun () -> ())
      store
      ~handle
      ~f
  =
  if max_source_opens < 0 || max_source_bytes < 0 || max_text_bytes < 0
  then Error Invalid_limits
  else
    let open Result.Let_syntax in
    let%map reader =
      Store.retained_reader
        store
        ~handle
        ~max_entries:(Int.min 8 max_source_opens)
        ~max_bytes:(Int.min (64 * 1024 * 1024) max_source_bytes)
      |> Result.map_error ~f:(fun _ -> Denied)
    in
    let t =
      { store
      ; handle
      ; reader
      ; check_host = checkpoint
      ; owner_domain = (Stdlib.Domain.self () :> int)
      ; active = true
      ; borrowed = false
      ; remaining_selected_bytes = Int.min (4 * 1024 * 1024) max_text_bytes
      }
    in
    Exn.protect ~f:(fun () -> f t) ~finally:(fun () -> t.active <- false)
;;

let borrow t store ~handle ~f =
  if
    (not (is_active t))
    || t.borrowed
    || not (phys_equal t.store store && phys_equal t.handle handle)
  then None
  else (
    t.borrowed <- true;
    Some (Exn.protect ~f ~finally:(fun () -> t.borrowed <- false)))
;;

let charge_source t bytes =
  if bytes < 0 then invalid_arg "negative recovery source work";
  checkpoint t;
  if Result.is_error (Reader.charge_bytes t.reader bytes) then raise Exhausted
;;

let charge_selected t bytes =
  if bytes < 0 then invalid_arg "negative selected recovery text";
  checkpoint t;
  if bytes > t.remaining_selected_bytes
  then raise Exhausted
  else t.remaining_selected_bytes <- t.remaining_selected_bytes - bytes
;;

let measure_json t ~limits json =
  D.Json.validate_and_measure_with_work
    ~limits
    ~checkpoint:(fun () -> checkpoint t)
    ~charge_work:(charge_source t)
    json
;;

let read_archive t ~filename ~max_bytes =
  let open Result.Let_syntax in
  checkpoint t;
  let%bind () =
    if
      String.equal (Filename.basename filename) filename
      && not (List.mem [ ""; "."; ".." ] filename ~equal:String.equal)
    then Ok ()
    else Error (Agent_store.Store_error.Corrupt "invalid recovery archive filename")
  in
  let%bind bounded = Reader.with_local_limits t.reader ~max_entries:1 ~max_bytes in
  Reader.read_promised_file_with_checkpoint
    bounded
    ~path:(Filename.concat "archive" filename)
    ~max_bytes
    ~checkpoint:(fun () -> checkpoint t)
;;
