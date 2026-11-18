open! Core
module P = Agent_protocol
module Store = Agent_store.Session_store
module Registry = Agent_server.Session_registry
module Actor = Agent_session.Session_actor

(* Only a caller that observed this exact entry's uncertain journal failure may
   use this law. Healthy closure remains strict, including the recovered host.
   A poisoned writer cannot install a fresh close snapshot over uncertain data. *)
let physical_inventory (entry : Registry.entry) ~env =
  let handle = Option.value_exn entry.store_handle in
  let inventory directory =
    Eio.Path.read_dir Eio.Path.(Eio.Stdenv.fs env / directory)
    |> List.sort ~compare:String.compare
    |> List.map ~f:(fun name ->
      name, Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / directory / name))
  in
  let snapshots = Store.Handle.snapshot_directory handle in
  let journal = Store.Handle.journal_directory handle in
  inventory snapshots, inventory journal
;;

let close_uncertain_entry (entry : Registry.entry) ~env ~store =
  let handle = Option.value_exn entry.store_handle in
  assert (not (Store.is_closed store));
  assert (Store.owns_handle store handle);
  let physical_bytes = physical_inventory entry ~env in
  let failure =
    match Agent_server.Session_registry.close_entry entry with
    | () -> failwith "uncertain writer unexpectedly installed a close checkpoint"
    | exception Registry.Cleanup_failed failure ->
      assert (Option.is_none (Registry.Cleanup_failure.exception_and_backtrace failure));
      Registry.Cleanup_failure.error failure
  in
  [%test_eq: P.Error.code] Persistence_error failure.code;
  [%test_eq: bool] false failure.retryable;
  [%test_eq: string]
    (Sexp.to_string_hum
       ([%sexp_of: Agent_store.Store_error.t]
          (Corrupt "cache maintenance requires an available commit writer")))
    failure.message;
  let assert_physical_conservation () =
    [%test_eq: (string * string) list * (string * string) list]
      physical_bytes
      (physical_inventory entry ~env)
  in
  assert_physical_conservation ();
  assert (not (Store.is_closed store));
  (* Ownership identity deliberately survives closure. Availability is checked
     through the original Handle, never inferred from owns_handle. *)
  assert (Store.owns_handle store handle);
  (match Store.Handle.metadata_checked handle with
   | Error (Corrupt message) -> [%test_eq: string] "session handle is closed" message
   | Error failure ->
     raise_s
       [%sexp "unexpected closed Handle failure", (failure : Agent_store.Store_error.t)]
   | Ok _ -> failwith "uncertain writer cleanup left its actual Handle open");
  assert (Result.is_error (Actor.state entry.actor));
  Agent_server.Session_registry.close_entry entry;
  assert_physical_conservation ()
;;
