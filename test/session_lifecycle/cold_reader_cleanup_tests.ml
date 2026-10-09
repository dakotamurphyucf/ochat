open! Core
open Agent_server_test_support
module P = Agent_protocol
module C = Agent_client
module D = Agent_server.Daemon
module R = Agent_server.Session_registry
module S = Agent_store.Session_store

(* Arm the existing release fault only after the cold reader owns its Handle.
   The same snapshot-open operation rejects the immutable read with a distinct
   primary failure. No sleeps or synthetic state/owner are involved. *)
let rec reject_snapshot_open
  :  'tags.
     bool ref
  -> bool ref
  -> Lifecycle_faults.t
  -> ([> Eio.Fs.dir_ty ] as 'tags) Eio.Resource.t
  -> prefix:string
  -> 'tags Eio.Resource.t
  =
  fun armed rejected fault (Eio.Resource.T (resource, handler)) ~prefix ->
  let module Original = (val Eio.Resource.get handler Eio.Fs.Pi.Dir) in
  let qualify name =
    if Filename.is_absolute name then name else Filename.concat prefix name
  in
  let module Directory = struct
    include Original

    let open_dir resource ~sw name =
      Original.open_dir resource ~sw name
      |> fun child ->
      reject_snapshot_open armed rejected fault child ~prefix:(qualify name)
    ;;

    let open_in resource ~sw name =
      let path = qualify name in
      if !armed && String.is_suffix path ~suffix:"/snapshot/CURRENT"
      then (
        armed := false;
        rejected := true;
        Lifecycle_faults.arm fault Actor_lock_release;
        raise (Core_unix.Unix_error (EIO, "injected cold read primary", "fixture")));
      Original.open_in resource ~sw name
    ;;
  end
  in
  Eio.Resource.T
    ( resource
    , Eio.Resource.handler
        (H (Eio.Fs.Pi.Dir, (module Directory)) :: Eio.Resource.bindings handler) )
;;

let wrap_env armed rejected fault (env : Eio_unix.Stdenv.base) : Eio_unix.Stdenv.base =
  let resource, prefix = Eio.Stdenv.fs env in
  let fs = reject_snapshot_open armed rejected fault resource ~prefix, prefix in
  object
    method fs = fs
    method cwd = env#cwd
    method stdin = env#stdin
    method stdout = env#stdout
    method stderr = env#stderr
    method net = env#net
    method domain_mgr = env#domain_mgr
    method process_mgr = env#process_mgr
    method clock = env#clock
    method mono_clock = env#mono_clock
    method secure_random = env#secure_random
    method debug = env#debug
    method backend_id = env#backend_id
  end
;;

let%expect_test
    "cold immutable read retains primary and cleanup failure without activating"
  =
  Eio_main.run (fun raw_env ->
    Mirage_crypto_rng_unix.use_default ();
    let fault = Lifecycle_faults.create () in
    let armed = ref false in
    let rejected = ref false in
    let env = Lifecycle_faults.wrap_env fault raw_env |> wrap_env armed rejected fault in
    Lifecycle_service_tests.with_lifecycle_root
      env
      (fun ~root ~content:_ ~configuration ->
         Eio.Switch.run (fun sw ->
           let providers = ref 0 in
           let policy =
             inference_policy
               ~default_model:"fixture-model"
               ~post_stream:(fun ~sw:_ ~inputs:_ ->
                 incr providers;
                 failwith "cold read activated provider")
           in
           let daemon =
             D.start
               ~sw
               ~env
               ~options:
                 { D.default_options with
                   startup_mode = Execute
                 ; inference_policy = policy
                 }
               ~config:configuration
               ~tool_dir:root
               ~home:root
               ~process_start_identity:None
               ()
             |> protocol_ok
           in
           let client = connection daemon (principal ()) in
           Exn.protect
             ~finally:(fun () ->
               C.Connection.close client;
               D.shutdown daemon |> protocol_ok)
             ~f:(fun () ->
               initialize client;
               let session, _ = create_session ~key:"cold-cleanup-reader" client in
               let other, _ = create_session ~key:"cold-cleanup-unrelated" client in
               let registry = D.registry daemon in
               let original = R.remove registry session.id |> Option.value_exn in
               Agent_server.Runtime_owner.close_and_wait original.runtime;
               original.close ();
               let indexed =
                 Agent_store.Session_index.find_checked
                   (S.session_index (D.store daemon))
                   session.id
                 |> Lifecycle_service_tests.store_ok
                 |> Option.value_exn
               in
               R.index registry indexed;
               armed := true;
               let primary =
                 match C.Admin.get_session client session.id with
                 | Error error ->
                   P.Error.equal_code error.code Persistence_error
                   && String.is_substring
                        error.message
                        ~substring:"injected cold read primary"
                 | Ok _ -> false
               in
               let fenced =
                 match R.with_lifecycle registry session.id (fun _ -> Ok ()) with
                 | Error { code = Conflict; _ } -> true
                 | Error _ | Ok _ -> false
               in
               let unrelated =
                 Result.is_ok (R.with_lifecycle registry other.id (fun _ -> Ok ()))
               in
               let cold = Option.is_none (R.find registry session.id) in
               let triggered = Lifecycle_faults.was_triggered fault in
               R.shutdown registry;
               print_s
                 [%sexp
                   { primary : bool
                   ; rejected = (!rejected : bool)
                   ; triggered : bool
                   ; fenced : bool
                   ; unrelated : bool
                   ; cold : bool
                   ; providers = (!providers : int)
                   }]))));
  [%expect
    {|
    ((primary true) (rejected true) (triggered true) (fenced true)
     (unrelated true) (cold true) (providers 0))
    |}]
;;
