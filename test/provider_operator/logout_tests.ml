open! Core
module P = Agent_protocol
module M = Credential_registry_model
module C = Credential_registry
module S = Private_storage
module Secret = Provider_secret_store

let ok value =
  Result.map_error value ~f:(fun _ -> "synthetic fixture failed") |> Result.ok_or_failwith
;;

let id value = M.Id.create value |> ok

let%expect_test "lost logout reply then reauthorization never redisables the new login" =
  Mirage_crypto_rng_unix.use_default ();
  Eio_main.run (fun env ->
    let path =
      "/tmp/ochat-operator-" ^ P.Id.Transaction.to_string (P.Id.Transaction.create ())
    in
    let anchor = Eio.Path.(Eio.Stdenv.fs env / path) in
    Eio.Path.mkdir ~perm:0o700 anchor;
    Exn.protect
      ~finally:(fun () -> Eio.Path.rmtree anchor)
      ~f:(fun () ->
        Eio.Switch.run (fun sw ->
          let directory =
            S.Directory.open_or_create
              ~sw
              ~anchor
              ~components:[ S.Name.create "private" |> ok ]
            |> ok
          in
          let secrets =
            Secret.open_private_files
              ~sw
              ~directory
              ~namespace:(Secret.Namespace.create "operator" |> ok)
            |> ok
          in
          let generated = ref 0 in
          let registry =
            C.initialize_new
              ~metadata_admission:C.Metadata_admission.nonblocking
              ~sw
              ~wall_clock:(Eio.Stdenv.clock env)
              ~new_operation:(fun () ->
                incr generated;
                id (sprintf "generated_%d" !generated))
              ~directory
              ~secrets
              ~environment:None
              ~host:(id "host")
              ~incarnation:(id "incarnation")
            |> ok
          in
          let binding = id "public" in
          let identity =
            M.Identity.api_key
              ~host:(id "host")
              ~provider:"openai"
              ~billing:"api"
              ~account:None
              ~key_reference:binding
            |> ok
          in
          let install operation material =
            let candidate =
              C.begin_candidate
                registry
                ~binding
                ~operation:(id operation)
                ~expectation:(M.Expectation.exact identity)
              |> ok
            in
            let verified =
              C.Verified.create
                ~identity
                ~grant:None
                ~material:
                  (C.Material.api_key
                     (Secret.Secret.of_bytes (Bytes.of_string material) |> ok))
              |> ok
            in
            C.commit_candidate registry candidate verified |> ok
          in
          install "login_first" "synthetic-first-key";
          let old_logout = id "logout_original" in
          C.For_testing.set_after_publication_hook
            registry
            (Some (fun () -> failwith "synthetic lost logout acknowledgement"));
          (match
             C.disable_with_operation
               registry
               ~sw
               ~clock:(Eio.Stdenv.mono_clock env)
               ~maximum_wait:(Time_ns.Span.of_sec 0.05)
               ~binding
               ~operation:old_logout
               ~mode:Fresh
               ~revocation:None
               ~reason:Logout
           with
           | exception Failure message
             when String.equal message "synthetic lost logout acknowledgement" -> ()
           | _ -> failwith "lost acknowledgement hook did not fire");
          C.For_testing.set_after_publication_hook registry None;
          (match C.reconcile_operation registry ~binding ~operation:old_logout |> ok with
           | Committed -> ()
           | _ -> failwith "original logout not durable");
          (* The lost acknowledgement occurred before drain/secret cleanup. Complete
             that local recovery before a replacement login is allowed to publish. *)
          let recovered =
            C.disable_with_operation
              registry
              ~sw
              ~clock:(Eio.Stdenv.mono_clock env)
              ~maximum_wait:(Time_ns.Span.of_sec 0.05)
              ~binding
              ~operation:old_logout
              ~mode:Reconcile
              ~revocation:None
              ~reason:Logout
            |> ok
          in
          assert recovered.disabled;
          assert (C.Status.equal_drain recovered.cleanup.drain Drained);
          install "login_later" "synthetic-later-key";
          let snapshot () =
            C.synchronize registry
            |> ok
            |> C.Host_snapshot.bindings
            |> List.find_exn ~f:(fun item -> M.Id.equal (C.Host_snapshot.id item) binding)
          in
          let before = snapshot () in
          (match
             C.disable_with_operation
               registry
               ~sw
               ~clock:(Eio.Stdenv.mono_clock env)
               ~maximum_wait:(Time_ns.Span.of_sec 0.05)
               ~binding
               ~operation:old_logout
               ~mode:Reconcile
               ~revocation:None
               ~reason:Logout
           with
           | Error Binding_unavailable -> ()
           | _ -> failwith "old logout was applied to later login");
          let after = snapshot () in
          assert (Int64.equal (C.Host_snapshot.epoch before) (C.Host_snapshot.epoch after));
          assert (
            Option.equal
              String.equal
              (C.Host_snapshot.credential_revision before)
              (C.Host_snapshot.credential_revision after));
          assert (
            C.Host_snapshot.equal_availability (C.Host_snapshot.availability after) Ready);
          print_endline
            "lost original reply is reconciled; later login survives old logout replay \
             unchanged")));
  [%expect
    {| lost original reply is reconciled; later login survives old logout replay unchanged |}]
;;
