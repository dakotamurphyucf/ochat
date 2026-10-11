open Core
module Owner = Session_factory_catalogs
module W = Agent_session.Workspace_catalog
module D = Agent_session.Workspace_definition
module P = Agent_session.Permission_policy

let ok = function
  | Ok value -> value
  | Error _ -> failwith "unexpected catalog fixture rejection"
;;

let workspace name =
  D.create
    ~id:(Agent_protocol.Id.Workspace_definition.of_string ("wsd_" ^ name) |> ok)
    ~config_name:name
    ~source:(Physical { configured_root = "/" ^ name })
    ~access:Read_only
    ~conflict_domain:None
    ~prompt_limits:[]
  |> ok
;;

let policy default =
  P.create
    ~id:"interactive"
    ~tool_default:default
    ~approval_timeout_ms:None
    ~fallback:Fallback_deny
    ~manifest_authorization:Deny_manifest
    ~evaluator:None
    ~evaluator_revision:None
    ~reviewer:None
  |> ok
;;

let with_owner f =
  Eio_main.run (fun env ->
    let a = workspace "a"
    and b = workspace "b" in
    let original = W.create [ a ] |> ok
    and replacement = W.create [ b ] |> ok in
    let old = policy Allow
    and next = policy Deny in
    let owner =
      Owner.create ~workspaces:original ~permission_profiles:[ old ] ~manifest_grants:[]
    in
    f env owner original replacement a b old next)
;;

let seal owner replacement next ~checkpoint =
  Owner.seal
    owner
    ~workspaces:replacement
    ~permission_profiles:[ next ]
    ~manifest_grants:[]
    ~checkpoint
;;

let check owner workspace current history =
  [%test_eq: string option]
    (Option.map (Owner.find_workspace owner workspace.D.id) ~f:(fun value ->
       value.config_name))
    (Some workspace.config_name);
  assert (phys_equal (Owner.find_profile owner "interactive" |> Option.value_exn) current);
  List.iter history ~f:(fun expected ->
    assert (
      phys_equal
        (Owner.find_profile_revision owner expected.P.revision_digest |> Option.value_exn)
        expected))
;;

let%expect_test "sealed replacement retains the old pinned policy object" =
  with_owner (fun _ owner original replacement a b old next ->
    check owner a old [ old ];
    let planned = seal owner replacement next ~checkpoint:ignore |> ok in
    (match
       Owner.install
         owner
         ~workspaces:replacement
         ~permission_profiles:[ next ]
         ~manifest_grants:[]
     with
     | () -> assert false
     | exception Invalid_argument _ -> ());
    (match W.install original ~replacement with
     | () -> assert false
     | exception Invalid_argument _ -> ());
    check owner a old [ old ];
    Owner.Seal.commit planned |> ok;
    check owner b next [ old; next ];
    Owner.install
      owner
      ~workspaces:original
      ~permission_profiles:[ old ]
      ~manifest_grants:[];
    (* original is the shared destination, now containing b; commit reuse must not
       reinstall the first plan's profile values after the compatibility update. *)
    Owner.Seal.commit planned |> ok;
    check owner b old [ old; next ];
    print_s
      [%sexp
        { current =
            ((Owner.find_profile owner "interactive" |> Option.value_exn).tool_default
             : P.tool_default)
        ; old_pin_same_object =
            (phys_equal
               (Owner.find_profile_revision owner old.revision_digest |> Option.value_exn)
               old
             : bool)
        ; new_pin_same_object =
            (phys_equal
               (Owner.find_profile_revision owner next.revision_digest |> Option.value_exn)
               next
             : bool)
        }]);
  [%expect {| ((current Allow) (old_pin_same_object true) (new_pin_same_object true)) |}]
;;

let%test_unit "reservation transitions are nonallocating and final commit does not yield" =
  with_owner (fun env owner original replacement a b old next ->
    let workspace_seal = W.seal original ~replacement ~checkpoint:ignore |> ok in
    (match seal owner replacement next ~checkpoint:ignore with
     | Error Workspaces_busy -> ()
     | _ -> assert false);
    Gc.For_testing.assert_no_allocation [%here] (fun () -> W.Seal.abort workspace_seal)
    |> ok;
    let calls = ref 0 in
    let planned = seal owner replacement next ~checkpoint:(fun () -> incr calls) |> ok in
    let before = !calls in
    (match seal owner replacement next ~checkpoint:ignore with
     | Error Busy -> ()
     | _ -> assert false);
    Eio.Switch.run (fun sw ->
      let ready, release = Eio.Promise.create () in
      let ran = ref false in
      Eio.Fiber.fork_daemon ~sw (fun () ->
        Eio.Promise.await ready;
        ran := true;
        `Stop_daemon);
      Eio.Fiber.yield ();
      Eio.Promise.resolve release ();
      Gc.For_testing.assert_no_allocation [%here] (fun () -> Owner.Seal.commit planned)
      |> ok;
      assert (not !ran);
      [%test_eq: int] before !calls;
      Eio.Fiber.yield ();
      assert !ran);
    check owner b next [ old; next ];
    (match
       Eio.Domain_manager.run env#domain_mgr (fun () -> Owner.Seal.commit planned)
     with
     | Error Foreign_domain -> ()
     | _ -> assert false);
    let uncertain = seal owner replacement old ~checkpoint:ignore |> ok in
    Gc.For_testing.assert_no_allocation [%here] (fun () ->
      Owner.Seal.mark_uncertain uncertain)
    |> ok;
    Gc.For_testing.assert_no_allocation [%here] (fun () ->
      Owner.Seal.mark_uncertain uncertain)
    |> ok;
    (match seal owner replacement old ~checkpoint:ignore with
     | Error Publication_uncertain -> ()
     | _ -> assert false);
    (match W.seal original ~replacement ~checkpoint:ignore with
     | Error Publication_uncertain -> ()
     | _ -> assert false);
    (match Owner.Seal.abort uncertain with
     | Error Publication_uncertain -> ()
     | _ -> assert false);
    check owner b next [ old; next ];
    ignore a)
;;

let%test_unit
    "reentrant replacement is detected and checkpoint failures release both reservations"
  =
  with_owner (fun _ owner original replacement a b old next ->
    let changed = ref false in
    (match
       seal owner replacement old ~checkpoint:(fun () ->
         if not !changed
         then (
           changed := true;
           Owner.install
             owner
             ~workspaces:replacement
             ~permission_profiles:[ next ]
             ~manifest_grants:[]))
     with
     | Error Stale_preparation -> ()
     | _ -> assert false);
    check owner b next [ old; next ];
    let candidate_a = W.create [ a ] |> ok in
    Owner.install
      owner
      ~workspaces:candidate_a
      ~permission_profiles:[ old ]
      ~manifest_grants:[];
    let calls = ref 0 in
    let planned = seal owner replacement next ~checkpoint:(fun () -> incr calls) |> ok in
    Gc.For_testing.assert_no_allocation [%here] (fun () -> Owner.Seal.abort planned) |> ok;
    assert (!calls > 0);
    List.iter [ `Throw; `Cancel ] ~f:(fun mode ->
      for at = 1 to !calls do
        let seen = ref 0 in
        let original_failure = Failure (Int.to_string at) in
        let outcome =
          Result.try_with (fun () ->
            Eio.Cancel.sub (fun context ->
              seal owner replacement next ~checkpoint:(fun () ->
                incr seen;
                if !seen = at
                then (
                  match mode with
                  | `Throw -> raise original_failure
                  | `Cancel -> Eio.Cancel.cancel context original_failure))))
        in
        (match outcome with
         | Error actual when phys_equal actual original_failure -> ()
         | Error (Eio.Cancel.Cancelled reason) ->
           assert (phys_equal reason original_failure)
         | _ -> assert false);
        check owner a old [ old; next ];
        let reusable = seal owner replacement next ~checkpoint:ignore |> ok in
        Owner.Seal.abort reusable |> ok
      done);
    (* A callback during index construction cannot bypass the temporary workspace
       reservation even before the factory's final admission. *)
    let blocked = ref 0 in
    let planned =
      seal owner replacement next ~checkpoint:(fun () ->
        match seal owner replacement next ~checkpoint:ignore with
        | Ok temporary -> Owner.Seal.abort temporary |> ok
        | Error Workspaces_busy ->
          incr blocked;
          (match
             Owner.install
               owner
               ~workspaces:replacement
               ~permission_profiles:[ next ]
               ~manifest_grants:[]
           with
           | () -> assert false
           | exception Invalid_argument _ -> ())
        | Error _ -> assert false)
      |> ok
    in
    assert (!blocked > 0);
    check owner a old [ old; next ];
    Owner.Seal.abort planned |> ok)
;;
