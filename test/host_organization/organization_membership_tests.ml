open! Core
module P = Agent_protocol
module V = P.Session_organization.Values
module Patch = P.Session_organization.Patch
module State = Agent_store.Organization_state
module Store = Agent_store.Organization_store

let ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp "membership fixture rejected", (error : P.Error.t)]
;;

let host = P.Id.Server.of_string "srv_membership" |> ok
let project = P.Id.Project.of_string "prj_membership" |> ok
let collection value = P.Id.Collection.of_string ("col_" ^ value) |> ok
let now = P.Timestamp.of_string "2026-10-09T00:00:00Z" |> ok

let owner =
  P.Principal.create
    ~id:(P.Id.Principal.of_string "pri_membership" |> ok)
    ~authentication_kind:"test"
    ~scopes:(P.Scope.Set.of_list [ Manage_organization; View_organization ])
    ~attributes:[]
  |> ok
;;

let key value = P.Idempotency_key.of_string value |> ok

let audit method_name idempotency_key mutation =
  Agent_store.Idempotency_store.Command_audit.
    { key = { principal_id = owner.id; session_id = None; method_name; idempotency_key }
    ; request_digest = State.request_digest mutation |> ok
    ; protected_record = false
    }
;;

let status = function
  | Ok _ -> "ok"
  | Error (error : P.Error.t) -> P.Error.code_to_string error.code
;;

let%expect_test "membership patches are independent, typed, bounded and validating" =
  let initial =
    V.create ~project_id:(Some project) ~collection_ids:[ collection "b"; collection "a" ]
    |> ok
  in
  let patch =
    Patch.create
      ~project:Keep
      ~add_collections:[ collection "c" ]
      ~remove_collections:[ collection "a" ]
    |> ok
  in
  let next = Patch.apply patch ~previous:initial |> ok in
  print_s
    [%sexp
      (Option.equal P.Id.Project.equal next.project_id initial.project_id : bool)
    , (List.map next.collection_ids ~f:P.Id.Collection.to_string : string list)];
  print_endline
    (status
       (Patch.create
          ~project:Clear
          ~add_collections:[ collection "a" ]
          ~remove_collections:[ collection "a" ]));
  print_endline
    (status
       (V.of_json
          (`Object
              [ ( "collection_ids"
                , `Array
                    [ P.Id.Collection.to_json (collection "a")
                    ; P.Id.Collection.to_json (collection "a")
                    ] )
              ])));
  let complete =
    V.create
      ~project_id:None
      ~collection_ids:(List.init 128 ~f:(fun i -> collection (Int.to_string i)))
    |> ok
  in
  let overflow =
    Patch.create
      ~project:Keep
      ~add_collections:[ collection "new" ]
      ~remove_collections:[]
    |> ok
  in
  print_endline (status (Patch.apply overflow ~previous:complete));
  print_s [%sexp (V.equal initial (V.of_json (V.to_json initial) |> ok) : bool)];
  [%expect
    {|
    (true (col_b col_c))
    invalid_request
    invalid_request
    invalid_request
    true
    |}]
;;

let%expect_test
    "live membership commit serializes deletion and releases exception-safe authority"
  =
  Eio_main.run (fun _env ->
    Eio.Switch.run (fun sw ->
      let store = Store.create_ephemeral ~server_id:host in
      let create =
        P.Organization_request.Create.
          { host_id = host
          ; name = P.Organization_group.Name.create "Membership" |> ok
          ; idempotency_key = key "create"
          }
      in
      let mutation = State.Mutation.Create_project create in
      ignore
        (Store.mutate
           store
           ~principal:owner
           ~audit:(audit "project.create" create.idempotency_key mutation)
           ~now
           ~candidate:(Some (State.Candidate.Project project))
           mutation
         |> ok);
      let references = V.create ~project_id:(Some project) ~collection_ids:[] |> ok in
      let entered, entered_r = Eio.Promise.create () in
      let release, release_r = Eio.Promise.create () in
      let done_, done_r = Eio.Promise.create () in
      Eio.Fiber.fork ~sw (fun () ->
        Store.with_live_membership
          store
          ~principal:owner
          ~host_id:host
          ~additions:references
          ~commit:(fun () ->
            Eio.Promise.resolve entered_r ();
            Eio.Promise.await release;
            Ok ())
        |> ok;
        Eio.Promise.resolve done_r ());
      Eio.Promise.await entered;
      let deletion_finished, deletion_finished_r = Eio.Promise.create () in
      let deleted = ref false in
      Eio.Fiber.fork ~sw (fun () ->
        let request =
          P.Organization_request.Project.Delete.
            { host_id = host
            ; id = project
            ; expected_revision = 0L
            ; idempotency_key = key "delete"
            }
        in
        let mutation = State.Mutation.Delete_project request in
        ignore
          (Store.mutate
             store
             ~principal:owner
             ~audit:(audit "project.delete" request.idempotency_key mutation)
             ~now
             ~candidate:None
             mutation
           |> ok);
        deleted := true;
        Eio.Promise.resolve deletion_finished_r ());
      Eio.Fiber.yield ();
      print_s [%sexp (!deleted : bool)];
      Eio.Promise.resolve release_r ();
      Eio.Promise.await done_;
      Eio.Promise.await deletion_finished;
      print_endline
        (status
           (Store.authorize_membership store ~principal:owner ~host_id:host ~references));
      print_endline
        (status
           (Store.with_live_membership
              store
              ~principal:owner
              ~host_id:host
              ~additions:references
              ~commit:(fun () -> failwith "deleted ID reached commit")));
      (try
         ignore
           (Store.with_live_membership
              store
              ~principal:owner
              ~host_id:host
              ~additions:V.empty
              ~commit:(fun () -> failwith "controlled persistence exception"))
       with
       | Failure _ -> ());
      print_endline
        (status
           (Store.authorize_membership
              store
              ~principal:owner
              ~host_id:host
              ~references:V.empty))));
  [%expect
    {|
    false
    ok
    organization_not_found
    ok
    |}]
;;
