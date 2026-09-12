open Core
open Agent_server_test_support
open Fixtures
module P = Agent_protocol

let replace env ~root ~daemon ~client ~handle ~entry ~filename ~source ~allow_migration =
  Eio.Path.save
    ~create:(`Or_truncate 0o600)
    Eio.Path.(Eio.Stdenv.fs env / root / filename)
    source;
  let catalog = Agent_server.Daemon.prompts daemon in
  let definitions =
    Agent_session.Prompt_catalog.entries catalog
    |> List.map ~f:(fun entry -> entry.definition)
  in
  let prepared =
    Agent_session.Prompt_catalog.prepare
      catalog
      ~transaction_id:(fun _ -> P.Id.Transaction.create ())
      ~created_at:(P.Timestamp.now ())
      definitions
  in
  Agent_session.Prompt_catalog.install catalog prepared;
  let target =
    match (List.hd_exn (Agent_session.Prompt_catalog.entries catalog)).availability with
    | Ready revision -> Agent_session.Prompt_revision.id revision
    | _ -> failwith "replacement prompt did not compile"
  in
  let before = A.state entry.Agent_server.Session_registry.actor |> protocol_ok in
  assert (not (P.Id.Prompt_revision.equal before.spec.prompt_revision_id target));
  let attempt = ref 0 in
  retry_runtime_busy env (fun () ->
    incr attempt;
    let current = A.state entry.actor |> protocol_ok in
    Agent_client.Connection.request
      client
      (Session_upgrade_prompt
         { session_id = before.identity.session_id
         ; attachment_id = (H.attachment handle).id
         ; expected_revision = current.counters.revision
         ; target_revision = target
         ; allow_migration
         ; idempotency_key =
             P.Idempotency_key.of_string (sprintf "upgrade-publisher-%d" !attempt)
             |> protocol_ok
         }))
  |> ignore
;;
