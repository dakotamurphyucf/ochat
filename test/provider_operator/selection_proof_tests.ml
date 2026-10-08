open! Core
module P = Agent_protocol
module DTO = P.Provider_operator
module A = Provider_operator.Profile_admin
module M = Credential_registry_model
module C = Credential_registry
module S = Private_storage
module Secret = Provider_secret_store

let ok r =
  Result.map_error r ~f:(fun _ -> "synthetic selection fixture failed")
  |> Result.ok_or_failwith
;;

let id s = M.Id.create s |> ok
let profile s = DTO.Profile_id.of_string s |> ok
let revision s = DTO.Revision.of_string s |> ok
let principal = P.Id.Principal.of_string "pri_selection" |> ok

let request i expected_revision : DTO.Select_request.t =
  { profile = profile "approved"
  ; expected_revision
  ; idempotency_key = P.Idempotency_key.of_string (sprintf "key-%d" i) |> ok
  }
;;

let%expect_test
    "bounded exact selection proofs reject collisions and expire without replay"
  =
  let first_request = request 0 (revision "initial") in
  let first_result : DTO.Selection_result.t =
    { profile = profile "approved"; revision = revision "r-0" }
  in
  let initial =
    A.Selection_proofs.add
      A.Selection_proofs.empty
      ~principal
      ~operation:(id "op-0")
      first_request
      first_result
    |> ok
  in
  assert (
    Result.is_error
      (A.Selection_proofs.add
         initial
         ~principal
         ~operation:(id "op-0")
         (request 1 first_request.expected_revision)
         first_result));
  let rec shuffled = function
    | `Object fields ->
      `Object (List.rev_map fields ~f:(fun (key, value) -> key, shuffled value))
    | `Array values -> `Array (List.map values ~f:shuffled)
    | value -> value
  in
  let shuffled_history =
    A.Selection_proofs.of_json (shuffled (A.Selection_proofs.to_json initial)) |> ok
  in
  assert (
    Option.is_some
      (A.Selection_proofs.lookup
         shuffled_history
         ~principal
         ~operation:(id "op-0")
         first_request
       |> ok));
  let encoded = A.Selection_proofs.to_json initial in
  (match encoded with
   | `Array [ proof ] ->
     assert (Result.is_error (A.Selection_proofs.of_json (`Array [ proof; proof ])))
   | _ -> assert false);
  let history =
    List.fold
      (List.init 63 ~f:(fun i -> i + 1))
      ~init:initial
      ~f:(fun proofs i ->
        let result : DTO.Selection_result.t =
          { profile = profile "approved"; revision = revision (sprintf "r-%d" i) }
        in
        A.Selection_proofs.add
          proofs
          ~principal
          ~operation:(id (sprintf "op-%d" i))
          (request i (revision (sprintf "r-%d" (i - 1))))
          result
        |> ok)
  in
  let history = A.Selection_proofs.of_json (A.Selection_proofs.to_json history) |> ok in
  assert (
    Option.is_some
      (A.Selection_proofs.lookup history ~principal ~operation:(id "op-0") first_request
       |> ok));
  let last : DTO.Selection_result.t =
    { profile = profile "approved"; revision = revision "r-64" }
  in
  let history =
    A.Selection_proofs.add
      history
      ~principal
      ~operation:(id "op-64")
      (request 64 (revision "r-63"))
      last
    |> ok
  in
  assert (A.Selection_proofs.length history = 64);
  assert (
    Option.is_none
      (A.Selection_proofs.lookup history ~principal ~operation:(id "op-0") first_request
       |> ok));
  print_endline
    "exact proof survives 64-entry roundtrip; collision rejected; oldest evidence \
     unavailable";
  [%expect
    {| exact proof survives 64-entry roundtrip; collision rejected; oldest evidence unavailable |}]
;;

let%expect_test
    "persisted selection A lost reply then B retains original A result on reopen"
  =
  Mirage_crypto_rng_unix.use_default ();
  Eio_main.run (fun env ->
    (* Same Eio mkdir pattern as the E2E Temporary_environment helper; that
       helper is not linked into this native inline-test library. *)
    let suffix = P.Id.Transaction.create () |> P.Id.Transaction.to_string in
    let anchor = Eio.Path.(Eio.Stdenv.fs env / "/tmp" / ("ochat-selection-" ^ suffix)) in
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
              ~namespace:(Secret.Namespace.create "selection" |> ok)
            |> ok
          in
          let registry =
            C.initialize_new
              ~metadata_admission:C.Metadata_admission.nonblocking
              ~sw
              ~wall_clock:(Eio.Stdenv.clock env)
              ~new_operation:(fun () -> id "unused")
              ~directory
              ~secrets
              ~environment:None
              ~host:(id "host")
              ~incarnation:(id "incarnation")
            |> ok
          in
          let identity =
            M.Identity.api_key
              ~host:(id "host")
              ~provider:"openai"
              ~billing:"api"
              ~account:None
              ~key_reference:(id "binding")
            |> ok
          in
          let template =
            A.Template.create
              ~profile:(profile "approved")
              ~binding:(id "binding")
              ~revision:(revision "config")
              ~authentication:Api_key
              ~expectation:(M.Expectation.exact identity)
              ~expected_account:None
              ~mapping:(fun _ ->
                Error Inference_host.Credential_bridge.Error.Invalid_mapping)
            |> ok
          in
          A.initialize
            directory
            ~incarnation:(id "incarnation")
            ~templates:[ template ]
            ~default_profile:(profile "approved")
            ~initial_revision:(revision "initial")
          |> ok;
          let next = ref 0 in
          let open_admin () =
            A.open_
              directory
              ~incarnation:(id "incarnation")
              ~registry
              ~templates:[ template ]
              ~publish:(fun _ -> Ok ())
              ~new_revision:(fun () ->
                incr next;
                revision (sprintf "published-%d" !next))
            |> ok
          in
          let a = request 0 (revision "initial") in
          (* Drop the first result as a lost transport reply. Reopen reads only disk. *)
          ignore
            (A.select
               (open_admin ())
               ~principal
               ~operation:(id "operation-a")
               ~reconcile:false
               a
             |> ok
             : DTO.Selection_result.t);
          (* External JSON formatting may reorder every object without changing
           its meaning; the closed schema still rejects unknown/duplicate keys. *)
          let metadata = S.Name.create "operator-profiles.json" |> ok in
          let json =
            S.Directory.read_bounded directory metadata ~max_bytes:(256 * 1024)
            |> ok
            |> Bytes.to_string
            |> Jsonaf.of_string
          in
          let rec shuffled = function
            | `Object fields ->
              `Object (List.rev_map fields ~f:(fun (key, value) -> key, shuffled value))
            | `Array values -> `Array (List.map values ~f:shuffled)
            | value -> value
          in
          S.Directory.replace_metadata
            directory
            metadata
            (Bytes.of_string (Jsonaf.to_string (shuffled json)))
          |> ok;
          let reopened = open_admin () in
          let after_a = A.selection reopened |> ok in
          let b = request 1 after_a.revision in
          let after_b =
            A.select reopened ~principal ~operation:(id "operation-b") ~reconcile:false b
            |> ok
          in
          let reopened = open_admin () in
          let original =
            A.select reopened ~principal ~operation:(id "operation-a") ~reconcile:true a
            |> ok
          in
          assert (DTO.Revision.equal original.revision after_a.revision);
          assert (not (DTO.Revision.equal original.revision after_b.revision));
          assert (
            DTO.Revision.equal (A.selection reopened |> ok).revision after_b.revision);
          assert (
            Result.is_error
              (A.select
                 reopened
                 ~principal
                 ~operation:(id "operation-a")
                 ~reconcile:true
                 b));
          assert (!next = 2);
          let entered, signal_entered = Eio.Promise.create () in
          let resume, signal_resume = Eio.Promise.create () in
          let current_authority = ref true in
          let delayed =
            A.open_
              directory
              ~incarnation:(id "incarnation")
              ~registry
              ~templates:[ template ]
              ~publish:(fun _ -> Ok ())
              ~new_revision:(fun () ->
                Eio.Promise.resolve signal_entered ();
                Eio.Promise.await resume;
                revision "denied-publication")
            |> ok
          in
          let result, finished = Eio.Promise.create () in
          Eio.Fiber.fork ~sw (fun () ->
            Eio.Promise.resolve
              finished
              (A.select
                 ~authorize_commit:(fun () -> !current_authority)
                 delayed
                 ~principal
                 ~operation:(id "denied-select")
                 ~reconcile:false
                 (request 2 after_b.revision)));
          Eio.Promise.await entered;
          current_authority := false;
          Eio.Promise.resolve signal_resume ();
          (match Eio.Promise.await result with
           | Error Authorization_denied -> ()
           | _ -> failwith "expired actor published fresh selection");
          assert (
            DTO.Revision.equal (A.selection reopened |> ok).revision after_b.revision);
          let original_after_expiry =
            A.select
              ~authorize_commit:(fun () -> false)
              reopened
              ~principal
              ~operation:(id "operation-a")
              ~reconcile:true
              a
            |> ok
          in
          assert (DTO.Revision.equal original_after_expiry.revision after_a.revision))));
  print_endline "A original result recovered after B and reopen, without selection replay";
  print_endline
    "authority lost under selection lease blocks fresh write; original committed proof \
     still reconciles";
  [%expect
    {|
    A original result recovered after B and reopen, without selection replay
    authority lost under selection lease blocks fresh write; original committed proof still reconciles |}]
;;
