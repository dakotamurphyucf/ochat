open Core
module H = Shell_runtime.Chatml_extension
module L = Chatml.Chatml_lang
module S = Chatml_source
module Snapshot = Session.Snapshot

let body =
  {|let initial_state = Counter.seed
let match_command ev state =
  Task.bind(Match.yes(Array.get(Shell.argv(ev),0) ++ ":" ++ to_string(state)),
    fun ignored -> if state == 2 then Task.fail("rejected") else Task.pure(state+1))|}
;;

let compiled env =
  let contract =
    H.compiler_contract
      Shell_matcher
      ~entrypoint:"match_command"
      ~checkpoint:Support.checkpoint
    |> Support.ok
  in
  let input =
    Support.input
      contract
      [ "entry.chatml", body, [ "Counter", "counter.chatml" ]
      ; "counter.chatml", "let seed = 0", []
      ]
  in
  H.compile_prepared
    input
    ~script:(Support.script body)
    ~entrypoint:"match_command"
    ~limits:Support.compiler_limits
    ~max_dependency_depth:16
    ~env
    ()
  |> Result.map_error ~f:(fun errors ->
    List.map errors ~f:Chatmd_shell_spec.Diagnostic.to_string |> String.concat ~sep:"; ")
  |> Result.ok_or_failwith
;;

let instantiate env compiled lifecycle =
  match H.instantiate ~env ~lifecycle compiled with
  | Ok instance -> instance
  | Error error -> failwith (Chatmd_shell_spec.Diagnostic.to_string error)
;;

let call instance = H.call instance ~context:Support.context ~event:Support.event

let%expect_test
    "captured original hook receives event/state and commits under persistent control"
  =
  Eio_main.run (fun env ->
    let compiled = compiled env in
    [%test_eq: string]
      (H.source_sha256 compiled)
      (S.Digest.of_contents body ~checkpoint:Support.checkpoint |> S.Digest.to_hex);
    assert (phys_equal compiled (H.with_entrypoint compiled "match_command" |> Support.ok));
    assert (Result.is_error (H.with_entrypoint compiled "different"));
    let persistent = instantiate env compiled Session in
    print_s [%sexp (call persistent |> Support.ok : H.action)];
    print_s [%sexp (call persistent |> Support.ok : H.action)];
    let before = H.snapshot persistent |> Support.ok in
    assert (Result.is_error (call persistent));
    let after = H.snapshot persistent |> Support.ok in
    assert (Int.equal (Snapshot.compare before after) 0);
    let fresh = instantiate env compiled Runtime in
    print_s [%sexp (call fresh |> Support.ok : H.action)];
    H.restore fresh before |> Support.ok;
    assert (Result.is_error (call fresh));
    let invocation = instantiate env compiled Invocation in
    for _ = 1 to 2 do
      [%test_eq: H.action]
        (call invocation |> Support.ok)
        (H.Match (true, "actual-event:0"))
    done);
  [%expect
    {|
    (Match true actual-event:0)
    (Match true actual-event:1)
    (Match true actual-event:0)
  |}]
;;

let%test_unit "shell native role and body constraints fail closed before initialization" =
  Eio_main.run (fun env ->
    assert (
      Result.is_error
        (H.compiler_contract
           Moderator
           ~entrypoint:"on_event"
           ~checkpoint:Support.checkpoint));
    let contract =
      H.compiler_contract
        Shell_matcher
        ~entrypoint:"match_command"
        ~checkpoint:Support.checkpoint
      |> Support.ok
    in
    let input =
      Support.input
        contract
        [ "entry.chatml", body, [ "Counter", "counter.chatml" ]
        ; "counter.chatml", "let seed = 0", []
        ]
    in
    let script = Support.script body in
    let forged = { script with source_sha256 = String.make 64 '0' } in
    assert (
      Result.is_error
        (H.compile_prepared
           input
           ~script:forged
           ~entrypoint:"match_command"
           ~limits:Support.compiler_limits
           ~max_dependency_depth:16
           ~env
           ()));
    let edited = Support.script (body ^ " ") in
    assert (
      Result.is_error
        (H.compile_prepared
           input
           ~script:edited
           ~entrypoint:"match_command"
           ~limits:Support.compiler_limits
           ~max_dependency_depth:16
           ~env
           ())))
;;

exception Persist_interrupted of int ref

let%test_unit
    "prospective prepared state remains unchanged when persistence raises or cancels"
  =
  Eio_main.run (fun env ->
    let instance = instantiate env (compiled env) Session in
    let before = H.snapshot instance |> Support.ok in
    let identity = ref 19 in
    H.set_snapshot_handler instance (fun _ -> raise (Persist_interrupted identity));
    (match call instance with
     | _ -> assert false
     | exception Persist_interrupted actual -> assert (phys_equal identity actual));
    assert (Int.equal (Snapshot.compare before (H.snapshot instance |> Support.ok)) 0);
    H.set_snapshot_handler instance (fun _ ->
      raise (Eio.Cancel.Cancelled (Persist_interrupted identity)));
    (match call instance with
     | _ -> assert false
     | exception Eio.Cancel.Cancelled (Persist_interrupted actual) ->
       assert (phys_equal identity actual));
    assert (Int.equal (Snapshot.compare before (H.snapshot instance |> Support.ok)) 0);
    let persisted = ref None in
    (match
       Eio.Cancel.sub (fun cancel ->
         H.set_snapshot_handler instance (fun snapshot ->
           Eio.Cancel.cancel cancel (Persist_interrupted identity);
           (* The admitted write may yield under its narrow cancellation protection.
              Installation must follow without another cancellation checkpoint;
              joining the outer execution scope may then report cancellation. *)
           Eio.Fiber.yield ();
           persisted := Some snapshot;
           Ok ());
         ignore (call instance : (H.action, Chatmd_shell_spec.Diagnostic.t) Result.t))
     with
     | _ -> assert false
     | exception Eio.Cancel.Cancelled (Persist_interrupted actual) ->
       assert (phys_equal identity actual));
    assert (
      Int.equal
        (Snapshot.compare
           (Option.value_exn !persisted)
           (H.snapshot instance |> Support.ok))
        0);
    H.set_snapshot_handler instance (fun _ -> Ok ());
    [%test_eq: H.action] (call instance |> Support.ok) (H.Match (true, "actual-event:1")))
;;

let array_body =
  {|let initial_state = {cell = [0]}
let match_command ev state =
  let old = Array.get(state.cell,0) in
  Array.set(state.cell,0,old+1);
  Task.bind(Match.yes(to_string(old)), fun ignored ->
    if Array.get(Shell.argv(ev),0) == "actual-event" then Task.fail("rejected") else Task.pure(state))|}
;;

let compiled_array env body =
  let contract =
    H.compiler_contract
      Shell_matcher
      ~entrypoint:"match_command"
      ~checkpoint:Support.checkpoint
    |> Support.ok
  in
  let input = Support.input contract [ "entry.chatml", body, [] ] in
  H.compile_prepared
    input
    ~script:(Support.script body)
    ~entrypoint:"match_command"
    ~limits:Support.compiler_limits
    ~max_dependency_depth:16
    ~env
    ()
  |> Result.map_error ~f:(fun errors ->
    List.map errors ~f:Chatmd_shell_spec.Diagnostic.to_string |> String.concat ~sep:"; ")
  |> Result.ok_or_failwith
;;

let cell instance =
  match H.snapshot instance |> Support.ok with
  | Snapshot.Record fields ->
    (match List.Assoc.find fields ~equal:String.equal "state" with
     | Some (Record [ ("cell", Array [ Int value ]) ]) -> value
     | _ -> assert false)
  | _ -> assert false
;;

let commit_event =
  match Support.event with
  | L.VRecord fields ->
    L.VRecord (Map.set fields ~key:"argv" ~data:(L.VArray [| L.VString "commit" |]))
  | _ -> assert false
;;

let%test_unit "Legacy and Prepared persistence errors isolate nested mutable state" =
  Eio_main.run (fun env ->
    let constructors =
      [ (fun () -> H.compile ~script:(Support.script array_body) |> Support.ok)
      ; (fun () -> compiled_array env array_body)
      ]
    in
    List.iter constructors ~f:(fun compile ->
      let instance = instantiate env (compile ()) Session in
      H.set_snapshot_handler instance (fun _ -> Error "persistence rejected");
      assert (
        Result.is_error (H.call instance ~context:Support.context ~event:commit_event));
      [%test_eq: int] (cell instance) 0;
      H.set_snapshot_handler instance (fun _ -> Ok ());
      H.call instance ~context:Support.context ~event:commit_event |> Support.ok |> ignore;
      [%test_eq: int] (cell instance) 1))
;;

let%test_unit "prepared data state copy keeps nested array mutation transactional" =
  Eio_main.run (fun env ->
    let instance = instantiate env (compiled_array env array_body) Session in
    [%test_eq: int] (cell instance) 0;
    assert (Result.is_error (call instance));
    [%test_eq: int] (cell instance) 0;
    H.call instance ~context:Support.context ~event:commit_event |> Support.ok |> ignore;
    [%test_eq: int] (cell instance) 1;
    assert (Result.is_error (call instance));
    [%test_eq: int] (cell instance) 1)
;;

exception Hook_cancelled of int ref

let%test_unit
    "actual prepared hook computation responds to owner cancellation and retains state"
  =
  Eio_main.run (fun env ->
    let body =
      {|let initial_state = {cell = [0]}
let rec spin n = spin(n+1)
let match_command ev state = Array.set(state.cell,0,1); spin(0)|}
    in
    let limits = { H.default_limits with fuel = 1_000_000 } in
    let instance =
      H.instantiate ~env ~limits ~lifecycle:Session (compiled_array env body)
      |> Support.ok
    in
    let ready, resolver = Eio.Promise.create () in
    let identity = ref 23 in
    (match
       Eio.Cancel.sub (fun cancel ->
         Eio.Fiber.both
           (fun () ->
              Eio.Promise.await ready;
              Eio.Fiber.yield ();
              Eio.Cancel.cancel cancel (Hook_cancelled identity))
           (fun () ->
              Eio.Promise.resolve resolver ();
              ignore
                (H.call instance ~context:Support.context ~event:L.VUnit
                 : (H.action, Chatmd_shell_spec.Diagnostic.t) Result.t)))
     with
     | _ -> assert false
     | exception Eio.Cancel.Cancelled (Hook_cancelled actual) ->
       assert (phys_equal identity actual));
    [%test_eq: int] (cell instance) 0)
;;

let%test_unit "minimal prepared shell hook is usable under default compiler limits" =
  Eio_main.run (fun env ->
    let body = "let initial_state = 0 let match_command event state = Task.pure(state)" in
    let c =
      H.compiler_contract
        Shell_matcher
        ~entrypoint:"match_command"
        ~checkpoint:Support.checkpoint
      |> Support.ok
    in
    let input = Support.input c [ "entry.chatml", body, [] ] in
    match
      H.compile_prepared
        input
        ~script:(Support.script body)
        ~entrypoint:"match_command"
        ~limits:Chatml_compilation.default_limits
        ~max_dependency_depth:16
        ~env
        ()
    with
    | Ok _ -> ()
    | Error diagnostics ->
      let allocated = ref 0 in
      let canonical =
        Support.Contract.input_bytes c ~checkpoint:Support.checkpoint ~allocate:(fun n ->
          allocated := !allocated + n)
      in
      failwith
        (sprintf
           "Default native admission rejected canonical=%d conservative=%d: %s"
           canonical
           !allocated
           (List.map diagnostics ~f:Chatmd_shell_spec.Diagnostic.to_string
            |> String.concat ~sep:"; ")))
;;
