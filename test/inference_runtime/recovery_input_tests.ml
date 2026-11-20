open! Core
open Runtime_tests
module Evidence = Runtime.Recovery_input
module Delivery = Runtime.Protected_delivery

let message sequence text =
  let payload =
    P.Semantic.create
      (Message
         { form = Input
         ; role = User
         ; phase = Absent
         ; content = [ Text { text; annotations = []; logprobs = Absent } ]
         })
      ~metadata:P.Metadata.empty
    |> ok
    |> P.authored
  in
  History_entry.create_with_id
    ~id:(History_entry.Id.create ~namespace:"prepared-recovery" ~sequence |> ok)
    payload
;;

let%test_unit "accepted protected preparation leaves holes for both ends and opaque input"
  =
  let call = Protected_delivery_tests.call in
  let output = Protected_delivery_tests.output "committed receipt" in
  let protected_payload = "private materialized result" in
  let source =
    Delivery.create
      ~call
      ~output
      ~materialize:(fun () -> Ok (Text protected_payload))
      ~custody:Local
      ~refresh:(fun ~scope:_ () -> true)
  in
  let observed = ref false in
  let prepare make_plan ~preparation_id request =
    let configuration =
      O.Configuration.of_target
        (R.target request)
        ~preparation_id
        ~transport:In_process
        ~capabilities:[]
        ~limits:O.Admission.observation
      |> ok
    in
    make_plan
      ~request
      ~configuration
      ~fingerprint:"private provider wire fingerprint"
      ~run:
        (fun
          ~sw:_
          ~scope:_
          ~accounting_id:_
          ~note_delivery:_
          ~on_event:_
          ~on_observation:_
          ~submission_guard:_
        -> failwith "prepared evidence allocated an attempt")
  in
  let context =
    Runtime.Adapter.create
      ~id:"synthetic"
      ~limits:Runtime.Limits.default
      ~bind:(fun _ -> Ok ())
      ~preflight_history:(fun ~target:_ _ -> Ok ())
      ~prepare:(prepare Runtime.Plan.create)
      ~prepare_protected:(fun ~policy:_ ~materialized ~preparation_id request ->
        (match Delivery.Materialized.find materialized (History_entry.id output) with
         | Some (Text text) -> assert (String.equal protected_payload text)
         | _ -> assert false);
        observed := true;
        prepare Runtime.Plan.create_isolated ~preparation_id request)
      ()
    |> ok
    |> Runtime.Context.create ~target
    |> ok
  in
  let visible = message 0 "public neutral text" in
  let opaque =
    let payload =
      P.Semantic.create
        (Unknown { provider_kind = "unknown provider item" })
        ~metadata:P.Metadata.empty
      |> ok
      |> P.authored
    in
    History_entry.with_payload (message 1 "") payload
  in
  let oversized = message 2 (String.make (70 * 1024) 'x') in
  let final_entry = message 3 "public tail" in
  let request =
    R.create
      ~target
      ~history:[ visible; call; output; opaque; oversized; final_entry ]
      ~tools:[]
      ~assets:[]
      ~limits
    |> ok
  in
  let prepared =
    Runtime.Context.prepare
      context
      ~protected_delivery:[ source ]
      ~preparation_id:"p"
      request
    |> ok
  in
  assert !observed;
  let witness = Runtime.Prepared.recovery_input prepared |> Option.value_exn in
  [%test_eq: int] 6 (Evidence.input_count witness);
  [%test_eq: int list]
    [ 0; 5 ]
    (List.map (Evidence.rows witness) ~f:(fun (row : Evidence.row) -> row.ordinal));
  assert (
    List.equal
      History_entry.Id.equal
      [ History_entry.id visible; History_entry.id final_entry ]
      (List.map (Evidence.rows witness) ~f:(fun (row : Evidence.row) -> row.occurrence)))
;;
