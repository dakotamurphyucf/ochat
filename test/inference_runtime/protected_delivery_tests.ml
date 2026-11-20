open! Core
module Delivery = Inference_runtime.Protected_delivery
module Payload = History_entry.Payload

let ok result =
  Result.ok_or_failwith (Result.map_error result ~f:(fun _ -> "fixture admission"))
;;

let entry sequence view =
  let payload =
    Payload.Semantic.create
      view
      ~metadata:{ Payload.Metadata.empty with call_id = Value "provider-pair" }
    |> ok
    |> Payload.authored
  in
  History_entry.create_with_id
    ~id:(History_entry.Id.create ~namespace:"protected" ~sequence |> ok)
    payload
;;

let call_at sequence ?(name = "read_note") ?(input_bytes = "{}") () =
  entry
    sequence
    (Call { kind = Function; name; namespace = Absent; input_bytes; async = Absent })
;;

let call = call_at 0 ()

let output_for ~call ~sequence text =
  entry
    sequence
    (Result
       { kind = Function; relation = Bound (History_entry.id call); output = Text text })
;;

let output = output_for ~call ~sequence:1

let scope =
  Transcript.Scope.create
    ~source:(Transcript.Source_id.of_string "owner" |> ok)
    ~attempt:(Transcript.Attempt_id.of_string "actual" |> ok)
    ~relation:Root
  |> ok
;;

let%expect_test "protected preparation consumes only a matching bound output once" =
  let materializations = ref 0 in
  let new_plan ?(call = call) ?(private_bytes = "private") () =
    Delivery.create
      ~call
      ~output:(output "receipt")
      ~materialize:(fun () ->
        incr materializations;
        Ok (Text private_bytes))
      ~refresh:(fun ~scope:_ () -> true)
  in
  let prepare plan history = Delivery.Materialized.prepare [ plan ] ~history in
  let check result =
    match result with
    | Ok _ -> print_endline "prepared"
    | Error error -> print_s [%sexp (error : Delivery.error)]
  in
  let missing = new_plan () in
  check (prepare missing [ call ]);
  check (prepare missing [ call; output "receipt" ]);
  check (prepare (new_plan ()) [ call; output "substituted" ]);
  check (prepare (new_plan ()) [ output "receipt" ]);
  check (prepare (new_plan ()) [ call_at 0 ~name:"different_tool" (); output "receipt" ]);
  check
    (prepare
       (new_plan ())
       [ call_at 0 ~input_bytes:"{\"different\":true}" (); output "receipt" ]);
  check (prepare (new_plan ~call:(call_at 2 ()) ()) [ call; output "receipt" ]);
  check (prepare (new_plan ()) [ output "receipt"; call ]);
  check (prepare (new_plan ()) [ call; call; output "receipt" ]);
  let older_call = call_at 2 () in
  let older_output = output_for ~call:older_call ~sequence:3 "older receipt" in
  check (prepare (new_plan ()) [ call; older_call; output "receipt" ]);
  check (prepare (new_plan ()) [ older_call; call; older_output; output "receipt" ]);
  check (prepare (new_plan ()) [ call; older_output; output "receipt" ]);
  check (prepare (new_plan ()) [ call; output "receipt"; output "receipt" ]);
  let accepted = new_plan () in
  let history = [ older_call; older_output; call; output "receipt" ] in
  let materialized = prepare accepted history |> ok in
  let refresh = Delivery.Materialized.bind materialized ~scope |> ok in
  let final = refresh () in
  final ();
  (try
     final ();
     assert false
   with
   | Delivery.Rejected error -> print_s [%sexp (error : Delivery.error)]);
  check (Delivery.Materialized.bind materialized ~scope);
  check (prepare accepted history);
  check (prepare (new_plan ~private_bytes:(String.make (1024 * 1024) 'x') ()) history);
  printf "materializations=%d\n" !materializations;
  [%expect
    {|
    Invalid_output
    Expired
    Invalid_output
    Invalid_output
    Invalid_output
    Invalid_output
    Invalid_output
    Invalid_output
    Invalid_output
    Invalid_output
    Invalid_output
    Invalid_output
    Invalid_output
    Expired
    Expired
    Expired
    Limit_exceeded
    materializations=2
    |}]
;;
