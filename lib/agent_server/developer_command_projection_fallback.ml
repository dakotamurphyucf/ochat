open! Core
module Receipt = Agent_store.Developer_command_receipt
module Diagnostic = Agent_store.Developer_command_diagnostic
module Fixture = Agent_store.Developer_command_fixture

let check_rejected =
  Receipt.Check
    (Receipt.Check.Rejected
       { Diagnostic.code = "developer.check.rejected"
       ; message = "Compilation was rejected; detail exceeded the receipt limit."
       ; location = None
       })
;;

let fixture_output_limit ~graph_fingerprint ~fixture_digest ~matched_steps =
  Receipt.Fixture
    (Fixture.Completed
       { Fixture.graph_fingerprint
       ; fixture_digest
       ; matched_steps
       ; logs = []
       ; outcome = Fixture.Failed Fixture.Failure.Output_limit
       })
;;

let fixture_max_size_probe =
  let digest = Chatml_source.Digest.of_contents ~checkpoint:(fun () -> ()) "" in
  fixture_output_limit
    ~graph_fingerprint:digest
    ~fixture_digest:digest
    ~matched_steps:Int.max_value
;;
