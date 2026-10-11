open Core
module Helpers = Signature_check_tests

exception Interrupted

let%test_unit "interruption at each signature checkpoint leaves weak exports unchanged" =
  let prepare () =
    let enabled = ref false in
    let calls = ref 0 in
    let stop_at = ref None in
    let checkpoint () =
      if !enabled
      then (
        incr calls;
        if Option.exists !stop_at ~f:(Int.equal !calls) then raise Interrupted)
    in
    let implementation =
      Helpers.implementation
        ~checkpoint
        {|let storage = []
          let first = storage
          module Nested = struct let second = storage end
          let identity value = value|}
    in
    enabled := true;
    implementation, enabled, calls, stop_at
  in
  let signature =
    {|val first : int array
      module Nested : sig val second : int array end
      val identity : fun('a) -> 'a|}
  in
  let baseline, _, calls, _ = prepare () in
  Helpers.require baseline signature true;
  assert (!calls > 0);
  for interruption = 1 to !calls do
    let actual, enabled, _, stop_at = prepare () in
    stop_at := Some interruption;
    (match Helpers.check actual signature with
     | exception Interrupted -> ()
     | _ -> failwith "signature checking swallowed or skipped interruption");
    enabled := false;
    (* This incompatible later claim can succeed only if the interrupted check
       did not publish its int constraint to either alias of the shared cell. *)
    Helpers.require
      actual
      "val first : bool array module Nested : sig val second : bool array end"
      true
  done
;;
