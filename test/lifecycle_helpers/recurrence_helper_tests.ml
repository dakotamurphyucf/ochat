open! Core
module A = Agent_session

let library_namespace = "ochat.capabilities.recurrence-requests"
let namespace = "ochat.examples.recurrence-requests-test"

let library_source =
  [%blob "../../docs-src/examples/libraries/recurrence-requests/chatml.project.json"]
;;

let members =
  [ ( "src/recurrence_requests.chatml"
    , [%blob
        "../../docs-src/examples/libraries/recurrence-requests/src/recurrence_requests.chatml"]
    )
  ; ( "src/recurrence_requests.chatmli"
    , [%blob
        "../../docs-src/examples/libraries/recurrence-requests/src/recurrence_requests.chatmli"]
    )
  ]
;;

let project =
  Request_package_support.test_project ~namespace ~module_name:"Recurrence_requests"
;;

let build entry =
  Request_package_support.compile
    ~library_namespace
    ~library_source
    ~members
    ~namespace
    ~project
    ~entry_path:"main.chatml"
    ~target:One_off_v1
    entry
  |> Request_package_support.ok
;;

let example =
  {|let require = fun value -> match value with
| `Ok(json) -> json
| `Error(error) -> `Object([{key = "unexpected_helper_error"; value = `Bool(true)}])
let main input =
  let source = {script_id = "native"; source_sha256 = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"; generation = 0; installation_epoch = "1"} in
  let basis = {generation = 0; catalog_revision = "0"; source = source; lifecycle_revision = "0"; key = "helper-request"} in
  let safe = {generation = 0; catalog_revision = "0"; key = "helper-control"} in
  let interval = require(Requests.interval(`Fixed_delay, "60000", "2100-01-01T00:00:00Z")) in
  let calendar = require(Requests.calendar({pattern = `Weekdays([1, 3, 5]); ofday = "09:00:00"; zone = "America/Chicago"; start_date = "2100-01-01"; ending = `No_end; gap = `Skip_gap; repeat = `Earlier})) in
  let policy = require(Requests.policy({misfire = `Catch_up(2); grace_ms = "60000"; max_pending = 2; max_history = 64; max_calendar_steps = 256})) in
  let worker = `Authored({name = "weekly_worker"; mode = `Workflow}) in
  let interval_capture = {schedule = interval; target = worker; policy = policy} in
  let calendar_capture = {schedule = calendar; target = worker; policy = policy} in
  let boundary_source = {script_id = "native"; source_sha256 = source.source_sha256; generation = 9007199254740991; installation_epoch = "1"} in
  let boundary = {generation = 9007199254740991; catalog_revision = "0"; source = boundary_source; lifecycle_revision = "0"; key = "safe-boundary"} in
  let boundary_safe = {generation = 9007199254740991; catalog_revision = "0"; key = "safe-control"} in
  let excessive_source = {script_id = "native"; source_sha256 = source.source_sha256; generation = 9007199254740992; installation_epoch = "1"} in
  let excessive = {generation = 9007199254740992; catalog_revision = "0"; source = excessive_source; lifecycle_revision = "0"; key = "unsafe-generation"} in
  let excessive_safe = {generation = 9007199254740992; catalog_revision = "0"; key = "unsafe-control"} in
  let rejected = fun result -> match result with
    | `Error(`Invalid_generation) -> `Bool(true)
    | _ -> `Bool(false) in
  Task.pure(`Array([
    Requests.list(), Requests.get("rec_native_helper"), Requests.status("rec_native_helper"), Requests.history("rec_native_helper"),
    require(Requests.create("rec_native_helper", basis, interval_capture)),
    require(Requests.edit("rec_native_helper", basis, "0", calendar_capture)),
    require(Requests.resume("rec_native_helper", basis, "0")),
    require(Requests.pause("rec_native_helper", safe, "0")),
    require(Requests.cancel("rec_native_helper", safe, "0")),
    require(Requests.create("rec_native_helper", boundary, interval_capture)),
    require(Requests.pause("rec_native_helper", boundary_safe, "0")),
    rejected(Requests.create("rec_native_helper", excessive, interval_capture)),
    rejected(Requests.pause("rec_native_helper", excessive_safe, "0"))]))|}
;;

let%expect_test
    "checked recurrence package imports and its requests pass the actual native decoder"
  =
  let program = build example in
  let values = Support.run program `Null |> Jsonaf.list_exn in
  let effects =
    A.Recurrence_native_admission.Effect.
      [ List; Get; Status; History; Create; Edit; Resume; Pause; Cancel ]
  in
  let values, boundaries = List.split_n values 9 in
  List.iter2_exn effects values ~f:(fun effect_kind json ->
    A.Recurrence_native_request.decode ~effect_kind json
    |> Result.map_error ~f:(fun error -> error.Agent_protocol.Error.message)
    |> Result.ok_or_failwith
    |> ignore);
  (match boundaries with
   | [ create; pause; `True; `True ] ->
     List.iter
       [ A.Recurrence_native_admission.Effect.Create, create; Pause, pause ]
       ~f:(fun (effect_kind, json) ->
         A.Recurrence_native_request.decode ~effect_kind json
         |> Result.map_error ~f:(fun error -> error.Agent_protocol.Error.message)
         |> Result.ok_or_failwith
         |> ignore;
         let basis = Jsonaf.member_exn "basis" json in
         assert (
           Document_schema.Json.equal
             (Jsonaf.member_exn "generation" basis)
             (`Number "9007199254740991")));
     assert (
       Document_schema.Json.equal
         (Jsonaf.member_exn
            "generation"
            (Jsonaf.member_exn "source" (Jsonaf.member_exn "basis" create)))
         (`Number "9007199254740991"))
   | _ -> failwith "helper did not reject generations beyond exact JSON representation");
  print_endline
    "real package import and linked execution; nine native requests; explicit \
     interval/calendar/policy constructors; exact-safe generation boundary";
  [%expect
    {| real package import and linked execution; nine native requests; explicit interval/calendar/policy constructors; exact-safe generation boundary |}]
;;
