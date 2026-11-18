open! Core
module A = Agent_session
module P = Agent_protocol
module D = Document_schema
module E = A.Recurrence_entry
module F = A.Recurrence_definition
module Lifecycle = P.Session_lifecycle.Revision

let protocol_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : P.Error.t)]
;;

let document_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : D.Error.t)]
;;

let session =
  P.Session_ref.create
    ~server_id:(P.Id.Server.of_string "srv_due" |> protocol_ok)
    ~session_id:(P.Id.Session.of_string "ses_due" |> protocol_ok)
;;

let principal_id = P.Id.Principal.of_string "pri_due" |> protocol_ok
let timestamp = P.Timestamp.of_string "2026-08-15T12:00:00Z" |> protocol_ok
let id name = P.Id.Recurrence.of_string ("rec_" ^ name) |> protocol_ok

let source epoch =
  P.Run_source.create
    ~observer:{ script_id = "due-fixture"; source_sha256 = String.make 64 'a' }
    ~generation:0
    ~installation_epoch:epoch
  |> protocol_ok
;;

let set json name value =
  match json with
  | `Object fields -> `Object (List.Assoc.add fields ~equal:String.equal name value)
  | _ -> failwith "expected object"
;;

let append json name value =
  match json with
  | `Object fields -> `Object (fields @ [ name, value ])
  | _ -> failwith "expected object"
;;

let field json name =
  match D.Json.field json ~name with
  | Value value -> value
  | Absent | Null -> failwith ("missing fixture field " ^ name)
;;

module R = Recurrence_rules
module H = A.Recurrence_frontier
module Plan = A.Recurrence_due_plan
module Phase = A.Recurrence_phase
module N = A.Recurrence_nominal
module I = A.Recurrence_identity

let timing = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : R.Timing_error.t)]
;;

let policy ?(steps = 256) ?(history = 8) ~misfire ~grace_ms () =
  A.Recurrence_policy.create
    ~misfire
    ~grace_ms
    ~max_pending:3
    ~max_history:history
    ~max_calendar_steps:steps
  |> protocol_ok
;;

let definition ?schedule ?policy name =
  let default_schedule =
    F.Schedule.Interval
      { rule =
          R.Rule.Interval.create
            ~mode:Fixed_rate
            ~period:(R.Rule.Period.create_ms 1000L |> timing)
      ; first_due = P.Timestamp.to_time_ns timestamp
      }
  in
  let target =
    A.Recurrence_target.create_existing
      ~session
      ~source:(source 1L)
      ~configuration_revision:0L
      ~input:
        P.Session.Message_content.
          { kind = Plain_text; text = "due fixture"; attachments = [] }
    |> protocol_ok
  in
  F.create
    ~key:
      (I.Definition.create ~controller:session ~recurrence_id:(id name) ~revision:0L
       |> protocol_ok)
    ~creator:principal_id
    ~source:(source 1L)
    ~birth_lifecycle_revision:Lifecycle.zero
    ~schedule:(Option.value schedule ~default:default_schedule)
    ~target
    ~policy:(Option.value policy ~default:A.Recurrence_policy.default)
    ~created_at:timestamp
  |> protocol_ok
;;

let enabled definition = E.create definition |> protocol_ok |> E.enable |> protocol_ok
let frontier entry = E.frontier entry |> protocol_ok

let slots entry =
  H.ready (frontier entry) |> List.map ~f:(fun n -> I.Occurrence.slot (N.key n))
;;

let at_ms milliseconds =
  Time_ns.add (P.Timestamp.to_time_ns timestamp) (Time_ns.Span.of_int_ms milliseconds)
;;

let snapshot ~wall ~elapsed_ms =
  Phase.Snapshot.create ~wall ~elapsed_ns:Int64.(of_int elapsed_ms * 1_000_000L)
  |> protocol_ok
;;

let prepare entry ?zone ~phase snapshot =
  Plan.prepare
    ~definition:(E.current entry)
    ~cursor:(E.cursor entry)
    ~frontier:(frontier entry)
    ~phase
    ~snapshot
    ~zone
  |> protocol_ok
;;

let step entry ?zone ~phase snapshot =
  match prepare entry ?zone ~phase snapshot with
  | Plan.Unchanged phase -> entry, phase, false
  | Candidate plan -> E.apply_due entry plan |> protocol_ok, Plan.next_phase plan, true
;;

let entry_json entry = E.to_document entry |> document_ok |> D.Document.json

let entry_of_json json =
  D.Document.inspect ~limits:D.Limits.default json
  |> document_ok
  |> E.of_document
  |> document_ok
;;

let restore entry = entry_of_json (entry_json entry)

let zone ?(edition = 'a') ?(offset = 0) () =
  let provenance =
    R.Zone.Provenance.create ~source:"due-test" ~sha256:(String.make 64 edition) |> timing
  in
  R.Zone.create
    ~name:"Fixture/Due"
    ~provenance
    ~from:(Time_ns.of_string_with_utc_offset "2026-01-01 00:00:00Z")
    ~until:(Time_ns.of_string_with_utc_offset "2027-01-01 00:00:00Z")
    ~initial_offset_seconds:offset
    ~transitions:[]
  |> timing
;;

let calendar ?(steps = 256) ?(misfire = A.Recurrence_policy.Coalesce) () =
  let rule =
    R.Rule.Calendar.create
      ~pattern:Daily
      ~ofday:(Time_ns.Ofday.of_string "12:00:00")
      ~zone:"Fixture/Due"
      ~start_date:(Date.of_string "2026-08-15")
      ()
    |> timing
  in
  definition
    ~schedule:(Calendar rule)
    ~policy:(policy ~steps ~misfire ~grace_ms:864000000L ())
    "calendar"
;;

let edit_frontier entry ~f =
  let json = entry_json entry in
  let payload = field json "payload" in
  let document = field payload "frontier" in
  set
    json
    "payload"
    (set payload "frontier" (set document "payload" (f (field document "payload"))))
  |> entry_of_json
;;
