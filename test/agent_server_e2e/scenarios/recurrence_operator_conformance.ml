open! Core
module P = Agent_protocol
module C = Agent_client
module W = P.Recurrence_control
module Q = P.Recurrence_query
module H = C.Session_handle
module Controls = C.Recurrence_controls
module Fixture = Support.Config_fixture

let configure ~env fixture =
  Fixture.grant_public_scopes fixture [ View_session_transcript; Send_messages ];
  Eio.Path.save
    ~create:(`Or_truncate 0o600)
    Eio.Path.(Eio.Stdenv.fs env / Fixture.prompt_path fixture)
    {|<developer>Offline recurrence operator conformance.</developer>
<script id="recurrence_conformance" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let on_event ctx state event = Task.pure(state)
</script>|}
;;

let require condition message =
  if not condition then raise_s [%sexp "recurrence conformance", (message : string)]
;;

let checked = function
  | Ok value -> value
  | Error error -> raise_s [%sexp "recurrence protocol error", (error : P.Error.t)]
;;

let require_denied = function
  | Error { P.Error.code = Permission_denied; _ } -> ()
  | Error error -> raise_s [%sexp "unexpected recurrence rejection", (error : P.Error.t)]
  | Ok _ -> require false "foreign HTTP caller was authorized"
;;

let available = function
  | Q.Result.Available view -> view
  | Unavailable _ -> raise_s [%sexp "recurrence controller is unavailable"]
;;

let initialize connection =
  let initialized = Support.Unix_driver.initialize connection |> checked in
  require
    (List.mem initialized.enabled_features Q.Features.inspect ~equal:String.equal
     && List.mem initialized.enabled_features W.Features.control ~equal:String.equal)
    "recurrence features were not negotiated";
  initialized
;;

let await_started ~clock connection session_id =
  (* Idle can precede the initial moderator checkpoint. Join the actual
     callback before selecting the capture basis. *)
  Eio.Time.with_timeout_exn clock 15. (fun () ->
    let rec loop () =
      match
        C.Connection.request connection (Session_get { session_id; history = None })
        |> checked
      with
      | Session_get snapshot ->
        let snapshot = P.Public_snapshot.fields snapshot in
        Option.iter snapshot.failure ~f:(fun error ->
          raise_s [%sexp "recurrence session startup", (error : P.Error.t)]);
        let receipts =
          List.filter snapshot.extension_status ~f:(fun receipt ->
            P.Extension_status.equal_kind receipt.kind Moderator_execution)
        in
        if
          (match snapshot.session.observed_state with
           | Idle -> true
           | _ -> false)
          && Option.is_none snapshot.session.active_operation
          && (not (List.is_empty receipts))
          && List.for_all receipts ~f:(fun receipt ->
            String.equal receipt.state "completed")
        then ()
        else (
          Eio.Time.sleep clock 0.025;
          loop ())
      | _ -> raise_s [%sexp "unexpected recurrence session startup response"]
    in
    loop ())
;;

let intent (basis : Q.Basis.t) ~key action =
  W.Intent.create
    ~generation:basis.generation
    ~expected_catalog_revision:basis.catalog_revision
    ~key:(P.Idempotency_key.of_string key |> checked)
    action
  |> checked
;;

let create_intent (basis : Q.Basis.t) id =
  let period =
    Recurrence_rules.Rule.Period.create_ms 600_000L
    |> Result.map_error ~f:(fun error ->
      Sexp.to_string_hum [%sexp (error : Recurrence_rules.Timing_error.t)])
    |> Result.ok_or_failwith
  in
  let schedule =
    P.Recurrence_schedule.Interval
      { rule = Recurrence_rules.Rule.Interval.create ~mode:Fixed_rate ~period
      ; first_due = Time_ns.add (Time_ns.now ()) (Time_ns.Span.of_sec 3600.)
      }
  in
  let capture =
    W.Capture.create
      ~schedule
      ~policy:P.Recurrence_policy.default
      ~selector:
        (Existing
           { session = basis.session
           ; input =
               { kind = Plain_text; text = "Future recurrence input"; attachments = [] }
           })
      ()
    |> checked
  in
  let source_basis =
    W.Source_basis.
      { source = Option.value_exn basis.source
      ; expected_lifecycle_revision = basis.lifecycle_revision
      }
  in
  intent
    basis
    ~key:"recurrence-conformance:create"
    (Create { id; basis = source_basis; capture })
;;

let verify_row (receipt : W.Receipt.t) (basis : Q.Basis.t) (row : Q.Definition.t) =
  require
    (P.Id.Recurrence.equal row.id receipt.recurrence_id
     && Int64.equal row.definition_revision receipt.definition_revision
     && Int64.equal row.control_revision receipt.control_revision
     && Int64.equal basis.catalog_revision receipt.catalog_revision)
    "read did not return the committed recurrence"
;;

let verify_http_boundary ~sw ~env fixture ~session ~writer ~basis ~entry =
  let connection =
    Agent_transport_http.Client.connect
      ~sw
      ~env
      ~uri:(Uri.of_string (sprintf "http://127.0.0.1:%d" (Fixture.http_port fixture)))
      ~bearer_token:(Some (Fixture.public_token fixture))
      ~notification_capacity:64
    |> checked
  in
  Exn.protect
    ~finally:(fun () -> C.Connection.close connection)
    ~f:(fun () ->
      let identity = initialize connection in
      require
        (P.Id.Principal.equal identity.principal.id (Fixture.public_principal fixture)
         && not (P.Id.Principal.equal identity.principal.id entry.Q.Definition.creator))
        "HTTP caller did not authenticate as the foreign token principal";
      require_denied (Controls.list (Controls.create connection ~session ()));
      let stolen =
        Controls.create connection ~session ~attachment:(fun () -> writer) ()
      in
      let pause =
        intent
          basis
          ~key:"recurrence-conformance:foreign"
          (Pause { id = entry.id; expected_control_revision = entry.control_revision })
      in
      require_denied (Controls.command stolen pause))
;;

let check ~sw ~env fixture ~spec =
  let clock = Eio.Stdenv.clock env in
  let connection =
    Support.Unix_driver.connect ~sw ~env ~socket_path:(Fixture.unix_socket fixture)
  in
  Exn.protect
    ~finally:(fun () -> C.Connection.close connection)
    ~f:(fun () ->
      ignore (initialize connection : P.Initialize.Response.t);
      let handle =
        H.create ~sw ~clock ~connection ~spec ~mode:Read_write ~subscribe:false ()
        |> checked
      in
      Exn.protect
        ~finally:(fun () -> H.close handle)
        ~f:(fun () ->
          ignore (H.start handle ~queue_if_limited:false |> checked : P.Session.t);
          await_started ~clock connection (H.session_id handle);
          let writer = H.recurrences handle |> checked in
          let initial = Controls.list writer |> checked |> available in
          require
            (initial.can_create && List.is_empty initial.value)
            "initial writer view did not advertise creation in an empty catalog";
          let basis = initial.basis in
          let id = P.Id.Recurrence.of_string "rec_conformance_operator" |> checked in
          let receipt = Controls.command writer (create_intent basis id) |> checked in
          require
            (W.Verb.equal receipt.verb Create)
            "unexpected recurrence command receipt";
          let session = basis.session in
          let reader = Controls.create connection ~session () in
          let rows = Controls.list reader |> checked |> available in
          (match rows.value with
           | [ row ] -> verify_row receipt rows.basis row
           | _ ->
             raise_s [%sexp "recurrence list did not return the single committed row"]);
          let found = Controls.get reader id |> checked |> available in
          verify_row receipt found.basis found.value;
          require
            (match found.value.control with
             | Paused Operator_pause -> true
             | _ -> false)
            "created recurrence was not paused";
          let status = Controls.status reader id |> checked |> available in
          verify_row receipt status.basis status.value.entry;
          require
            (List.is_empty status.value.ready && List.is_empty status.value.occurrences)
            "paused recurrence has a queued or executed occurrence";
          let history = Controls.history reader id |> checked |> available in
          verify_row receipt history.basis history.value.entry;
          require (List.is_empty history.value.occurrences) "paused recurrence executed";
          verify_http_boundary
            ~sw
            ~env
            fixture
            ~session
            ~writer:(H.attachment handle).id
            ~basis:found.basis
            ~entry:found.value;
          let unchanged = Controls.get reader id |> checked |> available in
          verify_row receipt unchanged.basis unchanged.value))
;;
