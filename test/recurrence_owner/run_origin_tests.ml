open! Core
open Support
module O = A.Recurrence_run_origin
module Index = A.Run_state
module State = A.Session_state_document
module Delta = A.Session_delta_document
module Store = Agent_store

let origin_field index = field (Index.to_jsonaf index) "recurrence_origins"

let index_document state =
  State.authored state
  |> fun value -> State.encode value ~limits:document_limits |> document_ok
;;

let inspect json = D.Document.inspect ~limits:document_limits json |> document_ok
let marker = P.Session_storage_semantics.recurrence_run_origin

let check ~initial ~admitted ~origin =
  let initial : A.Session_state.t = initial in
  let run = A.Run_admission.run admitted in
  let receipt = A.Run_admission.receipt admitted in
  let ordinary = A.Run_admission.index admitted in
  (* Same actual installed source as the producer; no live host proof is issued. *)
  let before =
    Index.replace_installation
      Index.empty
      ~installation:(Index.installation ordinary)
      ~retired_runs:[]
    |> protocol_ok
  in
  let before_raw = Index.to_jsonaf before in
  let roundtrip index = Index.to_jsonaf index |> Index.of_jsonaf |> protocol_ok in
  assert (Index.equal before (roundtrip before));
  assert (
    D.Json.equal before_raw (Index.of_jsonaf before_raw |> protocol_ok |> Index.to_jsonaf));
  let null_raw = set before_raw "recurrence_origins" `Null in
  let null = Index.of_jsonaf null_raw |> protocol_ok in
  assert (not (Index.equal before null));
  assert (Index.equal null (roundtrip null));
  assert (
    D.Json.equal null_raw (Index.of_jsonaf null_raw |> protocol_ok |> Index.to_jsonaf));
  let empty =
    set before_raw "recurrence_origins" (`Array []) |> Index.of_jsonaf |> protocol_ok
  in
  assert (
    List.mem
      (D.Document.required_semantics
         (index_document { initial with run_state = Some empty }))
      marker
      ~equal:String.equal);
  let index =
    Index.commit_recurrence_admission before ~run ~receipt ~origin |> protocol_ok
  in
  Index.validate_transition ~previous:before index |> protocol_ok;
  let duplicate =
    Index.commit_recurrence_admission index ~run ~receipt ~origin |> protocol_ok
  in
  assert (D.Json.equal (Index.to_jsonaf duplicate) (Index.to_jsonaf index));
  let changed_run =
    P.Run.to_json run
    |> fun json ->
    set json "revision" (`String "1")
    |> fun json ->
    set json "lifecycle" (P.Run.Lifecycle.to_json Active) |> P.Run.of_json |> protocol_ok
  in
  assert (
    Result.is_error
      (Index.commit_recurrence_admission index ~run:changed_run ~receipt ~origin));
  assert (
    Result.is_error (Index.commit_recurrence_admission ordinary ~run ~receipt ~origin));
  assert (Result.is_error (Index.validate_transition ~previous:ordinary index));
  let raw = Index.to_jsonaf index in
  let structural =
    append
      raw
      "structural_unknown"
      (`Array (List.init 100_000 ~f:(fun _ -> `Object [ "x", `Number "0" ])))
  in
  assert (String.length (Jsonaf.to_string structural) < P.Run_limits.max_document_bytes);
  assert (Result.is_error (Index.of_jsonaf structural));
  let rows =
    match origin_field index with
    | `Array rows -> rows
    | _ -> assert false
  in
  let retained =
    match rows with
    | [ row ] -> append row "future_wrapper" (`Number "1.00e+2")
    | _ -> assert false
  in
  let with_origin_unknown =
    match rows with
    | [ row ] ->
      set
        row
        "origin"
        (O.to_json origin |> fun json -> append json "future_origin" (`Number "2.00e1"))
    | _ -> assert false
  in
  (* Each carrier differs only in retained metadata. Equality must inspect both
     the outer row and its origin, rather than only the known Run/admission. *)
  List.iter [ retained; with_origin_unknown ] ~f:(fun row ->
    let annotated =
      set raw "recurrence_origins" (`Array [ row ]) |> Index.of_jsonaf |> protocol_ok
    in
    assert (
      Option.equal P.Run.equal (Index.find index run.id) (Index.find annotated run.id));
    assert (not (Index.equal index annotated));
    assert (Index.equal annotated (roundtrip annotated)));
  let retained =
    set
      retained
      "origin"
      (O.to_json origin |> fun json -> append json "future_origin" (`Number "2.00e1"))
  in
  let index =
    set raw "recurrence_origins" (`Array [ retained ]) |> Index.of_jsonaf |> protocol_ok
  in
  let origin = Index.recurrence_origin index run.id |> Option.value_exn in
  let retired =
    A.Run_retirement.recover index ~session_revision:2L ~now:timestamp |> protocol_ok
  in
  Index.validate_transition ~previous:index retired |> protocol_ok;
  assert (D.Json.equal (origin_field index) (origin_field retired));
  let replaced =
    A.Run_retirement.replace index ~change:Remove ~session_revision:2L ~now:timestamp
    |> protocol_ok
  in
  assert (D.Json.equal (origin_field index) (origin_field replaced));
  let erased =
    set (Index.to_jsonaf retired) "recurrence_origins" (`Array [])
    |> Index.of_jsonaf
    |> protocol_ok
  in
  assert (Result.is_error (Index.validate_transition ~previous:retired erased));
  let altered =
    set
      retained
      "run_id"
      (P.Id.Run.to_json (P.Id.Run.of_string "run_other" |> protocol_ok))
  in
  assert (
    Result.is_error (Index.of_jsonaf (set raw "recurrence_origins" (`Array [ altered ]))));
  let state = { initial with run_state = Some index } in
  let document = index_document state in
  assert (List.mem (D.Document.required_semantics document) marker ~equal:String.equal);
  let before_document =
    index_document { initial with run_state = Some before }
    |> State.decode ~limits:document_limits
    |> document_ok
  in
  let adopted =
    State.adopt before_document ~limits:document_limits (State.authored state)
    |> document_ok
  in
  assert (
    D.Json.equal
      (D.Document.json document)
      (State.encode adopted ~limits:document_limits |> document_ok |> D.Document.json));
  let restored = State.decode ~limits:document_limits document |> document_ok in
  assert (
    D.Json.equal
      (D.Document.json document)
      (State.encode restored ~limits:document_limits |> document_ok |> D.Document.json));
  let old =
    State.authored { initial with run_state = Some ordinary }
    |> fun state ->
    State.encode state ~limits:document_limits
    |> document_ok
    |> State.decode ~limits:document_limits
    |> document_ok
  in
  assert (
    Result.is_error (State.encode (State.with_value old state) ~limits:document_limits));
  assert (Result.is_error (State.adopt old ~limits:document_limits (State.authored state)));
  assert (Result.is_error (State.decode_after old ~limits:document_limits document));
  let previous = State.decode ~limits:document_limits document |> document_ok in
  assert (
    Result.is_error
      (State.encode
         (State.with_value previous { initial with run_state = Some erased })
         ~limits:document_limits));
  (* Both existing marker and new marker survive; no queue data is fabricated. *)
  let markers = [ P.Schedule_provenance.persistence_semantics; marker ] in
  let document =
    D.Document.json document
    |> fun json ->
    set json "required_semantics" (`Array (List.map markers ~f:(fun s -> `String s)))
    |> inspect
  in
  let restored = State.decode ~limits:document_limits document |> document_ok in
  assert (
    List.equal
      String.equal
      markers
      (D.Document.required_semantics
         (State.encode restored ~limits:document_limits |> document_ok)));
  let unmarked =
    D.Document.json document
    |> fun json -> set json "required_semantics" (`Array []) |> inspect
  in
  assert (Result.is_error (State.decode ~limits:document_limits unmarked));
  let old_codec =
    D.Domain_codec.create
      ~limits:document_limits
      ~kind:"session.state"
      ~version:State.current_version
      ~shape:D.Shape.value
      ~supported_semantics:[ P.Schedule_provenance.persistence_semantics ]
      ~decode:Result.return
      ~encode:Result.return
    |> document_ok
  in
  assert (Result.is_error (D.Domain_codec.decode old_codec document));
  let delta value =
    Delta.create value ~limits:document_limits ~state_document:State.authored
    |> document_ok
    |> Delta.document
  in
  List.iter
    [ delta (Run_state_changed index); delta (Created state) ]
    ~f:(fun delta ->
      assert (List.mem (D.Document.required_semantics delta) marker ~equal:String.equal);
      Delta.decode ~limits:document_limits delta |> document_ok |> ignore;
      let transaction =
        Store.Transaction.create
          ~limits:document_limits
          ~session_id:state.identity.session_id
          ~generation:state.identity.generation
          ~transaction_sequence:1L
          ~previous_transaction_hash:None
          ~session_revision:1L
          ~first_event_sequence:None
          ~last_event_sequence:None
          ~accepted_at_ns:0L
          ~command_audit:None
          ~delta
          ~durable_events:[]
        |> store_ok
      in
      let decoded =
        Store.Transaction.decode (Store.Transaction.encode transaction) |> store_ok
      in
      assert (D.Json.equal (D.Document.json decoded.delta) (D.Document.json delta));
      let outer =
        Store.Transaction.stored transaction
        |> Store.Transaction.Stored.record
        |> Store.Document_record.document
      in
      let outer =
        D.Document.json outer
        |> fun json ->
        set json "required_semantics" (`Array [ `String marker ]) |> inspect
      in
      let record =
        Store.Document_record.of_document outer ~limits:document_limits
        |> function
        | Ok r -> r
        | Error _ -> assert false
      in
      assert (Result.is_error (Store.Transaction.Stored.of_record record)));
  let snapshot =
    Store.Snapshot.create
      ~limits:document_limits
      ~session_id:state.identity.session_id
      ~transaction_sequence:0L
      ~transaction_hash:None
      ~event_sequence:0L
      ~created_at:state.identity.updated_at
      ~prompt_artifact:(P.Id.Prompt_revision.to_string state.spec.prompt_revision_id)
      ~workspace_identity:state.spec.workspace_instance.conflict_domain
      ~payload:document
    |> store_ok
  in
  let snapshot_document =
    Store.Snapshot.stored snapshot
    |> Store.Snapshot.Stored.record
    |> Store.Document_record.document
  in
  let frame =
    Store.Document_record.encode snapshot_document ~limits:document_limits ~flags:0
    |> function
    | Ok s -> s
    | Error _ -> assert false
  in
  let decoded =
    Store.Snapshot.decode_file
      ~max_payload_length:(D.Limits.max_bytes document_limits)
      frame
    |> store_ok
  in
  assert (D.Json.equal (D.Document.json decoded.payload) (D.Document.json document));
  let outer =
    D.Document.json snapshot_document
    |> fun json -> set json "required_semantics" (`Array [ `String marker ]) |> inspect
  in
  let record =
    Store.Document_record.of_document outer ~limits:document_limits
    |> function
    | Ok r -> r
    | Error _ -> assert false
  in
  assert (Result.is_error (Store.Snapshot.Stored.of_record record));
  (* Real recovery growth includes the full immutable table, never an 8KiB guess. *)
  let capacity =
    State.terminal_capacity restored ~document ~limits:document_limits |> document_ok
  in
  let measure doc =
    A.Capacity_growth.measure (D.Document.json doc) ~limits:document_limits |> protocol_ok
  in
  let current = measure document in
  (* Use the independent real retirement producer, not the storage projections
     from which the reserve itself was calculated. *)
  let recovered =
    A.Run_retirement.recover
      index
      ~session_revision:(Int64.succ state.counters.revision)
      ~now:timestamp
    |> protocol_ok
  in
  let transition =
    A.Session_transition.apply
      state
      ~delta:(Run_state_changed recovered)
      ~payloads:[]
      ~now:timestamp
    |> protocol_ok
  in
  let admitted =
    A.Session_document_transition.admit_encoded
      restored
      ~delta:transition.delta
      ~next:transition.state
      ~limits:document_limits
    |> document_ok
  in
  let future = State.Admitted.document admitted in
  assert (
    D.Json.equal
      (field (D.Document.payload future) "run_state"
       |> fun raw -> field raw "recurrence_origins")
      (origin_field index));
  let growth = A.Capacity_growth.positive_difference (measure future) ~previous:current in
  let reserved = A.Run_terminal_document.growth capacity in
  assert (
    A.Capacity_growth.encoded_bytes growth <= A.Capacity_growth.encoded_bytes reserved);
  assert (A.Capacity_growth.fields growth <= A.Capacity_growth.fields reserved);
  assert (A.Capacity_growth.nodes growth <= A.Capacity_growth.nodes reserved);
  assert (
    List.exists transition.events ~f:(fun event ->
      P.Event.Durable.equal_kind event.P.Event.Durable.kind Session_updated));
  let durable_events =
    List.map transition.events ~f:(fun event ->
      A.Durable_event_document.create event ~limits:document_limits
      |> document_ok
      |> A.Durable_event_document.document)
  in
  (* The actual recovery delta and emitted public summary also fit the real
     transaction encoder, with its maximal metadata widths and outer overhead. *)
  let transaction =
    Store.Transaction.Capacity.document
      ~limits:document_limits
      ~session_id:state.identity.session_id
      ~generation:state.identity.generation
      ~delta:(delta transition.delta)
      ~durable_events
    |> document_ok
  in
  D.Document.validate transaction ~limits:document_limits |> document_ok;
  (* Immutable table data remains exactly recoverable for compact references. *)
  assert (O.equal origin (Index.recurrence_origin retired run.id |> Option.value_exn))
;;
