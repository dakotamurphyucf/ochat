open Core
open Agent_store_test_fixtures
module S = Agent_store
module P = Agent_protocol
module D = Document_schema
module Audit = S.Audit_store

let limit = 16384

let limits =
  S.Document_fields.limits ~max_bytes:limit
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let doc_ok result =
  Result.map_error result ~f:(fun error -> S.Store_error.Document error) |> store_ok
;;

let segment_name = S.Journal_segment.Id.filename S.Journal_segment.Id.first

let open_audit env directory =
  Audit.open_or_create ~env ~directory ~max_payload_length:limit
;;

let append store name =
  Audit.append
    store
    ~timestamp
    ~level:Info
    ~name
    ~session_id:None
    ~principal_id:None
    ~payload:(`Object [ "safe", `True ])
    ~redacted:true
;;

let request () : P.Audit.Read_request.t =
  { page = P.Page.Request.create ~limit:32 () |> protocol_ok
  ; session_id = None
  ; principal_id = None
  ; minimum_level = None
  ; name_prefix = None
  }
;;

let read store = Audit.read store (request ())
let segment_path env directory = Eio.Path.(Eio.Stdenv.fs env / directory / segment_name)

let event_bytes sequence name =
  let event : P.Audit.t =
    { sequence
    ; timestamp
    ; level = Info
    ; name
    ; session_id = None
    ; principal_id = None
    ; payload = `Null
    ; redacted = true
    }
  in
  S.Audit_event_document.create event ~limits
  |> doc_ok
  |> fun event ->
  S.Audit_event_document.to_document event ~limits |> doc_ok |> D.Document.to_string
;;

let evidence_frame ?(flags = 0) ?(previous_hash = None) event_bytes =
  let hash =
    Digestif.SHA256.(
      digest_string (Option.value previous_hash ~default:"" ^ "\000" ^ event_bytes)
      |> to_hex)
  in
  let named =
    D.Document.create
      ~limits
      ~kind:"store.audit_evidence"
      ~version:1
      ~payload:
        (`Object
            [ ( "previous_hash"
              , S.Document_fields.option_json previous_hash ~f:(fun hash -> `String hash)
              )
            ; "record_hash", `String hash
            ; "event_document_bytes", `String event_bytes
            ])
    |> doc_ok
  in
  S.Document_record.encode named ~limits ~flags
  |> Result.map_error ~f:S.Document_fields.record_error
  |> store_ok
;;

let%expect_test "complete audit semantics are verified before torn-tail repair" =
  with_temp_directory "ochat-audit-semantic-tail" (fun env root ->
    let store = open_audit env root |> store_ok in
    append store "first" |> store_ok |> ignore;
    let path = segment_path env root in
    let original = Eio.Path.load path in
    let invalid = evidence_frame (event_bytes 2L "wrong.previous") in
    let corrupt = original ^ invalid ^ "torn" in
    Eio.Path.save ~create:(`Or_truncate 0o600) path corrupt;
    assert (Result.is_error (open_audit env root));
    [%test_eq: string] corrupt (Eio.Path.load path);
    Eio.Path.save ~create:(`Or_truncate 0o600) path (original ^ "torn");
    let reopened = open_audit env root |> store_ok in
    [%test_eq: string] original (Eio.Path.load path);
    let next = append reopened "second" |> store_ok in
    [%test_eq: int64] 2L next.sequence;
    print_endline
      "complete chain error left all bytes unchanged; valid tail repaired before next \
       sequence");
  [%expect
    {|complete chain error left all bytes unchanged; valid tail repaired before next sequence|}]
;;

let%expect_test "audit unsupported frame flags precede document parsing" =
  with_temp_directory "ochat-audit-frame-flags" (fun env root ->
    open_audit env root |> store_ok |> ignore;
    let bytes =
      S.Frame.encode ~max_payload_length:limit ~flags:1 "not JSON"
      |> Result.map_error ~f:(fun error ->
        S.Store_error.Corrupt (Sexp.to_string_hum (S.Frame.sexp_of_error error)))
      |> store_ok
    in
    let path = segment_path env root in
    Eio.Path.save ~create:(`Or_truncate 0o600) path bytes;
    (match open_audit env root with
     | Error (S.Store_error.Corrupt message) ->
       assert (String.is_substring message ~substring:"flags")
     | Ok _ | Error _ -> assert false);
    [%test_eq: string] bytes (Eio.Path.load path);
    print_endline "frame flags rejected before malformed payload with no repair");
  [%expect {|frame flags rejected before malformed payload with no repair|}]
;;

module Fault = struct
  type boundary =
    | Before_write
    | Partial_write
    | After_sync

  type failure =
    | Io
    | Timeout
    | Unexpected

  type t =
    { boundary : boundary
    ; failure : failure
    ; secondary_timeout : bool
    }

  exception Primary_failure

  let raise_failure = function
    | Io -> raise (Core_unix.Unix_error (EIO, "audit publication fault", "segment"))
    | Timeout -> raise Eio.Time.Timeout
    | Unexpected -> raise Primary_failure
  ;;

  let wrap_file (Eio.Resource.T (file, handler)) ~armed ~secondary =
    let module Original = (val Eio.Resource.get handler Eio.File.Pi.Write) in
    let reached selected =
      armed := None;
      secondary := selected.secondary_timeout;
      raise_failure selected.failure
    in
    let module File = struct
      include Original

      let single_write file buffers =
        match !armed with
        | Some ({ boundary = Before_write; _ } as selected) -> reached selected
        | Some ({ boundary = Partial_write; _ } as selected) ->
          let buffer = Cstruct.concat buffers in
          ignore
            (Original.single_write
               file
               [ Cstruct.sub buffer 0 (Int.min 8 (Cstruct.length buffer)) ]);
          reached selected
        | Some _ | None -> Original.single_write file buffers
      ;;

      let copy file ~src = Eio.Flow.Pi.simple_copy ~single_write file ~src

      let sync file =
        Original.sync file;
        match !armed with
        | Some ({ boundary = After_sync; _ } as selected) -> reached selected
        | Some _ | None -> ()
      ;;
    end
    in
    Eio.Resource.T (file, Eio.File.Pi.rw (module File))
  ;;

  let wrap env ~armed ~secondary =
    let directory, path = Eio.Stdenv.fs env in
    let native_directory = directory in
    let (Eio.Resource.T (native, handler)) = directory in
    let module Original = (val Eio.Resource.get handler Eio.Fs.Pi.Dir) in
    let module Directory = struct
      include Original

      let rename directory source _destination target =
        Original.rename directory source native_directory target
      ;;

      let open_out directory ~sw ~append ~create path =
        let file = Original.open_out directory ~sw ~append ~create path in
        if String.is_suffix path ~suffix:segment_name
        then wrap_file file ~armed ~secondary
        else file
      ;;

      let open_in directory ~sw path =
        if !secondary && String.is_suffix path ~suffix:segment_name
        then (
          secondary := false;
          raise Eio.Time.Timeout);
        Original.open_in directory ~sw path
      ;;
    end
    in
    let fs =
      ( Eio.Resource.T
          (native, Eio.Resource.handler [ H (Eio.Fs.Pi.Dir, (module Directory)) ])
      , path )
    in
    object
      method fs = fs
      method cwd = env#cwd
      method stdin = env#stdin
      method stdout = env#stdout
      method stderr = env#stderr
      method net = env#net
      method domain_mgr = env#domain_mgr
      method process_mgr = env#process_mgr
      method clock = env#clock
      method mono_clock = env#mono_clock
      method secure_random = env#secure_random
      method debug = env#debug
      method backend_id = env#backend_id
    end
  ;;
end

let%expect_test "audit publication faults reconcile exact authority without poisoning" =
  List.iter
    [ "before-write", Fault.Before_write, Fault.Io, 0
    ; "partial-cancellation", Partial_write, Timeout, 0
    ; "after-sync", After_sync, Io, 1
    ; "after-sync-cancellation", After_sync, Timeout, 1
    ; "after-sync-exception", After_sync, Unexpected, 1
    ]
    ~f:(fun (name, boundary, failure, count) ->
      with_temp_directory "ochat-audit-publication" (fun native_env root ->
        let armed = ref None
        and secondary = ref false in
        let env = Fault.wrap native_env ~armed ~secondary in
        let store = open_audit env root |> store_ok in
        armed := Some Fault.{ boundary; failure; secondary_timeout = false };
        (try
           match append store "uncertain" with
           | Error (S.Store_error.Io _) ->
             (match failure with
              | Io -> ()
              | Timeout | Unexpected -> assert false)
           | Ok _ | Error _ -> assert false
         with
         | Eio.Time.Timeout ->
           (match failure with
            | Timeout -> ()
            | Io | Unexpected -> assert false)
         | Fault.Primary_failure ->
           (match failure with
            | Unexpected -> ()
            | Io | Timeout -> assert false));
        assert (Option.is_none !armed);
        [%test_eq: int] count (List.length (read store |> store_ok).items);
        let original_bytes = Eio.Path.load (segment_path native_env root) in
        let reopened = open_audit native_env root |> store_ok in
        [%test_eq: int] count (List.length (read reopened |> store_ok).items);
        [%test_eq: string] original_bytes (Eio.Path.load (segment_path native_env root));
        let next = append store "next" |> store_ok in
        [%test_eq: int64] (Int64.of_int (count + 1)) next.sequence;
        print_endline (name ^ ": verified authority and next sequence usable")));
  [%expect
    {|
    before-write: verified authority and next sequence usable
    partial-cancellation: verified authority and next sequence usable
    after-sync: verified authority and next sequence usable
    after-sync-cancellation: verified authority and next sequence usable
    after-sync-exception: verified authority and next sequence usable
  |}]
;;

let%expect_test "audit secondary recovery cancellation cannot replace primary failure" =
  with_temp_directory "ochat-audit-secondary-failure" (fun native_env root ->
    let armed = ref None
    and secondary = ref false in
    let env = Fault.wrap native_env ~armed ~secondary in
    let store = open_audit env root |> store_ok in
    armed := Some Fault.{ boundary = After_sync; failure = Io; secondary_timeout = true };
    (match append store "installed" with
     | Error (S.Store_error.Io _) -> ()
     | Ok _ | Error _ -> assert false);
    assert (Option.is_none !armed);
    assert (not !secondary);
    assert (Result.is_error (read store));
    assert (Result.is_error (append store "unavailable"));
    let original = Eio.Path.load (segment_path native_env root) in
    let reopened = open_audit native_env root |> store_ok in
    [%test_eq: int] 1 (List.length (read reopened |> store_ok).items);
    [%test_eq: int64] 2L (append reopened "next" |> store_ok).sequence;
    assert (
      String.is_prefix (Eio.Path.load (segment_path native_env root)) ~prefix:original);
    print_endline
      "primary IO retained; live owner unavailable; exact installed chain reopens");
  [%expect {|primary IO retained; live owner unavailable; exact installed chain reopens|}]
;;

let%expect_test "semantic audit failure with valid frame and hash prevents tail repair" =
  with_temp_directory "ochat-audit-event-tail" (fun env root ->
    open_audit env root |> store_ok |> ignore;
    let valid = D.Document.decode ~limits (event_bytes 1L "valid") |> doc_ok in
    let invalid_json =
      match D.Document.json valid with
      | `Object fields ->
        `Object
          (List.map fields ~f:(fun (key, value) ->
             ( key
             , if String.equal key "payload"
               then (
                 match value with
                 | `Object payload ->
                   `Object
                     (List.map payload ~f:(fun (name, value) ->
                        name, if String.equal name "name" then `String "" else value))
                 | _ -> assert false)
               else value )))
      | _ -> assert false
    in
    let malformed_event =
      D.Document.inspect ~limits invalid_json |> doc_ok |> D.Document.to_string
    in
    let original = evidence_frame malformed_event ^ "torn" in
    let path = segment_path env root in
    Eio.Path.save ~create:(`Or_truncate 0o600) path original;
    assert (Result.is_error (open_audit env root));
    [%test_eq: string] original (Eio.Path.load path);
    print_endline
      "valid raw chain hash and framing cannot authorize repair of invalid event \
       semantics");
  [%expect
    {|valid raw chain hash and framing cannot authorize repair of invalid event semantics|}]
;;

let%expect_test "audit unexpected primary exception survives secondary Timeout" =
  with_temp_directory "ochat-audit-primary-exception" (fun native_env root ->
    let armed = ref None
    and secondary = ref false in
    let env = Fault.wrap native_env ~armed ~secondary in
    let store = open_audit env root |> store_ok in
    armed
    := Some
         Fault.{ boundary = After_sync; failure = Unexpected; secondary_timeout = true };
    (try
       ignore (append store "installed");
       assert false
     with
     | Fault.Primary_failure -> ());
    assert (Option.is_none !armed);
    assert (not !secondary);
    assert (Result.is_error (read store));
    assert (Result.is_error (append store "unavailable"));
    let reopened = open_audit native_env root |> store_ok in
    [%test_eq: int] 1 (List.length (read reopened |> store_ok).items);
    [%test_eq: int64] 2L (append reopened "next" |> store_ok).sequence;
    print_endline
      "primary exception retained; secondary Timeout leaves live owner unavailable and \
       reopen honest");
  [%expect
    {|primary exception retained; secondary Timeout leaves live owner unavailable and reopen honest|}]
;;

let%expect_test "audit prevalidation has no journal effects and no owner poisoning" =
  with_temp_directory "ochat-audit-prevalidation" (fun env root ->
    let store = open_audit env root |> store_ok in
    let path = segment_path env root in
    let original = Eio.Path.load path in
    assert (Result.is_error (append store ""));
    let oversized =
      Audit.append
        store
        ~timestamp
        ~level:Info
        ~name:"too-large"
        ~session_id:None
        ~principal_id:None
        ~payload:(`String (String.make limit 'x'))
        ~redacted:true
    in
    assert (Result.is_error oversized);
    [%test_eq: string] original (Eio.Path.load path);
    [%test_eq: int] 0 (List.length (read store |> store_ok).items);
    [%test_eq: int64] 1L (append store "valid" |> store_ok).sequence;
    print_endline
      "invalid name and complete envelope limit rejected before authority; valid \
       sequence remains first");
  [%expect
    {|invalid name and complete envelope limit rejected before authority; valid sequence remains first|}]
;;

let%expect_test
    "original named audit byte evidence survives append and protocol disclosure"
  =
  with_temp_directory "ochat-audit-original-evidence" (fun env root ->
    open_audit env root |> store_ok |> ignore;
    let event =
      " \n"
      ^ {|{"format":"ochat.document","schema_version":1,"kind":"store.audit_event","future_event_envelope":null,"payload":{"sequence":"1","timestamp":"2026-08-15T12:00:00Z","level":"info","name":"original.private","payload":null,"redacted":true,"future_private":1e+00}}|}
      ^ "\n "
    in
    let original = evidence_frame event in
    let path = segment_path env root in
    Eio.Path.save ~create:(`Or_truncate 0o600) path original;
    let store = open_audit env root |> store_ok in
    let page = read store |> store_ok in
    [%test_eq: int] 1 (List.length page.items);
    let projected = P.Audit.to_json (List.hd_exn page.items) in
    (match D.Json.field projected ~name:"future_private" with
     | Absent -> ()
     | Null | Value _ -> assert false);
    [%test_eq: int64] 2L (append store "second" |> store_ok).sequence;
    let appended = Eio.Path.load path in
    assert (String.is_prefix appended ~prefix:original);
    let reopened = open_audit env root |> store_ok in
    [%test_eq: int] 2 (List.length (read reopened |> store_ok).items);
    [%test_eq: string] appended (Eio.Path.load path);
    print_endline
      "original frame prefix immutable; private unknown fields withheld; extended raw \
       chain reopens");
  [%expect
    {|original frame prefix immutable; private unknown fields withheld; extended raw chain reopens|}]
;;

let%expect_test "audit startup scan and tail repair cancellation propagate unchanged" =
  with_temp_directory "ochat-audit-startup-cancellation" (fun native_env root ->
    let path = segment_path native_env root in
    open_audit native_env root |> store_ok |> ignore;
    let armed = ref None
    and secondary = ref true in
    let env = Fault.wrap native_env ~armed ~secondary in
    (try
       ignore (open_audit env root);
       assert false
     with
     | Eio.Time.Timeout -> ());
    assert (not !secondary);
    [%test_eq: string] "" (Eio.Path.load path);
    Eio.Path.save ~create:(`Or_truncate 0o600) path "torn";
    armed
    := Some Fault.{ boundary = After_sync; failure = Timeout; secondary_timeout = false };
    (try
       ignore (open_audit env root);
       assert false
     with
     | Eio.Time.Timeout -> ());
    assert (Option.is_none !armed);
    [%test_eq: string] "" (Eio.Path.load path);
    let recovered = open_audit native_env root |> store_ok in
    [%test_eq: int64] 1L (append recovered "first" |> store_ok).sequence;
    print_endline
      "scan Timeout unchanged; repaired-tail sync Timeout unchanged; exact authority \
       reopens");
  [%expect
    {|scan Timeout unchanged; repaired-tail sync Timeout unchanged; exact authority reopens|}]
;;
