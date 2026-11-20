open! Core
open Fixtures
module A = Agent_session
module P = Agent_protocol
module D = Document_schema
module State = A.Session_state
module Codec = A.Session_state_document
module Ledger = A.Recovery_window_ledger
module Scope = P.Recovery_window.Scope
module Citation = P.Recovery_window.Citation

let window_id name = P.Id.Context_window.of_string ("win_" ^ name) |> protocol_ok
let server id = P.Id.Server.of_string id |> protocol_ok
let owner = P.Session_ref.create ~server_id:(server "srv_local") ~session_id

let entry sequence =
  let id =
    History_entry.Id.create ~namespace:(P.Id.Session.to_string session_id) ~sequence
    |> Result.ok_or_failwith
  in
  A.History_codec.user_text ~id (Int.to_string sequence) |> A.History_codec.to_protocol
;;

let basis ~owner ~generation revision =
  Ledger.Source.basis ~owner ~generation ~revision |> protocol_ok
;;

let scope ~generation name =
  Scope.create ~session:owner ~generation ~window:(window_id name) |> protocol_ok
;;

let member
      scope
      ~source
      ~ordinal
      ?(provenance = Ledger.Member.Observed)
      (entry : P.History.entry)
  =
  let citation =
    Citation.create
      scope
      ~occurrence:entry.id
      ~version:(P.Recovery_item_version.canonical entry.content_revision)
    |> protocol_ok
  in
  Ledger.Member.create citation ~source ~source_ordinal:ordinal ~provenance
;;

let with_ledger f =
  with_actor_workspace (fun _ workspace_instance ->
    let initial =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
    in
    let canonical_history = [ entry 2; entry 1 ] in
    let previous : State.t =
      { initial with
        conversation =
          { initial.conversation with
            canonical_history
          ; next_history_sequence = 4L
          ; reserved_history_through = 4L
          }
      ; counters = { initial.counters with revision = 1L }
      }
    in
    let archive =
      A.Compaction_archive.reference
        (Codec.authored previous)
        ~limits:document_limits
        operation_id
      |> protocol_ok
    in
    let post_entry = entry 3 in
    let current : State.t =
      { previous with
        conversation =
          { previous.conversation with
            canonical_history = [ post_entry ]
          ; compaction_archives = [ archive ]
          }
      ; counters = { previous.counters with revision = 2L }
      }
    in
    let first = scope ~generation:0 "first" in
    let next = scope ~generation:0 "next" in
    let digest = Chatml_source.Digest.of_hex archive.sha256 |> Or_error.ok_exn in
    let archived =
      Ledger.Source.summary_archive
        (basis ~owner ~generation:0 1L)
        ~operation:operation_id
        ~digest
    in
    let live = Ledger.Source.Live (basis ~owner ~generation:0 2L) in
    let root_members =
      List.mapi canonical_history ~f:(fun ordinal entry ->
        member first ~source:archived ~ordinal entry |> protocol_ok)
    in
    let root =
      Ledger.Window.create first ~origin:Root ~state:Sealed root_members |> protocol_ok
    in
    let current_member =
      member
        next
        ~source:live
        ~ordinal:0
        ~provenance:(Derived_summary operation_id)
        post_entry
      |> protocol_ok
    in
    let rollover_id = P.Id.Operation.of_string "op_rollover" |> protocol_ok in
    let origin =
      Ledger.Window.Successor { previous = window_id "first"; operation = rollover_id }
    in
    let open_window members = Ledger.Window.create next ~origin ~state:Open members in
    let active = open_window [ current_member ] |> protocol_ok in
    let ledger = Ledger.create ~owner [ root; active ] |> protocol_ok in
    f initial current ledger root active)
;;

let update_object json ~field ~f =
  match json with
  | `Object fields ->
    `Object
      (List.map fields ~f:(fun (name, value) ->
         name, if String.equal name field then f value else value))
  | _ -> failwith "expected object"
;;

let extend_object json ~field value =
  match json with
  | `Object fields -> `Object (fields @ [ field, value ])
  | _ -> failwith "expected object"
;;

let update_first value ~f =
  match value with
  | `Array (head :: tail) -> `Array (f head :: tail)
  | _ -> failwith "expected nonempty array"
;;

let decode document = Codec.decode ~limits:document_limits document
let inspect json = D.Document.inspect ~limits:document_limits json |> document_ok

let%expect_test
    "window ledger keeps ordered archived and live members across state admission"
  =
  with_ledger (fun initial current ledger _ _ ->
    let legacy = state_document initial in
    let legacy_read = decode legacy |> document_ok in
    let legacy_encoded =
      Codec.encode legacy_read ~limits:document_limits |> document_ok
    in
    [%test_eq: Sexp.t]
      [%sexp Absent]
      (match
         D.Json.field (D.Document.payload legacy_encoded) ~name:"recovery_windows"
       with
       | Absent -> [%sexp Absent]
       | Null | Value _ -> [%sexp Present]);
    let semantics = P.Session_storage_semantics.recovery_windows in
    assert (
      not
        (List.mem
           (D.Document.required_semantics legacy_encoded)
           semantics
           ~equal:String.equal));
    let enabled = state_document { current with recovery_windows = Some ledger } in
    assert (List.mem (D.Document.required_semantics enabled) semantics ~equal:String.equal);
    let parsed = decode enabled |> document_ok |> Codec.value in
    let restored = Option.value_exn parsed.recovery_windows in
    List.iter (Ledger.windows restored) ~f:(fun window ->
      let members =
        List.init (Ledger.Window.length window) ~f:(fun ordinal ->
          let member = Ledger.Window.at window ~ordinal |> Option.value_exn in
          let id = Citation.occurrence (Ledger.Member.citation member) in
          History_entry.Id.sequence id)
      in
      print_s
        [%sexp (Ledger.Window.state window : Ledger.Window.state), (members : int list)]);
    assert (
      Result.is_error
        (Codec.encode
           (Codec.authored
              { parsed with
                conversation = { parsed.conversation with compaction_archives = [] }
              })
           ~limits:document_limits));
    let altered_archive =
      List.map parsed.conversation.compaction_archives ~f:(fun archive ->
        { archive with sha256 = String.make 64 '0' })
    in
    assert (
      Result.is_error
        (Codec.encode
           (Codec.authored
              { parsed with
                conversation =
                  { parsed.conversation with compaction_archives = altered_archive }
              })
           ~limits:document_limits));
    assert (
      Result.is_error
        (Codec.encode
           (Codec.authored
              { parsed with
                conversation =
                  { parsed.conversation with canonical_history = [ entry 1 ] }
              })
           ~limits:document_limits));
    print_endline "legacy absence preserved; mismatched live and archive anchors rejected");
  [%expect
    {|
    (Sealed (2 1))
    (Open (3))
    legacy absence preserved; mismatched live and archive anchors rejected
    |}]
;;

let%expect_test "window ledger carrier preserves unknown member and source fields" =
  with_ledger (fun _ current ledger root _ ->
    let document = state_document { current with recovery_windows = Some ledger } in
    let future = `Object [ "revision", `Number "1.00"; "inactive", `Null ] in
    let extended =
      update_object (D.Document.json document) ~field:"payload" ~f:(fun payload ->
        update_object payload ~field:"recovery_windows" ~f:(fun ledger ->
          extend_object ledger ~field:"future_ledger" future
          |> update_object
               ~field:"windows"
               ~f:
                 (update_first ~f:(fun window ->
                    extend_object window ~field:"future_window" future
                    |> update_object
                         ~field:"members"
                         ~f:
                           (update_first ~f:(fun member ->
                              extend_object member ~field:"future_member" future
                              |> update_object ~field:"source" ~f:(fun source ->
                                extend_object source ~field:"future_source" future)))))))
      |> inspect
    in
    let read = decode extended |> document_ok in
    let encoded = Codec.encode read ~limits:document_limits |> document_ok in
    assert (D.Json.equal (D.Document.json extended) (D.Document.json encoded));
    let root_scope = P.Recovery_window.scope (Ledger.Window.manifest root) in
    let source = Ledger.Source.Live (basis ~owner ~generation:0 2L) in
    let history = [ entry 2; entry 1 ] in
    let live_members =
      List.mapi history ~f:(fun ordinal entry ->
        member root_scope ~source ~ordinal entry |> protocol_ok)
    in
    let changed_window =
      Ledger.Window.create root_scope ~origin:Root ~state:Open live_members |> protocol_ok
    in
    let replacement =
      { current with
        conversation = { current.conversation with canonical_history = history }
      ; recovery_windows = Some (Ledger.create ~owner [ changed_window ] |> protocol_ok)
      }
    in
    (match Codec.encode (Codec.with_value read replacement) ~limits:document_limits with
     | Error (D.Error.Extension_conflict path)
       when String.equal (List.last_exn path) "source" -> ()
     | Ok _ | Error _ ->
       failwith "unknown source data migrated to a different source kind");
    let no_marker =
      update_object (D.Document.json extended) ~field:"required_semantics" ~f:(function
        | `Array values ->
          `Array
            (List.filter values ~f:(function
               | `String value ->
                 not (String.equal value P.Session_storage_semantics.recovery_windows)
               | _ -> true))
        | _ -> assert false)
      |> inspect
    in
    assert (Result.is_error (decode no_marker));
    let oversized =
      update_object (D.Document.json document) ~field:"payload" ~f:(fun payload ->
        update_object payload ~field:"recovery_windows" ~f:(fun ledger ->
          extend_object
            ledger
            ~field:"future_large"
            (`String (String.make (2 * 1024 * 1024) 'x'))))
      |> inspect
    in
    assert (Result.is_error (decode oversized));
    print_endline
      "unknown ledger fields survive same-kind encoding and fence source-kind changes";
    print_endline "missing required semantics and oversized extensions rejected");
  [%expect
    {|
    unknown ledger fields survive same-kind encoding and fence source-kind changes
    missing required semantics and oversized extensions rejected
    |}]
;;

let%expect_test "window ledger rejects foreign sources, duplicates and broken lineage" =
  with_ledger (fun _ _ ledger root active ->
    let first = Ledger.Window.at root ~ordinal:0 |> Option.value_exn in
    let scope = P.Recovery_window.scope (Ledger.Window.manifest root) in
    assert (
      Result.is_error
        (Ledger.Window.create scope ~origin:Root ~state:Sealed [ first; first ]));
    assert (Result.is_error (Ledger.create ~owner [ active ]));
    assert (Result.is_error (Ledger.create ~owner [ root; root; active ]));
    assert (Result.is_error (Ledger.create ~owner (List.init 65 ~f:(fun _ -> active))));
    assert (
      Result.is_error
        (Ledger.Window.create
           scope
           ~origin:Root
           ~state:Sealed
           (List.init 8193 ~f:(fun _ -> first))));
    let other = P.Session_ref.create ~server_id:(server "srv_other") ~session_id in
    let foreign = Ledger.Source.Live (basis ~owner:other ~generation:0 2L) in
    assert (
      Result.is_error
        (Ledger.Member.create
           (Ledger.Member.citation first)
           ~source:foreign
           ~source_ordinal:0
           ~provenance:Observed));
    let wrong_generation = Ledger.Source.Live (basis ~owner ~generation:1 2L) in
    assert (
      Result.is_error
        (Ledger.Member.create
           (Ledger.Member.citation first)
           ~source:wrong_generation
           ~source_ordinal:0
           ~provenance:Observed));
    let swapped =
      update_object (Ledger.to_jsonaf ledger) ~field:"windows" ~f:(function
        | `Array windows -> `Array (List.rev windows)
        | _ -> assert false)
    in
    assert (Result.is_error (Ledger.of_jsonaf swapped));
    let source = Ledger.Member.source first in
    let second = Ledger.Window.at root ~ordinal:1 |> Option.value_exn in
    let same_row =
      Ledger.Member.create
        (Ledger.Member.citation second)
        ~source
        ~source_ordinal:0
        ~provenance:Observed
      |> protocol_ok
    in
    let aliased =
      Ledger.Window.create scope ~origin:Root ~state:Sealed [ first; same_row ]
      |> protocol_ok
    in
    let aliased = Ledger.create ~owner [ aliased; active ] |> protocol_ok in
    assert (
      Result.is_error
        (Ledger.validate_state
           aliased
           ~session_id
           ~generation:0
           ~revision:2L
           ~history:[ entry 3 ]
           ~has_summary_archive:(fun _ -> true)
           ~has_summary_operation:(fun _ -> true)));
    let next_member = Ledger.Window.at active ~ordinal:0 |> Option.value_exn in
    let carried =
      Ledger.Member.create
        (Ledger.Member.citation next_member)
        ~source:(Ledger.Member.source next_member)
        ~source_ordinal:0
        ~provenance:(Carried_from (Ledger.Window.id root))
      |> protocol_ok
    in
    let missing_predecessor =
      Ledger.Window.create
        (Ledger.Window.scope active)
        ~origin:(Ledger.Window.origin active)
        ~state:Open
        [ carried ]
      |> protocol_ok
    in
    assert (Result.is_error (Ledger.create ~owner [ root; missing_predecessor ]));
    print_endline "foreign sources, conflicting archive rows and invalid lineage rejected");
  [%expect {| foreign sources, conflicting archive rows and invalid lineage rejected |}]
;;

let%expect_test "withdrawal preserves ordinals and cannot be bypassed by a carried copy" =
  with_ledger (fun _ current _ root active ->
    let at w pos = Ledger.Window.at w ~ordinal:pos |> Option.value_exn in
    let first = at root 0 in
    let second = at root 1 in
    let rebuild window members =
      Ledger.Window.create
        (Ledger.Window.scope window)
        ~origin:(Ledger.Window.origin window)
        ~state:(Ledger.Window.state window)
        members
      |> protocol_ok
    in
    let source = Ledger.Member.source first in
    let carried =
      member
        (Ledger.Window.scope active)
        ~source
        ~ordinal:0
        ~provenance:(Carried_from (Ledger.Window.id root))
        (entry 2)
      |> protocol_ok
    in
    let root = rebuild root [ Ledger.Member.withdraw first; second ] in
    let active_with carry = rebuild active [ carry; at active 0 ] in
    assert (Result.is_error (Ledger.create ~owner [ root; active_with carried ]));
    let active = active_with (Ledger.Member.withdraw carried) in
    let ledger = Ledger.create ~owner [ root; active ] |> protocol_ok in
    let json = Ledger.to_jsonaf ledger in
    assert (
      Result.is_error
        (Ledger.of_jsonaf (update_object json ~field:"version" ~f:(fun _ -> `Number "1"))));
    let state =
      state_document { current with recovery_windows = Some ledger }
      |> decode
      |> document_ok
      |> Codec.value
    in
    let view = A.Recovery_visibility.create state ~owner |> protocol_ok in
    let select m = A.Recovery_visibility.select view (Ledger.Member.citation m) in
    List.iter [ first; carried ] ~f:(fun m ->
      match select m with
      | Withdrawn -> ()
      | Eligible _ | Unavailable -> failwith "lost withdrawal");
    (match select second with
     | Eligible found -> assert (Ledger.Member.same_version found second)
     | Withdrawn | Unavailable -> failwith "unrelated member suppressed");
    let snapshot = handoff_snapshot 0 in
    let tombstone : Session.Moderator_state.Identity_snapshot.Tombstone.t =
      { target_id = Citation.occurrence (Ledger.Member.citation second); change_id = 0 }
    in
    let moderator =
      A.Moderator_checkpoint.encode
        { snapshot with revision = 1; next_change_id = 1; tombstones = [ tombstone ] }
    in
    let masked =
      A.Recovery_visibility.create { state with moderator = Some moderator } ~owner
      |> protocol_ok
    in
    (match A.Recovery_visibility.select masked (Ledger.Member.citation second) with
     | Withdrawn -> ()
     | Eligible _ | Unavailable ->
       failwith "current moderator tombstone did not suppress archived text");
    let unknown =
      Citation.create
        (Ledger.Window.scope root)
        ~occurrence:(entry 100).id
        ~version:(Citation.version (Ledger.Member.citation first))
      |> protocol_ok
    in
    (match A.Recovery_visibility.select view unknown with
     | Unavailable -> ()
     | Eligible _ | Withdrawn -> failwith "invented membership");
    assert (
      Option.equal
        Int.equal
        (P.Recovery_window.ordinal
           (Ledger.Window.manifest root)
           (Ledger.Member.citation second))
        (Some 1));
    print_endline
      "withdrawn in both windows; unaffected neighbour retains ordinal 1; unknown \
       remains unavailable");
  [%expect
    {| withdrawn in both windows; unaffected neighbour retains ordinal 1; unknown remains unavailable |}]
;;
