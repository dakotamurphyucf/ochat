open! Core
open Fixtures
module D = Document_schema
module A = Agent_session
module P = Agent_protocol

let fields = function
  | `Object fields -> fields
  | _ -> failwith "expected object fixture"
;;

let member json name = List.Assoc.find_exn (fields json) name ~equal:String.equal

let replace json name value =
  `Object
    (List.map (fields json) ~f:(fun (key, previous) ->
       key, if String.equal key name then value else previous))
;;

let without json name = `Object (List.Assoc.remove (fields json) name ~equal:String.equal)

let%expect_test "organization and content revision conversions compose in order" =
  with_actor_workspace (fun _ workspace_instance ->
    let initial =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
    in
    let entry sequence text =
      let id =
        History_entry.Id.create
          ~namespace:(P.Id.Session.to_string initial.identity.session_id)
          ~sequence
        |> Result.ok_or_failwith
      in
      A.History_codec.user_text ~id text |> A.History_codec.to_protocol
    in
    let state =
      { initial with
        conversation =
          { initial.conversation with
            canonical_history = [ entry 0 "saved" ]
          ; deferred_user_entries = [ entry 1 "pending" ]
          ; initial_prompt_entry_count = 0
          ; next_history_sequence = 2L
          ; reserved_history_through = 2L
          }
      }
    in
    let current = state_document state in
    let payload = D.Document.payload current in
    let identity = without (member payload "identity") "organization" in
    let identity = `Object (fields identity @ [ "future_identity", `Null ]) in
    let old_entries = function
      | `Array entries ->
        `Array
          (List.map entries ~f:(fun entry ->
             `Object
               (fields (without entry "content_revision")
                @ [ "future_entry", `Object [ "content_revision", `Null ] ])))
      | _ -> failwith "expected history fixture"
    in
    let conversation = member payload "conversation" in
    let conversation =
      List.fold
        [ "canonical_history"; "deferred_user_entries" ]
        ~init:conversation
        ~f:(fun json name -> replace json name (old_entries (member json name)))
    in
    let payload =
      replace (replace payload "identity" identity) "conversation" conversation
    in
    let document version =
      D.Document.create ~limits:document_limits ~kind:"session.state" ~version ~payload
      |> document_ok
    in
    let old = document 5 in
    let original = D.Document.to_string old in
    let restored =
      A.Session_state_document.decode old ~limits:document_limits |> document_ok
    in
    let value = A.Session_state_document.value restored in
    let encoded =
      A.Session_state_document.encode restored ~limits:document_limits |> document_ok
    in
    let encoded_payload = D.Document.payload encoded in
    let entries name =
      match member (member encoded_payload "conversation") name with
      | `Array entries -> entries
      | _ -> failwith "expected converted history"
    in
    let preserved entry =
      D.Json.equal (member entry "future_entry") (`Object [ "content_revision", `Null ])
    in
    print_s
      [%sexp
        (D.Document.version encoded : int)
      , (P.Session_organization.Values.equal
           value.identity.organization
           P.Session_organization.Values.empty
         : bool)
      , (List.for_all
           (value.conversation.canonical_history
            @ value.conversation.deferred_user_entries)
           ~f:(fun entry ->
             P.History.Content_revision.equal
               entry.content_revision
               P.History.Content_revision.zero)
         : bool)
      , (List.for_all
           (entries "canonical_history" @ entries "deferred_user_entries")
           ~f:preserved
         : bool)
      , (D.Json.equal (member (member encoded_payload "identity") "future_identity") `Null
         : bool)
      , (String.equal original (D.Document.to_string old) : bool)
      , (Result.is_error
           (A.Session_state_document.decode (document 7) ~limits:document_limits)
         : bool)]);
  [%expect {| (7 true true true true true true) |}]
;;
