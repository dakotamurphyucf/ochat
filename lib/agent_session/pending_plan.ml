open! Core
module P = Agent_protocol
module Input = P.Pending_input
module H = P.History

module Change = struct
  type t =
    | Enqueue of Pending_input_document.t list
    | Adopt of
        { boundary : Pending_eligibility.Boundary.t
        ; runtime_admission_open : bool
        }
    | Cancel of
        { history_id : H.Id.t
        ; expected_content_revision : H.Content_revision.t
        }
    | Replace_text of
        { history_id : H.Id.t
        ; expected_content_revision : H.Content_revision.t
        ; text : string
        }
    | Release of Input.Terminal_proof.t
    | Retire of Pending_disposition.Retirement_reason.t
  [@@deriving sexp]
end

type t =
  { previous : Session_state.t
  ; limits : Document_schema.Limits.t
  ; revision : Input.Revision.t
  ; pending : Pending_input_document.t list
  ; adopted_entries : H.entry list
  ; dispositions : Pending_disposition_document.t list
  ; expired_dispositions : Pending_disposition_document.t list
  ; requires_archive : bool
  }

let document_result value =
  Result.map_error value ~f:(fun error ->
    P.Error.invalid_request (Sexp.to_string_hum (Document_schema.Error.sexp_of_t error)))
;;

let conflict message = P.Error.create Conflict ~message ~retryable:false ()
let input document = Pending_input_document.value document
let entry document = Input.entry (input document)
let id document = Input.history_id (input document)

let locate queue history_id =
  let rec loop reversed = function
    | [] -> Error (conflict "pending occurrence is no longer available")
    | document :: suffix ->
      if H.Id.equal (id document) history_id
      then Ok (List.rev reversed, document, suffix)
      else loop (document :: reversed) suffix
  in
  loop [] queue
;;

let check_content document expected =
  if H.Content_revision.equal (entry document).content_revision expected
  then Ok ()
  else Error (conflict "pending content revision does not match")
;;

let make_disposition document ~revision ~outcome =
  Pending_disposition.create
    ~history_id:(id document)
    ~generation:(Input.generation (input document))
    ~pending_revision:revision
    ~outcome
;;

let prepare state ~expected_pending_revision ~(change : Change.t) ~limits ~retention =
  let open Result.Let_syntax in
  let conversation = state.Session_state.conversation in
  let%bind () =
    if Input.Revision.equal conversation.pending_revision expected_pending_revision
    then Ok ()
    else Error (conflict "pending queue revision does not match")
  in
  let queue = conversation.deferred_user_entries in
  let%bind needs_revision =
    match change with
    | Enqueue additions -> Ok (not (List.is_empty additions))
    | Cancel _ | Replace_text _ -> Ok true
    | Retire _ -> Ok (not (List.is_empty queue))
    | Release proof ->
      List.fold_result queue ~init:false ~f:(fun changed document ->
        let original = Input.binding (input document) in
        let%map binding = Input.Binding.release original proof in
        changed || not (Input.Binding.equal original binding))
    | Adopt { boundary; runtime_admission_open } ->
      let%bind eligibility =
        Pending_eligibility.create state ~boundary ~runtime_admission_open
      in
      let%map prefix = Pending_eligibility.eligible_prefix eligibility queue in
      not (List.is_empty prefix)
  in
  let%bind revision =
    if
      needs_revision
      || List.length conversation.pending_dispositions
         > Pending_disposition.Retention.max_records retention
    then Input.Revision.succ conversation.pending_revision
    else Ok conversation.pending_revision
  in
  let disposition document outcome =
    let%bind value = make_disposition document ~revision ~outcome in
    Pending_disposition_document.retired document ~disposition:value ~limits
    |> document_result
  in
  let%bind pending, adopted_entries, new_dispositions, requires_archive =
    match change with
    | Enqueue additions ->
      let ids = Hash_set.create (module H.Id) in
      List.iter conversation.canonical_history ~f:(fun entry -> Hash_set.add ids entry.id);
      List.iter queue ~f:(fun document -> Hash_set.add ids (id document));
      List.iter conversation.pending_dispositions ~f:(fun document ->
        Hash_set.add
          ids
          (Pending_disposition.history_id (Pending_disposition_document.value document)));
      let%map () =
        List.fold_result additions ~init:() ~f:(fun () document ->
          let%bind _ =
            Pending_input_document.to_jsonaf document ~limits |> document_result
          in
          let binding = Input.binding (input document) in
          let timing =
            match binding with
            | Safe_boundary -> Input.Timing.Safe_boundary
            | Await_idle | After_root _ -> After_current_operation
          in
          let%bind expected_binding =
            Input.Binding.create
              timing
              ~generation:state.identity.generation
              ~operation:state.active_operation
          in
          let%bind () =
            if Input.Binding.equal binding expected_binding
            then Ok ()
            else
              Error
                (P.Error.invalid_request
                   "pending binding does not capture actual current root")
          in
          if not (Int.equal state.identity.generation (Input.generation (input document)))
          then Error (conflict "pending input generation does not match")
          else if Hash_set.mem ids (id document)
          then
            Error
              (P.Error.invalid_request "pending occurrence identity is already retained")
          else (
            Hash_set.add ids (id document);
            Ok ()))
      in
      queue @ additions, [], [], false
    | Adopt { boundary; runtime_admission_open } ->
      let%bind eligibility =
        Pending_eligibility.create state ~boundary ~runtime_admission_open
      in
      let%bind adopted = Pending_eligibility.eligible_prefix eligibility queue in
      let%map dispositions =
        List.map adopted ~f:(fun document ->
          let%bind value =
            make_disposition
              document
              ~revision
              ~outcome:(Adopted (entry document).content_revision)
          in
          Pending_disposition_document.adopted document ~disposition:value ~limits
          |> document_result)
        |> Result.all
      in
      ( List.drop queue (List.length adopted)
      , List.map adopted ~f:entry
      , dispositions
      , false )
    | Cancel { history_id; expected_content_revision } ->
      let%bind prefix, document, suffix = locate queue history_id in
      let%bind () = check_content document expected_content_revision in
      let%map value = disposition document Cancelled in
      prefix @ suffix, [], [ value ], true
    | Replace_text { history_id; expected_content_revision; text } ->
      let%bind prefix, document, suffix = locate queue history_id in
      let%bind replacement =
        History_edit.replace_text (entry document) ~expected_content_revision ~text
      in
      let%bind value = Input.with_entry (input document) replacement in
      let%map document =
        Pending_input_document.with_value document value ~limits |> document_result
      in
      prefix @ (document :: suffix), [], [], true
    | Release proof ->
      let%map pending =
        List.map queue ~f:(fun document ->
          let value = input document in
          let%bind binding = Input.Binding.release (Input.binding value) proof in
          let%bind value = Input.with_binding value binding in
          Pending_input_document.with_value document value ~limits |> document_result)
        |> Result.all
      in
      pending, [], [], false
    | Retire reason ->
      let%map dispositions =
        List.map queue ~f:(fun document -> disposition document (Retired reason))
        |> Result.all
      in
      [], [], dispositions, not (List.is_empty queue)
  in
  let retained = new_dispositions @ conversation.pending_dispositions in
  let dispositions, expired_dispositions =
    List.split_n retained (Pending_disposition.Retention.max_records retention)
  in
  let changed =
    (not (List.equal Pending_input_document.equal queue pending))
    || (not (List.is_empty adopted_entries))
    || (not (List.is_empty new_dispositions))
    || not (List.is_empty expired_dispositions)
  in
  let revision = if changed then revision else conversation.pending_revision in
  Ok
    { previous = state
    ; limits
    ; revision
    ; pending
    ; adopted_entries
    ; dispositions
    ; expired_dispositions
    ; requires_archive
    }
;;

let validate_basis t state =
  let open Result.Let_syntax in
  let document_equal encode left right =
    let%bind left =
      List.map left ~f:(fun value -> encode value ~limits:t.limits |> document_result)
      |> Result.all
    in
    let%map right =
      List.map right ~f:(fun value -> encode value ~limits:t.limits |> document_result)
      |> Result.all
    in
    List.equal Jsonaf.exactly_equal left right
  in
  let%bind pending_equal =
    document_equal
      Pending_input_document.to_jsonaf
      t.previous.conversation.deferred_user_entries
      state.Session_state.conversation.deferred_user_entries
  in
  let%bind dispositions_equal =
    document_equal
      Pending_disposition_document.to_jsonaf
      t.previous.conversation.pending_dispositions
      state.conversation.pending_dispositions
  in
  if
    P.Id.Session.equal t.previous.identity.session_id state.identity.session_id
    && Int.equal t.previous.identity.generation state.identity.generation
    && Int64.equal t.previous.counters.revision state.counters.revision
    && Sexp.equal
         (Session_state.Lifecycle.sexp_of_t t.previous.lifecycle)
         (Session_state.Lifecycle.sexp_of_t state.lifecycle)
    && Option.equal
         (fun left right ->
            Sexp.equal (P.Operation.sexp_of_t left) (P.Operation.sexp_of_t right))
         t.previous.active_operation
         state.active_operation
    && Session_state.Runtime_initialization.equal
         t.previous.runtime_initialization
         state.runtime_initialization
    && Bool.equal t.previous.halted state.halted
    && Option.equal
         (fun left right -> Sexp.equal (P.Error.sexp_of_t left) (P.Error.sexp_of_t right))
         t.previous.failure
         state.failure
    && Input.Revision.equal
         t.previous.conversation.pending_revision
         state.conversation.pending_revision
    && List.equal
         H.equal_entry
         t.previous.conversation.canonical_history
         state.conversation.canonical_history
    && pending_equal
    && dispositions_equal
  then Ok ()
  else Error (conflict "pending transition basis changed")
;;

let revision t = t.revision
let pending t = t.pending
let adopted_entries t = t.adopted_entries
let dispositions t = t.dispositions
let expired_dispositions t = t.expired_dispositions
let requires_archive t = t.requires_archive

let apply t state =
  let%map.Result () = validate_basis t state in
  { state with
    conversation =
      { state.conversation with
        canonical_history = state.conversation.canonical_history @ t.adopted_entries
      ; deferred_user_entries = t.pending
      ; pending_revision = t.revision
      ; pending_dispositions = t.dispositions
      }
  }
;;

let legacy_adoption state ~limits =
  let open Result.Let_syntax in
  let queue = state.Session_state.conversation.deferred_user_entries in
  let%bind () =
    List.fold_result queue ~init:() ~f:(fun () document ->
      match Input.binding (input document) with
      | Safe_boundary -> Ok ()
      | Await_idle | After_root _ ->
        Error
          (P.Error.invalid_request
             "legacy adoption cannot consume explicitly timed input"))
  in
  let%bind revision =
    if List.is_empty queue
    then Ok state.conversation.pending_revision
    else Input.Revision.succ state.conversation.pending_revision
  in
  let%map dispositions =
    List.map queue ~f:(fun document ->
      let%bind disposition =
        make_disposition
          document
          ~revision
          ~outcome:(Adopted (entry document).content_revision)
      in
      Pending_disposition_document.adopted document ~disposition ~limits
      |> document_result)
    |> Result.all
  in
  { previous = state
  ; limits
  ; revision
  ; pending = []
  ; adopted_entries = List.map queue ~f:entry
  ; dispositions = dispositions @ state.conversation.pending_dispositions
  ; expired_dispositions = []
  ; requires_archive = false
  }
;;
