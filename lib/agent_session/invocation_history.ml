open Core
module P = Agent_protocol
module Payload = History_entry.Payload
module Kind = Payload.Call_kind

let invalid message = Error (P.Error.create Conflict ~message ~retryable:false ())

let call entry =
  let semantic = Payload.semantic (History_entry.payload entry) in
  match Payload.Semantic.view semantic, (Payload.Semantic.metadata semantic).call_id with
  | Call { kind; name; _ }, Payload.Presence.Value call_id -> Some (kind, call_id, name)
  | ( (Message _ | Call _ | Result _ | Reasoning _ | Unknown _)
    , (Payload.Presence.Absent | Null | Value _) ) -> None
;;

let output entry =
  let semantic = Payload.semantic (History_entry.payload entry) in
  match Payload.Semantic.view semantic, (Payload.Semantic.metadata semantic).call_id with
  | Result { kind; output; _ }, Payload.Presence.Value call_id ->
    Some (kind, call_id, output)
  | ( (Message _ | Call _ | Result _ | Reasoning _ | Unknown _)
    , (Payload.Presence.Absent | Null | Value _) ) -> None
;;

let canonical ~decode entry =
  match entry.P.History.provenance with
  | Canonical when not entry.redacted -> decode entry
  | _ -> invalid "invocation history must be canonical and usable as model input"
;;

let retained_output ~decode invocation entry =
  let open Result.Let_syntax in
  let%bind () =
    match Chat_response.Authoring_publication.validate_output invocation entry with
    | Ok () -> Ok ()
    | Error error -> invalid error.message
  in
  decode entry
;;

let validate_routing (invocation : P.Invocation.t) entry =
  match invocation.routing with
  | None -> Ok ()
  | Some routing ->
    let candidate =
      match Payload.Semantic.view (Payload.semantic (History_entry.payload entry)) with
      | Call { kind; input_bytes; _ } ->
        Some
          ( (match kind with
             | Kind.Function -> P.Invocation.Function
             | Custom -> Custom)
          , input_bytes )
      | Message _ | Result _ | Reasoning _ | Unknown _ -> None
    in
    (match candidate, routing.canonical_payload with
     | Some (kind, payload), Some expected
       when P.Invocation.equal_call_kind kind routing.kind
            && expected.byte_length = String.length payload
            && String.equal expected.sha256 (Chatmd_shell_spec.Source_ref.digest payload)
       -> Ok ()
     | _ -> invalid "canonical call differs from recorded routing provenance")
;;

let find_call history id =
  let _, rest =
    List.split_while history ~f:(fun (entry : P.History.entry) ->
      not (P.History.Id.equal entry.id id))
  in
  match rest with
  | [] -> None
  | entry :: following -> Some (entry, following)
;;

let bound_call_with_lookup ~decode ~find_call (invocation : P.Invocation.t) =
  let open Result.Let_syntax in
  match invocation.context.call_entry_id with
  | None -> invalid "invocation has no canonical call binding"
  | Some id ->
    (match find_call id with
     | None -> invalid "canonical invocation call is no longer retained"
     | Some (entry, following) ->
       let%bind decoded = canonical ~decode entry in
       let%bind () = validate_routing invocation decoded in
       (match call decoded with
        | Some (kind, provider_id, name)
          when Option.equal
                 String.equal
                 (Some provider_id)
                 invocation.context.provider_call_id
               && String.equal name invocation.context.tool_name ->
          Ok (kind, provider_id, following)
        | _ -> invalid "canonical call does not match the invocation"))
;;

let bound_call ~decode ~history invocation =
  bound_call_with_lookup ~decode ~find_call:(find_call history) invocation
;;

let same_pair kind provider_id item =
  match call item, output item with
  | Some (k, id, _), _ | _, Some (k, id, _) ->
    Kind.equal kind k && String.equal id provider_id
  | _ -> false
;;

let validate_call ~history invocation =
  let open Result.Let_syntax in
  let%bind kind, provider_id, following =
    bound_call ~decode:History_codec.of_protocol ~history invocation
  in
  List.fold_result following ~init:() ~f:(fun () entry ->
    let%bind decoded = History_codec.of_protocol entry in
    if same_pair kind provider_id decoded
    then invalid "canonical call already has a result or its provider ID was reused"
    else Ok ())
;;

let validate_output (invocation : P.Invocation.t) entry =
  let open Result.Let_syntax in
  let%bind () = P.Invocation.validate invocation in
  let%bind () =
    match
      ( Payload.Semantic.view (Payload.semantic (History_entry.payload entry))
      , invocation.context.call_entry_id )
    with
    | Result { relation = Bound id; _ }, Some expected
      when not (History_entry.Id.equal id expected) ->
      invalid "tool output is bound to a different host call occurrence"
    | Result { relation = Bound _; _ }, None ->
      invalid "bound tool output has no recorded host call occurrence"
    | Message _, _ | Call _, _ | Result _, _ | Reasoning _, _ | Unknown _, _ -> Ok ()
  in
  let%bind outcome =
    match invocation.status with
    | Resolved outcome | Published outcome -> Ok outcome
    | _ -> invalid "invocation has no recorded outcome"
  in
  match output entry with
  | Some (_, provider_id, Payload.Output.Text text)
    when Option.equal String.equal (Some provider_id) invocation.context.provider_call_id
         && String.equal text (Jsonaf.to_string (P.Invocation.outcome_to_json outcome)) ->
    Ok ()
  | _ -> invalid "tool output differs from the recorded invocation outcome"
;;

let validate_publication_with_bound_call ~decode ~bound_call (invocation : P.Invocation.t)
  =
  let open Result.Let_syntax in
  let%bind kind, provider_id, following = bound_call () in
  match invocation.output_entry_id with
  | None -> invalid "publication has no output receipt"
  | Some id ->
    let rec find = function
      | [] -> invalid "publication output occurrence is missing"
      | (entry : P.History.entry) :: rest ->
        let%bind decoded = decode entry in
        if P.History.Id.compare entry.id id = 0
        then (
          let%bind _ = retained_output ~decode invocation entry in
          let%bind () = validate_output invocation decoded in
          match output decoded with
          | Some (actual_kind, _, _) when Kind.equal kind actual_kind -> Ok ()
          | _ -> invalid "tool output kind differs from its canonical call")
        else if same_pair kind provider_id decoded
        then invalid "publication crosses an earlier output or a reused call ID"
        else find rest
    in
    find following
;;

let validate_publication ~history invocation =
  validate_publication_with_bound_call
    ~decode:History_codec.of_protocol
    ~bound_call:(fun () ->
      bound_call ~decode:History_codec.of_protocol ~history invocation)
    invocation
;;

let recover_output ~history invocation =
  let open Result.Let_syntax in
  let%bind kind, provider_id, following =
    bound_call ~decode:History_codec.of_protocol ~history invocation
  in
  let rec find = function
    | [] ->
      Ok
        (`Missing
            (match kind with
             | Kind.Function -> P.Invocation.Function
             | Custom -> Custom))
    | entry :: rest ->
      let%bind decoded = History_codec.of_protocol entry in
      if same_pair kind provider_id decoded
      then (
        match output decoded with
        | Some _ ->
          let%bind _ =
            retained_output ~decode:History_codec.of_protocol invocation entry
          in
          let%map () = validate_output invocation decoded in
          `Existing decoded
        | None -> invalid "cannot recover an output across reuse of its provider call ID")
      else find rest
  in
  find following
;;

let validate_retained_with_lookup
      ~decode
      ~find_entry
      ~bound_call
      (invocation : P.Invocation.t)
  =
  let open Result.Let_syntax in
  let call_is_retained =
    Option.exists invocation.context.call_entry_id ~f:(fun id ->
      Option.is_some (find_entry id))
  in
  let%bind () =
    if Option.is_some invocation.publication_discarded && call_is_retained
    then invalid "discarded invocation still has a retained canonical call"
    else Ok ()
  in
  let%bind () =
    if call_is_retained then Result.map (bound_call ()) ~f:(fun _ -> ()) else Ok ()
  in
  match invocation.output_entry_id with
  | None -> Ok ()
  | Some id ->
    (match find_entry id with
     | None -> Ok ()
     | Some entry ->
       let%bind decoded = retained_output ~decode invocation entry in
       let%bind () = validate_output invocation decoded in
       if call_is_retained
       then validate_publication_with_bound_call ~decode ~bound_call invocation
       else Ok ())
;;

let validate_retained ~history invocation =
  validate_retained_with_lookup
    ~decode:History_codec.of_protocol
    ~find_entry:(fun id ->
      List.find history ~f:(fun (entry : P.History.entry) ->
        P.History.Id.equal entry.id id))
    ~bound_call:(fun () ->
      bound_call ~decode:History_codec.of_protocol ~history invocation)
    invocation
;;

module Validated_history = struct
  module Key = struct
    type t = P.History.Id.t [@@deriving compare, sexp_of]

    include Comparator.Make (struct
        type nonrec t = t [@@deriving compare, sexp_of]
      end)
  end

  type occurrence =
    { protocol : P.History.entry
    ; decoded : History_entry.t
    ; following : P.History.entry list
    }

  type t =
    { entries : History_entry.t list
    ; occurrences : occurrence Map.M(Key).t
    }

  let create history =
    let open Result.Let_syntax in
    let%bind entries = History_codec.all_of_protocol history in
    let rec index history entries acc =
      match history, entries with
      | [], [] -> acc
      | protocol :: following, decoded :: rest ->
        let occurrence = { protocol; decoded; following } in
        index following rest ((protocol.P.History.id, occurrence) :: acc)
      | [], _ :: _ | _ :: _, [] ->
        raise_s [%sexp "decoded history length differs from its protocol snapshot"]
    in
    match Map.of_alist (module Key) (index history entries []) with
    | `Duplicate_key _ -> Error (P.Error.invalid_request "duplicate history identity")
    | `Ok occurrences -> Ok { entries; occurrences }
  ;;

  let entries t = t.entries

  let validate_retained t invocation =
    (* Only entries owned by this snapshot reach the decoder. Its immutable map
       records the constructor's successful proof, not a cross-transition cache. *)
    let decode (entry : P.History.entry) =
      Ok (Map.find_exn t.occurrences entry.id).decoded
    in
    let find_entry id =
      Option.map (Map.find t.occurrences id) ~f:(fun occurrence -> occurrence.protocol)
    in
    let find_call id =
      Option.map (Map.find t.occurrences id) ~f:(fun occurrence ->
        occurrence.protocol, occurrence.following)
    in
    (* Successful routing evidence belongs only to this invocation check. The
       publication phase still scans the original suffix for provider-ID reuse. *)
    let validated_call = ref None in
    let bound_call () =
      match !validated_call with
      | Some call -> Ok call
      | None ->
        Result.map (bound_call_with_lookup ~decode ~find_call invocation) ~f:(fun call ->
          validated_call := Some call;
          call)
    in
    validate_retained_with_lookup ~decode ~find_entry ~bound_call invocation
  ;;
end
