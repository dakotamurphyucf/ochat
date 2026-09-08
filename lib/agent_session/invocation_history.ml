open Core
module P = Agent_protocol
module Item = Openai.Responses.Item

let invalid message = Error (P.Error.create Conflict ~message ~retryable:false ())

let call = function
  | Item.Function_call c -> Some (`Function, c.call_id, c.name)
  | Custom_tool_call c -> Some (`Custom, c.call_id, c.name)
  | _ -> None
;;

let output = function
  | Item.Function_call_output o -> Some (`Function, o.call_id, o.output)
  | Custom_tool_call_output o -> Some (`Custom, o.call_id, o.output)
  | _ -> None
;;

let canonical entry =
  match entry.P.History.provenance with
  | Canonical when not entry.redacted -> History_codec.of_protocol entry
  | _ -> invalid "invocation history must be canonical and usable as model input"
;;

let validate_routing (invocation : P.Invocation.t) item =
  match invocation.routing with
  | None -> Ok ()
  | Some routing ->
    let candidate =
      match item with
      | Item.Function_call c -> Some (P.Invocation.Function, c.arguments)
      | Custom_tool_call c -> Some (P.Invocation.Custom, c.input)
      | _ -> None
    in
    (match candidate, routing.canonical_payload with
     | Some (kind, payload), Some expected
       when P.Invocation.equal_call_kind kind routing.kind
            && expected.byte_length = String.length payload
            && String.equal expected.sha256 (Chatmd_shell_spec.Source_ref.digest payload)
       -> Ok ()
     | _ -> invalid "canonical call differs from recorded routing provenance")
;;

let bound_call ~history (invocation : P.Invocation.t) =
  let open Result.Let_syntax in
  match invocation.context.call_entry_id with
  | None -> invalid "invocation has no canonical call binding"
  | Some id ->
    let _, rest =
      List.split_while history ~f:(fun (e : P.History.entry) ->
        not (P.History.Id.compare e.id id = 0))
    in
    (match rest with
     | [] -> invalid "canonical invocation call is no longer retained"
     | entry :: following ->
       let%bind decoded = canonical entry in
       let%bind () = validate_routing invocation (History_entry.item decoded) in
       (match call (History_entry.item decoded) with
        | Some (kind, provider_id, name)
          when Option.equal
                 String.equal
                 (Some provider_id)
                 invocation.context.provider_call_id
               && String.equal name invocation.context.tool_name ->
          Ok (kind, provider_id, following)
        | _ -> invalid "canonical call does not match the invocation"))
;;

let same_pair kind provider_id item =
  match call item, output item with
  | Some (k, id, _), _ | _, Some (k, id, _) ->
    Poly.equal kind k && String.equal id provider_id
  | _ -> false
;;

let validate_call ~history invocation =
  let open Result.Let_syntax in
  let%bind kind, provider_id, following = bound_call ~history invocation in
  List.fold_result following ~init:() ~f:(fun () entry ->
    let%bind decoded = History_codec.of_protocol entry in
    if same_pair kind provider_id (History_entry.item decoded)
    then invalid "canonical call already has a result or its provider ID was reused"
    else Ok ())
;;

let validate_output (invocation : P.Invocation.t) entry =
  let open Result.Let_syntax in
  let%bind () = P.Invocation.validate invocation in
  let%bind outcome =
    match invocation.status with
    | Resolved outcome | Published outcome -> Ok outcome
    | _ -> invalid "invocation has no recorded outcome"
  in
  match output (History_entry.item entry) with
  | Some (_, provider_id, Openai.Responses.Tool_output.Output.Text text)
    when Option.equal String.equal (Some provider_id) invocation.context.provider_call_id
         && String.equal text (Jsonaf.to_string (P.Invocation.outcome_to_json outcome)) ->
    Ok ()
  | _ -> invalid "tool output differs from the recorded invocation outcome"
;;

let validate_publication ~history (invocation : P.Invocation.t) =
  let open Result.Let_syntax in
  let%bind kind, provider_id, following = bound_call ~history invocation in
  match invocation.output_entry_id with
  | None -> invalid "publication has no output receipt"
  | Some id ->
    let rec find = function
      | [] -> invalid "publication output occurrence is missing"
      | (entry : P.History.entry) :: rest ->
        let%bind decoded = History_codec.of_protocol entry in
        if P.History.Id.compare entry.id id = 0
        then (
          let%bind _ = canonical entry in
          let%bind () = validate_output invocation decoded in
          match output (History_entry.item decoded) with
          | Some (actual_kind, _, _) when Poly.equal kind actual_kind -> Ok ()
          | _ -> invalid "tool output kind differs from its canonical call")
        else if same_pair kind provider_id (History_entry.item decoded)
        then invalid "publication crosses an earlier output or a reused call ID"
        else find rest
    in
    find following
;;

let recover_output ~history invocation =
  let open Result.Let_syntax in
  let%bind kind, provider_id, following = bound_call ~history invocation in
  let rec find = function
    | [] ->
      Ok
        (`Missing
            (match kind with
             | `Function -> P.Invocation.Function
             | `Custom -> Custom))
    | entry :: rest ->
      let%bind decoded = History_codec.of_protocol entry in
      if same_pair kind provider_id (History_entry.item decoded)
      then (
        match output (History_entry.item decoded) with
        | Some _ ->
          let%bind _ = canonical entry in
          let%map () = validate_output invocation decoded in
          `Existing decoded
        | None -> invalid "cannot recover an output across reuse of its provider call ID")
      else find rest
  in
  find following
;;

let validate_retained ~history (invocation : P.Invocation.t) =
  let open Result.Let_syntax in
  let%bind () =
    if
      Option.is_some invocation.publication_discarded
      && List.exists history ~f:(fun (entry : P.History.entry) ->
        Option.exists invocation.context.call_entry_id ~f:(fun id ->
          P.History.Id.compare entry.id id = 0))
    then invalid "discarded invocation still has a retained canonical call"
    else Ok ()
  in
  let%bind () =
    if
      List.exists history ~f:(fun (entry : P.History.entry) ->
        Option.exists invocation.context.call_entry_id ~f:(fun id ->
          P.History.Id.compare entry.id id = 0))
    then Result.map (bound_call ~history invocation) ~f:(fun _ -> ())
    else Ok ()
  in
  match invocation.output_entry_id with
  | None -> Ok ()
  | Some id ->
    (match
       List.find history ~f:(fun (e : P.History.entry) ->
         P.History.Id.compare e.id id = 0)
     with
     | None -> Ok ()
     | Some entry ->
       let%bind decoded = canonical entry in
       let%bind () = validate_output invocation decoded in
       if
         List.exists history ~f:(fun (e : P.History.entry) ->
           Option.exists invocation.context.call_entry_id ~f:(fun call_id ->
             P.History.Id.compare e.id call_id = 0))
       then validate_publication ~history invocation
       else Ok ())
;;
