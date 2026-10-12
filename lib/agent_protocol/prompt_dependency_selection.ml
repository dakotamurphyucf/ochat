open! Core
module Error = Protocol_error

let invalid message = Error (Error.invalid_request message)
let maximum_targets = 1024

let state json =
  Result.bind (Json_codec.string json) ~f:(fun value ->
    if String.length value <= 16
    then Ok value
    else invalid "dependency selection state exceeds its byte bound")
;;

let definition_id id =
  Id.Prompt_definition.of_string (Id.Prompt_definition.to_string id)
  |> Result.map ~f:ignore
;;

(* Admit the fixed shapes directly, before sorting fields, decoding arrays or
   serializing caller-constructed JSON. No recursive traversal of rejected values. *)
let fields json allowed =
  let open Result.Let_syntax in
  let%bind () =
    match json with
    | `Object values ->
      List.fold_result values ~init:0 ~f:(fun count (name, _) ->
        if count >= List.length allowed || String.length name > 32
        then invalid "dependency selection object exceeds its field bounds"
        else Ok (count + 1))
      |> Result.map ~f:ignore
    | _ -> invalid "dependency selection requires a JSON object"
  in
  let%bind result = Json_codec.fields json in
  let%map () =
    if
      List.for_all (Json_codec.to_alist result) ~f:(fun (name, _) ->
        List.mem allowed name ~equal:String.equal)
    then Ok ()
    else invalid "unknown dependency selection field"
  in
  result
;;

let count_targets values =
  let open Result.Let_syntax in
  let%bind count =
    List.fold_result values ~init:0 ~f:(fun count _ ->
      if count >= maximum_targets
      then invalid "dependency update exceeds 1024 targets"
      else Ok (count + 1))
  in
  if count = 0 then invalid "dependency update requires at least one target" else Ok ()
;;

let decode_targets decode = function
  | `Array values ->
    Result.bind (count_targets values) ~f:(fun () ->
      List.map values ~f:decode |> Result.all)
  | _ -> invalid "dependency update targets must be a JSON array"
;;

let sexp_result sexp = function
  | Ok value -> value
  | Error error ->
    Sexplib.Conv.of_sexp_error (Sexp.to_string_hum (Error.sexp_of_t error)) sexp
;;

module Generation = struct
  type t = int [@@deriving compare, equal, sexp]

  let of_int value =
    if value < 0 then invalid "dependency generation must be nonnegative" else Ok value
  ;;

  let to_int t = t
  let to_json t = `Number (Int.to_string t)

  let of_json = function
    | `Number value ->
      if
        String.is_empty value
        || String.length value > 19
        || (String.length value > 1 && Char.equal value.[0] '0')
        || not (String.for_all value ~f:Char.is_digit)
      then invalid "dependency generation must be a decimal JSON integer"
      else (
        match Int.of_string_opt value with
        | None -> invalid "dependency generation exceeds its integer bound"
        | Some value -> of_int value)
    | _ -> invalid "dependency generation must be a JSON integer"
  ;;

  let t_of_sexp sexp = sexp_result sexp (of_int (t_of_sexp sexp))
end

module Status_request = struct
  let of_json json = Result.map (fields json []) ~f:ignore
  let to_json () = `Object []
end

module Status = struct
  module History = struct
    type t =
      | Tracked of Generation.t
      | Missing
      | Invalid
    [@@deriving equal, sexp]

    let to_json = function
      | Tracked generation ->
        `Object
          [ "state", `String "tracked"; "generation", Generation.to_json generation ]
      | Missing -> `Object [ "state", `String "missing" ]
      | Invalid -> `Object [ "state", `String "invalid" ]
    ;;

    let of_json json =
      let open Result.Let_syntax in
      let%bind f = fields json [ "state"; "generation" ] in
      let%bind state = Json_codec.required_as f "state" state in
      match state with
      | "tracked" ->
        Json_codec.required_as f "generation" Generation.of_json
        |> Result.map ~f:(fun generation -> Tracked generation)
      | "missing" | "invalid" ->
        if Option.is_some (Json_codec.optional f "generation")
        then invalid "untracked dependency history cannot carry a generation"
        else Ok (if String.equal state "missing" then Missing else Invalid)
      | _ -> invalid "unknown dependency history state"
    ;;
  end

  module Publication = struct
    type t =
      | Available
      | Busy
      | Uncertain
      | Closed
    [@@deriving equal, sexp]

    let to_json = function
      | Available -> `String "available"
      | Busy -> `String "busy"
      | Uncertain -> `String "uncertain"
      | Closed -> `String "closed"
    ;;

    let of_json json =
      Result.bind (state json) ~f:(fun value ->
        Json_codec.enum
          ~name:"dependency publication"
          [ "available", Available
          ; "busy", Busy
          ; "uncertain", Uncertain
          ; "closed", Closed
          ]
          (`String value))
    ;;
  end

  type t =
    { history : History.t
    ; publication : Publication.t
    }
  [@@deriving equal, sexp]

  let to_json t =
    `Object
      [ "history", History.to_json t.history
      ; "publication", Publication.to_json t.publication
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind f = fields json [ "history"; "publication" ] in
    let%bind history = Json_codec.required_as f "history" History.of_json in
    let%map publication = Json_codec.required_as f "publication" Publication.of_json in
    { history; publication }
  ;;
end

module Update_request = struct
  type t =
    { definitions : Id.Prompt_definition.t list
    ; expected_generation : Generation.t
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving equal, sexp]

  let create ~definitions ~expected_generation ~idempotency_key =
    let open Result.Let_syntax in
    let%bind () = count_targets definitions in
    let%bind () =
      List.fold_result definitions ~init:() ~f:(fun () id -> definition_id id)
    in
    let%bind _ = Idempotency_key.of_string (Idempotency_key.to_string idempotency_key) in
    let definitions = List.sort definitions ~compare:Id.Prompt_definition.compare in
    if Option.is_some (List.find_a_dup definitions ~compare:Id.Prompt_definition.compare)
    then invalid "duplicate dependency update definition"
    else Ok { definitions; expected_generation; idempotency_key }
  ;;

  let definitions t = t.definitions
  let expected_generation t = t.expected_generation
  let idempotency_key t = t.idempotency_key

  let to_json t =
    `Object
      [ "definitions", `Array (List.map t.definitions ~f:Id.Prompt_definition.to_json)
      ; "expected_generation", Generation.to_json t.expected_generation
      ; "idempotency_key", Idempotency_key.to_json t.idempotency_key
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind f =
      fields json [ "definitions"; "expected_generation"; "idempotency_key" ]
    in
    let%bind definitions =
      Json_codec.required_as f "definitions" (decode_targets Id.Prompt_definition.of_json)
    in
    let%bind expected_generation =
      Json_codec.required_as f "expected_generation" Generation.of_json
    in
    let%bind idempotency_key =
      Json_codec.required_as f "idempotency_key" Idempotency_key.of_json
    in
    create ~definitions ~expected_generation ~idempotency_key
  ;;

  let t_of_sexp sexp =
    let { definitions; expected_generation; idempotency_key } = t_of_sexp sexp in
    sexp_result sexp (create ~definitions ~expected_generation ~idempotency_key)
  ;;
end

module Update_result = struct
  module Target = struct
    type outcome =
      | Ready of Id.Prompt_revision.t
      | Unavailable of { reason : string }
    [@@deriving equal, sexp]

    type t =
      { definition_id : Id.Prompt_definition.t
      ; outcome : outcome
      }
    [@@deriving equal, sexp]

    let validate t =
      let open Result.Let_syntax in
      let%bind () = definition_id t.definition_id in
      match t.outcome with
      | Ready revision ->
        Id.Prompt_revision.of_string (Id.Prompt_revision.to_string revision)
        |> Result.map ~f:ignore
      | Unavailable { reason } ->
        if
          String.is_empty reason
          || String.length reason > 4096
          || String.exists reason ~f:(Char.equal '\000')
          || not
               (Uutf.String.fold_utf_8
                  (fun valid _ -> function
                     | `Uchar _ -> valid
                     | `Malformed _ -> false)
                  true
                  reason)
        then
          invalid "dependency target reason must be nonempty UTF-8 text within 4096 bytes"
        else Ok ()
    ;;

    let to_json t =
      let outcome =
        match t.outcome with
        | Ready revision ->
          `Object
            [ "state", `String "ready"; "revision", Id.Prompt_revision.to_json revision ]
        | Unavailable { reason } ->
          `Object [ "state", `String "unavailable"; "reason", `String reason ]
      in
      `Object
        [ "definition_id", Id.Prompt_definition.to_json t.definition_id
        ; "outcome", outcome
        ]
    ;;

    let of_json json =
      let open Result.Let_syntax in
      let%bind f = fields json [ "definition_id"; "outcome" ] in
      let%bind definition_id =
        Json_codec.required_as f "definition_id" Id.Prompt_definition.of_json
      in
      let%bind outcome_json = Json_codec.required f "outcome" in
      let%bind f = fields outcome_json [ "state"; "revision"; "reason" ] in
      let%bind state = Json_codec.required_as f "state" state in
      let%bind outcome =
        match state with
        | "ready" ->
          if Option.is_some (Json_codec.optional f "reason")
          then invalid "ready dependency target cannot carry an unavailable reason"
          else
            Json_codec.required_as f "revision" Id.Prompt_revision.of_json
            |> Result.map ~f:(fun revision -> Ready revision)
        | "unavailable" ->
          if Option.is_some (Json_codec.optional f "revision")
          then invalid "unavailable dependency target cannot carry a revision"
          else
            Json_codec.required_as f "reason" Json_codec.string
            |> Result.map ~f:(fun reason -> Unavailable { reason })
        | _ -> invalid "unknown dependency target state"
      in
      let t = { definition_id; outcome } in
      let%map () = validate t in
      t
    ;;
  end

  type t =
    { previous_generation : Generation.t
    ; generation : Generation.t
    ; targets : Target.t list
    }
  [@@deriving equal, sexp]

  let create ~previous_generation ~generation ~targets =
    let open Result.Let_syntax in
    let previous = Generation.to_int previous_generation in
    let%bind () =
      if previous = Int.max_value || Generation.to_int generation <> previous + 1
      then invalid "dependency update must advance exactly one generation"
      else Ok ()
    in
    let%bind () = count_targets targets in
    let%bind () = List.fold_result targets ~init:() ~f:(fun () t -> Target.validate t) in
    let compare left right =
      Id.Prompt_definition.compare left.Target.definition_id right.Target.definition_id
    in
    let targets = List.sort targets ~compare in
    if Option.is_some (List.find_a_dup targets ~compare)
    then invalid "duplicate dependency update result target"
    else Ok { previous_generation; generation; targets }
  ;;

  let previous_generation t = t.previous_generation
  let generation t = t.generation
  let targets t = t.targets

  let matches_request t request =
    Generation.equal t.previous_generation (Update_request.expected_generation request)
    && List.equal
         Id.Prompt_definition.equal
         (List.map t.targets ~f:(fun target -> target.Target.definition_id))
         (Update_request.definitions request)
  ;;

  let to_json t =
    `Object
      [ "previous_generation", Generation.to_json t.previous_generation
      ; "generation", Generation.to_json t.generation
      ; "targets", `Array (List.map t.targets ~f:Target.to_json)
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind f = fields json [ "previous_generation"; "generation"; "targets" ] in
    let%bind previous_generation =
      Json_codec.required_as f "previous_generation" Generation.of_json
    in
    let%bind generation = Json_codec.required_as f "generation" Generation.of_json in
    let%bind targets =
      Json_codec.required_as f "targets" (decode_targets Target.of_json)
    in
    create ~previous_generation ~generation ~targets
  ;;

  let t_of_sexp sexp =
    let { previous_generation; generation; targets } = t_of_sexp sexp in
    sexp_result sexp (create ~previous_generation ~generation ~targets)
  ;;
end
