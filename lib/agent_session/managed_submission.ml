open Core
module P = Agent_protocol
module D = Agent_store.Delegation_store

type outcome =
  | Completed
  | Failed
  | Cancelled
  | Interrupted
  | Invalidated
[@@deriving equal, sexp]

type status =
  | Deferred
  | Ready
  | Assigned of P.Id.Operation.t
  | Terminal of P.Id.Operation.t option * outcome
[@@deriving equal, sexp]

type t =
  { reference : D.Reference.t
  ; key : P.Idempotency_key.t
  ; request_sha256 : string
  ; generation : int
  ; history_id : P.History.Id.t
  ; created_at : P.Timestamp.t
  ; updated_at : P.Timestamp.t
  ; status : status
  ; output_ids : P.History.Id.t list
  }
[@@deriving equal, sexp]

let same_key left right =
  D.Reference.equal left.reference right.reference
  && P.Idempotency_key.equal left.key right.key
;;

let validate t =
  let open Result.Let_syntax in
  let%bind () =
    D.validate_reference t.reference
    |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
  in
  let valid_hash =
    String.length t.request_sha256 = 64
    && String.for_all t.request_sha256 ~f:(function
      | '0' .. '9' | 'a' .. 'f' -> true
      | _ -> false)
  in
  match
    valid_hash
    && t.generation >= 0
    && P.Timestamp.compare t.updated_at t.created_at >= 0
    && (not (List.contains_dup t.output_ids ~compare:P.History.Id.compare))
    &&
    match t.status with
    | Deferred | Ready -> List.is_empty t.output_ids
    | Terminal (None, Invalidated) -> List.is_empty t.output_ids
    | Terminal (None, _) -> false
    | Assigned _ | Terminal (Some _, _) -> true
  with
  | true -> Ok ()
  | false -> Error (P.Error.invalid_request "invalid managed submission receipt")
;;

let create ~reference ~key ~request_sha256 ~generation ~history_id ~now =
  let t =
    { reference
    ; key
    ; request_sha256
    ; generation
    ; history_id
    ; created_at = now
    ; updated_at = now
    ; status = Deferred
    ; output_ids = []
    }
  in
  Result.map (validate t) ~f:(fun () -> t)
;;

let validate_transition ~previous next =
  let open Result.Let_syntax in
  let%bind () = validate next in
  let immutable =
    { next with
      status = previous.status
    ; output_ids = previous.output_ids
    ; updated_at = previous.updated_at
    }
  in
  let status_allowed =
    match previous.status, next.status with
    | Deferred, (Deferred | Ready | Assigned _ | Terminal (None, Invalidated)) -> true
    | Ready, (Ready | Assigned _ | Terminal (None, Invalidated)) -> true
    | Assigned before, Assigned after -> P.Id.Operation.equal before after
    | Assigned before, Terminal (Some after, _) -> P.Id.Operation.equal before after
    | Terminal _, Terminal _ -> equal previous next
    | _ -> false
  in
  match
    equal previous immutable
    && status_allowed
    && P.Timestamp.compare next.updated_at previous.updated_at >= 0
    && List.is_prefix
         next.output_ids
         ~prefix:previous.output_ids
         ~equal:P.History.Id.equal
  with
  | true -> Ok ()
  | false ->
    Error
      (P.Error.invalid_request
         "managed submission transition changes retained identity or outcome")
;;

let reconcile
      ~generation
      ~reference
      ~discarded
      ~adopted
      ~appended
      ~(operation : P.Operation.t option)
      ~terminals
      ~now
      t
  =
  match t.status with
  | Terminal _ -> t
  | Deferred | Ready | Assigned _ ->
    let status =
      match
        (not discarded)
        && Int.equal generation t.generation
        && Option.exists reference ~f:(D.Reference.equal t.reference)
      with
      | false ->
        Terminal
          ( (match t.status with
             | Assigned id -> Some id
             | _ -> None)
          , Invalidated )
      | true ->
        let ready =
          match t.status with
          | Ready -> true
          | Deferred -> adopted
          | _ -> false
        in
        (match ready, operation with
         | true, Some { kind = Turn _; id; _ } -> Assigned id
         | true, _ -> Ready
         | false, _ -> t.status)
    in
    let output_ids =
      match status, operation with
      | Assigned expected, Some operation when P.Id.Operation.equal expected operation.id
        ->
        List.fold appended ~init:t.output_ids ~f:(fun ids entry ->
          match entry.P.History.role, entry.kind with
          | Assistant, Message when not (List.mem ids entry.id ~equal:P.History.Id.equal)
            -> ids @ [ entry.id ]
          | _ -> ids)
      | _ -> t.output_ids
    in
    let status =
      match status with
      | Assigned id ->
        (match List.Assoc.find terminals id ~equal:P.Id.Operation.equal with
         | Some outcome -> Terminal (Some id, outcome)
         | None -> status)
      | _ -> status
    in
    (match
       equal_status status t.status
       && List.equal P.History.Id.equal output_ids t.output_ids
     with
     | true -> t
     | false -> { t with status; output_ids; updated_at = now })
;;
