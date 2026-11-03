open! Core
module P = Agent_protocol
module I = Instruction_content
module M = Instruction_selection_record
module C = Instruction_capture
module Command = Instruction_command
module R = Command.Receipt
module D = Document_schema
module X = Persistence_codec
module Carrier = D.Extension_carrier
module Capacity = Instruction_selection_capacity

type t = M.t Carrier.t

let limits = Capacity.limits
let empty = Carrier.of_authored_value M.empty
let value (t : t) : M.t = Carrier.value t
let revision t = (value t).revision
let project t = M.project (value t)
let skills t = M.skills (value t)
let captures t = (value t).captures
let receipts t = (value t).receipts
let invalid message = Error (P.Error.invalid_request message)
let conflict message = Error (P.Error.create Conflict ~message ~retryable:false ())

let document result =
  Result.map_error result ~f:(fun error ->
    P.Error.invalid_request (Sexp.to_string_hum (D.Error.sexp_of_t error)))
;;

let codec =
  X.codec_exn
    ~version:1
    ~limits
    ~kind:"ochat.instruction-selection"
    ~shape:M.shape
    ~decode:M.of_json
    ~encode:(fun value ->
      let%map.Result () = M.validate value in
      M.to_json value)
;;

(* This child owns its complete named document and preservation carrier.
   Outer session/delta codecs must not split away its unknown fields before
   calling of_jsonaf. Continuity remains enforced by validate_update/adopt. *)
let shape = D.Shape.value

let to_jsonaf t =
  let open Result.Let_syntax in
  let%bind encoded = D.Domain_codec.encode codec t |> document in
  let raw = D.Document.json encoded in
  let%map () = Capacity.validate ~active:(M.active (value t)) raw ~limits in
  raw
;;

let of_jsonaf raw =
  let open Result.Let_syntax in
  let%bind doc = D.Document.inspect raw ~limits |> document in
  let%bind carrier = D.Domain_codec.decode codec doc |> document in
  let%map () = Capacity.validate ~active:(M.active (value carrier)) raw ~limits in
  carrier
;;

let sexp_of_t t =
  match to_jsonaf t with
  | Ok raw -> Sexp.Atom (Jsonaf.to_string raw)
  | Error error -> raise_s [%sexp (error : P.Error.t)]
;;

let t_of_sexp sexp =
  match sexp with
  | Sexp.Atom text ->
    (match D.Json.decode ~limits text with
     | Error error ->
       Sexplib.Conv.of_sexp_error (Sexp.to_string_hum (D.Error.sexp_of_t error)) sexp
     | Ok raw ->
       (match of_jsonaf raw with
        | Ok t -> t
        | Error error -> Sexplib.Conv.of_sexp_error error.message sexp))
  | Sexp.List _ ->
    Sexplib.Conv.of_sexp_error "instruction selection must be encoded JSON" sexp
;;

let summary t =
  let state = value t in
  P.Instruction_status.create
    ~revision:state.revision
    ~project_selected:(Option.is_some state.project)
    ~project_files:(List.length (project t))
    ~skills:(List.length state.skills)
    ~retained_captures:(List.length state.captures)
    ~retained_receipts:(List.length state.receipts)
  |> function
  | Ok summary -> summary
  | Error error -> raise_s [%sexp (error : P.Error.t)]
;;

let validate t ~session_id ~generation =
  let open Result.Let_syntax in
  let%bind () = M.validate (value t) in
  List.fold_result (receipts t) ~init:() ~f:(fun () receipt ->
    if
      P.Id.Session.equal receipt.R.session_id session_id
      && receipt.generation <= generation
    then Ok ()
    else invalid "instruction receipt belongs to another session or future generation")
;;

let validate_transition previous incoming =
  let open Result.Let_syntax in
  let old = value previous
  and next = value incoming in
  let%bind () = M.validate next in
  let retained old next ~equal =
    List.length old <= List.length next
    && List.equal equal old (List.take next (List.length old))
  in
  let capture_equal a b = D.Json.equal (C.to_json a) (C.to_json b) in
  let payloads_retained =
    Map.for_alli old.pool ~f:(fun ~key ~data ->
      Option.exists (Map.find next.pool key) ~f:(fun incoming ->
        String.equal
          (Agent_store.Prompt_program_bundle.Payload.contents data)
          (Agent_store.Prompt_program_bundle.Payload.contents incoming)))
  in
  if
    Int64.(next.revision < old.revision)
    || (not (retained old.receipts next.receipts ~equal:R.equal))
    || (not (retained old.captures next.captures ~equal:capture_equal))
    || not payloads_retained
  then
    conflict "captured instructions and immutable receipts cannot be discarded or changed"
  else if
    Int64.equal old.revision next.revision
    && not (D.Json.equal (M.to_json old) (M.to_json next))
  then conflict "instruction selection changed without a fresh receipt"
  else Ok ()
;;

let adopt previous ~incoming =
  let open Result.Let_syntax in
  let%bind () = validate_transition previous incoming in
  let%bind carrier = D.Domain_codec.adopt codec ~previous ~incoming |> document in
  let%map _ = to_jsonaf carrier in
  carrier
;;

let validate_update previous ~incoming =
  let open Result.Let_syntax in
  let%bind preserved = adopt previous ~incoming in
  let%bind expected = to_jsonaf preserved in
  let%bind actual = to_jsonaf incoming in
  if D.Json.equal expected actual
  then Ok ()
  else conflict "instruction update discarded compatible unknown data"
;;

type decision =
  | Retained of R.t
  | Commit of
      { selection : t
      ; receipt : R.t
      }

let prepare t ~request ~principal_id ~session_id ~generation ~now =
  let open Result.Let_syntax in
  let%bind () = validate t ~session_id ~generation in
  let%bind _ = P.Id.Principal.of_json (P.Id.Principal.to_json principal_id) in
  let state = value t in
  let retained =
    List.find state.receipts ~f:(fun receipt ->
      P.Id.Principal.equal receipt.R.principal_id principal_id
      && P.Idempotency_key.equal receipt.key (Command.key request))
  in
  match retained with
  | Some receipt ->
    if String.equal receipt.request_sha256 (Command.digest request)
    then Ok (Retained receipt)
    else conflict "instruction command key has another input"
  | None ->
    if
      (not (P.Id.Session.equal (Command.session_id request) session_id))
      || Command.generation request <> generation
      || not (Int64.equal (Command.expected_revision request) state.revision)
    then conflict "instruction session, generation or selection frontier changed"
    else if Int64.equal state.revision Int64.max_value
    then conflict "instruction receipt frontier exhausted"
    else (
      let capture, project, skills =
        match Command.change request with
        | Command.Change.Replace_project capture ->
          Some capture, Some (C.id capture), state.skills
        | Replace_skills capture ->
          ( Some capture
          , state.project
          , List.map (C.loaded capture) ~f:(fun loaded ->
              { M.capture = C.id capture
              ; file = C.file_key (I.Observed.id (I.Loaded.observed loaded))
              }) )
        | Remove_project -> None, None, state.skills
        | Remove_skills ids ->
          let removed = String.Set.of_list (List.map ids ~f:C.file_key) in
          ( None
          , state.project
          , List.filter state.skills ~f:(fun reference ->
              not (Set.mem removed reference.M.file)) )
        | Clear -> None, None, []
      in
      let captures, pool =
        match capture with
        | None -> state.captures, state.pool
        | Some capture ->
          if
            List.exists state.captures ~f:(fun old ->
              String.equal (C.id old) (C.id capture))
          then state.captures, state.pool
          else
            ( state.captures @ [ capture ]
            , List.fold (C.payloads capture) ~init:state.pool ~f:(fun pool payload ->
                Map.set
                  pool
                  ~key:(Agent_store.Prompt_program_bundle.Payload.sha256 payload)
                  ~data:payload) )
      in
      let%bind receipt =
        R.create
          ~principal_id
          ~request
          ~revision:(Int64.succ state.revision)
          ~accepted_at:now
      in
      let next : M.t =
        { revision = receipt.revision
        ; project
        ; skills
        ; captures
        ; pool
        ; receipts = state.receipts @ [ receipt ]
        }
      in
      let%bind () = M.validate next in
      let selection = Carrier.with_value t next in
      let%map _ = to_jsonaf selection in
      Commit { selection; receipt })
;;

let validate_host_capacity t raw ~limits =
  Capacity.validate ~active:(M.active (value t)) raw ~limits
;;

let remaining_host_limits t limits =
  Capacity.remaining_limits ~active:(M.active (value t)) limits
;;
