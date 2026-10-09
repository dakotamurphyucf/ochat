open! Core
module J = Json_codec

type t =
  | Job of Job_result_reference.t
  | Operation of
      { operation_id : Id.Operation.t
      ; generation : int
      ; history_ids : History.Id.t list
      ; revision : int64
      }

let equal left right =
  match left, right with
  | Job left, Job right -> Job_result_reference.equal left right
  | Operation left, Operation right ->
    Id.Operation.equal left.operation_id right.operation_id
    && Int.equal left.generation right.generation
    && List.equal History.Id.equal left.history_ids right.history_ids
    && Int64.equal left.revision right.revision
  | Job _, Operation _ | Operation _, Job _ -> false
;;

let of_job_result result =
  Result.map (Job_result_reference.validate result) ~f:(fun () -> Job result)
;;

let of_job job = Result.map (Job_result_reference.of_job job) ~f:(fun value -> Job value)

let validate = function
  | Job result -> Job_result_reference.validate result
  | Operation t ->
    let open Result.Let_syntax in
    let%bind () = Run_limits.check_count (List.length t.history_ids) in
    let%bind () =
      Extension_codec.validate_id Id.Operation.to_json Id.Operation.of_json t.operation_id
    in
    let%bind () =
      List.fold_result t.history_ids ~init:() ~f:(fun () id ->
        Extension_codec.validate_id History.Id.to_json History.Id.of_json id)
    in
    if
      t.generation < 0
      || Int64.(t.revision < 0L)
      || List.is_empty t.history_ids
      || List.contains_dup t.history_ids ~compare:History.Id.compare
    then Error (Protocol_error.invalid_request "invalid operation run result reference")
    else Ok ()
;;

let operation ~operation_id ~generation ~history_ids ~revision =
  let t = Operation { operation_id; generation; history_ids; revision } in
  Result.map (validate t) ~f:(fun () -> t)
;;

let to_json = function
  | Job result ->
    `Object [ "kind", `String "job"; "result", Job_result_reference.to_json result ]
  | Operation t ->
    `Object
      [ "kind", `String "operation"
      ; "operation_id", Id.Operation.to_json t.operation_id
      ; "generation", `Number (Int.to_string t.generation)
      ; "history_ids", `Array (List.map t.history_ids ~f:History.Id.to_json)
      ; "revision", `String (Int64.to_string t.revision)
      ]
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind () =
    Extension_codec.validate_json
      ~max_bytes:Run_limits.max_document_bytes
      ~max_depth:Run_limits.max_depth
      json
  in
  let%bind f = J.fields json in
  let%bind kind = J.required_as f "kind" J.string in
  match kind with
  | "job" ->
    let%map result = J.required_as f "result" Job_result_reference.of_json in
    Job result
  | "operation" ->
    let%bind operation_id = J.required_as f "operation_id" Id.Operation.of_json in
    let%bind generation =
      J.required_as f "generation" (J.bounded_int ~min:0 ~max:Int.max_value)
    in
    let%bind history_ids =
      J.required_as f "history_ids" (Run_limits.list History.Id.of_json)
    in
    let%bind revision = J.required_as f "revision" History.Content_revision.of_json in
    operation
      ~operation_id
      ~generation
      ~history_ids
      ~revision:(History.Content_revision.to_int64 revision)
  | _ -> Error (Protocol_error.invalid_request "unsupported run result reference")
;;

let sexp_of_t t = Jsonaf.sexp_of_t (to_json t)

let t_of_sexp sexp =
  match of_json (Jsonaf.t_of_sexp sexp) with
  | Ok t -> t
  | Error e -> Sexplib.Conv.of_sexp_error e.message sexp
;;
