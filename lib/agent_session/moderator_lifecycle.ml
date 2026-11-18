open! Core
module P = Agent_protocol
module J = P.Json_codec
module X = Persistence_codec

type t =
  { source : P.Run_source.t
  ; execution_id : P.Id.Moderator_execution.t
  }
[@@deriving equal]

let source t = t.source
let execution_id t = t.execution_id

let create ~source ~execution_id =
  let open Result.Let_syntax in
  let%bind () = P.Run_source.validate source in
  let%map execution_id =
    P.Id.Moderator_execution.of_json (P.Id.Moderator_execution.to_json execution_id)
  in
  { source; execution_id }
;;

let lifecycle_phase = function
  | P.Moderator_execution.Session_start | Session_resume -> true
  | Turn_start
  | Message_appended
  | Pre_tool_call
  | Post_tool_response
  | Turn_end
  | Internal_event -> false
;;

let validate t ~session_id ~generation ~current_source ~executions =
  let open Result.Let_syntax in
  let%bind _ = create ~source:t.source ~execution_id:t.execution_id in
  let invalid () =
    Error
      (P.Error.invalid_request
         "lifecycle pointer differs from its current source or receipt")
  in
  if
    not
      (Int.equal generation t.source.generation
       && P.Run_source.equal t.source current_source)
  then invalid ()
  else (
    match
      List.find executions ~f:(fun e ->
        P.Id.Moderator_execution.equal e.P.Moderator_execution.context.id t.execution_id)
    with
    | None -> invalid ()
    | Some execution ->
      let%bind () = P.Moderator_execution.validate execution in
      let c = execution.P.Moderator_execution.context in
      if
        P.Id.Session.equal c.session_id session_id
        && Int.equal c.generation generation
        && P.Invocation.equal_observer c.source t.source.observer
        && lifecycle_phase c.phase
      then Ok ()
      else invalid ())
;;

let to_json t =
  `Object
    [ "source", P.Run_source.to_json t.source
    ; "execution_id", P.Id.Moderator_execution.to_json t.execution_id
    ]
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind fields = J.fields json in
  let%bind source = J.required_as fields "source" P.Run_source.of_json in
  let%bind execution_id =
    J.required_as fields "execution_id" P.Id.Moderator_execution.of_json
  in
  create ~source ~execution_id
;;

let sexp_of_t t = Jsonaf.sexp_of_t (to_json t)

let t_of_sexp sexp =
  match of_json (Jsonaf.t_of_sexp sexp) with
  | Ok t -> t
  | Error e -> Sexplib.Conv.of_sexp_error e.P.Error.message sexp
;;

let shape =
  X.shape_exn
    [ ( "source"
      , X.fields_shape
          [ "script_id"; "source_sha256"; "generation"; "installation_epoch" ] )
    ; "execution_id", Document_schema.Shape.value
    ]
;;

module Field = struct
  type pointer = t [@@deriving equal, sexp]

  type t =
    | Missing
    | Null
    | Present of pointer
  [@@deriving equal, sexp]

  let empty = Missing

  let value = function
    | Missing | Null -> None
    | Present p -> Some p
  ;;

  let present p = Present p

  let clear = function
    | Missing -> Missing
    | Null | Present _ -> Null
  ;;

  let validate_transition ~previous incoming =
    match previous, incoming with
    | Missing, Null | Null, Missing | Present _, Missing ->
      Error (P.Error.invalid_request "lifecycle field presence cannot be rewritten")
    | Missing, (Missing | Present _)
    | Null, (Null | Present _)
    | Present _, (Null | Present _) -> Ok ()
  ;;

  let fields = function
    | Missing -> []
    | Null -> [ "moderator_lifecycle", `Null ]
    | Present p -> [ "moderator_lifecycle", to_json p ]
  ;;

  let of_field = function
    | None -> Ok Missing
    | Some `Null -> Ok Null
    | Some json -> Result.map (of_json json) ~f:(fun p -> Present p)
  ;;
end
