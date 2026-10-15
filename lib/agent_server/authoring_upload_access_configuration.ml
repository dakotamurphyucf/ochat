open Core

module Grant = struct
  type operation =
    | Upload_write
    | Publish
    | Bundle_read
    | Rebind
  [@@deriving compare, equal, sexp]

  type t =
    { principal_id : Agent_protocol.Id.Principal.t
    ; authentication_kind : string
    ; operation : operation
    }
  [@@deriving compare, equal, sexp]
end

module Selection = struct
  type t =
    { principal_id : Agent_protocol.Id.Principal.t
    ; authentication_kind : string
    ; request : Authoring_project_access_configuration.Selection.request
    ; selected_environment : string
    ; target : Chatml.Chatml_compiler_contract.Target.t
    }
  [@@deriving sexp]

  let target_rank = function
    | Chatml.Chatml_compiler_contract.Target.One_off_v1 -> 0
    | Tool_v1 -> 1
    | Developer_tool_v1 -> 2
    | Moderator_v1 -> 3
    | Delegated_moderator_v1 -> 4
  ;;

  let compare left right =
    [%compare:
      Agent_protocol.Id.Principal.t
      * string
      * Authoring_project_access_configuration.Selection.request
      * string
      * int]
      ( left.principal_id
      , left.authentication_kind
      , left.request
      , left.selected_environment
      , target_rank left.target )
      ( right.principal_id
      , right.authentication_kind
      , right.request
      , right.selected_environment
      , target_rank right.target )
  ;;

  let equal left right = Int.equal (compare left right) 0
end

type mode =
  | Upload_only
  | Augment_core
[@@deriving compare, equal, sexp]

type t =
  { mode : mode
  ; grants : Grant.t list
  ; selections : Selection.t list
  }
[@@deriving compare, equal, sexp]

let max_grants = 256
let max_selections = 256
let max_text_bytes = 65_536

let principal_valid principal_id authentication_kind =
  Agent_protocol.Principal.create
    ~id:principal_id
    ~authentication_kind
    ~scopes:Agent_protocol.Scope.Set.empty
    ~attributes:[]
  |> Result.is_ok
;;

let request_valid = function
  | Authoring_project_access_configuration.Selection.Library { environment } ->
    Agent_protocol.Authoring.Context.Selection.create
      ~target:Agent_protocol.Authoring.Context.Selection.Library
      ~environment
    |> Result.is_ok
  | Entry { name; environment } ->
    Agent_protocol.Authoring.Context.Selection.create
      ~target:(Agent_protocol.Authoring.Context.Selection.Entry name)
      ~environment
    |> Result.is_ok
;;

let request_bytes = function
  | Authoring_project_access_configuration.Selection.Library { environment } ->
    Option.value_map environment ~default:0 ~f:String.length
  | Entry { name; environment } ->
    String.length name + Option.value_map environment ~default:0 ~f:String.length
;;

let validate t =
  let open Or_error.Let_syntax in
  let total_bytes = ref 0 in
  let charge bytes =
    if bytes < 0 || !total_bytes > max_text_bytes - bytes
    then Or_error.error_string "authoring upload access text limit"
    else (
      total_bytes := !total_bytes + bytes;
      Ok ())
  in
  let%bind () =
    if List.is_empty t.grants || List.length t.grants > max_grants
    then Or_error.error_string "authoring upload grant count"
    else Ok ()
  in
  let%bind () =
    if List.length t.selections > max_selections
    then Or_error.error_string "authoring upload selection count"
    else Ok ()
  in
  let%bind () =
    List.fold_result t.grants ~init:() ~f:(fun () grant ->
      let%bind () =
        charge
          (String.length (Agent_protocol.Id.Principal.to_string grant.principal_id)
           + String.length grant.authentication_kind)
      in
      if principal_valid grant.principal_id grant.authentication_kind
      then Ok ()
      else Or_error.error_string "invalid authoring upload principal")
  in
  let%bind () =
    match List.find_a_dup t.grants ~compare:Grant.compare with
    | None -> Ok ()
    | Some _ -> Or_error.error_string "duplicate authoring upload grant"
  in
  let%bind () =
    List.fold_result t.selections ~init:() ~f:(fun () selection ->
      let%bind () =
        charge
          (String.length (Agent_protocol.Id.Principal.to_string selection.principal_id)
           + String.length selection.authentication_kind
           + request_bytes selection.request
           + String.length selection.selected_environment)
      in
      if
        (not (principal_valid selection.principal_id selection.authentication_kind))
        || (not (request_valid selection.request))
        || not
             (Agent_protocol.Authoring.Context.Selection.create
                ~target:Agent_protocol.Authoring.Context.Selection.Library
                ~environment:(Some selection.selected_environment)
              |> Result.is_ok)
      then Or_error.error_string "invalid authoring upload selection"
      else Ok ())
  in
  let key (selection : Selection.t) =
    selection.principal_id, selection.authentication_kind, selection.request
  in
  let compare_selection left right =
    [%compare:
      Agent_protocol.Id.Principal.t
      * string
      * Authoring_project_access_configuration.Selection.request]
      (key left)
      (key right)
  in
  match List.find_a_dup t.selections ~compare:compare_selection with
  | None -> Ok ()
  | Some _ -> Or_error.error_string "duplicate authoring upload selection"
;;

let targets t =
  List.fold t.selections ~init:[] ~f:(fun acc selection ->
    if List.exists acc ~f:(Chatml.Chatml_compiler_contract.Target.equal selection.target)
    then acc
    else acc @ [ selection.target ])
;;
