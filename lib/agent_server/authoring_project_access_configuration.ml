open Core

module Read_grant = struct
  type t =
    { root : string
    ; principal_id : Agent_protocol.Id.Principal.t
    ; authentication_kind : string
    }
  [@@deriving compare, equal, sexp]
end

module Selection = struct
  type request =
    | Library of { environment : string option }
    | Entry of
        { name : string
        ; environment : string option
        }
  [@@deriving compare, equal, sexp]

  type t =
    { root : string
    ; project : string
    ; request : request
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
    [%compare: string * string * request * int]
      (left.root, left.project, left.request, target_rank left.target)
      (right.root, right.project, right.request, target_rank right.target)
  ;;

  let equal left right = Int.equal (compare left right) 0
end

type t =
  { read_grants : Read_grant.t list
  ; selections : Selection.t list
  }
[@@deriving compare, equal, sexp]

let max_grants = 256
let max_selections = 256
let max_text_bytes = 65_536

let validate t ~project_sources =
  let open Or_error.Let_syntax in
  let%bind () = Project_source_configuration.validate project_sources in
  let total_bytes = ref 0 in
  let count_and_charge rows ~limit ~charge =
    let rec loop count = function
      | [] -> Ok ()
      | row :: rest ->
        if count >= limit
        then Or_error.error_string "authoring access row limit"
        else (
          let row_bytes = charge row in
          if row_bytes < 0 || !total_bytes > max_text_bytes - row_bytes
          then Or_error.error_string "authoring access text limit"
          else (
            total_bytes := !total_bytes + row_bytes;
            loop (count + 1) rest))
    in
    loop 0 rows
  in
  let%bind () =
    count_and_charge t.read_grants ~limit:max_grants ~charge:(fun grant ->
      String.length grant.root
      + String.length (Agent_protocol.Id.Principal.to_string grant.principal_id)
      + String.length grant.authentication_kind)
  in
  let%bind () =
    count_and_charge t.selections ~limit:max_selections ~charge:(fun selected ->
      String.length selected.root
      + String.length selected.project
      +
      match selected.request with
      | Library { environment } ->
        Option.value_map environment ~default:0 ~f:String.length
      | Entry { name; environment } ->
        String.length name + Option.value_map environment ~default:0 ~f:String.length)
  in
  if List.is_empty t.read_grants || List.is_empty t.selections
  then Or_error.error_string "authoring access needs grants and selections"
  else (
    let roots = project_sources.external_roots in
    let valid_root root = List.mem roots root ~equal:String.equal in
    let%bind () =
      List.fold_result t.read_grants ~init:() ~f:(fun () grant ->
        if not (valid_root grant.root)
        then Or_error.error_string "authoring grant names an unknown root"
        else
          Agent_protocol.Principal.create
            ~id:grant.principal_id
            ~authentication_kind:grant.authentication_kind
            ~scopes:Agent_protocol.Scope.Set.empty
            ~attributes:[]
          |> Result.map ~f:(fun _ -> ())
          |> Result.map_error ~f:(fun _ -> Error.of_string "invalid authentication kind"))
    in
    let%bind () =
      match List.find_a_dup t.read_grants ~compare:Read_grant.compare with
      | None -> Ok ()
      | Some _ -> Or_error.error_string "duplicate authoring read grant"
    in
    let%bind () =
      List.fold_result t.selections ~init:() ~f:(fun () selected ->
        if not (valid_root selected.root)
        then Or_error.error_string "authoring selection names an unknown root"
        else if
          not
            (List.exists t.read_grants ~f:(fun grant ->
               String.equal grant.root selected.root))
        then Or_error.error_string "authoring selection has no read grant"
        else (
          let%bind _ = Source.Portable_path.create selected.project in
          let target, environment =
            match selected.request with
            | Library { environment } ->
              Agent_protocol.Authoring.Context.Selection.Library, environment
            | Entry { name; environment } ->
              Agent_protocol.Authoring.Context.Selection.Entry name, environment
          in
          Agent_protocol.Authoring.Context.Selection.create ~target ~environment
          |> Result.map ~f:(fun _ -> ())
          |> Result.map_error ~f:(fun _ -> Error.of_string "invalid authoring selection")))
    in
    let key (selected : Selection.t) =
      selected.root, selected.project, selected.request
    in
    match
      List.find_a_dup t.selections ~compare:(fun left right ->
        [%compare: string * string * Selection.request] (key left) (key right))
    with
    | None -> Ok ()
    | Some _ -> Or_error.error_string "duplicate authoring project selection")
;;
