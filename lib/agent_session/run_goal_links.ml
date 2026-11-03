open! Core
module P = Agent_protocol

module Key = struct
  include P.Id.Run
  include Comparator.Make (P.Id.Run)
end

type t =
  | Missing
  | Null
  | Present of Run_goal_link.t Map.M(Key).t

let empty = Missing
let invalid message = Error (P.Error.invalid_request message)

let entries = function
  | Missing | Null -> []
  | Present entries -> Map.data entries
;;

let find t key =
  match t with
  | Missing | Null -> None
  | Present entries -> Map.find entries key
;;

let fields = function
  | Missing -> []
  | Null -> [ "goal_links", `Null ]
  | Present entries ->
    [ "goal_links", `Array (Map.data entries |> List.map ~f:Run_goal_link.to_jsonaf) ]
;;

let check_size t =
  let open Result.Let_syntax in
  let%bind () = P.Run_limits.check_count (List.length (entries t)) in
  P.Extension_codec.validate_json
    ~max_bytes:P.Run_limits.max_document_bytes
    ~max_depth:P.Run_limits.max_depth
    (`Object (fields t))
;;

let add t link =
  let open Result.Let_syntax in
  let existing =
    match t with
    | Missing | Null -> Map.empty (module Key)
    | Present entries -> entries
  in
  let key = Run_goal_link.run_id link in
  match Map.find existing key with
  | Some retained ->
    if Run_goal_link.equal retained link
    then Ok t
    else invalid "Run is already permanently associated with a goal"
  | None ->
    let next = Present (Map.set existing ~key ~data:link) in
    let%map () = check_size next in
    next
;;

let of_field = function
  | None -> Ok Missing
  | Some `Null -> Ok Null
  | Some json ->
    let open Result.Let_syntax in
    let%bind () =
      P.Extension_codec.validate_json
        ~max_bytes:P.Run_limits.max_document_bytes
        ~max_depth:P.Run_limits.max_depth
        json
    in
    let%bind links = P.Run_limits.list Run_goal_link.of_jsonaf json in
    let%bind map =
      match
        Map.of_alist
          (module Key)
          (List.map links ~f:(fun link -> Run_goal_link.run_id link, link))
      with
      | `Ok map -> Ok map
      | `Duplicate_key _ -> invalid "duplicate Run goal association"
    in
    let t = Present map in
    let%map () = check_size t in
    t
;;

let shape =
  Document_schema.Shape.nullable
    (Persistence_codec.array_shape_exn ~identity_field:"run_id" Run_goal_link.shape)
;;

let validate_transition ~previous next =
  let open Result.Let_syntax in
  let%bind () = check_size next in
  let%bind () =
    match previous, next with
    | Missing, Missing | Null, Null | (Missing | Null | Present _), Present _ -> Ok ()
    | Missing, Null | Null, Missing | Present _, (Missing | Null) ->
      invalid "Run goal association field presence regressed"
  in
  List.fold_result (entries previous) ~init:() ~f:(fun () retained ->
    match find next (Run_goal_link.run_id retained) with
    | Some link when Run_goal_link.equal retained link -> Ok ()
    | None | Some _ -> invalid "immutable Run goal association disappeared or changed")
;;
