open! Core
module P = Agent_protocol
module Delivery = Run_job_delivery
module Key = Delivery.Key

type t =
  | Missing
  | Null
  | Present of Delivery.t Map.M(Key).t

let empty = Missing

let entries = function
  | Missing | Null -> []
  | Present entries -> Map.data entries
;;

let find t key =
  match t with
  | Missing | Null -> None
  | Present entries -> Map.find entries key
;;

let invalid message = Error (P.Error.invalid_request message)

let fields = function
  | Missing -> []
  | Null -> [ "job_deliveries", `Null ]
  | Present entries ->
    [ "job_deliveries", `Array (Map.data entries |> List.map ~f:Delivery.to_jsonaf) ]
;;

let reserved_bytes t =
  List.fold (entries t) ~init:0 ~f:(fun total delivery ->
    total + Delivery.disposition_reserve_bytes delivery)
;;

let check_size t =
  let max_bytes = P.Run_limits.max_document_bytes - reserved_bytes t in
  if max_bytes <= 0
  then invalid "run job delivery bookkeeping reserve exhausted"
  else
    P.Json_codec.validate_limits
      ~max_bytes
      ~max_depth:P.Run_limits.max_depth
      (`Object (fields t))
;;

let add t delivery =
  let open Result.Let_syntax in
  let entries =
    match t with
    | Missing | Null -> Map.empty (module Key)
    | Present entries -> entries
  in
  let key = Delivery.key delivery in
  match Map.find entries key with
  | Some retained ->
    if Delivery.equal retained delivery
    then Ok t
    else invalid "run job occurrence identity already retained"
  | None ->
    let%bind () = P.Run_limits.check_count (Map.length entries + 1) in
    let next = Present (Map.set entries ~key ~data:delivery) in
    let%map () = check_size next in
    next
;;

let replace t delivery =
  let open Result.Let_syntax in
  match t with
  | Missing | Null -> invalid "run job occurrence replacement is absent"
  | Present entries ->
    let key = Delivery.key delivery in
    (match Map.find entries key with
     | None -> invalid "run job occurrence replacement identity is absent"
     | Some previous ->
       let%bind () = Delivery.validate_transition ~previous delivery in
       let next = Present (Map.set entries ~key ~data:delivery) in
       let%map () = check_size next in
       next)
;;

let retire_run t ~run_id ~reason =
  match t with
  | Missing | Null -> t
  | Present entries ->
    Present
      (Map.map entries ~f:(fun delivery ->
         if P.Id.Run.equal (Delivery.run_id delivery) run_id
         then Delivery.retire delivery ~reason
         else delivery))
;;

let enqueued_frame t ~frame =
  let matches =
    entries t
    |> List.filter ~f:(fun delivery ->
      match Delivery.disposition delivery with
      | Enqueued _ ->
        Chat_response.Background_delivery.equal (Delivery.frame delivery) frame
      | Pending | Claimed _ | Retired _ -> false)
  in
  match matches with
  | [] -> Ok None
  | [ delivery ] -> Ok (Some delivery)
  | _ :: _ :: _ ->
    invalid "terminal job frame belongs to more than one enqueued run occurrence"
;;

let of_field = function
  | None -> Ok Missing
  | Some `Null -> Ok Null
  | Some json ->
    let open Result.Let_syntax in
    let%bind () =
      P.Json_codec.validate_limits
        ~max_bytes:P.Run_limits.max_document_bytes
        ~max_depth:P.Run_limits.max_depth
        json
    in
    let%bind deliveries = P.Run_limits.list Delivery.of_jsonaf json in
    let%bind entries =
      match
        Map.of_alist
          (module Key)
          (List.map deliveries ~f:(fun delivery -> Delivery.key delivery, delivery))
      with
      | `Ok entries -> Ok entries
      | `Duplicate_key _ -> invalid "duplicate retained run job occurrence"
    in
    let next = Present entries in
    let%map () = check_size next in
    next
;;

let shape =
  Document_schema.Shape.nullable
    (Persistence_codec.array_shape_exn ~identity_field:"id" Delivery.shape)
;;

let validate_transition ~previous next =
  let open Result.Let_syntax in
  let%bind () = check_size next in
  match previous, next with
  | Missing, Missing | Null, Null -> Ok ()
  | (Missing | Null), Present _ -> Ok ()
  | Missing, Null | Null, Missing | Present _, (Missing | Null) ->
    invalid "run job occurrence field presence regressed"
  | Present previous, Present next ->
    let%bind () =
      Map.fold next ~init:(Ok ()) ~f:(fun ~key ~data:delivery checked ->
        let%bind () = checked in
        if Map.mem previous key
        then Ok ()
        else (
          match Delivery.disposition delivery with
          | Pending | Enqueued _ -> Ok ()
          | Claimed _ | Retired _ ->
            invalid "new run job occurrence cannot invent a prior claim"))
    in
    Map.fold previous ~init:(Ok ()) ~f:(fun ~key ~data:retained checked ->
      let%bind () = checked in
      match Map.find next key with
      | None -> invalid "immutable run job occurrence was dropped"
      | Some delivery -> Delivery.validate_transition ~previous:retained delivery)
;;
