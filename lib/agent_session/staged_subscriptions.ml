open Core
module P = Agent_protocol
module S = P.Subscription

type limits =
  { max_active : int
  ; max_retained : int
  ; default_lifetime_ms : int
  ; max_lifetime_ms : int
  }

let default_limits =
  { max_active = 64
  ; max_retained = 4096
  ; default_lifetime_ms = 3_600_000
  ; max_lifetime_ms = 86_400_000
  }
;;

let validate_limits limits =
  match
    limits.max_active > 0
    && limits.max_active <= 1024
    && limits.max_retained >= limits.max_active
    && limits.default_lifetime_ms > 0
    && limits.default_lifetime_ms <= limits.max_lifetime_ms
    && limits.max_lifetime_ms <= 86_400_000
  with
  | true -> Ok ()
  | false -> Error (P.Error.invalid_request "invalid subscription limits")
;;

type entry =
  { receipt : int
  ; owner : P.Job.launch_owner
  ; previous : S.t option
  ; next : S.t
  ; mutable selected : bool
  }

type t =
  { mutable next_receipt : int
  ; mutable entries : entry list
  }

let create () = { next_receipt = 0; entries = [] }
let is_empty t = List.is_empty t.entries
let owned entry owner = P.Job.equal_launch_owner entry.owner owner
let same_id entry id = P.Id.Subscription.equal entry.next.context.id id
let conflict message = Error (P.Error.create Conflict ~message ~retryable:false ())
let reservations t = List.count t.entries ~f:(fun entry -> Option.is_none entry.previous)

let find t ~owner ~id =
  List.find_map t.entries ~f:(fun entry ->
    Option.some_if (owned entry owner && same_id entry id) entry.next)
;;

let stage t ~owner ~previous ~(next : S.t) =
  let open Result.Let_syntax in
  let%bind () =
    match next.context.source with
    | None -> conflict "subscription has no creating moderator source"
    | Some _ -> Ok ()
  in
  let%bind () = S.validate_transition ~previous next in
  let%bind () =
    match List.find t.entries ~f:(fun entry -> same_id entry next.context.id) with
    | Some entry when not (owned entry owner) ->
      conflict "subscription is staged by another owner"
    | Some entry when not (Option.equal S.equal (Some entry.next) previous) ->
      conflict "subscription mutation does not follow its provisional state"
    | _ -> Ok ()
  in
  match t.next_receipt = Int.max_value with
  | true ->
    Error
      (P.Error.create
         Resource_limit
         ~message:"subscription receipts exhausted"
         ~retryable:false
         ())
  | false ->
    let receipt = t.next_receipt in
    t.next_receipt <- receipt + 1;
    t.entries <- { receipt; owner; previous; next; selected = false } :: t.entries;
    Ok receipt
;;

let validate entries ~lookup =
  let open Result.Let_syntax in
  let provisional = Hashtbl.create (module P.Id.Subscription) in
  List.fold_result entries ~init:[] ~f:(fun values entry ->
    let id = entry.next.context.id in
    let current =
      match Hashtbl.find provisional id with
      | Some value -> Some value
      | None -> lookup id
    in
    match Option.equal S.equal current entry.previous with
    | false -> conflict "subscription changed before its transaction committed"
    | true ->
      let%map () = S.validate_transition ~previous:current entry.next in
      Hashtbl.set provisional ~key:id ~data:entry.next;
      entry.next :: values)
  |> Result.map ~f:List.rev
;;

let select t ~owner ~receipts ~lookup =
  let open Result.Let_syntax in
  let seen = Hash_set.create (module Int) in
  let%bind () =
    List.fold_result receipts ~init:() ~f:(fun () receipt ->
      match Hash_set.mem seen receipt with
      | true -> conflict "subscription receipt selected more than once"
      | false ->
        Hash_set.add seen receipt;
        Ok ())
  in
  let entries =
    List.rev t.entries
    |> List.filter ~f:(fun entry -> owned entry owner && Hash_set.mem seen entry.receipt)
  in
  let%bind () =
    match
      List.equal Int.equal receipts (List.map entries ~f:(fun entry -> entry.receipt))
    with
    | true -> Ok ()
    | false -> conflict "subscription receipts are foreign or out of execution order"
  in
  let%map _ = validate entries ~lookup in
  List.iter t.entries ~f:(fun entry ->
    if owned entry owner then entry.selected <- Hash_set.mem seen entry.receipt)
;;

let selected t ~owner ~lookup =
  List.rev t.entries
  |> List.filter ~f:(fun entry -> owned entry owner && entry.selected)
  |> validate ~lookup
;;

let abort t ~owner ~receipt =
  match List.find t.entries ~f:(fun entry -> Int.equal entry.receipt receipt) with
  | None -> Ok ()
  | Some entry when not (owned entry owner) ->
    conflict "subscription receipt belongs to another owner"
  | Some entry ->
    (match List.find t.entries ~f:(fun entry -> owned entry owner) with
     | Some newest when phys_equal newest entry ->
       t.entries
       <- List.filter t.entries ~f:(fun candidate -> not (phys_equal candidate entry));
       Ok ()
     | _ -> conflict "subscription rollback must discard newest receipt first")
;;

let release_owner t ~owner =
  t.entries <- List.filter t.entries ~f:(fun entry -> not (owned entry owner))
;;

let abort_all t = t.entries <- []
