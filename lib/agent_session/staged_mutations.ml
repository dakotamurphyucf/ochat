open Core
module P = Agent_protocol

module type Value = sig
  module Id : sig
    type t [@@deriving compare, equal, hash, sexp]
  end

  type t

  val name : string
  val id : t -> Id.t
  val equal : t -> t -> bool
  val validate_staging : t -> (unit, P.Error.t) result
  val validate_transition : previous:t option -> t -> (unit, P.Error.t) result
end

module Make (Value : Value) = struct
  type entry =
    { receipt : int
    ; owner : P.Job.launch_owner
    ; previous : Value.t option
    ; next : Value.t
    ; mutable selected : bool
    }

  type t =
    { mutable next_receipt : int
    ; mutable entries : entry list
    }

  let create () = { next_receipt = 0; entries = [] }
  let is_empty t = List.is_empty t.entries
  let owned entry owner = P.Job.equal_launch_owner entry.owner owner
  let same_id entry id = Value.Id.equal (Value.id entry.next) id

  let conflict message =
    Error
      (P.Error.create Conflict ~message:(Value.name ^ " " ^ message) ~retryable:false ())
  ;;

  let reservations t =
    List.count t.entries ~f:(fun entry -> Option.is_none entry.previous)
  ;;

  let find t ~owner ~id =
    List.find_map t.entries ~f:(fun entry ->
      Option.some_if (owned entry owner && same_id entry id) entry.next)
  ;;

  let stage_with_validation ~validate_transition t ~owner ~previous ~next =
    let open Result.Let_syntax in
    let%bind () = Value.validate_staging next in
    let%bind () = validate_transition ~previous next in
    let%bind () =
      match List.find t.entries ~f:(fun entry -> same_id entry (Value.id next)) with
      | Some entry when not (owned entry owner) -> conflict "is staged by another owner"
      | Some entry when not (Option.equal Value.equal (Some entry.next) previous) ->
        conflict "mutation does not follow its provisional state"
      | _ -> Ok ()
    in
    match Int.equal t.next_receipt Int.max_value with
    | true ->
      Error
        (P.Error.create
           Resource_limit
           ~message:(Value.name ^ " receipts exhausted")
           ~retryable:false
           ())
    | false ->
      let receipt = t.next_receipt in
      t.next_receipt <- receipt + 1;
      t.entries <- { receipt; owner; previous; next; selected = false } :: t.entries;
      Ok receipt
  ;;

  let validate entries ~lookup ~validate_transition =
    let open Result.Let_syntax in
    let provisional = Hashtbl.create (module Value.Id) in
    List.fold_result entries ~init:[] ~f:(fun values entry ->
      let id = Value.id entry.next in
      let current =
        match Hashtbl.find provisional id with
        | Some value -> Some value
        | None -> lookup id
      in
      match Option.equal Value.equal current entry.previous with
      | false -> conflict "changed before its transaction committed"
      | true ->
        let%map () = validate_transition ~previous:current entry.next in
        Hashtbl.set provisional ~key:id ~data:entry.next;
        entry.next :: values)
    |> Result.map ~f:List.rev
  ;;

  let select_with_validation ~validate_transition t ~owner ~receipts ~lookup =
    let open Result.Let_syntax in
    let seen = Hash_set.create (module Int) in
    let%bind () =
      List.fold_result receipts ~init:() ~f:(fun () receipt ->
        match Hash_set.mem seen receipt with
        | true -> conflict "receipt selected more than once"
        | false ->
          Hash_set.add seen receipt;
          Ok ())
    in
    let entries =
      List.rev t.entries
      |> List.filter ~f:(fun entry ->
        owned entry owner && Hash_set.mem seen entry.receipt)
    in
    let%bind () =
      match
        List.equal Int.equal receipts (List.map entries ~f:(fun entry -> entry.receipt))
      with
      | true -> Ok ()
      | false -> conflict "receipts are foreign or out of execution order"
    in
    let%map _ = validate entries ~lookup ~validate_transition in
    List.iter t.entries ~f:(fun entry ->
      if owned entry owner then entry.selected <- Hash_set.mem seen entry.receipt)
  ;;

  let selected_with_validation ~validate_transition t ~owner ~lookup =
    List.rev t.entries
    |> List.filter ~f:(fun entry -> owned entry owner && entry.selected)
    |> validate ~lookup ~validate_transition
  ;;

  let abort t ~owner ~receipt =
    match List.find t.entries ~f:(fun entry -> Int.equal entry.receipt receipt) with
    | None -> Ok ()
    | Some entry when not (owned entry owner) ->
      conflict "receipt belongs to another owner"
    | Some entry ->
      (match List.find t.entries ~f:(fun entry -> owned entry owner) with
       | Some newest when phys_equal newest entry ->
         t.entries
         <- List.filter t.entries ~f:(fun candidate -> not (phys_equal candidate entry));
         Ok ()
       | _ -> conflict "rollback must discard newest receipt first")
  ;;

  let release_owner t ~owner =
    t.entries <- List.filter t.entries ~f:(fun entry -> not (owned entry owner))
  ;;

  let values t = List.map t.entries ~f:(fun entry -> entry.next)
  let abort_all t = t.entries <- []

  let stage t ~owner ~previous ~next =
    stage_with_validation
      ~validate_transition:Value.validate_transition
      t
      ~owner
      ~previous
      ~next
  ;;

  let select t ~owner ~receipts ~lookup =
    select_with_validation
      ~validate_transition:Value.validate_transition
      t
      ~owner
      ~receipts
      ~lookup
  ;;

  let selected t ~owner ~lookup =
    selected_with_validation
      ~validate_transition:Value.validate_transition
      t
      ~owner
      ~lookup
  ;;
end
