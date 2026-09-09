open Core
module P = Agent_protocol

type capacity =
  { publish : unit -> unit
  ; abort : unit -> unit
  }

type entry =
  { mutable job : P.Job.t
  ; capacity : capacity
  ; mutable selected : bool
  }

type t = entry list ref

let create () = ref []
let owner entry = Option.map entry.job.launch ~f:(fun launch -> launch.owner)

let owned entry expected =
  Option.exists (owner entry) ~f:(P.Job.equal_launch_owner expected)
;;

let invalid message = Error (P.Error.invalid_request message)

let stage t ~job ~capacity =
  match job.P.Job.launch with
  | None -> invalid "staged job requires an owning launch"
  | Some _ ->
    (match List.exists !t ~f:(fun entry -> P.Id.Job.equal entry.job.id job.id) with
     | true -> invalid "job is already staged"
     | false ->
       t := { job; capacity; selected = false } :: !t;
       Ok ())
;;

let contains t ~owner ~id =
  List.exists !t ~f:(fun entry -> owned entry owner && P.Id.Job.equal entry.job.id id)
;;

let select t ~owner ~ids =
  let open Result.Let_syntax in
  let seen = Hash_set.create (module String) in
  let%map () =
    List.fold_result ids ~init:() ~f:(fun () id ->
      let key = P.Id.Job.to_string id in
      match Hash_set.mem seen key, contains t ~owner ~id with
      | true, _ -> invalid "job start occurs more than once in the transaction"
      | false, false -> invalid "job start does not belong to this transaction"
      | false, true ->
        Hash_set.add seen key;
        Ok ())
  in
  List.iter !t ~f:(fun entry ->
    if owned entry owner
    then entry.selected <- Hash_set.mem seen (P.Id.Job.to_string entry.job.id))
;;

let selected t ~owner =
  List.filter_map !t ~f:(fun entry ->
    match owned entry owner && entry.selected with
    | true -> Some entry.job
    | false -> None)
  |> List.rev
;;

let retire t ~matches ~commit =
  let retiring, remaining = List.partition_tf !t ~f:matches in
  t := remaining;
  Eio.Cancel.protect (fun () ->
    List.iter (List.rev retiring) ~f:(fun entry ->
      match commit && entry.selected, entry.job.status with
      | true, Queued -> entry.capacity.publish ()
      | _ -> entry.capacity.abort ()))
;;

let commit t ~owner = retire t ~matches:(fun entry -> owned entry owner) ~commit:true

let abort_owner t ~owner =
  retire t ~matches:(fun entry -> owned entry owner) ~commit:false
;;

let abort_all t = retire t ~matches:(fun _ -> true) ~commit:false

let abort t ~owner ~id =
  match List.find !t ~f:(fun entry -> P.Id.Job.equal entry.job.id id) with
  | None -> Ok ()
  | Some entry when not (owned entry owner) ->
    invalid "job reservation belongs to another transaction"
  | Some entry ->
    retire t ~matches:(phys_equal entry) ~commit:false;
    Ok ()
;;

let find_entry t ~owner ~id =
  match List.find !t ~f:(fun entry -> P.Id.Job.equal entry.job.id id) with
  | Some entry when not (owned entry owner) ->
    invalid "job reservation belongs to another transaction"
  | entry -> Ok entry
;;

let find t ~owner ~id =
  Result.map (find_entry t ~owner ~id) ~f:(Option.map ~f:(fun entry -> entry.job))
;;

let cancel t ~owner ~id ~now =
  let open Result.Let_syntax in
  let%map entry = find_entry t ~owner ~id in
  Option.map entry ~f:(fun entry ->
    (match entry.job.status with
     | Queued ->
       entry.job
       <- { entry.job with
            status = Cancelled
          ; completed_at = Some now
          ; result = Some (P.Completion.to_json (Cancelled "cancelled before launch"))
          };
       Eio.Cancel.protect entry.capacity.abort
     | _ -> ());
    entry.job)
;;
