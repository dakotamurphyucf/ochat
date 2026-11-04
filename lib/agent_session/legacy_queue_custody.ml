open! Core

module Row = struct
  type t =
    { manager : Chat_response.Moderator_manager.t
    ; source : Agent_protocol.Run_source.t
    ; entry : Moderator_queue.Entry.t
    ; continuation : Execution_custody.Continuation.t
    }

  let transfer t = Execution_custody.Continuation.transfer t.continuation
  let check t = Execution_custody.Continuation.check t.continuation
end

type t = Row.t list

let empty = []
let is_empty = List.is_empty

let retire t =
  List.iter t ~f:(fun (row : Row.t) ->
    Execution_custody.Continuation.retire row.continuation)
;;

let retain_facts t ~source ~metadata =
  match source, metadata with
  | Some source, Some metadata ->
    if Agent_protocol.Run_source.equal source (Moderator_queue.source metadata)
    then
      List.filter t ~f:(fun (row : Row.t) ->
        Agent_protocol.Run_source.equal row.source source
        && List.exists
             (Moderator_queue.entries metadata)
             ~f:(Moderator_queue.Entry.equal row.entry))
    else []
  | None, (Some _ | None) | Some _, None -> []
;;

let retain_runtime t ~manager =
  match manager with
  | None -> []
  | Some manager -> List.filter t ~f:(fun (row : Row.t) -> phys_equal row.manager manager)
;;

let matching t ~manager ~metadata =
  retain_facts t ~source:(Option.map metadata ~f:Moderator_queue.source) ~metadata
  |> retain_runtime ~manager
;;

(* Install is nonyielding and runs only after the actual checkpoint ACK. *)
let install previous ~incoming =
  List.iter previous ~f:(fun (row : Row.t) ->
    if not (List.mem incoming row ~equal:phys_equal)
    then Execution_custody.Continuation.retire row.continuation);
  incoming
;;

let find t ~manager ~source ~entry =
  List.find t ~f:(fun (row : Row.t) ->
    phys_equal row.manager manager
    && Agent_protocol.Run_source.equal row.source source
    && Moderator_queue.Entry.equal row.entry entry)
;;

let without t ~selected =
  List.filter t ~f:(fun (row : Row.t) -> not (Option.exists selected ~f:(phys_equal row)))
;;

(* On a partial staging failure every speculative loan is closed. The caller
   owns successful loans until it installs them or closes them before ACK. *)
let stage ~manager ~source ~entries ~parent =
  let staged = ref [] in
  let complete = ref false in
  Exn.protect
    ~finally:(fun () -> if not !complete then retire !staged)
    ~f:(fun () ->
      let open Result.Let_syntax in
      let%map () =
        List.fold_result entries ~init:() ~f:(fun () entry ->
          let%map admission = Execution_custody.Admission.transfer parent in
          staged
          := ({ manager; source; entry; continuation = Current admission } : Row.t)
             :: !staged)
      in
      complete := true;
      List.rev !staged)
;;

let candidate t ~manager ~metadata ~staged =
  let retained = matching t ~manager:(Some manager) ~metadata:(Some metadata) in
  let candidate = retained @ staged in
  let unique =
    List.length
      (List.dedup_and_sort candidate ~compare:(fun (a : Row.t) (b : Row.t) ->
         Int64.compare
           (Moderator_queue.Entry.ordinal a.entry)
           (Moderator_queue.Entry.ordinal b.entry)))
    = List.length candidate
  in
  if List.length candidate <= 256 && unique
  then Ok candidate
  else
    Error (Agent_protocol.Error.invalid_request "legacy queue custody limit or duplicate")
;;

(* Takes custody of the already staged completed-producer loan. No decoded
   producer/Entry or manager identity creates the supplied continuation. *)
let stage_delivery ~manager ~source ~entry ~continuation =
  [ ({ manager; source; entry; continuation } : Row.t) ]
;;

let validate_constructor_handoff t ~manager ~source ~ordinals =
  if
    List.for_all t ~f:(fun (row : Row.t) ->
      (not
         (List.mem ordinals (Moderator_queue.Entry.ordinal row.entry) ~equal:Int64.equal))
      || (phys_equal row.manager manager
          && Agent_protocol.Run_source.equal row.source source))
  then Ok ()
  else Error (Agent_protocol.Error.invalid_request "constructor queue custody changed")
;;

let without_constructor t ~manager ~source ~ordinals =
  List.filter t ~f:(fun (row : Row.t) ->
    not
      (phys_equal row.manager manager
       && Agent_protocol.Run_source.equal row.source source
       && List.mem ordinals (Moderator_queue.Entry.ordinal row.entry) ~equal:Int64.equal))
;;
