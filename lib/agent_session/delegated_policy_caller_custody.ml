open! Core
module P = Agent_protocol

module Owner = struct
  type t =
    | Operation of P.Id.Operation.t
    | Invocation of P.Id.Invocation.t
    | Moderator_event of P.Id.Moderator_execution.t
  [@@deriving equal, sexp]
end

type t =
  { issuer : unit ref
  ; domain : int
  ; session_id : P.Id.Session.t
  ; generation : int
  ; owner : Owner.t
  ; admission : (Execution_custody.Admission.t option, P.Error.t) result
  ; governance : (Delegated_policy_governance_data.t, P.Error.t) result
  ; current : unit -> (unit, P.Error.t) result
  ; live : bool Atomic.t
  }

let error message = P.Error.create Conflict ~message ~retryable:false ()
let on_domain t = Int.equal t.domain (Stdlib.Domain.self () :> int)

let check_current t =
  let open Result.Let_syntax in
  let%bind () =
    if on_domain t
    then Ok ()
    else Error (error "delegated policy Caller belongs to another Actor domain")
  in
  let%bind () =
    if Atomic.get t.live
    then Ok ()
    else Error (error "delegated policy Caller extent has ended")
  in
  let%bind () = t.current () in
  let%bind admission = t.admission in
  Option.value_map admission ~default:(Ok ()) ~f:Execution_custody.Admission.check
;;

let close t = if on_domain t then Atomic.set t.live false

module Host = struct
  let capture
        ~issuer
        ~domain
        ~session_id
        ~generation
        ~owner
        ~admission
        ~governance
        ~check_current:current
    =
    if not (Int.equal domain (Stdlib.Domain.self () :> int))
    then Error (error "delegated policy Caller belongs to another Actor domain")
    else
      Ok
        { issuer
        ; domain
        ; session_id
        ; generation
        ; owner
        ; admission
        ; governance
        ; current
        ; live = Atomic.make true
        }
  ;;

  let restrict t ~check_current:guard =
    if not (on_domain t)
    then Error (error "delegated policy Caller belongs to another Actor domain")
    else
      Ok
        { t with
          current =
            (fun () ->
              let open Result.Let_syntax in
              let%bind () = t.current () in
              guard ())
        }
  ;;

  let check_owner t ~issuer ~session_id ~generation ~owner =
    let open Result.Let_syntax in
    let%bind () = check_current t in
    if
      phys_equal t.issuer issuer
      && P.Id.Session.equal t.session_id session_id
      && Int.equal t.generation generation
      && Owner.equal t.owner owner
    then Ok ()
    else Error (error "delegated policy Caller belongs to another actual execution")
  ;;

  let governance t ~issuer ~session_id ~generation ~owner =
    let%bind.Result () = check_owner t ~issuer ~session_id ~generation ~owner in
    t.governance
  ;;

  let transfer t =
    let open Result.Let_syntax in
    let%bind () = check_current t in
    let%bind admission = t.admission in
    match admission with
    | None -> Ok None
    | Some admission ->
      Result.map (Execution_custody.Admission.transfer admission) ~f:Option.some
  ;;
end
