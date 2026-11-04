open! Core

type t =
  { authority : Agent_authority_scope.t
  ; live : bool Atomic.t
  }

let create actor =
  let live = Atomic.make true in
  { authority =
      Agent_authority_scope.create ~actor ~is_current:(fun () -> Atomic.get live)
  ; live
  }
;;

let actor t = Agent_authority_scope.actor t.authority

let transfer t =
  let live = Atomic.make true in
  Result.map
    (Agent_authority_scope.transfer t.authority ~is_current:(fun () -> Atomic.get live))
    ~f:(fun authority -> { authority; live })
;;

let retire t = Atomic.set t.live false
let same_origin t other = Agent_authority_scope.same_origin t.authority other.authority

module Admission = struct
  type custody = t

  type t =
    { custody : custody option
    ; admission_live : bool Atomic.t
    }

  let create custody = { custody; admission_live = Atomic.make true }
  let custody t = t.custody
  let close t = Atomic.set t.admission_live false

  let retire t =
    close t;
    Option.iter t.custody ~f:retire
  ;;

  let check t =
    if not (Atomic.get t.admission_live)
    then
      Error
        (Agent_protocol.Error.create
           Conflict
           ~message:"original execution admission has ended"
           ~retryable:false
           ())
    else (
      match t.custody with
      | None -> Ok ()
      | Some custody -> actor custody |> Result.map ~f:ignore)
  ;;

  let transfer t =
    let open Result.Let_syntax in
    let%bind () = check t in
    let%map custody =
      match t.custody with
      | None -> Ok None
      | Some custody -> Result.map (transfer custody) ~f:Option.some
    in
    create custody
  ;;

  module Status = struct
    type t =
      | Current
      | Closed
      | Authority_expired
  end

  let status t =
    if not (Atomic.get t.admission_live)
    then Status.Closed
    else (
      match t.custody with
      | None -> Status.Current
      | Some custody ->
        if Agent_authority_scope.is_current custody.authority
        then Status.Current
        else Status.Authority_expired)
  ;;
end

module Continuation = struct
  type t =
    | Current of Admission.t
    | Expired

  let transfer_completed admission =
    match Admission.status admission with
    | Closed | Authority_expired -> Ok Expired
    | Current ->
      (match Admission.transfer admission with
       | Ok child -> Ok (Current child)
       | Error failure ->
         (* A host's original guard may change between nonyielding checks. Only
            the independently checked negative fact attenuates this failure. *)
         (match Admission.status admission with
          | Closed | Authority_expired -> Ok Expired
          | Current -> Error failure))
  ;;

  let denied () =
    Agent_protocol.Error.create
      Permission_denied
      ~message:"original continuation admission expired"
      ~retryable:false
      ()
  ;;

  let transfer = function
    | Current admission -> Admission.transfer admission
    | Expired -> Error (denied ())
  ;;

  let check = function
    | Current admission -> Admission.check admission
    | Expired -> Error (denied ())
  ;;

  let retire = function
    | Current admission -> Admission.retire admission
    | Expired -> ()
  ;;
end
