open! Core
module P = Agent_protocol

module Run_id = struct
  include P.Id.Run
  include Comparator.Make (P.Id.Run)
end

type binding =
  { scope : Run_admission.Scope.t
  ; attachment_id : P.Id.Attachment.t
  }

type t = binding Map.M(Run_id).t

let empty = Map.empty (module Run_id)
let find t id = Map.find t id
let scope t = t.scope
let attachment_id t = t.attachment_id

let add t ~(run : P.Run.t) ~scope ~attachment_id =
  if
    P.Id.Principal.equal run.principal_id (Run_admission.Scope.principal_id scope)
    && P.Invocation.equal_observer
         run.source.observer
         (Run_admission.Scope.observer scope)
  then (
    match Map.add t ~key:run.id ~data:{ scope; attachment_id } with
    | `Ok t -> Ok t
    | `Duplicate -> Error (P.Error.invalid_request "run already has a host authority"))
  else Error (P.Error.invalid_request "run does not match the host authority")
;;

let retain_current t ~index ~generation =
  match index with
  | None -> empty
  | Some index ->
    Map.filteri t ~f:(fun ~key ~data ->
      match Run_state.find index key with
      | None -> false
      | Some run ->
        Int.equal run.source.generation generation
        && P.Id.Principal.equal
             run.principal_id
             (Run_admission.Scope.principal_id data.scope)
        && P.Invocation.equal_observer
             run.source.observer
             (Run_admission.Scope.observer data.scope)
        &&
          (match run.lifecycle with
          | Admitted | Active | Waiting _ -> true
          | Terminal _ -> false))
;;
