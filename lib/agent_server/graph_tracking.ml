open! Core
module A = Agent_session.Session_actor
module L = Agent_session.Inference_ledger
module R = Inference_runtime

module Upstream = struct
  type t =
    { new_preparation_id : unit -> string
    ; on_admitted :
        scope:Transcript.Scope.t
        -> accounting_id:Inference.Observation.Observation_id.t
        -> unit
    ; on_attempt : R.Attempt.t -> unit
    ; on_observation : Inference.Observation.t -> unit
    ; on_completion : Inference_client.Completion.t -> unit
    }
end

exception Rejected of Agent_protocol.Error.t

type t =
  { actor : A.t
  ; run_preparation : Agent_session.Run_preparation.t option ref
  ; owner : A.Inference_owner.t
  ; upstream : Upstream.t
  ; mutable routing : L.Handle.t Map.M(Transcript.Scope.Key).t
  ; mutable finished : bool
  }

let require = function
  | Ok value -> value
  | Error error -> raise (Rejected error)
;;

let create ?(run_preparation = ref None) actor ~source ~upstream =
  Eio.Cancel.protect (fun () ->
    Result.map (A.open_inference_owner actor ~source) ~f:(fun owner ->
      { actor
      ; run_preparation
      ; owner
      ; upstream
      ; routing = Map.empty (module Transcript.Scope.Key)
      ; finished = false
      }))
;;

let route t scope =
  match Map.find t.routing (Transcript.Scope.key scope) with
  | Some handle when Transcript.Scope.equal scope (L.Handle.scope handle) -> handle
  | Some _ | None ->
    raise
      (Rejected
         (Agent_protocol.Error.create
            Conflict
            ~message:"inference callback has no admitted graph routing"
            ~retryable:false
            ()))
;;

let constructor_mutation t ~ordinary ~prepared =
  match !(t.run_preparation) with
  | None -> ordinary
  | Some preparation -> prepared preparation
;;

let release t handle =
  Exn.protect
    ~f:(fun () ->
      (constructor_mutation t ~ordinary:A.release_inference ~prepared:(fun preparation ->
         A.Constructor_mutations.release_inference ~preparation))
        t.actor
        ~owner:t.owner
        ~handle
      |> require)
    ~finally:(fun () ->
      t.routing <- Map.remove t.routing (Transcript.Scope.key (L.Handle.scope handle)))
;;

let with_attempt t prepared ~relation ~f =
  let handle =
    Eio.Cancel.protect (fun () ->
      let handle =
        (constructor_mutation t ~ordinary:A.admit_inference ~prepared:(fun preparation ->
           A.Constructor_mutations.admit_inference ~preparation))
          t.actor
          ~owner:t.owner
          ~relation
          ~operation_id:None
          ~invocation_id:None
          ~configuration:(R.Prepared.configuration prepared)
        |> require
      in
      t.routing
      <- Map.set
           t.routing
           ~key:(Transcript.Scope.key (L.Handle.scope handle))
           ~data:handle;
      handle)
  in
  match
    t.upstream.on_admitted
      ~scope:(L.Handle.scope handle)
      ~accounting_id:(L.Handle.accounting_id handle);
    f ~scope:(L.Handle.scope handle) ~accounting_id:(L.Handle.accounting_id handle)
  with
  | result ->
    Eio.Cancel.protect (fun () -> release t handle);
    result
  | exception exn ->
    let backtrace = Stdlib.Printexc.get_raw_backtrace () in
    (try Eio.Cancel.protect (fun () -> release t handle) with
     | _ -> ());
    Exn.raise_with_original_backtrace exn backtrace
;;

let identity t : Inference_client.Identity.t =
  { new_preparation_id = t.upstream.new_preparation_id
  ; with_attempt = (fun prepared ~relation ~f -> with_attempt t prepared ~relation ~f)
  }
;;

let on_attempt t attempt =
  let handle = route t (R.Attempt.scope attempt) in
  (constructor_mutation t ~ordinary:A.acknowledge_inference ~prepared:(fun preparation ->
     A.Constructor_mutations.acknowledge_inference ~preparation))
    t.actor
    ~owner:t.owner
    ~handle
    attempt
  |> require;
  t.upstream.on_attempt attempt
;;

let on_observation t incoming =
  let scope = Inference.Observation.scope incoming in
  (match Map.find t.routing (Transcript.Scope.key scope) with
   | Some _ ->
     let handle = route t scope in
     (constructor_mutation t ~ordinary:A.observe_inference ~prepared:(fun preparation ->
        A.Constructor_mutations.observe_inference ~preparation))
       t.actor
       ~handle
       incoming
     |> require
   | None ->
     (constructor_mutation
        t
        ~ordinary:A.observe_owned_inference
        ~prepared:(fun preparation ->
          A.Constructor_mutations.observe_owned_inference ~preparation))
       t.actor
       ~owner:t.owner
       incoming
     |> require);
  t.upstream.on_observation incoming
;;

let on_completion t completion =
  let handle =
    route t (R.Attempt.scope (Inference_client.Completion.attempt completion))
  in
  (constructor_mutation t ~ordinary:A.complete_inference ~prepared:(fun preparation ->
     A.Constructor_mutations.complete_inference ~preparation))
    t.actor
    ~owner:t.owner
    ~handle
    completion
  |> require;
  t.upstream.on_completion completion
;;

let seal t = if t.finished then Ok () else A.seal_inference_owner t.actor ~owner:t.owner

let finish t =
  if t.finished
  then Ok ()
  else if not (Map.is_empty t.routing)
  then
    Error
      (Agent_protocol.Error.create
         Conflict
         ~message:"inference graph work has not joined"
         ~retryable:true
         ())
  else
    Result.map
      ((constructor_mutation
          t
          ~ordinary:A.finish_inference_owner
          ~prepared:(fun preparation ->
            A.Constructor_mutations.finish_inference_owner ~preparation))
         t.actor
         ~owner:t.owner)
      ~f:(fun () -> t.finished <- true)
;;
