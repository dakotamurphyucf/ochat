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

exception Rejected of Agent_protocol.Error.t [@@deriving sexp]

module Mutation_owner = struct
  type t =
    | Runtime of Agent_session.Run_preparation.t option ref
    | Runtime_construction of A.Runtime_constructor_graph.t
    | Held_input of A.Held_input_scope.t
end

type t =
  { actor : A.t
  ; mutation_owner : Mutation_owner.t
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
    let opened =
      match !run_preparation with
      | None -> A.open_inference_owner actor ~source
      | Some preparation -> A.open_prepared_inference_owner actor ~preparation ~source
    in
    Result.map opened ~f:(fun owner ->
      { actor
      ; mutation_owner = Runtime run_preparation
      ; owner
      ; upstream
      ; routing = Map.empty (module Transcript.Scope.Key)
      ; finished = false
      }))
;;

let create_for_runtime_construction actor ~construction ~source ~upstream =
  Eio.Cancel.protect (fun () ->
    Result.map
      (A.Runtime_constructor_mutations.open_inference_owner actor ~construction ~source)
      ~f:(fun graph ->
        { actor
        ; mutation_owner = Runtime_construction graph
        ; owner = A.Runtime_constructor_graph.owner graph
        ; upstream
        ; routing = Map.empty (module Transcript.Scope.Key)
        ; finished = false
        }))
;;

let create_for_held_input actor ~scope ~source ~upstream =
  Eio.Cancel.protect (fun () ->
    Result.map (A.Input_graph.open_owner actor ~scope ~source) ~f:(fun owner ->
      { actor
      ; mutation_owner = Held_input scope
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

let ledger_mutation t ~ordinary ~prepared ~construction ~input =
  match t.mutation_owner with
  | Held_input scope -> input scope
  | Runtime preparation ->
    (match !preparation with
     | None -> ordinary
     | Some preparation -> prepared preparation)
  | Runtime_construction graph -> construction graph
;;

let release t handle =
  Exn.protect
    ~f:(fun () ->
      (ledger_mutation
         t
         ~ordinary:A.release_inference
         ~prepared:(fun preparation ->
           A.Constructor_mutations.release_inference ~preparation)
         ~construction:(fun graph actor ~owner:_ ->
           A.Runtime_constructor_mutations.release_inference actor ~graph)
         ~input:(fun scope -> A.Input_graph.release_inference ~scope))
        t.actor
        ~owner:t.owner
        ~handle
      |> require)
    ~finally:(fun () ->
      t.routing <- Map.remove t.routing (Transcript.Scope.key (L.Handle.scope handle)))
;;

let with_attempt t ?allocate prepared ~relation ~f =
  let allocation =
    match allocate with
    | Some allocate ->
      allocate ~graph:t.owner ~relation ~configuration:(R.Prepared.configuration prepared)
      |> require
    | None ->
      let unbound admit =
        admit
          t.actor
          ~owner:t.owner
          ~relation
          ~operation_id:None
          ~invocation_id:None
          ~configuration:(R.Prepared.configuration prepared)
        |> require
      in
      (match t.mutation_owner with
       | Held_input _ ->
         raise
           (Rejected
              (Agent_protocol.Error.create
                 Permission_denied
                 ~message:"held input requires its exact execution allocator"
                 ~retryable:false
                 ()))
       | Runtime preparation ->
         unbound
           (match !preparation with
            | None -> A.admit_inference_allocation
            | Some preparation ->
              A.Constructor_mutations.admit_inference_allocation ~preparation)
       | Runtime_construction _ ->
         raise
           (Rejected
              (Agent_protocol.Error.create
                 Permission_denied
                 ~message:"constructed runtime requires its exact execution allocator"
                 ~retryable:false
                 ())))
  in
  let handle = Agent_session.Inference_allocation.handle allocation in
  t.routing
  <- Map.set t.routing ~key:(Transcript.Scope.key (L.Handle.scope handle)) ~data:handle;
  let handle = Agent_session.Inference_allocation.handle allocation in
  match
    t.upstream.on_admitted
      ~scope:(L.Handle.scope handle)
      ~accounting_id:(L.Handle.accounting_id handle);
    f
      ~scope:(L.Handle.scope handle)
      ~accounting_id:(L.Handle.accounting_id handle)
      ~submission_guard:(Agent_session.Inference_allocation.submission_guard allocation)
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

let bind_identity t ~allocate : Inference_client.Identity.t =
  { new_preparation_id = t.upstream.new_preparation_id
  ; with_attempt =
      (fun prepared ~relation ~f -> with_attempt t ~allocate prepared ~relation ~f)
  }
;;

let on_attempt t attempt =
  let handle = route t (R.Attempt.scope attempt) in
  (ledger_mutation
     t
     ~ordinary:A.acknowledge_inference
     ~prepared:(fun preparation ->
       A.Constructor_mutations.acknowledge_inference ~preparation)
     ~construction:(fun graph actor ~owner:_ ->
       A.Runtime_constructor_mutations.acknowledge_inference actor ~graph)
     ~input:(fun scope -> A.Input_graph.acknowledge_inference ~scope))
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
     (ledger_mutation
        t
        ~ordinary:A.observe_inference
        ~prepared:(fun preparation ->
          A.Constructor_mutations.observe_inference ~preparation)
        ~construction:(fun graph actor ->
          A.Runtime_constructor_mutations.observe_inference actor ~graph)
        ~input:(fun scope -> A.Input_graph.observe_inference ~scope ~owner:t.owner))
       t.actor
       ~handle
       incoming
     |> require
   | None ->
     (ledger_mutation
        t
        ~ordinary:A.observe_owned_inference
        ~prepared:(fun preparation ->
          A.Constructor_mutations.observe_owned_inference ~preparation)
        ~construction:(fun graph actor ~owner:_ ->
          A.Runtime_constructor_mutations.observe_owned_inference actor ~graph)
        ~input:(fun scope -> A.Input_graph.observe_owned_inference ~scope))
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
  (ledger_mutation
     t
     ~ordinary:A.complete_inference
     ~prepared:(fun preparation ->
       A.Constructor_mutations.complete_inference ~preparation)
     ~construction:(fun graph actor ~owner:_ ->
       A.Runtime_constructor_mutations.complete_inference actor ~graph)
     ~input:(fun scope -> A.Input_graph.complete_inference ~scope))
    t.actor
    ~owner:t.owner
    ~handle
    completion
  |> require;
  t.upstream.on_completion completion
;;

let seal t =
  if t.finished
  then Ok ()
  else (
    match t.mutation_owner with
    | Runtime _ -> A.seal_inference_owner t.actor ~owner:t.owner
    | Runtime_construction graph ->
      A.Runtime_constructor_mutations.seal_inference_owner t.actor ~graph
    | Held_input scope -> A.Input_graph.seal_inference_owner t.actor ~scope ~owner:t.owner)
;;

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
      ((ledger_mutation
          t
          ~ordinary:A.finish_inference_owner
          ~prepared:(fun preparation ->
            A.Constructor_mutations.finish_inference_owner ~preparation)
          ~construction:(fun graph actor ~owner:_ ->
            A.Runtime_constructor_mutations.finish_inference_owner actor ~graph)
          ~input:(fun scope -> A.Input_graph.finish_inference_owner ~scope))
         t.actor
         ~owner:t.owner)
      ~f:(fun () -> t.finished <- true)
;;
