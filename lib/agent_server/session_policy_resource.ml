open! Core
module B = Agent_session.Runtime_builder
module A = Agent_session.Session_actor
module P = Agent_protocol
module Constructor_graph_slot = Constructor_inference_graph

let conflict message = P.Error.create Conflict ~message ~retryable:true ()

module Key = struct
  module Family = struct
    type t =
      | Authored
      | Captured of Agent_store.Captured_source_kind.t
      | Authored_child
      | Generated of (string * string) list
    [@@deriving equal]
  end

  type t =
    { actor : A.t
    ; handle : Agent_store.Session_store.Handle.t
    ; domain : int
    ; generation : int
    ; spec : Agent_session.Session_state.Spec.Resource_identity.t
    ; artifact : Agent_store.Prompt_artifact_store.Artifact.t
    ; invocation : Agent_session.Runtime_script_preparation.Invocation.t
    ; family : Family.t
    ; profile : string
    ; source : P.Run_source.t option
    ; initialized : bool
    }

  let capture_identity
        ~actor
        ~handle
        ~artifact
        ~invocation
        ~family
        ~(profile : Agent_session.Permission_policy.t)
        ~(expected : Agent_session.Session_state.t)
    =
    let open Result.Let_syntax in
    let%bind () =
      if
        P.Id.Session.equal
          (Agent_store.Session_store.Handle.session_id handle)
          expected.identity.session_id
        && String.equal profile.revision_digest expected.spec.permission_profile_digest
      then Ok ()
      else Error (conflict "policy resource belongs to another session or profile")
    in
    let%bind observer = Agent_session.Moderator_checkpoint.observer expected.moderator in
    let%map installed =
      match observer with
      | None -> Ok None
      | Some _ ->
        Agent_session.Moderator_source_installation.capture expected
        |> Result.map ~f:(fun (_, source) -> Some source)
    in
    { actor
    ; handle
    ; domain = (Stdlib.Domain.self () :> int)
    ; generation = expected.identity.generation
    ; spec = Agent_session.Session_state.Spec.Resource_identity.capture expected.spec
    ; artifact
    ; invocation
    ; family
    ; profile = profile.revision_digest
    ; source = installed
    ; initialized =
        (match expected.runtime_initialization with
         | Ready -> true
         | Pending _ -> false)
    }
  ;;

  let capture ~actor ~handle ~source ~profile ~expected =
    let open Result.Let_syntax in
    let%bind () = B.Source.validate_target source expected in
    let family =
      match B.Source.selection source with
      | Authored _ -> Family.Authored
      | Captured captured ->
        Family.Captured
          (Agent_session.Recurrence_child_origin.source_kind
             (Agent_session.Captured_recurrence_source.origin captured))
      | Authored_child _ -> Family.Authored_child
      | Generated definition ->
        Family.Generated (Agent_session.Generated_definition.capability_pins definition)
      | Generated_artifact source ->
        Family.Generated
          (Agent_session.Generated_definition.Source.capability_pins source)
    in
    capture_identity
      ~actor
      ~handle
      ~profile
      ~expected
      ~artifact:(B.Source.artifact source)
      ~invocation:(B.Source.invocation source)
      ~family
  ;;

  let capture_authored
        ~actor
        ~handle
        ~revision
        ~profile
        ~(expected : Agent_session.Session_state.t)
    =
    let open Result.Let_syntax in
    let artifact = Agent_session.Prompt_revision.artifact revision in
    let%bind () =
      if
        Option.is_none expected.spec.delegation
        && P.Id.Prompt_revision.equal
             artifact.revision_id
             expected.spec.prompt_revision_id
        &&
        match expected.spec.prompt_definition_id with
        | Some id ->
          P.Id.Prompt_definition.equal
            id
            (Agent_session.Prompt_revision.definition revision).id
        | None -> Option.is_none artifact.prompt_definition_id
      then Ok ()
      else
        Error (conflict "ordinary policy product differs from its actual authored root")
    in
    (* The existing Factory restore already verified this revision. Capturing a
       key adds no second compilation or current authority read under Owner. *)
    capture_identity
      ~actor
      ~handle
      ~profile
      ~expected
      ~artifact
      ~invocation:
        (Agent_session.Runtime_script_preparation.Invocation.of_root
           expected.spec.root_invocation)
      ~family:Family.Authored
  ;;

  let check_policy
        t
        ~actor
        ~handle
        ~(profile : Agent_session.Permission_policy.t)
        ~(expected : Agent_session.Session_state.t)
    =
    let open Result.Let_syntax in
    let%bind source =
      let%bind observer =
        Agent_session.Moderator_checkpoint.observer expected.moderator
      in
      match observer with
      | None -> Ok None
      | Some _ ->
        Agent_session.Moderator_source_installation.capture expected
        |> Result.map ~f:(fun (_, source) -> Some source)
    in
    let ready =
      match expected.runtime_initialization with
      | Ready -> true
      | Pending _ -> false
    in
    if
      phys_equal t.actor actor
      && phys_equal t.handle handle
      && Int.equal t.domain (Stdlib.Domain.self () :> int)
      && Int.equal t.generation expected.identity.generation
      && P.Id.Session.equal
           (Agent_store.Session_store.Handle.session_id handle)
           expected.identity.session_id
      && Agent_session.Session_state.Spec.Resource_identity.equal
           t.spec
           (Agent_session.Session_state.Spec.Resource_identity.capture expected.spec)
      && String.equal t.profile expected.spec.permission_profile_digest
      && String.equal t.profile profile.revision_digest
      && t.initialized
      && ready
      && Option.equal P.Run_source.equal t.source source
    then Ok ()
    else Error (conflict "retained policy lease lost its actual source or profile")
  ;;

  let same_construction t other =
    phys_equal t.actor other.actor
    && phys_equal t.handle other.handle
    && Int.equal t.domain other.domain
    && Int.equal t.generation other.generation
    && Agent_session.Session_state.Spec.Resource_identity.equal t.spec other.spec
    && P.Id.Prompt_revision.equal t.artifact.revision_id other.artifact.revision_id
    && Agent_store.Prompt_artifact_store.Artifact.same_content t.artifact other.artifact
    && Agent_session.Runtime_script_preparation.Invocation.equal
         t.invocation
         other.invocation
    && Family.equal t.family other.family
    && String.equal t.profile other.profile
  ;;

  let equal t other =
    t.initialized
    && other.initialized
    && same_construction t other
    && Option.equal P.Run_source.equal t.source other.source
  ;;
end

module Native_source_key = struct
  type t =
    { actor : A.t
    ; handle : Agent_store.Session_store.Handle.t
    ; domain : int
    ; generation : int
    ; spec : Agent_session.Session_state.Spec.Resource_identity.t
    ; artifact : Agent_store.Prompt_artifact_store.Artifact.t
    ; invocation : Agent_session.Runtime_script_preparation.Invocation.t
    ; family : Key.Family.t
    ; profile : string
    ; source : P.Run_source.t option
    }

  let family source =
    match B.Source.selection source with
    | Authored _ -> Key.Family.Authored
    | Captured captured ->
      Key.Family.Captured
        (Agent_session.Recurrence_child_origin.source_kind
           (Agent_session.Captured_recurrence_source.origin captured))
    | Authored_child _ -> Key.Family.Authored_child
    | Generated definition ->
      Key.Family.Generated (Agent_session.Generated_definition.capability_pins definition)
    | Generated_artifact source ->
      Key.Family.Generated
        (Agent_session.Generated_definition.Source.capability_pins source)
  ;;

  let matches_source t source =
    let artifact = B.Source.artifact source in
    Int.equal t.domain (Stdlib.Domain.self () :> int)
    && P.Id.Prompt_revision.equal t.artifact.revision_id artifact.revision_id
    && Agent_store.Prompt_artifact_store.Artifact.same_content t.artifact artifact
    && Agent_session.Runtime_script_preparation.Invocation.equal
         t.invocation
         (B.Source.invocation source)
    && Key.Family.equal t.family (family source)
  ;;

  let installed_source (expected : Agent_session.Session_state.t) =
    match expected.run_state with
    | None -> Ok None
    | Some index ->
      let installation = Agent_session.Run_state.installation index in
      (match installation.source with
       | None -> Ok None
       | Some _ ->
         Agent_session.Run_source_installation.captured
           installation
           ~generation:expected.identity.generation
         |> Result.map ~f:Option.some)
  ;;

  let capture
        ~actor
        ~handle
        ~source
        ~(profile : Agent_session.Permission_policy.t)
        ~(expected : Agent_session.Session_state.t)
    =
    let open Result.Let_syntax in
    let%bind () = B.Source.validate_target source expected in
    let%bind () =
      if
        P.Id.Session.equal
          (Agent_store.Session_store.Handle.session_id handle)
          expected.identity.session_id
        && String.equal profile.revision_digest expected.spec.permission_profile_digest
      then Ok ()
      else Error (conflict "native resource belongs to another session or profile")
    in
    (* Absence is factual too. Never project a source from a checkpoint: native
       preparation may precede the genuine ordinary initialization ACK. *)
    let%map installed = installed_source expected in
    let family = family source in
    { actor
    ; handle
    ; domain = (Stdlib.Domain.self () :> int)
    ; generation = expected.identity.generation
    ; spec = Agent_session.Session_state.Spec.Resource_identity.capture expected.spec
    ; artifact = B.Source.artifact source
    ; invocation = B.Source.invocation source
    ; family
    ; profile = profile.revision_digest
    ; source = installed
    }
  ;;

  let equal t other =
    let domain = (Stdlib.Domain.self () :> int) in
    Int.equal t.domain domain
    && Int.equal other.domain domain
    && phys_equal t.actor other.actor
    && phys_equal t.handle other.handle
    && Int.equal t.generation other.generation
    && Agent_session.Session_state.Spec.Resource_identity.equal t.spec other.spec
    && P.Id.Prompt_revision.equal t.artifact.revision_id other.artifact.revision_id
    && Agent_store.Prompt_artifact_store.Artifact.same_content t.artifact other.artifact
    && Agent_session.Runtime_script_preparation.Invocation.equal
         t.invocation
         other.invocation
    && Key.Family.equal t.family other.family
    && String.equal t.profile other.profile
    && Option.equal P.Run_source.equal t.source other.source
  ;;

  let check_current
        t
        ~actor
        ~handle
        ~(profile : Agent_session.Permission_policy.t)
        ~(expected : Agent_session.Session_state.t)
    =
    let open Result.Let_syntax in
    let%bind () =
      if Int.equal t.domain (Stdlib.Domain.self () :> int)
      then Ok ()
      else
        Error (P.Error.invalid_request "native source key belongs to another Eio domain")
    in
    let%bind source = installed_source expected in
    if
      phys_equal t.actor actor
      && phys_equal t.handle handle
      && P.Id.Session.equal
           (Agent_store.Session_store.Handle.session_id handle)
           expected.identity.session_id
      && Int.equal t.generation expected.identity.generation
      && Agent_session.Session_state.Spec.Resource_identity.equal
           t.spec
           (Agent_session.Session_state.Spec.Resource_identity.capture expected.spec)
      && String.equal t.profile profile.revision_digest
      && String.equal t.profile expected.spec.permission_profile_digest
      && Option.equal P.Run_source.equal t.source source
    then Ok ()
    else Error (conflict "retained native resource lost its actual source or profile")
  ;;

  (* A loaded-first Product is already genuinely initialized. Its own exact
     Key.check_policy validates the checkpoint-derived installation separately;
     this native key still records ONLY the actual Run_state source option. *)
  let of_product_key (key : Key.t) ~(expected : Agent_session.Session_state.t) =
    let open Result.Let_syntax in
    let%map source = installed_source expected in
    { actor = key.actor
    ; handle = key.handle
    ; domain = key.domain
    ; generation = key.generation
    ; spec = key.spec
    ; artifact = key.artifact
    ; invocation = key.invocation
    ; family = key.family
    ; profile = key.profile
    ; source
    }
  ;;
end

module Failure = Constructor_graph_slot.Failure

module Close_report = struct
  module Observation = Agent_session.Close_observation
  module Diagnostic = Agent_session.Cleanup_diagnostic
  module Callback_failure = Agent_session.Callback_cleanup.Failure

  type t = (unit, P.Error.t) result Observation.t

  let outcome (t : t) =
    match Observation.exit t with
    | Returned (Ok ()) -> Ok ()
    | Returned (Error error) -> Error (Failure.Protocol error)
    | Raised (exn, backtrace) -> Error (Failure.Raised (exn, backtrace))
  ;;

  let of_outcome (outcome : (unit, Failure.t) result) : t =
    match outcome with
    | Ok () -> Observation.create (Returned (Ok ())) ~diagnostics:[] ()
    | Error failure ->
      let exit, failure =
        match failure with
        | Failure.Protocol error ->
          Observation.Returned (Error error), Callback_failure.Protocol error
        | Raised (exn, backtrace) ->
          Observation.Raised (exn, backtrace), Callback_failure.Raised (exn, backtrace)
      in
      let diagnostic = Diagnostic.create [ failure ] in
      Observation.create exit ~diagnostics:[ diagnostic ] ~primary:diagnostic ()
  ;;

  let capture f = of_outcome (Failure.capture f)
  let success () = of_outcome (Ok ())

  (* Both operands have already executed in the original order. Preserve the
     first raw/Protocol failure while retaining actual independent secondaries. *)
  let combine (first : t) (second : t) =
    let selected =
      match outcome first with
      | Ok () -> second
      | Error _ -> first
    in
    Observation.create
      (Observation.exit selected)
      ~diagnostics:(Observation.diagnostics first @ Observation.diagnostics second)
      ?primary:(Observation.primary selected)
      ()
  ;;

  let of_unit_observation observation =
    let exit =
      match Observation.exit observation with
      | Returned () -> Observation.Returned (Ok ())
      | Raised (exn, backtrace) -> Observation.Raised (exn, backtrace)
    in
    Observation.create
      exit
      ~diagnostics:(Observation.diagnostics observation)
      ?primary:(Observation.primary observation)
      ()
  ;;

  let capture_reported f =
    match f () with
    | report -> report
    | exception exn ->
      of_outcome (Error (Failure.Raised (exn, Stdlib.Printexc.get_raw_backtrace ())))
  ;;

  let resume = Observation.resume
end

module Partial_close = struct
  module Adoption = struct
    type t =
      | Original_running
      | Not_created
      | Closed_runtime of
          { runtime : B.t
          ; observation : unit Agent_session.Close_observation.t
          }
  end

  type t =
    { domain : int
    ; close : B.Close.t
    ; adoption : unit -> Adoption.t
    ; mutable registered : bool
    }

  let create ~close ~adoption =
    { domain = (Stdlib.Domain.self () :> int); close; adoption; registered = false }
  ;;

  let check_domain t =
    if Int.equal t.domain (Stdlib.Domain.self () :> int)
    then Ok ()
    else Error (P.Error.invalid_request "partial closer belongs to another Eio domain")
  ;;

  let register t =
    let open Result.Let_syntax in
    let%bind () = check_domain t in
    if t.registered
    then Error (conflict "partial closer already belongs to an actual construction")
    else (
      t.registered <- true;
      Ok ())
  ;;

  let refuse message =
    Close_report.of_outcome (Error (Failure.Protocol (conflict message)))
  ;;

  let close_reported t ~product =
    match check_domain t with
    | Error error -> Close_report.of_outcome (Error (Failure.Protocol error))
    | Ok () ->
      (match t.adoption () with
       | Adoption.Original_running ->
         refuse "partial construction driver has not exited its original scope"
       | Not_created | Closed_runtime _ ->
         let scoped = Close_report.of_unit_observation (B.Close.run_reported t.close) in
         let fallback =
           match t.adoption (), product with
           | Adoption.Original_running, _ ->
             refuse "partial construction lost its original completion"
           | Not_created, None -> Close_report.success ()
           | Not_created, Some (actual, _) ->
             Close_report.of_unit_observation (B.Product.close_reported actual)
           | Closed_runtime _, None ->
             refuse "adopted partial runtime has no actual Product binding"
           | Closed_runtime { runtime; observation }, Some (_, binding) ->
             let actual = B.Product.Binding.runtime binding in
             if
               phys_equal actual.worker runtime.B.worker
               && Option.equal
                    phys_equal
                    actual.moderator_manager
                    runtime.moderator_manager
               && Option.equal phys_equal actual.native_runtime runtime.native_runtime
             then Close_report.of_unit_observation observation
             else refuse "adopted partial runtime differs from its actual binding"
         in
         (* A negative original runtime close remains in scoped; its actual
            adoption proves only that Product fallback must not be replayed. *)
         Close_report.combine scoped fallback)
  ;;
end

module Partial = struct
  type t =
    | Unallocated
    | Tracking of Graph_tracking.t
    | Builder of
        { tracking : Graph_tracking.t
        ; product : B.Product.t
        ; binding : B.Product.Binding.t
        }
    | Physical of
        { product : B.Product.t
        ; binding : B.Product.Binding.t
        ; runtime : B.t
        }

  let finish_tracking_reported tracking resource =
    (* Attempt every factual leg even when physical release failed. A later
       explicit retry retains this SAME tracking owner and primary outcome. *)
    let sealed = Close_report.capture (fun () -> Graph_tracking.seal tracking) in
    let finished = Close_report.capture (fun () -> Graph_tracking.finish tracking) in
    Close_report.combine resource (Close_report.combine sealed finished)
  ;;

  let close_reported ?construction partial =
    match partial, construction with
    | Physical { runtime; _ }, _ ->
      Close_report.of_unit_observation (B.close_reported runtime)
    | (Unallocated | Tracking _ | Builder _), Some construction ->
      let product =
        match partial with
        | Builder { product; binding; _ } -> Some (product, binding)
        | Unallocated | Tracking _ | Physical _ -> None
      in
      Close_report.capture_reported (fun () ->
        Partial_close.close_reported construction ~product)
    | Unallocated, None -> Close_report.success ()
    | Tracking tracking, None ->
      finish_tracking_reported tracking (Close_report.success ())
    | Builder { tracking; product; _ }, None ->
      let resource =
        Close_report.of_unit_observation (B.Product.close_reported product)
      in
      finish_tracking_reported tracking resource
  ;;
end

module Cell_tag = struct
  type t = unit ref

  let create () = ref ()
  let equal (t : t) (other : t) = phys_equal t other
end

module Cleanup_identity = struct
  module State = struct
    type t =
      | Held
      | Released
  end

  type t =
    { domain : int
    ; cell : Cell_tag.t
    ; mutable state : State.t
    }

  let create ~domain ~cell = { domain; cell; state = State.Held }
  let equal (t : t) (other : t) = phys_equal t other
  let release t = t.state <- State.Released
end

type building =
  { cleanup_target : Cleanup_identity.t
  ; key : Key.t
  ; mutable partial : Partial.t
  ; mutable partial_close : Partial_close.t option
  }

type native_scope =
  { scope : Runtime_native_scope.t
  ; close : unit -> (unit, P.Error.t) result Agent_session.Close_observation.t
  }

type native_building =
  { cleanup_target : Cleanup_identity.t
  ; native_key : Native_source_key.t
  ; mutable tracking : Graph_tracking.t option
  ; mutable scope : native_scope option
  ; mutable prepared : B.Prepared_native.t option
  }

type promotion_partial =
  { key : Key.t
  ; mutable attachment : B.Product_attachment.t option
  ; mutable product : (B.Product.t * B.Product.Binding.t) option
  ; mutable runtime : B.t option
  ; mutable acknowledged : A.Initialization_scope.Source_handoff.t option
  ; mutable pinned : bool
  }

type native_attachment =
  | No_attachment
  | Attaching of promotion_partial
  | Attached of promotion_partial
  | Failed_attachment of promotion_partial * Failure.t

module Close_state = struct
  type t =
    | Open
    | Closed of Close_report.t

  let report = function
    | Open -> None
    | Closed report -> Some report
  ;;

  let is_closed = function
    | Open -> false
    | Closed _ -> true
  ;;
end

type native_resource =
  { cleanup_target : Cleanup_identity.t
  ; mutable native_key : Native_source_key.t
  ; prepared : B.Prepared_native.t
  ; scope : native_scope
  ; mutable attachment : native_attachment
  ; mutable constructor_graph : Constructor_graph_slot.slot option
  ; mutable references : int
  ; close_state : Close_state.t ref
  }

type resource =
  { cleanup_target : Cleanup_identity.t
  ; key : Key.t
  ; product : B.Product.t
  ; runtime : B.t
  ; constructor_graph : Constructor_graph_slot.slot
  ; native : native_resource option
  ; mutable references : int
  ; close_state : Close_state.t ref
  }

type cleanup =
  | Partial of building
  | Resource of resource
  | Native_partial of native_building
  | Native_resource of native_resource

type closing =
  { target : cleanup
  ; result : Close_report.t Eio.Promise.t
  }

type phase =
  | Empty
  | Building of building
  | Native_building of native_building
  | Native_live of native_resource
  | Live of resource
  | Retained of cleanup * Failure.t option
  | Closing of closing

type t =
  { domain : int
  ; tag : Cell_tag.t
  ; mutable phase : phase
  }

let check_domain t =
  if Int.equal t.domain (Stdlib.Domain.self () :> int)
  then Ok ()
  else Error (P.Error.invalid_request "policy resource belongs to another Eio domain")
;;

let pending t =
  match check_domain t with
  | Error error -> invalid_arg error.message
  | Ok () ->
    (match t.phase with
     | Empty -> false
     | Live resource -> Constructor_graph_slot.pending resource.constructor_graph
     | Native_live resource ->
       (match resource.attachment with
        | No_attachment -> false
        | Attaching _ | Attached _ | Failed_attachment _ -> true)
     | Building _ | Native_building _ | Retained _ | Closing _ -> true)
;;

let same_target first second =
  match first, second with
  | Partial first, Partial second -> phys_equal first second
  | Resource first, Resource second -> phys_equal first second
  | Native_partial first, Native_partial second -> phys_equal first second
  | Native_resource first, Native_resource second -> phys_equal first second
  | (Partial _ | Resource _ | Native_partial _ | Native_resource _), _ -> false
;;

let resource_references (resource : resource) =
  match resource.native with
  | None -> resource.references
  | Some native -> native.references
;;

let change_resource_references (resource : resource) delta =
  match resource.native with
  | None -> resource.references <- resource.references + delta
  | Some native -> native.references <- native.references + delta
;;

let resource_closed (resource : resource) = Close_state.is_closed !(resource.close_state)

let resource_target (resource : resource) =
  match resource.native with
  | None -> Resource resource
  | Some native -> Native_resource native
;;

let check_resource_scope (resource : resource) =
  match resource.native with
  | None -> Ok ()
  | Some native -> Runtime_native_scope.check_open native.scope.scope
;;

let close_native_scope_reported scope = Close_report.capture_reported scope.close

let close_native_partial_reported (building : native_building) =
  match building.scope with
  | Some scope -> close_native_scope_reported scope
  | None ->
    (match building.tracking with
     | None -> Close_report.success ()
     | Some tracking ->
       Partial.finish_tracking_reported tracking (Close_report.success ()))
;;

let close_native_resource_reported (resource : native_resource) =
  let attachment =
    match resource.attachment with
    | No_attachment -> Close_report.success ()
    | Attaching partial | Attached partial | Failed_attachment (partial, _) ->
      (match partial.runtime, partial.attachment with
       | Some runtime, _ -> Close_report.of_unit_observation (B.close_reported runtime)
       | None, Some attachment ->
         Close_report.capture (fun () ->
           B.Product_attachment.close attachment;
           Ok ())
       | None, None -> Close_report.success ())
  in
  (* These are the existing independently attempted legs, in actual order. The
     immutable Cell receipt retains their secondaries before legacy projection. *)
  let constructor =
    match resource.constructor_graph with
    | None -> Close_report.success ()
    | Some graph ->
      Close_report.capture (fun () -> Constructor_graph_slot.close_pending graph)
  in
  let common = close_native_scope_reported resource.scope in
  Close_report.combine attachment (Close_report.combine constructor common)
;;

let cleanup_identity = function
  | Partial building -> building.cleanup_target
  | Resource resource -> resource.cleanup_target
  | Native_partial building -> building.cleanup_target
  | Native_resource resource -> resource.cleanup_target
;;

module Cleanup_target = struct
  type cell = t
  type t = Cleanup_identity.t

  module Facets = struct
    type t =
      { native : B.Native_identity.t option
      ; product : B.Product_identity.t option
      }
  end

  module Status = struct
    type t =
      | Held of Facets.t
      | Released
  end

  let equal = Cleanup_identity.equal

  let partial_product = function
    | Partial.Unallocated | Tracking _ -> None
    | Builder { product; _ } | Physical { product; _ } ->
      Some (B.Product.identity product)
  ;;

  let native_facets (resource : native_resource) =
    let product =
      match resource.attachment with
      | No_attachment -> None
      | Attaching partial | Attached partial | Failed_attachment (partial, _) ->
        Option.map partial.product ~f:(fun (product, _) -> B.Product.identity product)
    in
    { Facets.native = Some (B.Prepared_native.identity resource.prepared); product }
  ;;

  let facets = function
    | Partial building ->
      { Facets.native = None; product = partial_product building.partial }
    | Native_partial building ->
      { Facets.native = Option.map building.prepared ~f:B.Prepared_native.identity
      ; product = None
      }
    | Resource resource ->
      { Facets.native =
          Option.map resource.native ~f:(fun native ->
            B.Prepared_native.identity native.prepared)
      ; product = Some (B.Product.identity resource.product)
      }
    | Native_resource resource -> native_facets resource
  ;;

  let current = function
    | Empty -> None
    | Building building -> Some (Partial building)
    | Native_building building -> Some (Native_partial building)
    | Live resource -> Some (Resource resource)
    | Native_live resource -> Some (Native_resource resource)
    | Retained (target, _) | Closing { target; _ } -> Some target
  ;;

  let check (target : t) ~(cell : cell) =
    let open Result.Let_syntax in
    let%bind () =
      if Int.equal target.domain (Stdlib.Domain.self () :> int)
      then check_domain cell
      else Error (P.Error.invalid_request "cleanup target belongs to another Eio domain")
    in
    let%bind () =
      if Cell_tag.equal target.cell cell.tag
      then Ok ()
      else Error (conflict "cleanup target belongs to another Cell")
    in
    match target.state with
    | Released -> Ok Status.Released
    | Held ->
      (match current cell.phase with
       | Some actual when Cleanup_identity.equal target (cleanup_identity actual) ->
         Ok (Status.Held (facets actual))
       | Some _ | None -> Error (conflict "cleanup target has no matching Cell unit"))
  ;;
end

let close_target_reported t target =
  match check_domain t with
  | Error error -> Close_report.of_outcome (Error (Failure.Protocol error))
  | Ok () ->
    Eio.Cancel.protect (fun () ->
      match t.phase with
      | Empty -> Close_report.success ()
      | Closing current when same_target current.target target ->
        (* SAME actual producer, including every independently observed leg. *)
        Eio.Promise.await current.result
      | Retained (current, _) when same_target current target ->
        let result, resolver = Eio.Promise.create () in
        t.phase <- Closing { target; result };
        let report =
          match target with
          | Partial build ->
            Partial.close_reported ?construction:build.partial_close build.partial
          | Native_partial building -> close_native_partial_reported building
          | Native_resource resource -> close_native_resource_reported resource
          | Resource resource ->
            let physical =
              Close_report.of_unit_observation (B.close_reported resource.runtime)
            in
            let graph =
              Close_report.capture (fun () ->
                Constructor_graph_slot.close_pending resource.constructor_graph)
            in
            Close_report.combine physical graph
        in
        (match Close_report.outcome report with
         | Ok () ->
           (* Publish the actual successful report on this SAME target before
              releasing its identity, clearing the Cell or resolving joiners.
              A later last-lease call may follow a close_pending repair. *)
           (match target with
            | Resource resource -> resource.close_state := Closed report
            | Native_resource resource -> resource.close_state := Closed report
            | Partial _ | Native_partial _ -> ());
           Cleanup_identity.release (cleanup_identity target);
           t.phase <- Empty
         | Error failure -> t.phase <- Retained (target, Some failure));
        (* Write the immutable observation once BEFORE resolving the existing
           producer promise. A retained retry creates a new producer/report. *)
        Eio.Promise.resolve resolver report;
        report
      | Building _ | Native_building _ | Native_live _ | Live _ | Closing _ | Retained _
        ->
        Close_report.of_outcome
          (Error (Failure.Protocol (conflict "policy resource cleanup owner changed"))))
;;

let close_target t target = Close_report.resume (close_target_reported t target)

let close_pending_reported t =
  match check_domain t with
  | Error error -> Close_report.of_outcome (Error (Failure.Protocol error))
  | Ok () ->
    (match t.phase with
     | Empty -> Close_report.success ()
     | Live resource ->
       Close_report.capture (fun () ->
         Constructor_graph_slot.close_pending resource.constructor_graph)
     | Retained (target, _) | Closing { target; _ } -> close_target_reported t target
     | Native_live resource ->
       (match resource.attachment with
        | No_attachment | Attached _ | Failed_attachment _ -> Close_report.success ()
        | Attaching _ ->
          Close_report.of_outcome
            (Error
               (Failure.Protocol (conflict "native promotion callback has not joined"))))
     | Building _ | Native_building _ ->
       Close_report.of_outcome
         (Error
            (Failure.Protocol (conflict "policy construction callback has not joined"))))
;;

let close_pending t = Close_report.resume (close_pending_reported t)

module Lease = struct
  type owner = t

  type state =
    | Active
    | Released
    | Last_release

  type t =
    { owner : owner
    ; resource : resource
    ; mutable state : state
    }

  let cleanup_target t = t.resource.cleanup_target
  let product t = t.resource.product
  let runtime t = t.resource.runtime

  let check_key t ~key =
    let open Result.Let_syntax in
    let%bind () = check_domain t.owner in
    match t.state, t.owner.phase with
    | Active, Live resource
      when phys_equal resource t.resource && Key.equal resource.key key ->
      check_resource_scope resource
    | ( (Active | Released | Last_release)
      , ( Empty
        | Building _
        | Native_building _
        | Native_live _
        | Live _
        | Retained _
        | Closing _ ) ) ->
      Error (conflict "retained policy source changed before binding publication")
  ;;

  let check_policy_key t ~actor ~handle ~profile ~expected =
    let open Result.Let_syntax in
    let%bind () = check_domain t.owner in
    let%bind () =
      match t.state, t.owner.phase with
      | Active, Live resource when phys_equal resource t.resource ->
        let%bind () = check_resource_scope resource in
        Key.check_policy resource.key ~actor ~handle ~profile ~expected
      | ( (Active | Released | Last_release)
        , ( Empty
          | Building _
          | Native_building _
          | Native_live _
          | Live _
          | Retained _
          | Closing _ ) ) ->
        Error (conflict "retained policy access has no active same-Cell lease")
    in
    Ok ()
  ;;

  let policy_access t ~actor ~handle ~profile ~expected =
    let open Result.Let_syntax in
    let%bind () = check_policy_key t ~actor ~handle ~profile ~expected in
    B.Product.policy_access t.resource.product ~runtime:t.resource.runtime ~expected
  ;;

  let check_restored_policy_access t ~access ~actor ~handle ~profile ~expected =
    let open Result.Let_syntax in
    let%bind () = check_policy_key t ~actor ~handle ~profile ~expected in
    let%bind () =
      if
        B.Product_identity.equal
          (B.Product.identity t.resource.product)
          (B.Restored_policy_access.product_identity access)
      then Ok ()
      else Error (conflict "restored policy access belongs to another physical Product")
    in
    B.Restored_policy_access.check access ~expected
  ;;

  let close_reported t =
    match check_domain t.owner with
    | Error error -> Close_report.of_outcome (Error (Failure.Protocol error))
    | Ok () ->
      (match t.state with
       | Released -> Close_report.success ()
       | Last_release ->
         (match Close_state.report !(t.resource.close_state) with
          | Some report -> report
          | None -> close_target_reported t.owner (resource_target t.resource))
       | Active ->
         (match t.owner.phase with
          | Live current
            when phys_equal current t.resource && resource_references current > 0 ->
            change_resource_references current (-1);
            if resource_references current > 0
            then (
              t.state <- Released;
              Close_report.success ())
            else (
              t.state <- Last_release;
              let target = resource_target current in
              t.owner.phase <- Retained (target, None);
              close_target_reported t.owner target)
          | Empty
          | Building _
          | Native_building _
          | Native_live _
          | Retained _
          | Closing _
          | Live _ ->
            Close_report.of_outcome
              (Error (Failure.Protocol (conflict "policy resource lease was displaced")))))
  ;;

  let close t = Close_report.resume (close_reported t)
end

module Constructor_graph = struct
  type t = Constructor_graph_slot.t

  let reserve (lease : Lease.t) =
    let open Result.Let_syntax in
    let%bind () = check_domain lease.owner in
    match lease.state, lease.owner.phase with
    | Active, Live resource
      when phys_equal resource lease.resource && not (resource_closed resource) ->
      let%bind () = check_resource_scope resource in
      Constructor_graph_slot.reserve resource.constructor_graph
    | ( (Active | Released | Last_release)
      , ( Empty
        | Building _
        | Native_building _
        | Native_live _
        | Live _
        | Retained _
        | Closing _ ) ) ->
      Error (conflict "constructor graph requires its actual retained policy lease")
  ;;

  let allocated = Constructor_graph_slot.allocated
  let inference = Constructor_graph_slot.inference
  let abandon_unallocated = Constructor_graph_slot.abandon_unallocated
  let callback_exited = Constructor_graph_slot.callback_exited
  let finish = Constructor_graph_slot.finish
end

module Build = struct
  type owner = t

  type t =
    { owner : owner
    ; building : building
    }

  let check t =
    let open Result.Let_syntax in
    let%bind () = check_domain t.owner in
    match t.owner.phase with
    | Building actual when phys_equal actual t.building -> Ok ()
    | Empty
    | Building _
    | Native_building _
    | Native_live _
    | Live _
    | Retained _
    | Closing _ -> Error (conflict "policy construction owner was displaced")
  ;;

  let cleanup_target t = t.building.cleanup_target

  let tracking t tracking =
    let open Result.Let_syntax in
    let%bind () = check t in
    match t.building.partial with
    | Unallocated ->
      t.building.partial <- Tracking tracking;
      Ok ()
    | Tracking _ | Builder _ | Physical _ ->
      Error (conflict "policy tracking was already attached")
  ;;

  let partial_close t close =
    let open Result.Let_syntax in
    let%bind () = check t in
    match t.building.partial, t.building.partial_close with
    | Tracking _, None ->
      let%map () = Partial_close.register close in
      t.building.partial_close <- Some close
    | (Unallocated | Tracking _ | Builder _ | Physical _), (None | Some _) ->
      Error (conflict "partial construction closer was not attached before its driver")
  ;;

  let product t ~product ~binding =
    let open Result.Let_syntax in
    let%bind () = check t in
    match t.building.partial with
    | Tracking tracking ->
      t.building.partial <- Builder { tracking; product; binding };
      Ok ()
    | Unallocated | Builder _ | Physical _ ->
      Error (conflict "policy product lost its original tracking")
  ;;

  let physical t runtime =
    let open Result.Let_syntax in
    let%bind () = check t in
    match t.building.partial with
    | Builder { product; binding; _ } ->
      let actual = B.Product.Binding.runtime binding in
      if
        phys_equal actual.worker runtime.B.worker
        && Option.equal phys_equal actual.moderator_manager runtime.moderator_manager
        && Option.equal phys_equal actual.native_runtime runtime.native_runtime
      then (
        t.building.partial <- Physical { product; binding; runtime };
        t.building.partial_close <- None;
        Ok ())
      else Error (conflict "policy physical runtime differs from its actual binding")
    | Unallocated | Tracking _ | Physical _ ->
      Error (conflict "policy physical runtime was already selected")
  ;;

  let binding t =
    let open Result.Let_syntax in
    let%bind () = check t in
    match t.building.partial with
    | Builder { binding; _ } | Physical { binding; _ } -> Ok binding
    | Unallocated | Tracking _ ->
      Error (conflict "policy construction has no Builder binding")
  ;;

  let closed t =
    let open Result.Let_syntax in
    let%bind () = check t in
    (* Only the caller of the actual positive full closer may publish this.
       It is not derived from a returned constructor error or missing product. *)
    match t.building.partial with
    | Physical _ ->
      Cleanup_identity.release t.building.cleanup_target;
      t.owner.phase <- Empty;
      Ok ()
    | Unallocated | Tracking _ | Builder _ ->
      Error (conflict "policy construction has no positive physical close")
  ;;

  let fail t failure =
    match check t with
    | Error _ -> ()
    | Ok () -> t.owner.phase <- Retained (Partial t.building, Some failure)
  ;;

  let restored_policy_access t ~source ~basis ~expected =
    let open Result.Let_syntax in
    let%bind () = check t in
    match t.building.partial with
    | Physical { product; runtime; _ } ->
      B.Product.restored_policy_access product ~runtime ~source ~basis ~expected
    | Unallocated | Tracking _ | Builder _ ->
      Error (conflict "restored policy access requires the actual physical Product")
  ;;

  let publish_resource t ~key =
    let open Result.Let_syntax in
    let%bind () = check t in
    match t.building.partial with
    | Physical { product; runtime; _ }
      when key.Key.initialized && Key.same_construction t.building.key key ->
      let resource =
        { cleanup_target = t.building.cleanup_target
        ; key
        ; product
        ; runtime
        ; constructor_graph = Constructor_graph_slot.create ()
        ; native = None
        ; references = 1
        ; close_state = ref Close_state.Open
        }
      in
      t.owner.phase <- Live resource;
      Ok { Lease.owner = t.owner; resource; state = Active }
    | Physical _ -> Error (conflict "policy initialization ACK changed its actual source")
    | Unallocated | Tracking _ | Builder _ ->
      Error (conflict "policy product has no physical cleanup owner")
  ;;

  let publish t ~key = publish_resource t ~key

  let publish_restored t ~key ~access ~expected =
    let open Result.Let_syntax in
    let%bind () = check t in
    let%bind () =
      if Key.equal t.building.key key
      then Ok ()
      else Error (conflict "restored policy publication changed its original source key")
    in
    let%bind () =
      match t.building.partial with
      | Physical { product; binding; runtime }
        when B.Product_identity.equal
               (B.Product.identity product)
               (B.Restored_policy_access.product_identity access)
             && Option.equal
                  phys_equal
                  (B.Product.Binding.runtime binding).native_runtime
                  runtime.native_runtime -> Ok ()
      | Physical _ | Unallocated | Tracking _ | Builder _ ->
        Error (conflict "restored policy publication lost its actual physical Product")
    in
    let%bind () = B.Restored_policy_access.check access ~expected in
    let%bind () =
      Agent_session.Policy_restore_basis.check_construction
        (B.Restored_policy_access.basis access)
        ~state:expected
    in
    publish_resource t ~key
  ;;
end

module Retained_selection = struct
  type t =
    | No_retained_product
    | Retained of Lease.t
end

let pin_retained t ~expected_product =
  let open Result.Let_syntax in
  let%bind () = check_domain t in
  match t.phase, expected_product with
  | Empty, None -> Ok Retained_selection.No_retained_product
  | Native_live resource, None
    when resource.references > 0 && not (Close_state.is_closed !(resource.close_state)) ->
    (match resource.attachment with
     | No_attachment ->
       let%map () = Runtime_native_scope.check_open resource.scope.scope in
       (* There is no Product to pin yet. The subsequent construction selector
          still validates and reserves this same native unit for promotion. *)
       Retained_selection.No_retained_product
     | Attaching _ | Attached _ | Failed_attachment _ ->
       Error (conflict "native Product construction or cleanup remains owned"))
  | Live resource, Some expected
    when resource_references resource > 0
         && (not (resource_closed resource))
         && (not (Constructor_graph_slot.pending resource.constructor_graph))
         && B.Product_identity.equal expected (B.Product.identity resource.product) ->
    let%bind () = check_resource_scope resource in
    change_resource_references resource 1;
    Ok (Retained_selection.Retained { Lease.owner = t; resource; state = Active })
  | ( ( Empty
      | Building _
      | Native_building _
      | Native_live _
      | Live _
      | Retained _
      | Closing _ )
    , (Some _ | None) ) ->
    Error (conflict "selected ordinary policy resource is unavailable or changed")
;;

module Selection = struct
  type t =
    | Build of Build.t
    | Reuse of Lease.t
end

let create () =
  { domain = (Stdlib.Domain.self () :> int); tag = Cell_tag.create (); phase = Empty }
;;

let select t ~key ~expected_product =
  let open Result.Let_syntax in
  let%bind () = check_domain t in
  match t.phase, expected_product with
  | Empty, None ->
    let cleanup_target = Cleanup_identity.create ~domain:t.domain ~cell:t.tag in
    let building =
      { cleanup_target; key; partial = Partial.Unallocated; partial_close = None }
    in
    t.phase <- Building building;
    Ok (Selection.Build { Build.owner = t; building })
  | Live resource, Some expected
    when resource_references resource > 0
         && (not (resource_closed resource))
         && (not (Constructor_graph_slot.pending resource.constructor_graph))
         && B.Product_identity.equal expected (B.Product.identity resource.product)
         && Key.equal resource.key key ->
    let%bind () = check_resource_scope resource in
    change_resource_references resource 1;
    Ok (Selection.Reuse { Lease.owner = t; resource; state = Active })
  | ( ( Empty
      | Building _
      | Native_building _
      | Native_live _
      | Live _
      | Retained _
      | Closing _ )
    , (Some _ | None) ) ->
    Error (conflict "expected physical policy resource is unavailable or changed")
;;

(* Product Key intentionally may project checkpoint installation. Native keys
   never do. At promotion begin compare construction facts, not those unequal
   source options; only the genuine Init ACK can advance the native option. *)
let native_product_construction (native : Native_source_key.t) (key : Key.t) =
  let domain = (Stdlib.Domain.self () :> int) in
  Int.equal native.domain domain
  && Int.equal key.domain domain
  && phys_equal native.actor key.actor
  && phys_equal native.handle key.handle
  && Int.equal native.generation key.generation
  && Agent_session.Session_state.Spec.Resource_identity.equal native.spec key.spec
  && P.Id.Prompt_revision.equal native.artifact.revision_id key.artifact.revision_id
  && Agent_store.Prompt_artifact_store.Artifact.same_content native.artifact key.artifact
  && Agent_session.Runtime_script_preparation.Invocation.equal
       native.invocation
       key.invocation
  && Key.Family.equal native.family key.family
  && String.equal native.profile key.profile
;;

(* Retaining a loan permits factual release, not fresh use while the SAME
   Cell owns attachment construction or unfinished constructor cleanup. Keep
   retained checks and new pins on this one nonyielding eligibility rule. *)
let check_native_borrow t =
  match t.phase with
  | Native_live resource ->
    (match resource.attachment with
     | No_attachment -> Ok ()
     | Attaching _ | Attached _ | Failed_attachment _ ->
       Error (conflict "native attachment construction or cleanup remains owned"))
  | Live resource ->
    if Constructor_graph_slot.pending resource.constructor_graph
    then Error (conflict "native Product constructor cleanup remains owned")
    else Ok ()
  | Empty | Building _ | Native_building _ | Retained _ | Closing _ ->
    Error (conflict "expected native resource is unavailable or changed")
;;

module Native_lease = struct
  type owner = t

  type state =
    | Active
    | Released
    | Last_release

  type direct =
    { owner : owner
    ; resource : native_resource
    ; mutable state : state
    }

  type t =
    | Native of direct
    | Product of
        { lease : Lease.t
        ; native_key : Native_source_key.t
        }

  let create owner resource = Native { owner; resource; state = Active }

  let cleanup_target = function
    | Native t -> t.resource.cleanup_target
    | Product { lease; _ } -> Lease.cleanup_target lease
  ;;

  let identity = function
    | Native t -> B.Prepared_native.identity t.resource.prepared
    | Product { lease; _ } -> B.Product.native_identity (Lease.product lease)
  ;;

  let check_active (t : direct) =
    let open Result.Let_syntax in
    let%bind () = check_domain t.owner in
    match t.state, t.owner.phase with
    | Active, Native_live resource
      when phys_equal resource t.resource
           && not (Close_state.is_closed !(resource.close_state)) -> Ok ()
    | Active, Live resource
      when Option.exists resource.native ~f:(phys_equal t.resource)
           && not (Close_state.is_closed !(t.resource.close_state)) -> Ok ()
    | ( (Active | Released | Last_release)
      , ( Empty
        | Building _
        | Native_building _
        | Native_live _
        | Live _
        | Retained _
        | Closing _ ) ) -> Error (conflict "native resource lease is closed or displaced")
  ;;

  let current_with_native_phase ~check_phase = function
    | Native t ->
      let open Result.Let_syntax in
      let%bind () = check_active t in
      let%bind () = check_phase t in
      Runtime_native_scope.check_open t.resource.scope.scope
    | Product { lease; _ } ->
      let open Result.Let_syntax in
      let%bind () = Lease.check_key lease ~key:lease.resource.key in
      check_native_borrow lease.owner
  ;;

  let current =
    current_with_native_phase ~check_phase:(fun t -> check_native_borrow t.owner)
  ;;

  let check_key t ~key =
    let open Result.Let_syntax in
    let%bind () = current t in
    let actual =
      match t with
      | Native t -> t.resource.native_key
      | Product { native_key; _ } -> native_key
    in
    if Native_source_key.equal actual key
    then Ok ()
    else Error (conflict "retained native source changed")
  ;;

  let check_current_with ~current t ~actor ~handle ~profile ~expected =
    let open Result.Let_syntax in
    let%bind () = current t in
    let%bind key =
      match t with
      | Native t -> Ok t.resource.native_key
      | Product { lease; native_key } ->
        let%map () = Lease.check_policy_key lease ~actor ~handle ~profile ~expected in
        native_key
    in
    Native_source_key.check_current key ~actor ~handle ~profile ~expected
  ;;

  let check_current = check_current_with ~current

  let selected_source = function
    | Native t -> B.Prepared_native.selected_source t.resource.prepared
    | Product { lease; _ } -> B.Product.selected_source (Lease.product lease)
  ;;

  let check_retained_source t ~source ~actor ~handle ~profile ~expected =
    let open Result.Let_syntax in
    (* An actual policy construction may recheck its already retained Source
       after reserving the promoted attachment on this same native unit. *)
    let check_phase (native : direct) =
      match native.owner.phase, native.resource.attachment with
      | Native_live actual, Attaching { key; pinned = true; _ }
        when phys_equal actual native.resource
             && native_product_construction native.resource.native_key key -> Ok ()
      | _ -> check_native_borrow native.owner
    in
    let current = current_with_native_phase ~check_phase in
    let%bind () = check_current_with ~current t ~actor ~handle ~profile ~expected in
    let%bind actual = selected_source t in
    if phys_equal actual source
    then Ok ()
    else Error (conflict "retained native Source identity changed")
  ;;

  let with_resources t ~f =
    let open Result.Let_syntax in
    let%bind () = current t in
    match t with
    | Native t -> B.Prepared_native.with_resources t.resource.prepared ~f
    | Product { lease; _ } ->
      B.Product.with_native_resources (Lease.product lease) ~f:(fun resources ->
        (* Ordinary Factory attaches its actual source loan to the runtime after
           Builder construction. Borrow that SAME retained loan only when the
           common resources still have the legacy view; never replace an already
           attached original receipt, including unavailable or revoked views. *)
        let%bind resources =
          if Agent_session.Prompt_source_lease.is_legacy resources.B.source_lease
          then
            B.with_resource_source_lease
              resources
              ~source_lease:
                (Agent_session.Prompt_source_lease.borrow
                   (Lease.runtime lease).B.source_lease)
            |> Result.map_error ~f:(fun error ->
              P.Error.invalid_request (Error.to_string_hum error))
          else Ok resources
        in
        f resources)
  ;;

  let with_source_resources t ~f =
    with_resources t ~f:(fun resources ->
      let open Result.Let_syntax in
      let%bind source = selected_source t in
      f ~source resources)
  ;;

  let close_reported = function
    | Product { lease; _ } -> Lease.close_reported lease
    | Native t ->
      (match check_domain t.owner with
       | Error error -> Close_report.of_outcome (Error (Failure.Protocol error))
       | Ok () ->
         (match t.state with
          | Released -> Close_report.success ()
          | Last_release ->
            (match Close_state.report !(t.resource.close_state) with
             | Some report -> report
             | None -> close_target_reported t.owner (Native_resource t.resource))
          | Active ->
            (match check_active t with
             | Error error -> Close_report.of_outcome (Error (Failure.Protocol error))
             | Ok () ->
               if t.resource.references <= 0
               then
                 Close_report.of_outcome
                   (Error
                      (Failure.Protocol
                         (conflict "native resource reference was already released")))
               else (
                 t.resource.references <- t.resource.references - 1;
                 if t.resource.references > 0
                 then (
                   t.state <- Released;
                   Close_report.success ())
                 else (
                   let target = Native_resource t.resource in
                   t.state <- Last_release;
                   t.owner.phase <- Retained (target, None);
                   close_target_reported t.owner target)))))
  ;;

  let close t = Close_report.resume (close_reported t)
end

module Native_source = struct
  type cell = t

  type selection =
    | Native of native_resource
    | Product of resource

  type t =
    { owner : cell
    ; selection : selection
    ; source : B.Source.t
    ; identity : B.Native_identity.t
    ; actor : Agent_session.Session_actor.t
    ; handle : Agent_store.Session_store.Handle.t
    }

  let source t = t.source
  let identity t = t.identity

  let selected_identity = function
    | Native resource -> B.Prepared_native.identity resource.prepared
    | Product resource -> B.Product.native_identity resource.product
  ;;

  let selected_source owner selection ~actor ~handle ~profile ~expected =
    let open Result.Let_syntax in
    let%bind () = check_domain owner in
    let%bind () = check_native_borrow owner in
    let%bind () =
      match selection, owner.phase with
      | Native selected, Native_live actual
        when phys_equal selected actual
             && selected.references > 0
             && not (Close_state.is_closed !(selected.close_state)) -> Ok ()
      | Native selected, Live actual
        when Option.exists actual.native ~f:(phys_equal selected)
             && selected.references > 0
             && not (Close_state.is_closed !(selected.close_state)) -> Ok ()
      | Product selected, Live actual
        when phys_equal selected actual
             && selected.references > 0
             && not (Close_state.is_closed !(selected.close_state)) -> Ok ()
      | ( (Native _ | Product _)
        , ( Empty
          | Building _
          | Native_building _
          | Native_live _
          | Live _
          | Retained _
          | Closing _ ) ) ->
        Error (conflict "installed native Source is closed or displaced")
    in
    match selection with
    | Native resource ->
      let%bind () = Runtime_native_scope.check_open resource.scope.scope in
      let%bind () =
        Native_source_key.check_current
          resource.native_key
          ~actor
          ~handle
          ~profile
          ~expected
      in
      B.Prepared_native.selected_source resource.prepared
    | Product resource ->
      let%bind () = Key.check_policy resource.key ~actor ~handle ~profile ~expected in
      B.Product.selected_source resource.product
  ;;

  let check t ~profile ~expected =
    let open Result.Let_syntax in
    let%bind actual =
      selected_source
        t.owner
        t.selection
        ~actor:t.actor
        ~handle:t.handle
        ~profile
        ~expected
    in
    if
      phys_equal actual t.source
      && B.Native_identity.equal t.identity (selected_identity t.selection)
    then Ok ()
    else Error (conflict "installed native Source identity changed")
  ;;

  let capture owner ~expected_native ~actor ~handle ~profile ~expected =
    let open Result.Let_syntax in
    let%bind () = check_domain owner in
    match owner.phase, expected_native with
    | Empty, None -> Ok None
    | Native_live resource, Some identity
    | Live { native = Some resource; _ }, Some identity ->
      let selection = Native resource in
      let%bind source =
        selected_source owner selection ~actor ~handle ~profile ~expected
      in
      if B.Native_identity.equal identity (selected_identity selection)
      then Ok (Some { owner; selection; source; identity; actor; handle })
      else Error (conflict "installed native Source differs from its Owner identity")
    | Live resource, Some identity ->
      let selection = Product resource in
      let%bind source =
        selected_source owner selection ~actor ~handle ~profile ~expected
      in
      if B.Native_identity.equal identity (selected_identity selection)
      then Ok (Some { owner; selection; source; identity; actor; handle })
      else Error (conflict "installed native Source differs from its Owner identity")
    | ( ( Empty
        | Building _
        | Native_building _
        | Native_live _
        | Live _
        | Retained _
        | Closing _ )
      , (Some _ | None) ) ->
      Error (conflict "installed native Source is unavailable or has incomplete custody")
  ;;
end

module Native_build = struct
  type owner = t

  type t =
    { owner : owner
    ; building : native_building
    }

  let check t =
    let open Result.Let_syntax in
    let%bind () = check_domain t.owner in
    match t.owner.phase with
    | Native_building actual when phys_equal actual t.building -> Ok ()
    | Empty
    | Building _
    | Native_building _
    | Native_live _
    | Live _
    | Retained _
    | Closing _ -> Error (conflict "native construction owner was displaced")
  ;;

  let cleanup_target t = t.building.cleanup_target

  let tracking t tracking =
    let open Result.Let_syntax in
    let%bind () = check t in
    match t.building.tracking, t.building.scope, t.building.prepared with
    | None, (None | Some _), None ->
      t.building.tracking <- Some tracking;
      Ok ()
    | (Some _ | None), (Some _ | None), (Some _ | None) ->
      Error (conflict "native tracking was already attached")
  ;;

  let scope t scope ~close =
    let open Result.Let_syntax in
    let%bind () = check t in
    match t.building.tracking, t.building.scope, t.building.prepared with
    | (None | Some _), None, None ->
      t.building.scope <- Some { scope; close };
      Ok ()
    | (Some _ | None), (Some _ | None), (Some _ | None) ->
      Error (conflict "native scope was already attached or preparation was published")
  ;;

  let prepared t prepared ~source =
    let open Result.Let_syntax in
    let%bind () = check t in
    let%bind () =
      B.Prepared_native.check
        prepared
        ~source
        ~session_id:
          (Agent_store.Session_store.Handle.session_id t.building.native_key.handle)
        ~permission_profile_digest:t.building.native_key.profile
    in
    let%bind () =
      if Native_source_key.matches_source t.building.native_key source
      then Ok ()
      else Error (conflict "prepared native has another captured source")
    in
    match t.building.tracking, t.building.scope, t.building.prepared with
    | Some _, Some _, None ->
      t.building.prepared <- Some prepared;
      Ok ()
    | (Some _ | None), (Some _ | None), (Some _ | None) ->
      Error (conflict "native preparation lost its original scope")
  ;;

  let publish t ~key =
    let open Result.Let_syntax in
    let%bind () = check t in
    let%bind () =
      if Native_source_key.equal t.building.native_key key
      then Ok ()
      else Error (conflict "native publication changed its actual source key")
    in
    match t.building.scope, t.building.prepared with
    | Some scope, Some prepared ->
      let%bind () = Runtime_native_scope.check_prepared scope.scope ~prepared in
      let resource : native_resource =
        { cleanup_target = t.building.cleanup_target
        ; native_key = key
        ; prepared
        ; scope
        ; attachment = No_attachment
        ; constructor_graph = None
        ; references = 1
        ; close_state = ref Close_state.Open
        }
      in
      t.owner.phase <- Native_live resource;
      Ok (Native_lease.create t.owner resource)
    | None, (Some _ | None) | Some _, None ->
      Error (conflict "native publication has no actual physical cleanup owner")
  ;;

  let fail t failure =
    match check t with
    | Error _ -> ()
    | Ok () -> t.owner.phase <- Retained (Native_partial t.building, Some failure)
  ;;
end

module Native_selection = struct
  type t =
    | Build of Native_build.t
    | Reuse of Native_lease.t
end

let select_native t ~key ~expected_native =
  let open Result.Let_syntax in
  let%bind () = check_domain t in
  let reuse (resource : native_resource) expected =
    let%bind () = Runtime_native_scope.check_open resource.scope.scope in
    if
      resource.references > 0
      && (not (Close_state.is_closed !(resource.close_state)))
      && Native_source_key.equal resource.native_key key
      && B.Native_identity.equal expected (B.Prepared_native.identity resource.prepared)
    then (
      resource.references <- resource.references + 1;
      Ok (Native_selection.Reuse (Native_lease.create t resource)))
    else Error (conflict "selected native resource is unavailable or changed")
  in
  match t.phase, expected_native with
  | Empty, None ->
    let cleanup_target = Cleanup_identity.create ~domain:t.domain ~cell:t.tag in
    let building =
      { cleanup_target; native_key = key; tracking = None; scope = None; prepared = None }
    in
    t.phase <- Native_building building;
    Ok (Native_selection.Build { Native_build.owner = t; building })
  | Native_live resource, Some expected
  | Live { native = Some resource; _ }, Some expected ->
    let%bind () = check_native_borrow t in
    reuse resource expected
  | Live ({ native = None; _ } as resource), Some expected
    when resource.references > 0
         && (not (Close_state.is_closed !(resource.close_state)))
         && resource.key.Key.initialized
         && native_product_construction key resource.key
         && Option.equal
              P.Run_source.equal
              key.Native_source_key.source
              resource.key.source
         && B.Native_identity.equal expected (B.Product.native_identity resource.product)
    ->
    (* Loaded-first bridge: borrow the ORIGINAL Product Cell reference and
       closer. No Native_build, new scope, Manager or second native owner. *)
    let%bind () = check_native_borrow t in
    resource.references <- resource.references + 1;
    let lease = { Lease.owner = t; resource; state = Active } in
    Ok (Native_selection.Reuse (Native_lease.Product { lease; native_key = key }))
  | ( ( Empty
      | Building _
      | Native_building _
      | Native_live _
      | Live _
      | Retained _
      | Closing _ )
    , (Some _ | None) ) ->
    Error (conflict "expected native resource is unavailable or changed")
;;

let pin_native t ~expected_native ~actor ~handle ~profile ~expected =
  let open Result.Let_syntax in
  let%bind () = check_domain t in
  let key_and_select key =
    let%bind () = Native_source_key.check_current key ~actor ~handle ~profile ~expected in
    match select_native t ~key ~expected_native:(Some expected_native) with
    | Ok (Native_selection.Reuse loan) -> Ok loan
    | Ok (Build _) -> Error (conflict "retained native pin cannot begin construction")
    | Error _ as failure -> failure
  in
  match t.phase with
  | Native_live resource -> key_and_select resource.native_key
  | Live { native = Some resource; _ } -> key_and_select resource.native_key
  | Live ({ native = None; _ } as resource)
    when resource.references > 0
         && (not (Close_state.is_closed !(resource.close_state)))
         && B.Native_identity.equal
              expected_native
              (B.Product.native_identity resource.product) ->
    let%bind () = Key.check_policy resource.key ~actor ~handle ~profile ~expected in
    let%bind native_key = Native_source_key.of_product_key resource.key ~expected in
    let%bind () =
      Native_source_key.check_current native_key ~actor ~handle ~profile ~expected
    in
    let%bind () = check_native_borrow t in
    resource.references <- resource.references + 1;
    let lease = { Lease.owner = t; resource; state = Active } in
    Ok (Native_lease.Product { lease; native_key })
  | Empty | Building _ | Native_building _ | Live _ | Retained _ | Closing _ ->
    Error (conflict "retained native resource is unavailable or changed")
;;

module Promotion = struct
  type owner = t

  type t =
    { owner : owner
    ; resource : native_resource
    ; partial : promotion_partial
    }

  let cleanup_target t = t.resource.cleanup_target
  let prepared t = t.resource.prepared

  let check_owner t =
    let open Result.Let_syntax in
    let%bind () = check_domain t.owner in
    match t.owner.phase, t.resource.attachment with
    | Native_live actual, Attaching partial
      when phys_equal actual t.resource && phys_equal partial t.partial && partial.pinned
      -> Ok ()
    | ( ( Empty
        | Building _
        | Native_building _
        | Native_live _
        | Live _
        | Retained _
        | Closing _ )
      , (No_attachment | Attaching _ | Attached _ | Failed_attachment _) ) ->
      Error (conflict "native promotion owner was displaced")
  ;;

  let check t =
    let open Result.Let_syntax in
    let%bind () = check_owner t in
    Runtime_native_scope.check_open t.resource.scope.scope
  ;;

  let attachment t attachment =
    let open Result.Let_syntax in
    let%bind () = check_owner t in
    let%bind () =
      if
        B.Native_identity.equal
          (B.Product_attachment.native_identity attachment)
          (B.Prepared_native.identity t.resource.prepared)
      then Ok ()
      else Error (conflict "promotion attachment has another native unit")
    in
    match t.partial.attachment with
    | None ->
      t.partial.attachment <- Some attachment;
      Ok ()
    | Some _ -> Error (conflict "promotion attachment was already selected")
  ;;

  let product t ~product ~binding =
    let open Result.Let_syntax in
    let%bind () = check_owner t in
    let%bind attachment =
      match t.partial.attachment with
      | Some attachment -> Ok attachment
      | None -> Error (conflict "promotion Product lost its actual attachment")
    in
    let%bind () = B.Product.check_attachment product ~attachment in
    let%bind binding_product = B.Product.Binding.product_identity binding in
    let%bind () =
      if
        B.Native_identity.equal
          (B.Product.native_identity product)
          (B.Prepared_native.identity t.resource.prepared)
        && B.Product_identity.equal (B.Product.identity product) binding_product
      then Ok ()
      else Error (conflict "promotion Product/binding has another physical owner")
    in
    match t.partial.product with
    | None ->
      t.partial.product <- Some (product, binding);
      Ok ()
    | Some _ -> Error (conflict "promotion Product was already attached")
  ;;

  let physical t runtime =
    let open Result.Let_syntax in
    let%bind () = check_owner t in
    match t.partial.product, t.partial.runtime with
    | Some (_, binding), None ->
      let actual = B.Product.Binding.runtime binding in
      if
        phys_equal actual.worker runtime.B.worker
        && Option.equal phys_equal actual.moderator_manager runtime.moderator_manager
        && Option.equal phys_equal actual.native_runtime runtime.native_runtime
      then (
        t.partial.runtime <- Some runtime;
        Ok ())
      else Error (conflict "promotion physical runtime differs from its actual binding")
    | None, (Some _ | None) | Some _, Some _ ->
      Error (conflict "promotion physical runtime was already selected")
  ;;

  let actual_product t =
    match t.partial.product, t.partial.runtime with
    | Some (product, binding), Some runtime -> Ok (product, binding, runtime)
    | None, (Some _ | None) | Some _, None ->
      Error (conflict "promotion has no actual physical Product")
  ;;

  let binding t =
    let open Result.Let_syntax in
    let%bind () = check t in
    let%map _, binding, _ = actual_product t in
    binding
  ;;

  let restored_policy_access t ~source ~basis ~expected =
    let open Result.Let_syntax in
    let%bind () = check t in
    let%bind product, _, runtime = actual_product t in
    B.Product.restored_policy_access product ~runtime ~source ~basis ~expected
  ;;

  let advance_initialized t ~acknowledgement ~native_key ~key ~expected =
    let open Result.Let_syntax in
    let%bind () = check_owner t in
    let%bind _, _, runtime = actual_product t in
    let%bind () =
      A.Initialization_scope.Source_handoff.check
        acknowledgement
        ~actor:native_key.Native_source_key.actor
        ~moderator_manager:runtime.B.moderator_manager
        ~expected
    in
    let before = A.Initialization_scope.Source_handoff.before_source acknowledgement in
    let after = A.Initialization_scope.Source_handoff.after_source acknowledgement in
    let%bind () =
      if
        native_product_construction native_key key
        && Key.same_construction t.partial.key key
        && key.Key.initialized
        && Option.equal P.Run_source.equal native_key.source after
        && Option.equal P.Run_source.equal key.source after
      then Ok ()
      else Error (conflict "initialization source handoff changed promotion identity")
    in
    match t.partial.acknowledged with
    | Some actual when phys_equal actual acknowledgement ->
      if Native_source_key.equal t.resource.native_key native_key
      then Ok ()
      else Error (conflict "repeated initialization handoff changed its native key")
    | Some _ -> Error (conflict "promotion received another initialization handoff")
    | None ->
      if
        (not t.partial.key.Key.initialized)
        && Option.equal P.Run_source.equal t.resource.native_key.source before
        && Native_source_key.equal
             { t.resource.native_key with source = after }
             native_key
      then (
        t.resource.native_key <- native_key;
        t.partial.acknowledged <- Some acknowledgement;
        Ok ())
      else
        Error (conflict "initialization source handoff has another native before basis")
  ;;

  let publish_resource t ~key =
    let open Result.Let_syntax in
    let%bind () = check t in
    let%bind product, _, runtime = actual_product t in
    let%bind () =
      if
        key.Key.initialized
        && native_product_construction t.resource.native_key key
        && Option.equal P.Run_source.equal t.resource.native_key.source key.source
        && Key.same_construction t.partial.key key
      then Ok ()
      else Error (conflict "promotion publication changed its actual source")
    in
    let constructor_graph = Constructor_graph_slot.create () in
    let resource =
      { cleanup_target = t.resource.cleanup_target
      ; key
      ; product
      ; runtime
      ; constructor_graph
      ; native = Some t.resource
      ; references = 0
      ; close_state = t.resource.close_state
      }
    in
    (* M becomes C, without changing the common refcount or any zero interval. *)
    t.partial.pinned <- false;
    t.resource.constructor_graph <- Some constructor_graph;
    t.resource.attachment <- Attached t.partial;
    t.owner.phase <- Live resource;
    Ok { Lease.owner = t.owner; resource; state = Active }
  ;;

  let publish_ordinary t ~key =
    let open Result.Let_syntax in
    let%bind () = check t in
    let%bind () =
      if t.partial.key.Key.initialized
      then
        if Key.equal t.partial.key key
        then Ok ()
        else Error (conflict "ordinary Ready promotion changed its source key")
      else (
        match t.partial.acknowledged with
        | Some _ -> Ok ()
        | None -> Error (conflict "ordinary Pending promotion has no genuine Init ACK"))
    in
    publish_resource t ~key
  ;;

  let publish_restored t ~key ~access ~expected =
    let open Result.Let_syntax in
    let%bind () = check t in
    let%bind product, _, _ = actual_product t in
    let%bind () =
      if
        Key.equal t.partial.key key
        && B.Product_identity.equal
             (B.Product.identity product)
             (B.Restored_policy_access.product_identity access)
      then Ok ()
      else Error (conflict "restored promotion changed its actual Product/source")
    in
    let%bind () = B.Restored_policy_access.check access ~expected in
    let%bind () =
      Agent_session.Policy_restore_basis.check_construction
        (B.Restored_policy_access.basis access)
        ~state:expected
    in
    publish_resource t ~key
  ;;

  let closed t =
    let open Result.Let_syntax in
    let%bind () = check_owner t in
    let%bind _, _, runtime = actual_product t in
    Eio.Cancel.protect (fun () ->
      (* Real callback completion joins attachment-local work. N remains owned
         by A/B, and a later actual admission may construct another attachment. *)
      B.close runtime;
      t.partial.pinned <- false;
      t.resource.references <- t.resource.references - 1;
      t.resource.attachment <- No_attachment;
      if t.resource.references > 0
      then Ok ()
      else (
        let target = Native_resource t.resource in
        t.owner.phase <- Retained (target, None);
        close_target t.owner target))
  ;;

  let fail t failure =
    match check_owner t with
    | Error _ -> ()
    | Ok () ->
      t.partial.pinned <- false;
      t.resource.references <- t.resource.references - 1;
      t.resource.attachment <- Failed_attachment (t.partial, failure);
      if t.resource.references = 0
      then t.owner.phase <- Retained (Native_resource t.resource, Some failure)
  ;;
end

let reserve_native_promotion t (resource : native_resource) ~key =
  let partial =
    { key
    ; attachment = None
    ; product = None
    ; runtime = None
    ; acknowledged = None
    ; pinned = true
    }
  in
  resource.references <- resource.references + 1;
  resource.attachment <- Attaching partial;
  Ok { Promotion.owner = t; resource; partial }
;;

let begin_native_promotion t ~(loan : Native_lease.t) ~native_key ~key =
  let open Result.Let_syntax in
  let%bind () = check_domain t in
  let%bind direct =
    match loan with
    | Native direct -> Ok direct
    | Product _ -> Error (conflict "native unit already has its actual Product")
  in
  let%bind () =
    if phys_equal direct.owner t
    then Ok ()
    else Error (conflict "native promotion loan belongs to another Cell")
  in
  let%bind () = Native_lease.check_key loan ~key:native_key in
  let%bind () =
    if
      native_product_construction native_key key
      && ((not key.Key.initialized)
          || Option.equal
               P.Run_source.equal
               native_key.Native_source_key.source
               key.source)
    then Ok ()
    else Error (conflict "native promotion has another Product construction")
  in
  match t.phase, direct.resource.attachment with
  | Native_live resource, No_attachment when phys_equal resource direct.resource ->
    reserve_native_promotion t resource ~key
  | ( ( Empty
      | Building _
      | Native_building _
      | Native_live _
      | Live _
      | Retained _
      | Closing _ )
    , (No_attachment | Attaching _ | Attached _ | Failed_attachment _) ) ->
    Error (conflict "native unit already has Product construction or retained cleanup")
;;

module Product_selection = struct
  type t =
    | Build of Build.t
    | Reuse of Lease.t
    | Promote of Promotion.t
end

let select_for_product t ~key ~profile ~expected ~expected_product =
  let open Result.Let_syntax in
  let%bind () = check_domain t in
  match t.phase, expected_product with
  | Native_live resource, None ->
    let%bind () =
      Native_source_key.check_current
        resource.native_key
        ~actor:key.Key.actor
        ~handle:key.handle
        ~profile
        ~expected
    in
    let%bind () = Runtime_native_scope.check_open resource.scope.scope in
    let%bind () =
      if
        resource.references > 0
        && (not (Close_state.is_closed !(resource.close_state)))
        && Bool.equal
             key.Key.initialized
             (match expected.Agent_session.Session_state.runtime_initialization with
              | Ready -> true
              | Pending _ -> false)
        && native_product_construction resource.native_key key
        && ((not key.Key.initialized)
            || Option.equal P.Run_source.equal resource.native_key.source key.source)
      then Ok ()
      else Error (conflict "native Product selection changed its actual construction")
    in
    (match resource.attachment with
     | No_attachment ->
       let%map promotion = reserve_native_promotion t resource ~key in
       Product_selection.Promote promotion
     | Attaching _ | Attached _ | Failed_attachment _ ->
       Error (conflict "native Product construction or cleanup remains owned"))
  | ( ( Empty
      | Building _
      | Native_building _
      | Native_live _
      | Live _
      | Retained _
      | Closing _ )
    , (Some _ | None) ) ->
    let%map selected = select t ~key ~expected_product in
    (match selected with
     | Build build -> Product_selection.Build build
     | Reuse loan -> Product_selection.Reuse loan)
;;
