open! Core
module P = Agent_protocol
module Source = Policy_cause_data.Caller_source

type t =
  { domain : int
  ; session_id : P.Id.Session.t
  ; artifact : Agent_store.Prompt_artifact_store.Artifact.t
  ; worker : Operation_worker.t
  }

let conflict message = Error (P.Error.create Conflict ~message ~retryable:false ())

let check_installation t ~(state : Session_state.t) ~worker ~manager =
  let open Result.Let_syntax in
  let%bind () =
    if Int.equal t.domain (Stdlib.Domain.self () :> int)
    then Ok ()
    else conflict "unmoderated source belongs to another domain"
  in
  let%bind () =
    if
      phys_equal t.worker worker
      && P.Id.Session.equal t.session_id state.identity.session_id
      && P.Id.Prompt_revision.equal t.artifact.revision_id state.spec.prompt_revision_id
    then Ok ()
    else conflict "unmoderated source differs from its actual worker or Artifact"
  in
  let%bind () =
    if Option.is_none manager && Option.is_none state.moderator
    then Ok ()
    else conflict "unmoderated source cannot replace an installed moderator"
  in
  let%bind () =
    match
      state.runtime_initialization, Inference.Selection.view state.spec.inference_target
    with
    | Ready, Captured _ -> Ok ()
    | Ready, Unresolved | Pending _, (Captured _ | Unresolved) ->
      conflict "unmoderated source requires an initialized captured target"
  in
  let%map source =
    Source.Unmoderated.create
      ~generation:state.identity.generation
      ~prompt_revision_id:state.spec.prompt_revision_id
      ~configuration_revision:state.spec.configuration_revision
      ~artifact_manifest_sha256:t.artifact.manifest_sha256
  in
  Source.unmoderated source
;;

module Binding = struct
  type source = t

  type t =
    { source : source
    ; generation : int
    ; spec : Session_state.Spec.t
    ; identity : Session_state.Spec.Resource_identity.t
    ; value : Source.t
    }

  let capture source ~(state : Session_state.t) value =
    { source
    ; generation = state.identity.generation
    ; spec = state.spec
    ; identity = Session_state.Spec.Resource_identity.capture state.spec
    ; value
    }
  ;;

  let check t ~(state : Session_state.t) ~worker ~manager =
    let open Result.Let_syntax in
    let%bind value = check_installation t.source ~state ~worker ~manager in
    if
      Int.equal t.generation state.identity.generation
      && (phys_equal t.spec state.spec
          || Session_state.Spec.Resource_identity.equal
               t.identity
               (Session_state.Spec.Resource_identity.capture state.spec))
      && Source.equal t.value value
    then Ok t.value
    else conflict "unmoderated source installation basis changed"
  ;;

  let check_source t source ~state ~worker ~manager =
    let open Result.Let_syntax in
    let%bind _ = check t ~state ~worker ~manager in
    if phys_equal t.source source
    then Ok ()
    else conflict "retained runtime has a different original no-moderator fact"
  ;;

  let reconfigure t ~previous ~(candidate : Session_state.t) ~worker ~manager =
    let open Result.Let_syntax in
    let%bind _ = check t ~state:previous ~worker ~manager in
    let%bind () =
      if
        (not (Int64.equal previous.spec.configuration_revision Int64.max_value))
        && Int64.equal
             candidate.spec.configuration_revision
             Int64.(previous.spec.configuration_revision + 1L)
        && P.Id.Session.equal previous.identity.session_id candidate.identity.session_id
        && Int.equal previous.identity.generation candidate.identity.generation
      then Ok ()
      else
        conflict
          "unmoderated configuration transition has a different identity or revision"
    in
    let expected =
      { previous.spec with
        inference_target = candidate.spec.inference_target
      ; configuration_revision = candidate.spec.configuration_revision
      }
    in
    let%bind () =
      if
        Session_state.Spec.Resource_identity.equal
          (Session_state.Spec.Resource_identity.capture expected)
          (Session_state.Spec.Resource_identity.capture candidate.spec)
      then Ok ()
      else conflict "unmoderated configuration transition changes another runtime field"
    in
    let%map value = check_installation t.source ~state:candidate ~worker ~manager in
    capture t.source ~state:candidate value
  ;;
end

let bind t ~state ~worker ~manager =
  let%map.Result value = check_installation t ~state ~worker ~manager in
  Binding.capture t ~state value
;;

module Host = struct
  let create ~session_id ~artifact ~worker =
    { domain = (Stdlib.Domain.self () :> int); session_id; artifact; worker }
  ;;
end
