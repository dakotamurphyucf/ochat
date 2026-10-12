open Core

(** One actual native implementation and its registration-owned source preparer.
    Never serialized, discovered by name/body/path, or created from a public tool
    reference. The upper owner creates this beside its genuine native function,
    from the actual emitted declaration and admitted original source lease.

    The callback is borrowed for that resource/runtime lifetime and must reject
    after either original source or native owner closes. It receives the original
    launch owner and bounded request from the actual selected job scope; neither
    request JSON nor a stored artifact reference supplies source authority. *)
type t

val create
  :  implementation:Ochat_function.t
  -> prepare:
       (owner:Agent_protocol.Job.launch_owner
        -> request:Background_request.t
        -> checkpoint:(unit -> unit)
        -> (Background_request.Agent_artifact.t option, Agent_protocol.Error.t) result)
  -> t

(** Before any callback, require the exact actual native function value stored in
    this registration. Managed and different native implementations reject. This
    physical identity is intentional: Tool_capability retains the actual admitted
    function unchanged, including inherited bindings. Names, descriptors or equal
    code do not establish that identity.

    The caller must first resolve this binding within its exact selected registry
    and validate the request against those same pins. The callback checks current
    actor/generation and original source ownership before reads. It distinguishes
    persistent resume/replay through its existing admitted request decoder; those
    operations must not read fresh source or reserve a new child here. New capture
    installs/verifies immutable selected code before returning a reference. This
    adapter invokes no native implementation and creates no session/job effects.

    Poll before callback and after any yielding preparation before result delivery.
    Original checkpoint/callback exceptions and cancellation propagate unchanged;
    no callbacks, invocation Startup or borrowed checkpoint are retained by the
    returned immutable reference. Count/work admission for registration-list scans
    belongs to the actual upper runtime; this operation checks one registration. *)
val prepare
  :  t
  -> binding:Tool_capability.binding
  -> owner:Agent_protocol.Job.launch_owner
  -> request:Background_request.t
  -> checkpoint:(unit -> unit)
  -> (Background_request.Agent_artifact.t option, Agent_protocol.Error.t) result
