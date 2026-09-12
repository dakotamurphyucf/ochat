(** Shared native extension catalog for runtime construction and contract audits.
    These are the standard extensibility/authoring tools, not all ordinary native
    tools or user-provided registrations. Names derive from the same exhaustive
    variant used to construct registrations. *)
val names : string list

(** Exact builtin-declaration lookup. Does not inspect or expand other tool forms. *)
val declares : Prompt.Chat_markdown.top_level_elements list -> string -> bool

(** Whether a prompt explicitly declares any standard catalog tool. This is
    syntax inspection only and does not grant registration or execution. *)
val declares_any : Prompt.Chat_markdown.top_level_elements list -> bool

(** Construct only explicitly declared standard extension registrations. Execution
    tools require the supplied opt-in one-off policy; authoring tools require an
    explicit host or return a configuration error. This preserves runtime-builder
    ordering and availability. It neither invokes implementations nor borrows a
    session, creates child sessions, initializes scripts or installs resources.
    The reference helper creates its private query key and validates its corpus.
    Actual invocation still requires the owning runtime's current service scopes. *)
val registrations
  :  env:Eio_unix.Stdenv.base
  -> elements:Prompt.Chat_markdown.top_level_elements list
  -> one_off_policy:Chat_response.One_off_request.policy option
  -> authoring_validation_host:Chat_response.Authoring_validation.host option
  -> (Chat_response.Agent_runtime.native_registration list, Agent_protocol.Error.t) result
