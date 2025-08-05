(** Minimal representation of a user task.

    A value of type [t] captures *what* the agent is supposed to do in a
    format that is easy to serialise and embed in system- or
    meta-prompts.  Higher-level layers (planners, schedulers, …) can
    attach additional metadata in separate structures without having to
    modify this module, which keeps the on-disk format of persisted
    sessions stable. *)

type t =
  { description : string
    (** Human-readable specification of the task.  Must be non-empty;
            this module does not enforce the invariant but downstream
            consumers may rely on it. *)
  ; context : string option
    (** Additional background information that may help the agent
            fulfil the request.  When [None] or [[Some ""]] the context
            block is omitted from the rendered Markdown. *)
  ; tags : string list
    (** Free-form tags such as ["refactor"], ["urgent"], … used for
            filtering or routing.  Empty list if no tags are provided. *)
  }

(** [make ?context ?tags description] returns a fresh task record.

    Default values:
    • [?context] = [None]
    • [?tags]    = [[]]

    Example creating a tagged task with extra context:
    {[
      let t =
        Task_intf.make
          ~context:"The repository is large; focus on src/."
          ~tags:[ "refactor"; "high-prio" ]
          "Rename module X to Y everywhere" in
      print_endline (Task_intf.to_markdown t)
      (* output:
         ## Task
         Rename module X to Y everywhere

         ### Context
         The repository is large; focus on src/.
         Tags: refactor, high-prio
      *)
    ]} *)
val make : ?context:string -> ?tags:string list -> string -> t

(** [to_markdown t] renders a task as a Markdown fragment with the
    following layout:

    {v
      ## Task
      <description>

      ### Context
      <context>
      Tags: tag1, tag2
    v}

    The [Context] block and [Tags] footer are omitted when the
    corresponding fields are absent or empty.  The function performs no
    Markdown escaping. *)
val to_markdown : t -> string
