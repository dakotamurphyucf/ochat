(** Prompt generator functor.

    {1 Overview}

    The [Meta_prompt] module provides a {e minimal} – yet fully-typed –
    bridge between a {b task description} (any OCaml value that can be
    rendered as Markdown) and a {b prompt} record that is ready to be sent to
    an LLM.  It is implemented as the functor {!Make}, which only requires
    two small interfaces:

    • A [Task] module exposing the type [t] together with a single
      serialisation function {!Task.to_markdown}.

    • A [Prompt] constructor offering {!Prompt.make}.  This mirrors the
      record defined in {!module:Prompts} but the functor stays agnostic –
      any module with a compatible signature will do.

    The generated [generate] function takes an optional template on disk, a
    list of key–value pairs for placeholder substitution, and an optional
    {{!Eio.Stdenv}Eio} environment in case the template needs to be read from
    the filesystem.  When no template is supplied the task is converted to
    Markdown verbatim and wrapped in a default explanatory header.

    {1 Placeholder substitution}

    A template can reference placeholders using the familiar
    {{[ {{KEY}} ]}} syntax supported by {!Template.render}.  The mapping is
    built as follows (later entries shadow earlier ones):

    {ol
      {- The caller-supplied [params] list.}
      {- A single automatically-generated pair ["TASK_MARKDOWN"] rendered by
         {!Task.to_markdown}.}}

    {1 API}
  *)

module Make
    (Task : sig
       type t

       (** Serialise a task to GitHub-flavoured Markdown. *)
       val to_markdown : t -> string
     end)
    (Prompt : sig
       type t

       (** Low-level prompt constructor.  Except for the mandatory [body]
           argument all fields are optional and default to sensible values in
           the functor implementation. *)
       val make
         :  ?header:string
         -> ?footnotes:string list
         -> ?metadata:(string * string) list
         -> body:string
         -> unit
         -> t
     end) : sig
  (** [generate ?env ?template ?params task] returns a concrete prompt ready
        to be dispatched to the LLM back-end.

        @param env   An {{!Eio.Stdenv}Eio} environment providing filesystem
                      access.  It is {b required} only when [template] points
                      to an external file.
        @param template  Path to a template file.  If omitted, the task’s
                        Markdown representation is used as the prompt body and
                        a default header explaining the meta-prompting set-up
                        is prepended.
        @param params    Extra placeholder bindings.  These take precedence
                        over automatically generated ones.
        @raise Invalid_argument  if [template] is set but [env] is [None].
      *)
  val generate
    :  ?env:< fs : Eio.Fs.dir_ty Eio.Path.t ; .. >
    -> ?template:string
    -> ?params:(string * string) list
    -> Task.t
    -> Prompt.t
end
