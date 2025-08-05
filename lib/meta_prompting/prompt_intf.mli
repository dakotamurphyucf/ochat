(** Structured representation of a textual prompt destined for a Large
    Language Model.

    The record keeps the different logical sections of the prompt separate so
    that callers can generate, inspect or transform them individually before
    collapsing everything into a single string with {!to_string}.  The exact
    layout produced by {!to_string} is documented below. *)

type t =
  { header : string option
    (** Optional, typically short, prelude that is inserted *verbatim* at
            the very beginning of the prompt.

            Common usages include “system” directives or a one-line high-level
            description of the task for the model.  Leading and trailing
            whitespace is stripped; an empty result is ignored. *)
  ; body : string
    (** Main prompt content.  End-users usually provide this in
            ChatMarkdown / ChatML format but {!to_string} does not try to parse
            or validate the value – it is concatenated as-is. *)
  ; footnotes : string list
    (** Additional blocks appended *after* [body].  Each block is
            separated from the previous one by the ASCII ruler “\n---\n”.

            Use this for long explanations, examples or citations that would
            otherwise distract from the main instruction. *)
  ; metadata : (string * string) list
    (** Arbitrary key/value pairs copied verbatim at the end of the final
            prompt as HTML comments, one per line, of the form:

            {v <!-- key: value --> v}

            This information is invisible to the model but is convenient for
            debugging and provenance tracking.  More recent entries appear
            first when rendered by {!to_string}. *)
  }

(** [make ?header ?footnotes ?metadata ~body ()] constructs a prompt record.

    Parameters default to:
    • [?header] – absent
    • [?footnotes] – [[]]
    • [?metadata] – [[]]

    No validation is performed; callers are responsible for supplying content
    that makes sense for their LLM backend. *)
val make
  :  ?header:string
  -> ?footnotes:string list
  -> ?metadata:(string * string) list
  -> body:string
  -> unit
  -> t

(** [to_string t] concatenates all the sections in the following order and
    returns a single string suitable for transmission to an LLM:

    {ol
    {- [header] followed by a newline (omitted if [header] is [None] or empty).}
    {- [body].}
    {- [footnotes] blocks separated by "\n---\n" if [footnotes] is non-empty.}
    {- [metadata] rendered as HTML comments, one per line, in reverse insertion
       order.}}

    An empty prompt is therefore possible but unlikely useful. *)
val to_string : t -> string

(** [add_metadata t ~key ~value] returns a fresh prompt identical to [t] but
    with [(key, value)] prepended to {!metadata}. *)
val add_metadata : t -> key:string -> value:string -> t
