(** Prompting guides and best-practice references.

    This module contains a set of large, multi-line strings that act as
    reference guides when constructing prompts for Large Language Models
    (LLMs).  They are intended to be embedded verbatim into the system or
    developer messages that steer an agent.

    The contents are purely textual – there is no parsing or rendering
    logic here.  Each value is a plain [string]; callers can pass it
    directly to the OpenAI API or compose new prompt fragments from it.

    {1 Guides exposed}

    • {!val:gpt4_1_model_prompting_guide} – GPT-4.1 specific
      recommendations
    • {!val:o_series_prompting_guide} – Guidance for the O-series
      reasoning models (o1, o3, o4-mini, …)
    • {!val:general_prompting_guide} – Model-agnostic prompting
      principles
    • {!val:combined_headers} – Short header block introducing the
      combined guide
    • {!val:combined_guides} – All of the above concatenated together
*)

(** Alias for a prompting guide in plain text. *)
type t = string

(** Prompting best practices for **GPT-4.1** models. *)
val gpt4_1_model_prompting_guide : t

(** Prompting best practices for the **O-series reasoning models** (o1,
    o3, o4-mini, …). *)
val o_series_prompting_guide : t

(** High-level, model-agnostic prompting principles. *)
val general_prompting_guide : t

(** Short header that introduces the combined prompting guide. *)
val combined_headers : t

(** [combined_guides] concatenates {!combined_headers},
    {!general_prompting_guide}, {!o_series_prompting_guide}, and
    {!gpt4_1_model_prompting_guide} in that order, separated by blank
    lines. *)
val combined_guides : t
