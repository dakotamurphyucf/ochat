(** Online helpers for prompt generation and iteration.

    {1 Overview}

    [Prompt_factory_online] uses explicitly selected neutral inference.
    It implements the online
    branch of the prompt–engineering feedback loop used by
    {!module:Meta_prompting} and purposefully exposes a
    **minimal** surface:

    - {!val:extract_section}
    - {!val:iterate_revised_prompt}
    - {!val:create_pack_online}

    Expected completion failures return [None]. Missing selected inference,
    cancellation and strict callback exceptions propagate; file I/O remains
    scoped to the supplied environment.

    {1 Runtime expectations}

    • Selected Execution – explicitly supplied for every online completion.
    • Optional template overrides are looked up relative to the current
      working directory:
      {ul
      {- [meta-prompt/templates/iteration_prompt_v2.txt]}
      {- [meta-prompt/templates/generator_prompt_v2.txt]}}
    • Repository-wide guard-rails are appended from
      [meta-prompt/integration/system_prompt_guardrails.txt] when the
      file exists; otherwise a baked-in fallback is used.

    {1 Behaviour overrides}

    Many defaults (reasoning effort, domain, markdown policy, …) can be
    tuned without recompilation via a key–value file referenced by the
    [META_PROMPT_ONLINE_CONFIG] environment variable.  See
    {!load_kv_overrides} in the implementation for the exhaustive key
    list.
*)

(** [extract_section ~text ~section] searches [text] for a heading named
    [section] (including the trailing newline) and returns the body of
    the first match.

    A heading must start at the beginning of a line.  The returned slice
    extends to the next recognised heading – {i Overview}, {i Revised_Prompt},
    etc. – or to the end of the string.  Leading and trailing whitespace
    is stripped.  If the section is missing or empty the function
    returns [None].

    Example extracting the *Revised_Prompt* section:
    {[
      let txt =
        "Overview\nfoo\nRevised_Prompt\nbar\nIssues_Found\nbaz" in
      assert (
        extract_section ~text:txt ~section:"Revised_Prompt\n" = Some "bar")
    ]}
*)
val extract_section : text:string -> section:string -> string option

(** [iterate_revised_prompt ~env ~goal ~current_prompt ?proposer_model]
    ask the model to produce an improved version of [current_prompt]
    given [goal].

    The helper:
    {ol
    {- loads [iteration_prompt_v2.txt] from [meta-prompt/templates] or
       falls back to {!Templates.iteration_prompt_v2};}
    {- appends the repository-wide guard-rail snippet;}
    {- sends a single blocking request through
       the explicitly selected Execution, retaining host attempt observations;}
    {- extracts the *Revised_Prompt* payload with {!extract_section}.}}

    Return value:
    {ul
    {- [Some revised] – success;}
    {- [None] – expected completion failure or the
       absence of a *Revised_Prompt* section.}}

    Expected completion failures produce None. Cancellation and strict host
    callback exceptions propagate; no ambient credential/model selection occurs.
*)
val iterate_revised_prompt
  :  env:Eio_unix.Stdenv.base
  -> inference:Inference_client.Execution.t
  -> goal:string
  -> current_prompt:string
  -> proposer_model:string option
  -> string option

(** [create_pack_online ~env ~agent_name ~goal ?proposer_model] generate a
    *fresh* prompt-pack for [agent_name].

    Implementation details mirror {!iterate_revised_prompt} but the
    system prompt is seeded with
    [generator_prompt_v2.txt]/[!Templates.generator_prompt_v2].  The
    assistant output – typically a multi-section Markdown document – is
    returned **verbatim** so that callers can post-process it however
    they see fit.

    Returns [None] under the same failure conditions as
    {!iterate_revised_prompt}. *)
val create_pack_online
  :  env:Eio_unix.Stdenv.base
  -> inference:Inference_client.Execution.t
  -> agent_name:string
  -> goal:string
  -> proposer_model:string option
  -> string option

val get_iterate_system_prompt : Eio_unix.Stdenv.base -> string
