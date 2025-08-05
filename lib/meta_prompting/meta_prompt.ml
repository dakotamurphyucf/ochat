(** Functor {e Make}

    {1 Purpose}

    Bridges a {b task} value – any OCaml type that can be serialised to
    Markdown – with a concrete {b prompt} record expected by the
    meta-prompting sub-system.  The functor stays intentionally thin: it
    delegates *all* formatting work to the generic {!module:Template}
    helper while taking care of sensible fall-backs when no template is
    provided.

    {1 Workflow}

    {ol
      {- Convert the [task] to Markdown using {!Task.to_markdown}.}
      {- If a [template] path is supplied, load it (requires an [env]
         object with filesystem capability), substitute placeholders via
         {!Template.render}, and use the resulting string as the prompt
         body.}
      {- Otherwise fall back to the raw task Markdown and prepend a short
         default header reminding the LLM of prompt-writing best
         practices.}}

    {1 Placeholder mapping}

    The substitution map is the concatenation of

    • the caller-supplied [params] list, and
    • a single automatically generated pair ["TASK_MARKDOWN"].

    Duplicate keys are resolved by {e last write wins} semantics, i.e.
    explicit [params] overshadow the default mapping.
  *)
open Core

module Make
    (Task : sig
       type t

       val to_markdown : t -> string
     end)
    (Prompt : sig
       type t

       val make
         :  ?header:string
         -> ?footnotes:string list
         -> ?metadata:(string * string) list
         -> body:string
         -> unit
         -> t
     end) =
struct
  (*------------------------------------------------------------------*)
  (* Defaults                                                         *)
  (*------------------------------------------------------------------*)

  let default_header =
    {|<!-- Meta-Prompt Generated -->
You are an advanced AI assistant specialised in translating task
specifications into production-quality prompts.  Follow best practices
for clarity, brevity, and structure.

### Prompting Guidelines (extract)

• Encourage "reasoning → answer" ordering unless examples dictate otherwise.  
• Prefer imperative, active voice.  
• State invariants and constraints explicitly.  
• Separate sections with clear headings (#, ##, ###).  
• Avoid redundant boilerplate; focus on what is non-obvious.|}
  ;;

  (*------------------------------------------------------------------*)
  (* Utilities                                                        *)
  (*------------------------------------------------------------------*)

  let read_template ~env path =
    let fs = Eio.Stdenv.fs env in
    Io.load_doc ~dir:fs path
  ;;

  (* -----------------------------------------------------------------
     Functor-based template renderer using the richer API from
     [Template.Make_Template].  We instantiate it once with a module
     that treats a list of key/value pairs as the renderable object. *)

  module KV = struct
    type t = (string * string) list

    let to_key_value_pairs t = t
  end

  module KV_Template = Template.Make_Template (KV)

  (*------------------------------------------------------------------*)
  (* Public API                                                       *)
  (*------------------------------------------------------------------*)

  let generate ?env ?template ?(params = []) (task : Task.t) : Prompt.t =
    (* Build placeholder mapping – explicit [params] override derived
       task fields.  *)
    let task_mapping = [ "TASK_MARKDOWN", Task.to_markdown task ] in
    let mapping = params @ task_mapping in
    (* Resolve template – fall back to identity behaviour when absent. *)
    let body, header =
      match template with
      | None -> Task.to_markdown task, Some default_header
      | Some path ->
        let env = Option.value_exn ~message:"Eio Runtime is required" env in
        (match Result.try_with (fun () -> read_template ~env path) with
         | Error _ ->
           let warning =
             Printf.sprintf
               "<!-- WARNING: template '%s' not found; falling back to task markdown -->"
               path
           in
           Task.to_markdown task ^ "\n\n" ^ warning, Some default_header
         | Ok tmpl_str ->
           let tmpl = KV_Template.create tmpl_str in
           let substituted = KV_Template.render tmpl mapping in
           substituted, None)
    in
    Prompt.make ?header ~body ()
  ;;
end
