open Core

(** Structural artifact custody. No compiler, source acquisition or contract
    retargeting. No source loader or callback is retained. *)
type t = Chatml_prompt_programs.t

(** Existing artifact policy: complete metadata262144bytes; joint authored/private
    physical256files/8MiB; independently summed canonical program inputs8MiB.
    Count ceilings additionally bound parser inputs and program/logical IDs. *)
val limits : unit -> Chatml_prompt_programs.Limits.t

val authored
  :  root_file:string
  -> sources:(string * string) list
  -> Chatmd_source_bundle.t Or_error.t

val prepare
  :  Chatmd_source_bundle.t
  -> parsed_contexts:Prompt.Chat_markdown.Parsed.t list
  -> root:Chatmd_parser_input.t
  -> supplied:t option
  -> checkpoint:(unit -> unit)
  -> (t, Chatml_prompt_programs.Error.t) Result.t

val admit
  :  Agent_store.Prompt_program_bundle.t
  -> authored:Chatmd_source_bundle.t
  -> checkpoint:(unit -> unit)
  -> (t, Chatml_prompt_programs.Error.t) Result.t

(** Fixed complete artifact-manifest policy, independent of host index limits. *)
val manifest_limits : unit -> Document_schema.Limits.t

(** Hash the complete admitted storage manifest, including unknown metadata and
    private digest/length inventory; exclude outer revision/time/self fields. *)
val fingerprint
  :  t
  -> checkpoint:(unit -> unit)
  -> (string, Chatml_prompt_programs.Error.t) Result.t

(** Replay one retained Root with a privately constructed captured-only loader,
    fixed owned limits and an optional borrowed checkpoint (Eio cancellation by
    default). Original checkpoint exceptions/backtraces remain outside parser
    diagnostic conversion. Explicit external agent
    declarations retain the registered-prompt policy; imports and script reads
    remain captured-only. Pinning a child is a separate authority check.
    This reuses the established
    prompt-owner diagnostic policy: parser exceptions become Error, cancellation
    and timeout propagate unchanged. No arbitrary caller callback is interpreted. *)
val replay
  :  ?checkpoint:(unit -> unit)
  -> Chatmd_parser_input.t
  -> authored:Chatmd_source_bundle.t
  -> dir:Eio.Fs.dir_ty Eio.Path.t
  -> Prompt.Chat_markdown.Parsed.t Or_error.t
