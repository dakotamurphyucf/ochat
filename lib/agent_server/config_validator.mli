(** Multi-phase normalization and validation for raw server configuration.
    Prompt preflight uses bounded owned parsing, including captured-document and
    configured-script declarations. It does not acquire documents or project
    dependencies, compile scripts, or initialize modules. Actual source capture
    and admission remain the catalog's responsibility. Cancellation propagates. *)

val validate
  :  env:Eio_unix.Stdenv.base
  -> Config_parser.Raw_config.t
  -> (Config.t, Config.Diagnostic.t list) result

(** Local-stdio policy uses the same bounded source/read/upload, Prompt
    permission-profile, manifest-grant and Developer section grammars as server
    configuration. Permitted top-level sections are [project_sources],
    [authoring_project_access], [authoring_upload_access], one optional row in
    [permission_profiles], [manifest_grants], mandatory [developer], and the
    local-only [developer_scopes] list of exact Developer method scopes. It may
    name only the closed [Embedded_operator] selector in principal fields. It
    cannot configure a listener, data root, session, workspace or Prompt
    definition. No parse result is an actor or grant. *)
module Embedded_local : sig
  type t
  type bound

  (** Read and preflight a finite local policy before opening the daemon Store.
      Resolve paths relative to this file, never the process working directory.
      The operator selector remains unbound at this stage. *)
  val load
    :  env:Eio_unix.Stdenv.base
    -> path:string
    -> (t, Config.Diagnostic.t list) result

  (** Bind every principal occurrence in project read grants, upload grants and
      selections, manifest grants and Developer selections to the one exact
      Store-owned local ID. The authentication kind is [embedded.local]. Empty
      manifest principal lists and literal principal IDs reject. Reuse the
      ordinary section validators after binding; never substitute text or
      construct a second Store to discover this ID. *)
  val bind
    :  t
    -> env:Eio_unix.Stdenv.base
    -> operator_id:Agent_protocol.Id.Principal.t
    -> (bound, Config.Diagnostic.t list) result

  (** Change only the existing project source/read/upload, Developer, selected
      embedded Prompt profile and exact manifest-grant fields. Reject an
      explicit base-field conflict and require the real [embedded.prompt] and
      [embedded.workspace] identities. All other Config fields are preserved.
      Daemon repeats admission for the final Config before source/catalog work. *)
  val apply : bound -> config:Config.t -> (Config.t, Agent_protocol.Error.t) result

  val operator_id : bound -> Agent_protocol.Id.Principal.t

  (** Only requested Developer_check/fixture/live/repl method scopes, validated
      independently of the local principal's existing session scopes. *)
  val developer_scopes : bound -> Agent_protocol.Scope.Set.t
end
