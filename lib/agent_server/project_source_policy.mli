open Core

(** Named finite server source-acquisition profile. This is separate from runtime
    restoration/compilation policy. Its literals are built through the existing
    Roots/Local/Git/Project/Custody constructors; it grants no authority and has no
    callbacks, clock, native owner or credential. *)
type t

(** Override only the actual independent temporal/work ceilings. Existing source,
    count, path, custody, pack/metadata and store bounds remain the documented
    server profile. Positive finite span and positive overflow-safe work values
    are admitted. Low work allowances may legitimately reject before acquisition;
    maxima are not increased to make a requested graph fit.

    Memory is deliberately absent: enabled Git's ONE Configuration value supplies
    both Git.Limits and Bounded_process deployment probes at open_owned. *)
val create
  :  attempt_wall_time:Time_ns.Span.t
  -> max_semantic_work_steps:int
  -> max_native_work_steps:int
  -> max_acquisition_work_steps:int
  -> t Or_error.t

(** 120s original catalog attempt, 10m semantic/helper work, 40m per native bracket,
    160m aggregate backend/semantic work. A 1MiB pack +16KiB metadata reserves
    8454144 payload bytes and 33817600 native work units before fetching; aggregate
    work therefore cannot be the semantic 10m ceiling. These are cooperative
    weighted adapter units, not CPU/syscall/disk-quota guarantees.

    Profile: individual source256KiB, aggregate canonical input/physical custody
    independently8MiB/256 files, manifest262144 bytes, parser inputs512,
    context roles512/reference visits4096; script-plan metadata 8 MiB, matching the
    runtime preparation profile. This is canonical compiler-contract metadata,
    separate from the 262144-byte artifact manifest. The planner also retains its
    conservative allocation debit: the shipped versioned moderator contracts
    require more than 8 MiB of debit despite encoding to less than 80 KiB.
    Local entries4096/components64;
    roots33/retained128/100000 work; Git objects1024/16MiB/object1MiB/tree8192;
    native60s/CPU30s/FD128; attempt requests4MiB/output32MiB/acquisition128MiB;
    cache8 repositories/64MiB/1024 entries/100000 admission work. Document schema
    and pure composition/planning ceilings reuse existing named bounded policies.
    Runtime's existing5s policy is unchanged. No process-memory default or dynamic
    inherited-footprint widening is introduced. *)
val default : t

val attempt_wall_time : t -> Time_ns.Span.t

(** The SAME immutable custody ceiling used to construct Sources.Policy and final
    source admission. Bootstrap/baseline association reuses this value rather than
    reconstructing another structural or semantic policy from stored metadata. *)
val custody_limits : t -> Chatml_prompt_programs.Limits.t

module Configured : sig
  (** Pure derived limits for one already validated administrative configuration;
      no directories, credentials or process authority. *)
  type t

  val source_preparation : t -> Agent_session.Prompt_script_preparation.Policy.t
  val git_probe_limits : t -> Shell_access.Bounded_process.Limits.t option
  val git_store_limits : t -> Chatml_git_host.Store_limits.t
end

(** Insert enabled Git's sole configured memory cap into native and probe limits.
    Disabled Git gets an inert native policy with zero pack/output/acquisition
    capacity; positive mandatory process fields cannot authorize a process.
    Final configuration validation is repeated before deriving these values. *)
val configure : t -> Project_source_configuration.t -> Configured.t Or_error.t
