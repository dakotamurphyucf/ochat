open! Core

(** Pure producer library [chatmd_authoring_assistance]. It consumes
    exact current captures and host-projected metadata; it performs no Eio I/O,
    preprocessing, resolver calls, compilation, model inference, script/tool
    execution or authority decisions. The agent service separately checks the
    original/current actor and every source/selection before and after work.
    Returning a wire DTO does not install a protocol handler. *)

module Wire = Agent_protocol.Authoring_assistance.Assistance

module Error : sig
  type t =
    | Invalid_limits
    | Source_mismatch
    | Parsed_input_mismatch
    | Missing_import_capture
    | Foreign_import_capture
    | Selected_without_project
    | Selected_project_mismatch
    | Invalid_path_provenance
    | Candidate_limit
    | Work_limit
    | Invalid_display_text
    | Wire of Agent_protocol.Error.t
  [@@deriving sexp_of]
end

module Limits : sig
  type t

  (** All source/candidate/lookup work, including discarded candidates, is
      charged before retention; wire result limits remain additional ceilings.
      Per-input source bytes also obey the admitted context/parser budgets. *)
  val create
    :  max_work_steps:int
    -> max_authored_bytes:int
    -> max_total_captured_bytes:int
    -> max_parsed_inputs:int
    -> max_paths:int
    -> max_selected_tools:int
    -> max_candidates:int
    -> max_label_bytes:int
    -> t Or_error.t
end

module Captured_input : sig
  type t

  (** Host supplies the exact capture associated with one actual parser input,
      including imports. The pure comparison checks exact authored bytes/digest
      and length; the host remains responsible for path/root custody. An import
      without authenticated capture cannot become a source-owned reference. *)
  val create
    :  input:Chatmd_parser_input.t
    -> source:Chatml_source.t
    -> max_bytes:int
    -> checkpoint:(unit -> unit)
    -> (t, Error.t) result

  val input : t -> Chatmd_parser_input.t
  val source : t -> Chatml_source.t
end

module Path_candidate : sig
  module Edit_spelling : sig
    type t

    (** Relative authored spelling, at most 1024 UTF-8 bytes. Unlike a captured
        inventory path this may contain [.] and [..] components, as genuine
        nested/sibling ChatMD paths do. It rejects absolute, empty, backslash,
        colon, NUL and line-break spellings. This validates display/edit bytes,
        not the path's target or any read permission. *)
    val create : string -> (t, Error.t) result

    val to_string : t -> string
  end

  type captured_route =
    { authored_base_id : Chatml_source.Id.t
    ; authored_base_digest : Chatml_source.Digest.t
    ; base_directory : Source.Portable_path.t option
    ; edit_spelling : Edit_spelling.t
    }

  type origin =
    | Admitted_root of Agent_protocol.Id.Authoring_root.t
    | Project_source of
        { project : Chatml_project.t
        ; route : captured_route
        }
    | Decoded_member of
        { project : Chatml_project.t
        ; role : Chatml_project.Declared_paths.role
        ; member : Source.Portable_path.t
        ; route : captured_route
        }
    | Selected_closure of
        { prepared : Chatml_prepared_program.t
        ; source_id : Chatml_source.Id.t
        ; route : captured_route
        }

  type t

  (** [edit_spelling] is the exact intended attribute value, as projected from
      the current authorized nonrecursive host listing, decoded project
      declaration, or selected immutable closure. For Selected_closure the host
      supplies the current authored base/directory and an actual qualified
      captured member. [Facts.create] compares that base with the current
      source, checks actual decoded project field-role/member provenance or
      selected Build/Prepared member identity, and charges bounded work. A
      Project_source witness is distinct from an internal Decoded_member path.
      Local-dependency paths retain their local role; Git URLs are never edit
      path candidates. The host must separately establish that its actual routing
      maps this spelling from that base to that exact member; equal path suffix
      or namespace alone is no proof. Captured-only completion need not repeat
      an existing ChatMD declaration. Decoded project paths are visible as
      declarations, not verified files. A suggestion grants no read. The host
      rechecks the original/current actor, grant and selection before publishing. *)
  val create : origin:origin -> edit_spelling:Edit_spelling.t -> t

  val origin : t -> origin
  val edit_spelling : t -> Edit_spelling.t
end

module Selected_tool : sig
  type t

  (** Immutable projection of one binding in the ACTUALLY selected host tool
      registry. The host derives ID/name/fingerprints from the same
      Tool_capability binding and selection. No descriptor implementation,
      callback, invocation handle or unselected registry member is retained.
      Wire uses a distinct Selected_tool Reference evidence arm;
      Selected_catalog is a project Build, not a tool registry. Both actual
      Tool_capability fingerprints are canonical SHA-256 hex and enter through
      Digest.of_hex, never arbitrary caller strings. An invalid UTF-8 name may
      be retained as source metadata; only a relevant candidate is withheld
      with typed Unrepresentable_text truncation. *)
  val create
    :  id:Agent_protocol.Id.Capability.t
    -> name:string
    -> binding_fingerprint:Chatml_source.Digest.t
    -> selection_fingerprint:Chatml_source.Digest.t
    -> (t, Error.t) result

  val name : t -> string
  val evidence : t -> Wire.Selected_tool.t
end

module Facts : sig
  type t

  (** [authored] is the current document or project source
      revision and MUST match [request.source]. [lexical_input] is created from
      its exact authored bytes, even if full parse fails. A supplied document
      needs no host root or selected Build for tag/attribute/value help.

      Without [parsed], [lexical_input] must be a Root input with exact
      [authored] bytes. With [parsed], it may instead be a genuine imported
      input physically present in that same parse, with exactly one matching
      [captured_inputs] source equal to [authored]. This lets an authorized
      request scan its imported authored source without projecting root byte
      offsets onto it. [parsed] is only a successful current Prompt parse,
      never reconstructed from old output after an overlay. [captured_inputs]
      binds every root/import input to its exact current captured source;
      mismatch rejects parsed facts atomically, without guessing source IDs.
      The effective revision remains the actual parser input, including for
      repeated imports of the same source with distinct input objects.

      [project] is the actual separately captured/decoded project declaration
      relevant to the cursor's script selection; its bounded public
      environment-names accessor is required. It may exist when Build selection
      fails, permitting environment-name correction. [selected] is an actual
      admitted Build for that script when available; the selected-script adapter
      and host check
      its membership/contract/closure. A project path string is never authority.
      [paths] and [selected_tools] are already host-filtered projections; this
      function validates bounds/consistency, not operator permission. Source,
      capture, member and aggregate byte ceilings are checked before comparing
      or retaining arbitrary host lists; each bounded visit charges work. *)
  val create
    :  request:Wire.Request.t
    -> authored:Chatml_source.t
    -> lexical_input:Chatmd_parser_input.t
    -> parsed:Prompt.Chat_markdown.Parsed.t option
    -> captured_inputs:Captured_input.t list
    -> project:Chatml_project.t option
    -> selected:Chatml_project_build.t option
    -> paths:Path_candidate.t list
    -> selected_tools:Selected_tool.t list
    -> limits:Limits.t
    -> checkpoint:(unit -> unit)
    -> (t, Error.t) result
end

(** Produces the portable protocol's seven suggestion kinds with exact AUTHORED byte
    replacements. It first obtains a source-owned lexical cursor from the
    existing scanner. Tag/Attribute/Value and static ChatMD grammar templates
    need no successful parse or Build. References come from current actual
    Prompt declarations/elements or separately selected tool references, with
    imported definition provenance. Paths come from actual parsed project
    declarations, admitted host listing or selected captured closure; declared
    paths are visibly candidates, not verified file reads. Environments come
    from the captured decoded project map, even before successful Build.
    Installed asset IDs come only from Builtin_instructions' reviewed embedded
    installed-ID enumeration, never a caller name or hypothetical skills/goals.

    This initial producer offers parser-validated structural ChatMD snippets.
    It has no checked-example witness input and therefore emits no
    selected-environment ChatML program template. Any later such template
    requires a separately admitted exact source/contract/environment checked
    example witness. No completion call runs a compiler merely to manufacture
    a template.

    `Complete []` is a genuine empty local result. A missing selected catalog
    simply omits catalog-backed candidates while local help remains available;
    inconsistent parsed/captured inputs reject during Facts admission. A
    cursor scan that cannot supply local context returns typed Unavailable.
    Budget exhaustion returns explicit Truncated status with only completed
    items.
    Native invalid UTF-8 bytes are never changed; wire labels/insert text must
    pass the existing UTF-8 display validator or the candidate is withheld
    with typed status. Callback/cancellation exceptions propagate unchanged. *)
val produce : Facts.t -> checkpoint:(unit -> unit) -> (Wire.Result.t, Error.t) result
