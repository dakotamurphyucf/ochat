open! Core
open Jsonaf.Export
module S = Chatmd_shell_spec.Shell_spec

type capability_ceiling =
  { network : bool
  ; child_processes : bool
  ; arbitrary_code : bool
  ; privilege_change : bool
  ; external_backends : bool
  ; executable_hooks : bool
  ; model_reviewers : bool
  ; durable_approvals : bool
  }
[@@deriving sexp, compare, equal, jsonaf]

type entry =
  { id : string
  ; canonical_root : string
  ; repository_identity : string option
  ; source_sha256 : string list
  ; signer_ids : string list
  ; capabilities : capability_ceiling
  }
[@@deriving sexp, compare, equal, jsonaf]

type policy = { entries : entry list } [@@deriving sexp, compare, equal, jsonaf]

type evidence =
  { source_file : string
  ; source_sha256 : string
  ; trusted_source_id : string
  ; repository_identity : string option
  ; signer_id : string option
  }
[@@deriving sexp, compare, equal]

type error =
  { code : string
  ; source_file : string
  ; message : string
  }
[@@deriving sexp, compare, equal]

let under root path =
  let root = String.chop_suffix_if_exists root ~suffix:Filename.dir_sep in
  String.equal root path || String.is_prefix path ~prefix:(root ^ Filename.dir_sep)
;;

let source_matches entry ?repository_identity ?signer_id source =
  under entry.canonical_root source.Chatmd_shell_spec.Source_ref.source_dir
  && List.mem entry.source_sha256 source.source_sha256 ~equal:String.Caseless.equal
  && Option.equal String.equal entry.repository_identity repository_identity
  && (List.is_empty entry.signer_ids
      || Option.exists signer_id ~f:(fun id ->
        List.mem entry.signer_ids id ~equal:String.equal))
;;

let requested runtime =
  let true_ = function S.Set true -> true | Set false | Inherit | Clear -> false in
  let capabilities = Option.value_exn runtime.S.capabilities in
  let backends = Option.value_exn runtime.backends in
  let reviewers = Option.value_exn runtime.reviewers in
  let approvals = Option.value_exn runtime.approvals in
  let external_backends =
    List.exists backends.values ~f:(function S.External _ -> true | _ -> false)
  in
  let executable_hooks =
    List.exists reviewers.values ~f:(function S.Executable_reviewer _ -> true | _ -> false)
    || List.exists (Option.value_exn runtime.interceptors).values ~f:(fun item ->
      match item.extension with S.Executable_extension _ -> true | _ -> false)
    || List.exists (Option.value_exn runtime.effect_analysis).analyzers ~f:(fun item ->
      match item.extension with S.Executable_extension _ -> true | _ -> false)
  in
  let model_reviewers =
    List.exists reviewers.values ~f:(function S.Model_reviewer _ -> true | _ -> false)
  in
  let durable_approvals =
    List.mem approvals.scopes S.Durable_exact ~equal:S.equal_approval_scope
  in
  true_ capabilities.network,
  true_ capabilities.child_processes,
  true_ capabilities.arbitrary_code,
  true_ capabilities.privilege_change,
  external_backends,
  executable_hooks,
  model_reviewers,
  durable_approvals
;;

let capabilities_allow entry runtime =
  let network, child, code, privilege, external_backend, hooks, model, durable =
    requested runtime
  in
  let ceiling = entry.capabilities in
  (not network || ceiling.network)
  && (not child || ceiling.child_processes)
  && (not code || ceiling.arbitrary_code)
  && (not privilege || ceiling.privilege_change)
  && (not external_backend || ceiling.external_backends)
  && (not hooks || ceiling.executable_hooks)
  && (not model || ceiling.model_reviewers)
  && (not durable || ceiling.durable_approvals)
;;

let sources manifest =
  let runtime_sources =
    List.map manifest.Chatmd_shell_spec.Manifest.payload.runtimes ~f:(fun runtime ->
      runtime.S.source)
  in
  let tool_sources = List.map manifest.payload.tools ~f:(fun tool -> tool.source) in
  runtime_sources @ tool_sources
  |> List.dedup_and_sort ~compare:Chatmd_shell_spec.Source_ref.compare
;;

let runtimes_for_source manifest source =
  List.filter manifest.Chatmd_shell_spec.Manifest.payload.runtimes ~f:(fun runtime ->
    Chatmd_shell_spec.Source_ref.equal runtime.S.source source)
;;

let verify_source policy manifest ?repository_identity ?signer_id source =
  match
    List.find policy.entries ~f:(fun entry ->
      source_matches entry ?repository_identity ?signer_id source
      && List.for_all (runtimes_for_source manifest source) ~f:(capabilities_allow entry))
  with
  | Some entry ->
    Ok
      { source_file = source.file
      ; source_sha256 = source.source_sha256
      ; trusted_source_id = entry.id
      ; repository_identity
      ; signer_id
      }
  | None ->
    Error
      { code = "shell.trusted_source_mismatch"
      ; source_file = source.file
      ; message = "source identity, digest, signer, or requested capabilities are not trusted"
      }
;;

let verify_manifest policy ?repository_identity ?signer_id manifest =
  List.map (sources manifest) ~f:(verify_source policy manifest ?repository_identity ?signer_id)
  |> Result.combine_errors
;;

let load ~fs ~path =
  try
    Eio.Path.(fs / path)
    |> Eio.Path.load
    |> Jsonaf.of_string
    |> policy_of_jsonaf
    |> Result.return
  with
  | exn ->
    Error
      [ { code = "shell.trusted_source_load_failed"
        ; source_file = path
        ; message = Exn.to_string exn
        }
      ]
;;
