open! Core
open Jsonaf.Export

type t = string [@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

module Set = String.Set

let fixed_tool = "shell.tool.fixed.v1"
let structured_tool = "shell.tool.structured.v1"
let chain_tool = "shell.tool.chain.v1"
let raw_tool = "shell.tool.raw.v1"
let script_tool = "shell.tool.script.v1"
let seatbelt_backend = "shell.backend.seatbelt.v1"
let bubblewrap_backend = "shell.backend.bubblewrap.v1"
let direct_backend = "shell.backend.direct.v1"
let resource_limits = "shell.resource_limits.v1"
let literal_secrets = "shell.secret.literal.v1"
let chatml_matcher = "shell.chatml.matcher.v1"
let chatml_reviewer = "shell.chatml.reviewer.v1"
let chatml_before_interceptor = "shell.chatml.before_interceptor.v1"
let chatml_after_interceptor = "shell.chatml.after_interceptor.v1"
let chatml_effect_analyzer = "shell.chatml.effect_analyzer.v1"
let chatml_audit_filter = "shell.chatml.audit_filter.v1"
let model_reviewer = "shell.model_reviewer.v1"
let executable_hooks = "shell.executable_hooks.v1"
let external_backend = "shell.external_backend.v1"

let phase1 =
  Set.of_list
    [ fixed_tool
    ; structured_tool
    ; seatbelt_backend
    ; bubblewrap_backend
    ; direct_backend
    ; resource_limits
    ; literal_secrets
    ]
;;

let phase2 = Set.of_list (Core.Set.to_list phase1 @ [ chain_tool; raw_tool; script_tool ])

let phase3 =
  Set.of_list
    (Core.Set.to_list phase2
     @ [ chatml_matcher
       ; chatml_reviewer
       ; chatml_before_interceptor
       ; chatml_after_interceptor
       ; chatml_effect_analyzer
       ; chatml_audit_filter
       ; model_reviewer
       ])
;;

let phase4 = Set.of_list (Core.Set.to_list phase3 @ [ executable_hooks; external_backend ])
