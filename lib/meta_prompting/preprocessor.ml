open Core

[@@@warning "-27-32"]

(** Prompt Preprocessor
    ------------------
    This module provides a transparent hook that can be inserted right
    before the ChatMarkdown parsing step.  When enabled, the hook runs
    the {e Recursive Meta-Prompting} refinement loop on the raw prompt
    string.  Enablement happens via either:

    1. Environment variable [OCHAT_META_REFINE] set to a truthy value
       ("1", "true", "yes", "on").
    2. Presence of the sentinel comment "<!-- META_REFINE -->" within
       the prompt.

    The transformed prompt preserves ChatMarkdown validity—the added
    metadata is encoded as HTML comments so that downstream parsers are
    oblivious to it. *)

let truthy = function
  | "1" | "true" | "yes" | "on" -> true
  | _ -> false
;;

let env_enabled () =
  match Sys.getenv "OCHAT_META_REFINE" with
  | Some v when truthy (String.lowercase v) -> true
  | _ -> false
;;

let marker = "<!-- META_REFINE -->"
let marker_enabled (s : string) = String.is_substring s ~substring:marker
let enabled s = env_enabled () || marker_enabled s

let preprocess (prompt_raw : string) : string =
  if enabled prompt_raw
  then (
    let prompt_t = Prompt_intf.make ~body:prompt_raw () in
    let refined = Recursive_mp.refine prompt_t in
    Prompt_intf.to_string refined)
  else prompt_raw
;;
