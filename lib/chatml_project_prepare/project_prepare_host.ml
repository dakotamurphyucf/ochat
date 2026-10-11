open Core
module Roots = Chatml_local_roots
module Git = Chatml_git_host

(** Borrowed capabilities; the caller retains their lifetimes through joined
    preparation. The finite exact-route map never retains ambient lookup. *)
type t =
  { anchors : Roots.Anchor.t list
  ; git_store : Git.Store.t option
  ; git_grants : Git.Grant.t String.Map.t
  }

let create anchors ~git_store ~git_grants ~max_grants ~max_grant_bytes =
  if max_grants < 0 || max_grant_bytes < 0
  then Or_error.error_string "invalid Git grant inventory limits"
  else (
    let rec collect remaining_count remaining_bytes routes = function
      | [] -> Ok { anchors; git_store; git_grants = routes }
      | grant :: rest ->
        let bytes = Git.Grant.input_bytes grant in
        if remaining_count = 0 || bytes > remaining_bytes
        then Or_error.error_string "Git grant inventory limit exceeded"
        else (
          let key = Git.Grant.repository grant in
          match Map.add routes ~key ~data:grant with
          | `Duplicate -> Or_error.error_string "duplicate exact Git route grant"
          | `Ok routes ->
            collect (remaining_count - 1) (remaining_bytes - bytes) routes rest)
    in
    collect max_grants max_grant_bytes String.Map.empty git_grants)
;;
