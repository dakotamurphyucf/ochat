open Core
module Id = Chatml_git_host.Object_id

(** Private minimal root-tree interpretation of a hash-verified commit body.
    Kind/hash proof comes from Git_object. No parent traversal, branch/tag lookup,
    reachability/policy/signature authentication or complete git-fsck claim. *)
module Error : sig
  type t =
    | Wrong_kind
    | Invalid
  [@@deriving equal, sexp_of]
end

(** Exact first tree header with format-sized hex and header separator; reject
    duplicate tree headers before message body. Unknown valid extended/continued
    headers remain opaque, including signed commits. Bounded scans poll; message
    body is not reinterpreted as headers or retained in failure diagnostics. *)
val root_tree : Git_object.t -> checkpoint:(unit -> unit) -> (Id.t, Error.t) Result.t
