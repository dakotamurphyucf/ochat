open! Core

(** Trusted, immutable cross-model replay declarations for one provider profile.
    Declarations are directed and never transitively inferred. They do not grant
    authentication, tool execution or support for an unknown wire field. *)
module Item_class : sig
  type t =
    | Assistant_text
    | Function_call
    | Custom_call
    | Reasoning
  [@@deriving equal, compare, sexp_of]
end

type t

val exact_origin_only : t

(** At most 256 distinct directed model pairs, nonempty exact model names of at
    most 512 bytes, and distinct nonempty class lists. Reasoning requires separately
    qualified same-family encrypted replay; no model-name heuristic is applied. *)
val create : transitions:(string * string * Item_class.t list) list -> t Or_error.t

(** Trusted host composition only: one explicitly qualified canonical owner and
    its declared same-owner profile IDs. At most128 distinct bounded IDs; the
    canonical ID must be a member. Replaces the previous group, preserves directed
    cross-model declarations. This grants no authentication or capability support.
    The host must independently prove exact shared credential ownership.

    Cross-profile replay preserves original logical provenance and requires
    consistent provider/profile IDs, identical known model, adapter, account,
    endpoint and replay version. Only closed assistant text/function/custom call
    shapes qualify. Unknown fields/classes and reasoning (including encrypted
    reasoning) refuse. Cross-profile and cross-model permissions never compose. *)
val with_compatible_profiles
  :  t
  -> canonical_profile:string
  -> profiles:string list
  -> t Or_error.t

(** Requires available, identical adapter/provider/account/endpoint/profile and
    replay version. Same complete origin remains admitted. Cross-model input
    requires an explicit pair and a closed known wire shape; unknown fields or
    classes refuse. An explicitly qualified compatible profile group follows the
    narrower rules above. Neither replaces independent semantic/raw validation. *)
val permits
  :  t
  -> actual:History_entry.Payload.Origin.t
  -> expected:History_entry.Payload.Origin.t
  -> raw:Jsonaf.t
  -> bool
