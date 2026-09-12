(** Model-facing description derived from actual selected metadata. Authentic
    helpers receive callable pointers only when present in [capabilities]. Stable
    package/topic identifiers remain when helpers are omitted, including manual
    policy. Ordinary tools retain their original descriptions.

    This is a presentation of a binding, not a new registration: it changes no
    schema, implementation, capability identity or execution authority. Pass the
    original description on each use; do not repeatedly decorate the result. *)
val describe
  :  capabilities:Tool_capability.t
  -> name:string
  -> description:string option
  -> string option
