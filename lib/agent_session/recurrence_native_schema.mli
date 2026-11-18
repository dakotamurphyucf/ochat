(** Shared strict selected-tool schema. Request decoding remains authoritative;
    numeric overflow, rule association and native policy are not schema grants. *)
val parameters : Recurrence_native_admission.Effect.t -> Jsonaf.t
