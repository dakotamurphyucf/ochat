open Core

(** Shell inspection through legacy text or an explicit Operator_only configured
    catalog owner. Validate source/retained-role/signing option combinations before
    opening the daemon or performing capture/storage admission. The configured
    owner preserves original captured contracts and never initializes hooks or
    providers. Requested canonical identity remains available when its security
    check rejects; output is not an execution grant. Scoped Eio owners close before
    an unsuccessful command exits. *)
val command : Command.t
