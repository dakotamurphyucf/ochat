(** One physical Root's short mutation owner. Store remains the durable writer;
    this gate orders transient Root writer slots, terminal queue publication and
    exact acknowledgements. A Root constructs exactly one gate and keeps it for
    its whole lifetime. This module is private to [Agent_server]. *)

type t

exception Recursive_mutation

val create : unit -> t

(** Cancellation-safe acquisition and release. A fiber already holding any
    Root mutation gate raises [Recursive_mutation] before waiting; callers must
    not enter recursively through terminal recovery or a Store callback. Eio
    fiber bindings inherit into children, so a child is barred while the
    parent's gate is live. The inherited marker expires at unlock and cannot
    reject unrelated later child work.

    Only trusted Root mutations belong in [f]. Never execute language/user
    callbacks, await a Worker/child, close a Root, or run outer recovery here.
    Root callers recheck their physical actor/selection/Claim and obtain a fresh
    current witness after any wait and before a new effect or Store write. This
    gate grants no authority itself. Unexpected exceptions and cancellation
    propagate with their original backtraces after a guaranteed unlock. The
    gate is not poisoned by an expected admission rejection or cancellation. *)
val with_owner : t -> f:(unit -> 'a) -> 'a
