(** Recovery of interrupted private artifact installations. The caller owns the
    exclusive storage lock and must exclude all artifact publishers until return.
    This is startup recovery, never concurrent artifact retention. *)
val discard
  :  env:Eio_unix.Stdenv.base
  -> root:string
  -> max_entries:int
  -> checkpoint:(unit -> unit)
  -> (int, Store_error.t) result
(** Only directories named [.install-<valid transaction ID>] are candidates.
    Installed revisions and unknown names are untouched. One bounded reader
    validates every candidate and descendant before any deletion; links and
    special files reject the whole scan. No file contents are read. The entry
    budget bounds enumeration, inspection and the resulting deletion plan.

    Returns the number of staging roots removed. A scan error or cancellation
    deletes nothing. An I/O failure or cancellation during deletion can leave a
    partially removed staging tree, which the next exclusive recovery can finish.
    Callback exceptions and cancellation propagate unchanged. *)
