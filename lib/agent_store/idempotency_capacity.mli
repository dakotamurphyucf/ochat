open! Core

(** Pure complete receipt-metadata capacity. Profiles preserve the original new
    admission budget and derive finite compatibility headroom for old receipts.
    No outcome files, receipt ownership or execution authority live here.
    This capacity check does not replace receipt-domain validation. *)
type t

type mode =
  | Fresh
  | Existing
[@@deriving sexp]

(** Explicit finite profiles for tests/adapters; depth remains 256. Derivation
    rejects nonpositive bounds and arithmetic overflow. This is not a host option. *)
val create
  :  max_bytes:int
  -> max_fields:int
  -> max_nodes:int
  -> (t, Document_schema.Error.t) result

(** Original 16MiB/1M fields/2M nodes new admission budget. Existing bounds add
    only the proved 100,000-record normalization/terminalization headroom. *)
val default : t

val limits : t -> mode:mode -> Document_schema.Limits.t

(** Validate actual complete JSON and reserve componentwise remaining growth for
    every independently completable receipt.
    Pending reserves the maximum terminal reference and every receipt reserves
    its maximum accepted sequence. Omitted nullable members and timestamps are
    normalized once without shrinking current charges. Existing reference extensions and all outer unknown fields
    remain charged. Pending outcome extensions remain charged until their own completion;
    their later artifact custody transfer belongs to the receipt owner. *)
val check : t -> Jsonaf.t -> mode:mode -> (unit, Document_schema.Error.t) result
