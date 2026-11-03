open Core

(** Pure immutable TZif v2/v3 data. No IO, ambient timezone lookup or civil
    resolution engine. Provenance hashes the exact supplied bytes. Parsed data
    retains no raw-byte copy. Host acquisition/admission is a separate owner. *)
type t

module Error : sig
  type t =
    | Too_large
    | Malformed of
        { byte_offset : int
        ; reason : string
        }
    | Unsupported_version of char
    | Unsupported_leap_seconds
    | Unsupported_footer of string
    | Unsupported_offset of int
    | Coverage_unavailable
    | Projection_limit
    | Invalid of Recurrence_rules.Timing_error.t
  [@@deriving equal, sexp_of]
end

(** Fixed resource bounds, checked before count-derived allocation: 1048576
    input bytes, 4096 raw/projected transitions, 256 types, 65536 designation
    bytes per block, and 4096 footer bytes. *)
val maximum_bytes : int

val maximum_transitions : int
val maximum_types : int
val maximum_designation_bytes : int
val maximum_footer_bytes : int

(** Validate both structural blocks and the exact footer. Leap-bearing files,
    v1/v4, implementation-defined DST defaults and external TZ references are
    explicit errors. [source] obeys Zone.Provenance's bounded-label contract;
    TZif has no embedded IANA release identity. A label grants no authority. *)
val of_string : string -> source:string -> (t, Error.t) Result.t

val provenance : t -> Recurrence_rules.Zone.Provenance.t

(** Produce the whole inclusive UTC interval or fail; never clip it or extend
    the final table offset without a footer. Before the first transition use
    type 0. With zero transitions and an empty footer use type 0 throughout.
    Any required [-00] interval is unavailable. With transitions and an empty
    footer, coverage ends strictly before the final transition. Nonempty footer
    rules control the final transition onward (all time if no transitions).

    Self-contained Jn/n/Mm.w.d rules support default offsets/times, southern,
    negative/nonhour/all-year DST and v3 signed +/-167-hour rule times. Required
    offsets outside Zone's +/-86400 seconds fail. At most 4096 projected changes;
    comparisons precede conversion of signed 64-bit seconds into Time_ns. *)
val zone
  :  t
  -> name:string
  -> from:Time_ns.Alternate_sexp.t
  -> until:Time_ns.Alternate_sexp.t
  -> (Recurrence_rules.Zone.t, Error.t) Result.t
