(** Score aggregation utilities.

    The module provides a small DSL for collapsing a collection of judge
    scores – floats in [[0.0, 1.0]] by convention – into a single scalar
    that can be compared across completions.  An *aggregator* is simply a
    function of type {{!t}[float list -> float]} and thus can be passed
    around, partially applied, or composed like any other value.

    All strategies in this module are *total*: they accept the empty list
    and return [0.0] so that callers do not need to special-case the
    degenerate situation where no votes are available.

    {1 Type}
*)

(** A strategy for combining a (possibly empty) list of scores into a
    single scalar. *)
type t = float list -> float

(** {1 Pre-defined strategies} *)

(** [mean scores] returns the arithmetic mean of [scores].  The result is
    [0.0] when [scores] is empty. *)
val mean : t

(** [median scores] returns the median (50ᵗʰ percentile) of [scores].  When
    the number of elements is even, the mean of the two middle values is
    used.  Returns [0.0] for an empty list. *)
val median : t

(** [trimmed_mean ~trim scores] discards the lowest [trim] fraction and the
    highest [trim] fraction of [scores] before computing the mean.

    The [trim] parameter must lie in the interval [[0.0, 0.5)]; values
    outside this range raise [Invalid_argument].  A common setting is
    [trim = 0.1] which yields a 10 % trimmed mean.

    If all elements are trimmed away, the function returns [0.0].

    @raise Invalid_argument if [trim] ∉ [[0.0, 0.5)). *)
val trimmed_mean : trim:float -> t

(** [weighted ~weights scores] returns the weighted arithmetic mean of
    [scores] using the supplied [weights].

    • The two lists must have the same length; otherwise the plain mean is
      returned as a safe fallback.
    • Negative weights are allowed, albeit discouraged.
    • If the sum of weights is zero the function returns [0.0]. *)
val weighted : weights:float list -> t

(** [min scores] returns the minimum of [scores] or [0.0] when the list is
    empty. *)
val min : t

(** [max scores] returns the maximum of [scores] or [0.0] when the list is
    empty. *)
val max : t
