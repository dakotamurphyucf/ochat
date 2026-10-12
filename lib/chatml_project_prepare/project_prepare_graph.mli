open Core
module Attempt = Project_prepare_attempt
module Host = Project_prepare_host
module Request = Project_prepare_request
module Build = Chatml_project_build
module Error = Project_prepare_error

(** Acquire the complete selected declaration closure under the supplied host
    grants and this invocation's single aggregate ledger/deadline. Check the
    immutable selection gate before root transport and every incoming dependency
    route before transport or reuse. Prior associations are gate-affine; aliases
    never identify prior owners by namespace, path basename or traversal order.

    Keep one local Roots owner through acquisition and pure composition. Local
    package identity is retained directory identity plus exact declaration member;
    Git identity is exact repository/format/full pin/package root/member. Reuse
    captured catalogs only when effective namespace and selected own environment
    agree. Local dependencies are relative to the declaring configuration's
    directory. Relative Local under Git resolves within its exact pinned tree.
    Absolute Local requires an explicit native host anchor.

    Iterative work queues charge unique packages, every incoming dependency visit,
    route/prior-owner traversal and long identity comparisons before work. Declaration cycles
    terminate through package/lexical-route/prior-owner triples; executable cycle checks remain
    compiler-owned. Dependencies are selected by the actual captured catalog's
    scopes, never all declarations or a root/importer environment.

    Git scopes run sequentially: no nested Store acquisitions. Each live Reader
    shares object/source memo and one cumulative receipt across its package work.
    Already captured catalogs are reused across scopes. If a different repository
    reveals an uncaptured package at an earlier pin, reopen that repository through
    the same admitted Stage bracket, charging every new native/semantic receipt
    and reservation from remaining capacity. No lifetime-spanning memo is assumed.
    No native identity, cursor, repository, reader, callback or mutable queue enters
    the result. Backend and pure receipts are charged once through Stage brackets;
    failed stages poison the attempt and authorize neither retry nor refund.

    Compose the complete immutable inventory with the authentic supplied compiler
    contract, then select the resulting exact required tool names against [tools].
    Selection does not invoke a tool or create a grant, and the live selected
    registry is not retained in Build. Preserve source-owned declaration failures
    and original checkpoint/cancellation exceptions after joined cleanup.

    Every borrowed host capability must remain alive through this call. The result
    does not publish a selected lock, compile or execute code, or admit runtime
    custody. *)
val prepare
  :  Attempt.t
  -> host:Host.t
  -> request:Request.t
  -> tools:Chat_response.Tool_capability.t
  -> (Build.t, Error.t) Result.t

(** Same source acquisition/composition, without claiming live tool admission. *)
val prepare_source
  :  Attempt.t
  -> host:Host.t
  -> request:Request.t
  -> (Project_prepare_source_result.t, Error.t) Result.t
