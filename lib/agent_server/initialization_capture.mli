(** Opaque original constructor-cleanup transport. Advanced constructors receive
    it from their actual Owner invocation. Only trusted first-party Factory
    initialization callsites enroll facts; custom constructors do not acquire
    permission or a public ownership-assignment operation. *)
type t = Initialization_capture_data.t
