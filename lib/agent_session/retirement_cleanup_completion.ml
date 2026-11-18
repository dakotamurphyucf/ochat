module Boundary = struct
  type t =
    | Before_release
    | Released
end

type t =
  { reply : unit Actor_reply.t
  ; boundary : Boundary.t
  }
