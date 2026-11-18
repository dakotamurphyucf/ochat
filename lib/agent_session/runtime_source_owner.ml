open! Core

module Identity = struct
  type t =
    { domain : int
    ; identity : unit ref
    }

  let equal t other = phys_equal t.identity other.identity
end

type t = Identity.t

let create () = { Identity.domain = (Stdlib.Domain.self () :> int); identity = ref () }

let check_domain t =
  if not (Int.equal t.Identity.domain (Stdlib.Domain.self () :> int))
  then invalid_arg "runtime source issuer belongs to another Eio domain"
;;

let identity t =
  check_domain t;
  t
;;

let matches t identity =
  check_domain t;
  Identity.equal t identity
;;
