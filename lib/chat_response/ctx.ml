(* We keep the type abstract enough so that we don't have to pin the
     exact polymorphic object type exposed by [Eio.Stdenv.t].  The few
     things we require for now are [#net] and that we can recover a
     filesystem root from it. *)

type 'env t =
  { env : 'env
  ; dir : Eio.Fs.dir_ty Eio.Path.t
  ; cache : Cache.t
  }

let create ~env ~dir ~cache = { env; dir; cache }

(* Convenience constructor when [dir] is the process' filesystem root. *)
let of_env ~env ~cache = { env; dir = Eio.Stdenv.fs env; cache }
let net t = t.env#net
let env t = t.env
let dir t = t.dir
let cache t = t.cache
