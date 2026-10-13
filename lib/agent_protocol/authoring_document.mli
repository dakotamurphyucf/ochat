open Authoring_common
module Context = Authoring_context_core.Context

module Document : sig
  module Version : sig
    (** Source CAS receipt: exact source revision and whole context generation.
        [overlay_open] distinguishes a base version after a close. The last
        accepted editor revision remains in the CAS even after closing an
        overlay, so a later replacement cannot reuse it. *)
    type t [@@deriving sexp_of]

    val create
      :  context:Context.Expected.t
      -> source:Source.t
      -> last_editor_revision:int option
      -> overlay_open:bool
      -> (t, Error.t) Result.t

    val context : t -> Context.Expected.t
    val source : t -> Source.t
    val last_editor_revision : t -> int option
    val overlay_open : t -> bool
    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) Result.t
  end

  module Version_request : sig
    (** Read current source CAS after any context edit. The server authorizes
        and resolves the source under this context; a caller cannot assert a
        fresh version using a source digest. *)
    type t = private
      { context : Context.Expected.t
      ; source_id : Chatml_source.Id.t
      }
    [@@deriving sexp_of]

    val create
      :  context:Context.Expected.t
      -> source_id:Chatml_source.Id.t
      -> (t, Error.t) Result.t

    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) Result.t
  end

  module Replace_request : sig
    (** Full exact UTF-8 text only. Arbitrary byte content uses bundle chunks;
        this method never repairs invalid UTF-8 or writes a host file. *)
    type t = private
      { expected : Version.t
      ; next_editor_revision : int
      ; contents : string
      }
    [@@deriving sexp_of]

    val create
      :  expected:Version.t
      -> next_editor_revision:int
      -> contents:string
      -> (t, Error.t) Result.t

    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) Result.t
  end

  module Close_request : sig
    type t = private { expected : Version.t } [@@deriving sexp_of]

    val create : expected:Version.t -> t
    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) Result.t
  end

  module Result : sig
    type t = private
      { context : Context.t
      ; version : Version.t
      }
    [@@deriving sexp_of]

    val create : context:Context.t -> version:Version.t -> (t, Error.t) Core.Result.t
    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) Core.Result.t
  end
end
