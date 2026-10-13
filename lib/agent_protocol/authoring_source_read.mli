open Authoring_common
module Context = Authoring_context_core.Context

module Source_read : sig
  (** Host-root IDs are connection-local candidates, not path grants. Host
      browse/read observes a granted root afresh; context inventory/read uses
      exact captured effective bytes at a generation, including an open
      overlay. Only explicit context refresh reobserves host bytes for that
      context. Path strings are validated portable members. *)
  module Root : sig
    type t = private
      { id : Id.Authoring_root.t
      ; display_name : string
      }
    [@@deriving sexp_of]

    val create : id:Id.Authoring_root.t -> display_name:string -> (t, Error.t) Result.t
    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) Result.t
  end

  module Roots_result : sig
    (** Authorized roots available to this connection at the time of read.
        A subsequent open reauthorizes the chosen root. *)
    type t = private { roots : Root.t list } [@@deriving sexp_of]

    val create : roots:Root.t list -> (t, Error.t) Result.t
    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) Result.t
  end

  module Host_list_request : sig
    type t = private
      { root : Id.Authoring_root.t
      ; directory : string option
      }
    [@@deriving sexp_of]

    val create
      :  root:Id.Authoring_root.t
      -> directory:string option
      -> (t, Error.t) Result.t

    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) Result.t
  end

  module Host_list_result : sig
    (** Complete bounded nonrecursive candidates from one granted root.
        Entries do not prove file type or grant a subsequent read. If native
        enumeration exceeds its cap, the whole request fails Resource_limit. *)
    type t = private
      { root : Id.Authoring_root.t
      ; members : string list
      }
    [@@deriving sexp_of]

    val create : root:Id.Authoring_root.t -> members:string list -> (t, Error.t) Result.t
    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) Result.t
  end

  module Host_read_request : sig
    type t = private
      { root : Id.Authoring_root.t
      ; member : string
      }
    [@@deriving sexp_of]

    val create : root:Id.Authoring_root.t -> member:string -> (t, Error.t) Result.t
    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) Result.t
  end

  module Host_read_result : sig
    (** New read observation with host-constructed Source ID/digest/length.
        Content is exact base64 bytes and does not mutate any open context. *)
    type t [@@deriving sexp_of]

    val create
      :  root:Id.Authoring_root.t
      -> member:string
      -> source:Source.t
      -> contents:string
      -> (t, Error.t) Result.t

    val root : t -> Id.Authoring_root.t
    val member : t -> string
    val source : t -> Source.t
    val contents : t -> string
    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) Result.t
  end

  module List_request : sig
    type t = private
      { context : Context.Expected.t
      ; package_namespace : string option
      ; directory : string option
      }
    [@@deriving sexp_of]

    val create
      :  context:Context.Expected.t
      -> package_namespace:string option
      -> directory:string option
      -> (t, Error.t) Result.t

    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) Result.t
  end

  module Read_request : sig
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

  module List_result : sig
    (** A finite authorized inventory prefix sorted by full Source ID.
        [omitted] is the number of
        further entries in this exact observed manifest; [truncated] iff it
        is positive. A changed context requires a fresh list. *)
    type t [@@deriving sexp_of]

    val create
      :  context:Context.Expected.t
      -> sources:Source.t list
      -> omitted:int
      -> (t, Error.t) Result.t

    val context : t -> Context.Expected.t
    val sources : t -> Source.t list
    val omitted : t -> int
    val truncated : t -> bool
    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) Result.t
  end

  module Read_result : sig
    type t [@@deriving sexp_of]

    (** Wire contents are base64 of exact source bytes. Source ID/digest/length
        must match decoded bytes before this value is admitted. The context
        echo lets a client reject a response from a superseded generation. *)
    val create
      :  context:Context.Expected.t
      -> source:Source.t
      -> contents:string
      -> (t, Error.t) Result.t

    val context : t -> Context.Expected.t
    val source : t -> Source.t
    val contents : t -> string
    val to_json : t -> Jsonaf.t
    val of_json : Jsonaf.t -> (t, Error.t) Result.t
  end
end
