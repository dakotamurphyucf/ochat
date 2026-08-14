(** Detached Chat message rendering inputs and results.

    Jobs contain immutable snapshots of every value used to render one Chat
    transcript message. They are safe to prepare on the UI domain and execute
    without access to mutable page state. *)

module Priority : sig
  type t =
    | Visible
    | Prefetch
    | Background
end

module Layout_plan : sig
  type t =
    { min_width : int
    ; max_width : int option
    }

  val unconstrained : t
  val unknown : t
  val intersect : t -> t -> t
  val allows : t -> width:int -> bool
end

module Key : sig
  type t =
    { transcript_generation : int
    ; row_id : Projected_message.Id.t
    ; row_revision : int
    ; message_index : int
    ; message_revision : int
    ; width : int
    ; role : string
    ; text : string
    ; tool_output : Types.tool_output_kind option
    ; tool_call_outcome : Ochat_function.Trace.outcome option
    ; theme_generation : int
    ; grammar_generation : int
    }

  (** [equal a b] returns [true] when every stable rendering identity and
      presentation field is equal. [message_index], [message_revision], and
      [transcript_generation] are scheduling hints; [row_id] and
      [row_revision] own stale-work identity. *)
  val equal : t -> t -> bool
end

module Layout : sig
  type line = (Notty.A.t * string) list

  type t =
    { width : int
    ; lines : line list
    }
end

module Code_cache : sig
  type role_class =
    | Toollike
    | Userlike

  type t

  val create : capacity:int -> t
  val clear : t -> unit
  val length : t -> int

  (** [width_bucket width] returns the width used by cached code images. *)
  val width_bucket : int -> int

  val find
    :  t
    -> role_class:role_class
    -> grammar_generation:int
    -> lang:string option
    -> code:string
    -> width:int
    -> Layout.line list option

  val set
    :  t
    -> role_class:role_class
    -> grammar_generation:int
    -> lang:string option
    -> code:string
    -> width:int
    -> Layout.line list
    -> unit
end

module Highlight_cache : sig
  type binding
  type t

  val create : capacity:int -> t
  val clear : t -> unit
  val length : t -> int

  val find_plain
    :  t
    -> theme_generation:int
    -> grammar_generation:int
    -> lang:string option
    -> text:string
    -> Highlight_tm_engine.span list list option

  val set_plain
    :  t
    -> theme_generation:int
    -> grammar_generation:int
    -> lang:string option
    -> text:string
    -> Highlight_tm_engine.span list list
    -> unit

  val find_scoped
    :  t
    -> theme_generation:int
    -> grammar_generation:int
    -> lang:string option
    -> text:string
    -> (Highlight_tm_engine.scoped_span list list * Highlight_tm_engine.info) option

  val set_scoped
    :  t
    -> theme_generation:int
    -> grammar_generation:int
    -> lang:string option
    -> text:string
    -> Highlight_tm_engine.scoped_span list list * Highlight_tm_engine.info
    -> unit

  val install : t -> binding -> unit
  val capture : t -> (unit -> 'a) -> 'a * binding list
end

module Prepared_message : sig
  type paragraph =
    { text : string
    ; fallback_spans : (Notty.A.t * string) list
    ; tool_call_parts : (string * string * string * string) option
    }

  type block =
    | Text of paragraph list
    | Code of
        { lang : string option
        ; code : string
        }

  type body =
    | Default of block list
    | Apply_patch of
        { status : paragraph list
        ; patch : string option
        }
    | Read_file of
        { lang : string
        ; code : string
        }

  type t =
    { role : string
    ; text : string
    ; body : body
    }
end

type semantic_seed =
  { prepared : Prepared_message.t
  ; highlights : Highlight_cache.binding list
  }

type t =
  { key : Key.t
  ; geometry_generation : int
  ; request_generation : int
  ; render_generation : int
  ; submission_generation : int
  ; semantic_seed : semantic_seed option
  ; priority : Priority.t
  }

(** [create ...] snapshots one message render request. [message_index] is the
    current scheduling/layout hint and may change while detached work runs.
    [semantic_seed] carries validated width-independent preparation into a
    detached worker. *)
val create
  :  transcript_generation:int
  -> row_id:Projected_message.Id.t
  -> row_revision:int
  -> message_index:int
  -> message_revision:int
  -> width:int
  -> role:string
  -> text:string
  -> tool_output:Types.tool_output_kind option
  -> tool_call_outcome:Ochat_function.Trace.outcome option
  -> theme_generation:int
  -> grammar_generation:int
  -> geometry_generation:int
  -> request_generation:int
  -> render_generation:int
  -> submission_generation:int
  -> semantic_seed:semantic_seed option
  -> priority:Priority.t
  -> t

module Prepared_cache : sig
  type t

  val create : capacity:int -> t
  val clear : t -> unit
  val length : t -> int

  val find
    :  t
    -> row_id:Projected_message.Id.t
    -> row_revision:int
    -> role:string
    -> text:string
    -> tool_output:Types.tool_output_kind option
    -> Prepared_message.t option

  val set
    :  t
    -> row_id:Projected_message.Id.t
    -> row_revision:int
    -> role:string
    -> text:string
    -> tool_output:Types.tool_output_kind option
    -> Prepared_message.t
    -> unit
end

module Wrapped_cache : sig
  type line = (Notty.A.t * string) list
  type t

  val create : capacity:int -> t
  val clear : t -> unit
  val find : t -> string -> line list option
  val set : t -> string -> line list -> unit
  val stats : t -> int * int
end

module Runtime : sig
  type t

  (** [create ~hi_engine ~theme_generation ~grammar_generation ?code_cache ()]
      creates an explicitly owned synchronous rendering runtime. The
      generation values identify the immutable theme and grammar
      configuration represented by [hi_engine]. Omitting [code_cache] disables
      detached code-image caching.

      A runtime and its cache have one execution owner and must not be used
      concurrently. *)
  val create
    :  hi_engine:Highlight_tm_engine.t
    -> theme_generation:int
    -> grammar_generation:int
    -> ?is_cancelled:(unit -> bool)
    -> ?code_cache:Code_cache.t
    -> ?highlight_cache:Highlight_cache.t
    -> ?prepared_cache:Prepared_cache.t
    -> ?wrapped_cache:Wrapped_cache.t
    -> unit
    -> t

  val hi_engine : t -> Highlight_tm_engine.t
  val theme_generation : t -> int
  val grammar_generation : t -> int
  val is_cancelled : t -> unit -> bool
  val code_cache : t -> Code_cache.t option
  val highlight_cache : t -> Highlight_cache.t option
  val prepared_cache : t -> Prepared_cache.t option
  val wrapped_cache : t -> Wrapped_cache.t option
end

type result =
  { key : Key.t
  ; geometry_generation : int
  ; request_generation : int
  ; render_generation : int
  ; submission_generation : int
  ; prepared : Prepared_message.t option
  ; image : Notty.I.t
  ; height : int
  ; layout : Layout.t
  ; highlights : Highlight_cache.binding list
  ; layout_plan : Layout_plan.t
  }

(** [result job ?prepared ~layout ~image] attaches the complete stale-work
    identity and any width-independent prepared representation to the
    overlay-neutral layout and image. *)
val result
  :  ?prepared:Prepared_message.t
  -> ?highlights:Highlight_cache.binding list
  -> ?layout_plan:Layout_plan.t
  -> ?layout:Layout.t
  -> t
  -> image:Notty.I.t
  -> result

(** [result_matches result job] returns [true] when [result] was produced for
    exactly the same render key, model generations, and worker submission as
    [job]. *)
val result_matches : result -> t -> bool
