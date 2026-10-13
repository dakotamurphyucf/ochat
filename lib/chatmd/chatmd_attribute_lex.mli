open! Core

(** Shared, pure attribute spelling rules for the strict ChatMD lexer and the
    bounded authoring cursor. The selected byte range is the exact fragment
    after an opening tag's name; returned offsets are relative to the supplied
    source string. This module does not recognize tags, validate declarations,
    or fetch sources. *)

module Byte_range : sig
  type t

  val start_byte : t -> int
  val end_byte : t -> int
end

module Attribute : sig
  type t

  (** Decoded exactly as the strict lexer does, including its five named HTML
      entities; unrecognized entities and retained backslashes stay literal.
      Order and duplicate names are preserved. *)
  val decoded : t -> Chatmd_ast.attribute

  val raw_name : t -> Byte_range.t

  (** Includes the original single/double quotes when present. [None] is a
      flag attribute, not an empty quoted value. *)
  val raw_value : t -> Byte_range.t option
end

module Quote : sig
  type t =
    | Single
    | Double
  [@@deriving equal, sexp_of]
end

module Pending : sig
  type t =
    | Between_attributes
    | Attribute_name of Byte_range.t
    | After_name of
        { name : string
        ; raw_name : Byte_range.t
        }
    | Before_value of
        { name : string
        ; raw_name : Byte_range.t
        }
    | Attribute_value of
        { name : string
        ; raw_value : Byte_range.t
        ; quote : Quote.t option
        ; trailing_matching_quote_escape : bool
        ; open_entity : bool
        }
    | Opaque

  (** A current name or value can still become a family discriminator. The
      schema must not treat it as a completed decoded attribute. *)
  val uncertain_discriminator : t -> bool
end

module Prefix : sig
  type t

  val completed : t -> Attribute.t list
  val pending : t -> Pending.t
end

module Error : sig
  type t =
    | Invalid_range
    | Unterminated_quote of
        { name : string
        ; content_start_byte : int
        ; expected_quote : char
        }
  [@@deriving equal, sexp_of]
end

(** Parse only [source[start_byte,end_byte)], which may end within a name,
    quote, escape, or entity. The full source is borrowed; no prefix copy is
    made before range/source/work admission. This is the same scanner/decoder
    used by [complete_exn], with a different EOF result. [checkpoint] charges
    every inspected byte, including entity lookahead, and may raise unchanged.
    The scan remains linear in admitted bytes even for repeated ampersands
    without semicolons. A complete name followed only by whitespace stays
    [After_name]: [=] may still follow. After [=], [Before_value] has an empty
    prefix. A closing quote completes the attribute immediately; an unquoted
    value completes only at a delimiter or finished tag. No source is repaired. *)
val prefix
  :  source:string
  -> start_byte:int
  -> end_byte:int
  -> checkpoint:(unit -> unit)
  -> (Prefix.t, Error.t) Result.t

(** The strict lexer's complete-fragment behavior, including its existing
    failure on an unterminated quoted value. No new accepted spelling or entity
    decoding is introduced. The strict lexer may use a no-op callback under
    its existing parser admission; the authoring scanner charges this work to
    its bounded scan. *)
val complete_exn : string -> checkpoint:(unit -> unit) -> Chatmd_ast.attribute list

(** The same completed scan with absolute raw byte ranges for the shared
    highlighter/cursor scanner. Malformed quotes return a typed error so a
    tolerant scan never catches an unrelated callback [Failure]. Callback
    exceptions and cancellation propagate unchanged. *)
val complete_with_ranges
  :  source:string
  -> start_byte:int
  -> end_byte:int
  -> checkpoint:(unit -> unit)
  -> (Attribute.t list, Error.t) Result.t
