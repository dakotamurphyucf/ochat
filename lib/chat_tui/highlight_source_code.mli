open Core

(** Bounded ChatML/ChatMD code-fence presentation for the chat renderer. The
    source identity is scoped to this immutable display fragment and does not
    claim an authored file or support semantic navigation. No TextMate regex is
    run on these languages. Other fence languages retain their existing path. *)

module Language : sig
  type t =
    | Chatml
    | Chatmd
  [@@deriving equal]

  val of_hint : string option -> t option
end

type error =
  | Too_large
  | Not_renderable

(** Replaces controls and malformed UTF-8 for Notty, preserving valid source
    characters. This is display-only; the lexical snapshot keeps exact bytes. *)
val plain_preview
  :  text:string
  -> is_cancelled:(unit -> bool)
  -> Terminal_highlighting.Highlight_tm_engine.span list list

(** [is_cancelled] is polled during capture, scanning and span projection;
    cancellation raises [Exit], matching the existing renderer contract. *)
val highlight
  :  Language.t
  -> text:string
  -> theme:Terminal_highlighting.Highlight_theme.t
  -> is_cancelled:(unit -> bool)
  -> (Terminal_highlighting.Highlight_tm_engine.span list list, error) Result.t
