(** Reusable Notty styles and helpers for highlight theming.

    Provide readable names and small constructors for commonly used
    {!Notty.A} attributes so call-sites do not need to embed raw
    expressions. Prefer composing attributes with [(++)].

    Invariants and ranges mirror {!Notty.A}:
    - Grayscale levels are in [0, 23]
    - Truecolor channels are in [0, 255] (via [rgb_888])
    - Convenience helpers exist for the 256-color cube with channels in [0, 5]

    See also: {!Notty.A} for the underlying colour and style semantics,
    including behaviour of attribute composition and terminal support notes. *)

(** [a ++ b] composes attributes.

    Left-associative; attributes form a monoid with {!empty}. Later values
    take precedence when set, styles are unioned.

    Example composing foreground and bold:
    {[
      let a = Notty.A.(fg red) in
      let b = Notty.A.(st bold) in
      let composed = Chat_tui.Highlight_styles.(a ++ b)
      (* [composed] has red foreground and bold style *)
    ]} *)
val ( ++ ) : Notty.A.t -> Notty.A.t -> Notty.A.t

(** [bold] adds bold style. *)
val bold : Notty.A.t

(** [italic] adds italic style. *)
val italic : Notty.A.t

(** [underline] adds underline style. *)
val underline : Notty.A.t

(** [empty] is the empty attribute (terminal defaults, no styles). *)
val empty : Notty.A.t

(** [fg_gray n] sets foreground to grayscale level [n].

    Level must be in [0, 23].
    @raise Invalid_argument if [n] is outside [0, 23]. *)
val fg_gray : int -> Notty.A.t

(** [bg_gray n] sets background to grayscale level [n].

    Level must be in [0, 23].
    @raise Invalid_argument if [n] is outside [0, 23]. *)
val bg_gray : int -> Notty.A.t

(** [fg_rgb ~r ~g ~b] sets foreground using 24-bit truecolor.

    Channels must be in [0, 255].
    @raise Invalid_argument if any channel is outside [0, 255]. *)
val fg_rgb : r:int -> g:int -> b:int -> Notty.A.t

(** [bg_rgb ~r ~g ~b] sets background using 24-bit truecolor.

    Channels must be in [0, 255].
    @raise Invalid_argument if any channel is outside [0,255]. *)
val bg_rgb : r:int -> g:int -> b:int -> Notty.A.t

(** ANSI colour convenience attributes for foreground. *)
val fg_black : Notty.A.t

val fg_red : Notty.A.t
val fg_green : Notty.A.t
val fg_yellow : Notty.A.t
val fg_blue : Notty.A.t
val fg_magenta : Notty.A.t
val fg_cyan : Notty.A.t
val fg_lightwhite : Notty.A.t

(** ANSI colour convenience attributes for background. *)
val bg_black : Notty.A.t

val bg_white : Notty.A.t
val bg_lightwhite : Notty.A.t

(** [fg_rgb6 ~r ~g ~b] sets foreground using the 256-color cube, mapping
    channels in [0, 5] to their XTerm RGB approximations ([0x00; 0x5f; 0x87;
    0xaf; 0xd7; 0xff]) via truecolor. Useful for legacy themes.

    Channels must be in [0, 5].
    @raise Invalid_argument if any channel is outside [0, 5]. *)
val fg_rgb6 : r:int -> g:int -> b:int -> Notty.A.t

(** [bg_rgb6 ~r ~g ~b] sets background using the 256-color cube, mapping
    channels in [0, 5] to their XTerm RGB approximations via truecolor.

    Channels must be in [0, 5].
    @raise Invalid_argument if any channel is outside [0, 5]. *)
val bg_rgb6 : r:int -> g:int -> b:int -> Notty.A.t

(** [hex_to_rgb s] parses [s] as a hex colour and returns [(r, g, b)].

    Accepted forms: "#RRGGBB", "RRGGBB", "#RRGGBBAA"/"RRGGBBAA" (alpha is
    ignored), and short forms "#RGB"/"RGB" (nibbles doubled). Returns [None]
    if parsing fails or any channel is out of range. *)
val hex_to_rgb : string -> (int * int * int) option

(** [fg_hex hex] sets foreground to [hex] if it parses, otherwise returns
    {!empty}. *)
val fg_hex : string -> Notty.A.t

(** [bg_hex hex] sets background to [hex] if it parses, otherwise returns
    {!empty}. *)
val bg_hex : string -> Notty.A.t
