# Utf8_edit — grapheme-safe editor offsets

Ochat stores byte offsets, but shared draft/command/search cursor setters and
editing operations align them to extended grapheme boundaries. `Utf8_edit`
uses `Uuseg_string` segmentation for `floor`, `ceil`, `previous` and `next`;
`uchar` encodes a Notty Unicode input event into UTF-8. Inputs must be valid
UTF-8. Combining accents and joined emoji are edited as whole clusters.

The [controller](controller.doc.md) uses these boundaries for insertion,
horizontal movement, backspace, selection deletion and Normal character edits.
Word motions still use whitespace; terminal cell width is the separate
`Input_display`/Notty responsibility. Segmentation scans the buffer and is not
an indexed rope implementation.

`Model.with_edit_checkpoint` provides one undo snapshot per changed draft
action unless that action already updates its undo stack. General input uses
the same behavior in all three hosts. Typeahead retains whole-snippet and
single-line acceptance, preview expansion, and its existing checkpoints.
Unmodified `ß` types text; selection remains Meta+S/Meta+V.

See [implementation](../../../lib/chat_tui/utf8_edit.ml),
[interface](../../../lib/chat_tui/utf8_edit.mli), and
[editor regressions](../../../test/chat_tui_input_and_stream_scheduling/chat_tui_type_ahead_test.ml).
