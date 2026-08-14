# `Chat_tui.Renderer_shell_security_palette`

The Shell Security UI uses one centralized modern palette built with
`Highlight_styles.fg_hex` and `bg_hex`. Colors target modern terminals with a
256-color cube while preserving contrast when approximated.

The palette separates background/elevated surfaces, title/primary/secondary
text, selected controls, borders, success, warning, and destructive/YOLO red.
Color never carries the only meaning: labels, markers, confirmation screens,
and ASCII/Unicode border fallbacks preserve usability.
