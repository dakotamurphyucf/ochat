# Research: pqwy/notty examples — patterns for gradients, progress, tables, borders, canvas, animation, mouse

Scope: Review Notty examples at https://github.com/pqwy/notty/tree/master/examples and extract patterns relevant to:
- Gradient backgrounds
- Progress bars / gauges
- Tables / grid layout
- Borders / boxes
- Half-block or braille canvas demos
- Animation patterns
- Mouse interaction

Focus: Identify image composition APIs (I.tabulate, I.hcat/vcat, I.hsnap/vsnap, pad/crop), attribute usage (A.fg/bg/st), reusable idioms, short code snippets, and performance considerations.

Method: Use live GitHub pages, extract code and comments, summarize per-page findings, and synthesize guidance.



## Source: Notty module documentation — examples and performance
- URL: https://pqwy.github.io/notty/doc/notty/Notty/index.html
- Key findings:
  - Composition operators: I.(<|>) hcat beside, I.(<->) vcat above, I.(</>) overlay; snaps and crops: I.hsnap/vsnap and I.hcrop/vcrop; tabulate for programmatic image generation.
  - Attributes: A.fg, A.bg, A.st to set foreground/background colors and styles; styles include bold, italic, underline, blink, reverse.
  - Examples show padding and spacing (I.hpad/vpad), overlay, and pretty-printing via I.strf and I.pp_attr.
  - Performance model: image rendering cost depends on image complexity (number of composition/crop ops), not raw image dimensions; avoid repeated cropping that grows complexity (prefer single-pass slicing like wrap2); rendering scales with output dimensions.
  - Simple interaction example uses Notty_unix.Term for events and redraw on resize and key presses.
- Relevance:
  - Establishes the core APIs we’ll look for in examples (I.tabulate, I.hcat/vcat, I.hsnap/vsnap, pad/crop) and provides authoritative performance guidance to inform playground patterns.


## Source: examples/colors.ml — attributes and color ramps
- URL: https://github.com/pqwy/notty/blob/master/examples/colors.ml
- Key findings:
  - Demonstrates A.fg, A.bg, and A.st usage across system colors and styles.
  - Builds attribute samples using I.string and composes columns with I.vcat and I.hcat.
  - Uses helper Images.* from common.ml to render 256-color cube, grayscale ramp, and a 24-bit rainbow using I.tabulate.
  - Pattern: assemble labeled sections via a small combine that vcat sections, pad, and hcat labels.
- Snippets:
  - A.st composition: A.(st bold ++ st italic), A.(st underline ++ st reverse)
  - 24-bit gradient line: Images.c_rainbow (w - 2) 1 |> I.tabulate to fill with A.bg rgb888 colors
- Relevance:
  - Shows attribute API (A.fg/bg/st) in practice and gradient generation patterns for background stripes; useful for gradient backgrounds and a palette inspector mode.


## Source: examples/common.ml — shared image utilities (borders, gradients, half-block canvas)
- URL: https://github.com/pqwy/notty/blob/master/examples/common.ml
- Key findings:
  - Image tiling and tabulation helpers: tile w h i = I.tabulate w h (fun _ _ -> i)
  - Gradients:
    - Images.c_gray_ramp: I.tabulate 24 1 (A.bg (gray g) " ")
    - Images.c_cube_ix/rgb: nested I.tabulate to form 6x6 color cube with A.bg (rgb …)
    - Images.c_rainbow: continuous 24-bit rainbow via A.rgb_888 and I.char with A.bg color
  - Borders/boxes: Images.outline attr i builds a 1px border around image i using box-drawing runes (U+2500–U+2570) and grid [[a; hbar; b]; [vbar; i; vbar]; [d; hbar; c]] -> I.hcat/vcat.
  - Half-block canvas: pxmatrix w h f makes a 2x vertical resolution canvas using the half-block rune "▄", setting A.bg to pixel (x, y) and A.fg to pixel (x, y+1).
  - Grid/table: grid xxs = xxs |> List.map I.hcat |> I.vcat — a general combinator to build tables.
- Snippets:
  - Border box:
    let outline attr i =
      let (w,h) = I.(width i, height i) in
      let chr x = I.uchar attr (Uchar.of_int x) 1 1 in
      let hbar = I.uchar attr (Uchar.of_int 0x2500) w 1
      and vbar = I.uchar attr (Uchar.of_int 0x2502) 1 h in
      let (a,b,c,d) = (chr 0x256d, chr 0x256e, chr 0x256f, chr 0x2570) in
      grid [[a; hbar; b]; [vbar; i; vbar]; [d; hbar; c]]
  - Half-block canvas:
    let halfblock = "▄"
    let pxmatrix w h f =
      I.tabulate w h (fun x y ->
        let y = y * 2 in
        I.string A.(bg (f x y) ++ fg (f x (y+1))) halfblock)
- Relevance:
  - Provides reusable idioms for borders/boxes, gradient backgrounds, table/grid combinators, and a pixel-like canvas with half-blocks — all directly applicable to playground modes.


## Source: examples/almondbread.ml — Mandelbrot via half-block pixel matrix (gradient background)
- URL: https://github.com/pqwy/notty/blob/master/examples/almondbread.ml
- Key findings:
  - Computes a Mandelbrot escape-time value and maps it to A.rgb_888 color using sin phases; demonstrates continuous gradient coloring.
  - Uses Common.pxmatrix to render a dense field with half-blocks, leveraging A.bg for upper pixel and A.fg for lower pixel.
  - Transforms coordinate space via xlate, scale, rot; renders with render_unit f (w,h) using I.tabulate inside pxmatrix.
- Snippet:
  - let render_unit f (w,h) =
      let sw = 1. /. float w and sh = 1. /. float (2*h) in
      pxmatrix w h (fun x y -> f (float x *. sw) (float y *. sh))
- Relevance:
  - Clear pattern for gradient backgrounds and half-block canvas rendering; shows performance-aware approach by computing per-cell color directly in tabulate.


## Source: examples/crops.ml — cropping and snapping edge cases, borders
- URL: https://github.com/pqwy/notty/blob/master/examples/crops.ml
- Key findings:
  - hdistribute distributes images across width w by hsnap ~align (w/n) and I.hcat.
  - take w h i = I.(vsnap h i |> hsnap w) — composing vsnap/hsnap to a target rectangle.
  - Demonstrates I.uchar to make a solid bullet-filled rectangle (U+2022), contrasted with cropped and void versions; all wrapped in Images.outline borders.
  - Interactive resizing with arrow keys shows how snapping adapts layouts.
- Snippet:
  - let hdistribute ?align w imgs =
      let n = List.length imgs in I.(List.map (hsnap ?align (w/n)) imgs |> hcat)
- Relevance:
  - Useful for table/grid layout and panel sizing; illustrates hsnap/vsnap and composition with outlines for boxes.


## Source: examples/cuts.ml — text cropping across graphemes and wide chars
- URL: https://github.com/pqwy/notty/blob/master/examples/cuts.ml
- Key findings:
  - Visualizes I.hcrop a b across all possible left/right cuts for a sample string; pads with '.' using hpadwith A.(fg lightblack) '.' a b.
  - Demonstrates behavior with emoji/wide chars and combining sequences; shows multiple styles including A.st blink and background colors.
  - Uses I.pad to offset and zcat to overlay blinking decorations; also uses I.vpad/hpad for spacing.
- Snippet:
  - let cuts i =
      let w = I.width i in
      List.(range 0 w |> map (fun a -> range 0 (w - a) |> map (fun b ->
        i |> I.hcrop a b |> hpadwith A.(fg lightblack) '.' a b) |> I.vcat |> I.hpad 1 1))
      |> I.hcat |> I.vpad 1 1
- Relevance:
  - Informs safe cropping strategies and preview tools; supports building robust text-wrapping in playground without breaking grapheme clusters.


## Source: examples/inline.ml — manual cursor positioning and partial redraws
- URL: https://github.com/pqwy/notty/blob/master/examples/inline.ml
- Key findings:
  - Implements output_subst ~prev to only redraw changed lines by moving the cursor up and re-emitting minimal content.
  - Demonstrates I.tabulate to build hidden content and overlay, and progressive drawing with variable background bars.
  - Uses I.eol to force line termination and maintains previous image height to compute deltas.
- Snippet:
  - let output_subst ~prev i =
      let h = I.height prev in
      let d = h - I.height i in
      if d > 0 then (rewind (d - 1); output_image (I.void 0 d));
      rewind (h - 1); output_image i
- Relevance:
  - Shows a performance optimization for animations: partial updates instead of full-screen redraws; useful pattern for smooth progress bars, gauges, and dynamic layouts.


## Source: examples/mouse.ml — mouse interaction (drag, scroll), crosshair and status bar
- URL: https://github.com/pqwy/notty/blob/master/examples/mouse.ml
- Key findings:
  - Listens for `Mouse` events: `Press Left`/`Drag`, `Release`, `Press (Scroll s)`; demonstrates modifier keys in status bar.
  - Renders a crosshair at (x,y) using box-drawing runes via I.uchar and composes with I.vpad/hpad; snaps to viewport with crop ~t/l/r and vsnap ~align:`Top.
  - Shows a vertical “scroll gauge” built by vcat of Images.dot A.(gray level) and vsnap Bottom.
  - Footer status line is right-aligned via I.hsnap ~align:`Right w and uses A.st to highlight active modifiers.
- Snippet:
  - let cross =
      let a = match st with `Drag -> A.(fg lightgreen) | `Down -> A.(fg green) in
      (uchar a lnh x 1 |> vpad y 0)
      <|> (uchar a lnv 1 y <-> uchar a crs 1 1 <-> uchar a lnv 1 (h - y))
      <|> (uchar a lnh (w - x - 1) 1 |> vpad y 0)
      |> crop ~t:1 ~l:1 ~r:3 |> hpad 1 1 |> vsnap ~align:`Top (h - 1)
- Relevance:
  - Directly applicable mouse-handling and HUD patterns (crosshairs, scroll indicators, status bars) for interactive playground modes.


## Source: examples/life.ml — animation loop with background gradient and mouse editing
- URL: https://github.com/pqwy/notty/blob/master/examples/life.ml
- Key findings:
  - Uses I.tabulate over (w, h-1) to render each cell: living cells as a red dot (I.string A.(fg lightred) "●"), dead cells as a computed background "." with grayscale gradient A.(fg (gray k)).
  - Footer with generation counter aligned right via I.hsnap ~align:`Right w.
  - Event loop with Notty_lwt: timer-based redraws, mouse to toggle cells, resize-triggered re-render; torus topology to wrap coordinates.
- Snippet:
  - let render (w,h) step life =
      I.tabulate w (h-1) (fun x y -> if CSet.mem (x,y) life then dot else background step (x,y))
      <-> I.(strf ~attr:A.(fg lightblack) "[generation %04d]" step |> hsnap ~align:`Right w)
- Relevance:
  - Establishes an animation pattern with periodic timer, incremental state update, and layout composition; background gradient idea applies to gradient modes, while I.tabulate shows per-cell image construction.


## Source: examples/rain.ml — timed animation (Matrix-style rain), performance-conscious loop
- URL: https://github.com/pqwy/notty/blob/master/examples/rain.ml
- Key findings:
  - Manages frame timing using Unix.gettimeofday and a frame period; interleaves event polling with deadline-driven rendering (event ~delay).
  - Generates columns with varying window sizes and speeds; builds each column as a vcat of I.string cells colored by a decay function color i n using A.rgb_888.
  - Composes full frame via I.hcat of columns, with a constant background I.char bgc ' ' w h.
  - Resets scene on resize or space key; uses Notty_unix.Term directly (non-Lwt) for fine-grained control.
- Snippet:
  - let show ((w,h), xs) =
      let f = function
        | `Wait _ -> I.void 1 0
        | `Line (i, sym, win, _) -> ... chars |> images [] off |> I.vcat |> I.vpad (max 0 (i - win)) 0 in
      (List.map f xs |> I.hcat) (I.char bgc ' ' w h)
- Relevance:
  - A robust animation pattern with precise timing; shows efficient per-column composition and reuse — applicable to progress/gauge animations and particle effects.


## Source: examples/runes.ml — layout with diverse scripts, centered text, custom outline
- URL: https://github.com/pqwy/notty/blob/master/examples/runes.ml
- Key findings:
  - Defines centered attr xs: measure max width of I.string lines, pad with I.char ' ' to center, then I.vcat — a centering idiom.
  - Composes note blocks with a bold "Note:" and lines via I.vcat and I.hpad; wraps content in Images.outline attr and pads margins.
  - Demonstrates I.hcat/vcat grid builders; verifies geometry and alignment under complex Unicode text.
- Snippet:
  - let centered attr xs =
      let lns = List.map (I.string attr) xs in
      let w = List.fold_left (fun a i -> max a I.(width i)) 0 lns in
      lns |> List.map (fun ln -> let d = w - I.width ln in I.char attr ' ' (d/2) 1 <|> ln <|> I.char attr ' ' (d - d/2) 1) |> I.vcat
- Relevance:
  - Provides a reusable centering/grid idiom and demonstrates border boxing; techniques generalize to table and banner layouts.


## Source: examples/letters.ml — grid layout and live input animation
- URL: https://github.com/pqwy/notty/blob/master/examples/letters.ml
- Key findings:
  - Accepts typed characters and places them into a nw x nh grid using List.chunks and I.hcat/I.vcat; colors vary with position via A.bg (rgb ~g:i ~b:j).
  - Uses I.uchar to render arbitrary Uchars; shows List.take to cap history and animated updates via simpleterm.
  - Pads and snaps the grid with I.pad ~t/l and I.hsnap ~align:`Left (nw+1), then tiles horizontally via tile nw 1.
- Snippet:
  - mapi (fun i us -> mapi (fun j u -> I.uchar A.(fg white ++ bg (rgb ~r:0 ~g:i ~b:j)) u 1 1) us |> I.hcat) uus |> I.vcat
- Relevance:
  - Good pattern for table/grid layouts, character dashboards, and live-updating boards.


## Source: examples/testpatterns.ml — composition stress test
- URL: https://github.com/pqwy/notty/blob/master/examples/testpatterns.ml
- Key findings:
  - Renders Images.i3, Images.i5, Images.checker1 with I.eol; these exercise composition, cropping, and padding complexity.
  - Serves as a canary for regressions in composition performance.
- Relevance:
  - Encourages using small, composable primitives and testing with complex layouts to observe performance.

## Source: examples/common_lwt.ml — Lwt terminal helpers for event loops
- URL: https://github.com/pqwy/notty/blob/master/examples/common_lwt.ml
- Key findings:
  - simpleterm_lwt and simpleterm_lwt_timed encapsulate a redraw-on-event/timer loop; return variants: `Continue s | `Redraw (s, i) | `Stop`.
  - Provides timer and event helpers; on resize, redraws with current state and size.
- Snippet:
  - let simpleterm_lwt_timed ?delay ~f s0 =
      let term = T.create () in
      let rec loop (e,t) dim s = (e <&> t) >>= function
        | `Resize dim as evt -> invoke (event term, t) dim s evt
        | `Timer as evt -> invoke (e, timer delay) dim s evt
      and invoke es dim s e = match f dim s e with
        | `Redraw (s,i) -> T.image term i >>= fun () -> loop es dim s
        | `Continue s -> loop es dim s
        | `Stop -> Lwt.return_unit
- Relevance:
  - Reusable animation/event-loop scaffolding for playground modes needing timers and responsive redraw.


## Source: examples/cursor.ml — cursor positioning with Term.cursor
- URL: https://github.com/pqwy/notty/blob/master/examples/cursor.ml
- Key findings:
  - Shows Term.cursor t (Some (x,y)) to place cursor over a composed image (a checkmark and coordinate readout).
  - Keyboard navigation updates position; mouse drag updates position; resize triggers redraw.
- Relevance:
  - Complements mouse.ml with explicit cursor control; useful for editing modes where cursor and image must align.


## Synthesis: Idioms, composition patterns, and snippets to reuse
- Composition patterns:
  - Programmatic images: I.tabulate w h (fun x y -> ...)
  - Layout: I.hcat, I.vcat; grids via List.map I.hcat |> I.vcat; centering by measuring max width and padding with I.char ' '.
  - Snapping to slots: I.hsnap ~align width, I.vsnap ~align height to distribute panels; compose with hdistribute w imgs.
  - Cropping: I.hcrop a b and I.vcrop, often after composing; avoid repeated cropping per performance model.
  - Padding and margins: I.hpad l r, I.vpad t b; also pad ~t/~l/~r/~b for surrounding space.
  - Overlay: I.(</>) to composite foreground over background layers.
- Attributes:
  - Foreground/background: A.fg color, A.bg color; colors include 16 system colors, A.rgb ~r/g/b (0..5), A.rgb_888 for 24-bit, and A.gray.
  - Styles: A.st bold/italic/underline/blink/reverse; compose with ++.
- Borders and boxes:
  - Images.outline attr i builds a one-cell box frame using box-drawing runes around an image i; combine with pad for margins.
- Gradient backgrounds:
  - Continuous rainbow: Images.c_rainbow w h (I.char A.(bg (rgb_888 ...)) ' ' 1 h) and almondbread’s sin-based color mapping.
  - Gray ramps and 6x6x6 color cube via nested I.tabulate and A.bg.
- Half-block canvas:
  - pxmatrix w h f maps two vertical pixels into a single cell using A.bg (upper) and A.fg (lower) with the "▄" glyph; ideal for dense gradients and pixel art.
- Tables / grid layout:
  - grid xxs = xxs |> List.map I.hcat |> I.vcat (common); letters.ml shows chunking inputs into a rectangular grid with I.uchar cells.
- Animation patterns:
  - Lwt timed loop: common_lwt.simpleterm_lwt_timed with `Timer` events driving periodic redraws (life.ml, linear.ml).
  - Deadline-driven frame loop with Unix timers (rain.ml) for precise FPS and input interleaving.
  - Partial redraws by computing diffs and using cursor moves (inline.ml’s output_subst) to avoid full-screen redraw.
- Mouse interaction:
  - Read `Mouse` events (Press/Drag/Release, Scroll) and render overlays (mouse.ml crosshair) and direct state edits (life.ml painting cells).
- Progress bars and gauges (pattern from examples):
  - Horizontal fill bar: width-driven I.hsnap combined with I.char A.(bg color) ' ' slice.
    Example:
    Notty_unix.output_image_size @@ fun (w,_) ->
      let filled = int_of_float (ratio *. float w) in
      I.(char A.(bg green) ' ' filled 1 <|> char A.(bg black) ' ' (w - filled) 1)
  - Circular/dial-like indicators can use I.tabulate in polar coords (cf. almondbread/linear for per-cell algorithms) with colors indicating magnitude.
- Performance considerations:
  - Rendering cost scales with image complexity (# of composition/crop ops), not input image dimensions; share work structurally (avoid repeatedly rebuilding/recropping images in loops).
  - Prefer single-pass slicing (e.g., wrap2 pattern) over iterative cropping (wrap1) to avoid O(n^2) complexity.
  - Keep frequently reused primitives (glyphs, styled strings) around; constructing Unicode-heavy primitives has an upfront cost.
  - For animations, limit work per frame: compose per-column/per-row lists (rain.ml), use diffed partial updates when possible (inline.ml), and respond to resize by recomputing only what’s needed.
