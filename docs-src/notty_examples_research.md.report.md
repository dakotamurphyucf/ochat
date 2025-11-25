 # Notty examples review — gradients, gauges, grids, borders, canvas, animation, mouse

 Synthesis answer
 - Gradient backgrounds: Use I.tabulate with A.bg (and optionally A.fg) to paint per-cell colors. Common.Images provides ready-made gradients (c_gray_ramp, c_cube_ix/rgb, c_rainbow). For dense fields, pxmatrix renders two vertical pixels per cell with the half-block "▄", enabling smooth gradients and pixel art. Almondbread applies a sin-phased A.rgb_888 mapping for Mandelbrot coloring.
 - Progress bars/gauges: Build horizontal bars with I.char A.(bg color) ' ' width and compose filled/unfilled via I.hcat. Size by hsnap to the container width. For animated gauges, combine a timer loop (common_lwt.simpleterm_lwt_timed) with partial redraws (inline.ml’s output_subst) to minimize updates. Circular or heatmap-style gauges can follow the per-cell I.tabulate approach used in life/almondbread.
 - Tables/grid layout: Compose rows with I.hcat and stack via I.vcat. Reusable helpers: grid xxs (common.ml). letters.ml shows chunking a stream into nw×nh grids with I.uchar per cell and using hsnap/pad/tile for alignment.
 - Borders/boxes: Images.outline draws a 1-cell border around any image using box-drawing runes; combine with pad for margins. mouse.ml also shows crosshair overlays with I.uchar box-drawing characters.
 - Half-block/braille canvas: Provided half-block-based pxmatrix; braille-specific demo isn’t in the examples, but the same I.uchar approach applies with braille runes to achieve finer dithering.
 - Animation patterns: Two core patterns: (1) Lwt-driven timer loops (life.ml, linear.ml) with `Timer` events, resizing, and event-driven state updates; (2) deadline/FPS loop (rain.ml) using Unix.gettimeofday for precise frame pacing. inline.ml demonstrates partial redraws using cursor movement to update only changed lines.
 - Mouse interaction: mouse.ml reads `Mouse` events (Press/Drag/Release, Scroll with modifiers), renders a crosshair HUD and a scroll gauge; life.ml lets you paint cells with the mouse.

 Key image and attribute APIs observed
 - Composition/layout: I.hcat, I.vcat, I.(<|>) beside, I.(<->) above, I.(</>) overlay; I.hpad/vpad and pad ~t/l/r/b; I.hsnap/vsnap to size/align; I.hcrop/vcrop for slicing; I.tabulate for programmatic images; I.uchar for arbitrary Unicode glyphs.
 - Attributes: A.fg, A.bg for colors (system 16, A.rgb ~r/g/b 0..5, A.rgb_888 24-bit, A.gray), A.st for styles (bold, italic, underline, blink, reverse), composed via A.(x ++ y).

 Reusable idioms for the playground
 - Border any widget: Images.outline attr img |> I.pad ~t ~l ~r ~b.
 - Grid/table builder: let grid xxs = xxs |> List.map I.hcat |> I.vcat.
 - Horizontal progress bar: Notty_unix.output_image_size @@ fun (w,_) ->
   let filled = int_of_float (ratio *. float w) in
   I.(char A.(bg green) ' ' filled 1 <|> char A.(bg black) ' ' (w - filled) 1).
 - Pixel canvas: pxmatrix w h f where f x y returns colors for two stacked pixels per cell via A.bg/A.fg with "▄".
 - Distribute panels: let hdistribute ?align w imgs = let n = List.length imgs in I.(List.map (hsnap ?align (w/n)) imgs |> hcat).
 - Partial redraw: maintain previous image and use cursor moves (inline.ml output_subst) to only update changed lines.
 - Timed animation loop: use common_lwt.simpleterm_lwt_timed with `Timer` to drive redraws; or a Unix deadline loop (rain.ml) for FPS control.

 Performance considerations (from Notty docs and examples)
 - Rendering cost depends on image complexity (count of composition/crop operators), not raw image dimensions. Avoid iterative cropping that increases complexity per line; prefer single-pass slicing (wrap2 pattern in docs).
 - Reuse primitives with expensive construction (e.g., styled Unicode glyphs). Compose cheaply; defer work to rendering only once per frame.
 - Limit per-frame work: compose per-column lists (rain.ml), use hsnap/vsnap to size once, and apply partial updates when feasible (inline.ml).

 Ranked sources and why
 1) common.ml — https://github.com/pqwy/notty/blob/master/examples/common.ml
    - Core reusable building blocks: grids, borders, gradients, half-block canvas. Many idioms we can copy directly.
 2) almondbread.ml — https://github.com/pqwy/notty/blob/master/examples/almondbread.ml
    - Clear gradient technique on a half-block canvas with coordinate transforms; perfect for gradient modes.
 3) mouse.ml — https://github.com/pqwy/notty/blob/master/examples/mouse.ml
    - Complete mouse HUD example (crosshair, scroll gauge, status bar) and event handling patterns.
 4) life.ml — https://github.com/pqwy/notty/blob/master/examples/life.ml
    - Lwt-timer animation with per-cell I.tabulate rendering and mouse editing.
 5) rain.ml — https://github.com/pqwy/notty/blob/master/examples/rain.ml
    - FPS/deadline-based animation pattern and efficient per-column composition.
 6) colors.ml — https://github.com/pqwy/notty/blob/master/examples/colors.ml
    - Attribute usage across A.fg/bg/st and ready-made gradient ramps.
 7) crops.ml — https://github.com/pqwy/notty/blob/master/examples/crops.ml
    - Practical hsnap/vsnap distribution and cropping of shaped images; good for panel sizing.
 8) inline.ml — https://github.com/pqwy/notty/blob/master/examples/inline.ml
    - Partial redraw technique via cursor movement; valuable performance optimization.
 9) letters.ml — https://github.com/pqwy/notty/blob/master/examples/letters.ml
    - Grid layout with I.uchar; useful for table-like dashboards.
 10) runes.ml — https://github.com/pqwy/notty/blob/master/examples/runes.ml
     - Centering and outlining; validates alignment under complex text.
 11) Notty module docs — https://pqwy.github.io/notty/doc/notty/Notty/index.html
     - Authoritative API and performance model underpinning the examples.

 Additional insights
 - Braille canvas: While not in these examples, a braille-based canvas can be implemented analogously to pxmatrix by mapping 2×4 pixel blocks to braille runes via I.uchar and setting cell A.fg; this enables even finer vertical resolution than half-blocks.
 - Snap vs crop: Prefer hsnap/vsnap for sizing panels to a frame grid; reserve hcrop/vcrop for extracting slices of a larger composed image to avoid unnecessary complexity growth.
 - Status/HUD layering: Compose HUD lines with I.hsnap ~align:`Right and overlay crosshairs with I.(</>) to keep the scene composable.

 Reference
 - Full per-source notes and snippets: docs-src/notty_examples_research.md
