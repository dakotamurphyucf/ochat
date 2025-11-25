Chat TUI Renderer v2 – Architecture & Algorithms (Short Whitepaper)

Overview
- Goal: make the Chat TUI renderer easier to reason about and maintain while preserving strict visual parity with the legacy renderer during the transition.
- Outcomes:
  - Clear separation of concerns (parse → highlight → wrap → paint → cache → virtualize → compose).
  - Width-dependent work is isolated and explicit.
  - Performance optimizations are localized, with crisp invariants.
  - Compatibility shims preserve parity where the old behavior was inconsistent.

Scope
- Renderer2 is a new module added alongside the existing renderer. No call sites change until we cut over.
- The public entry point mirrors the old API: render_full ~size ~model : Notty.image * (int * int).
- All state that belongs to the UI remains in Model.t (scroll offset, per-message image cache, height arrays, selection, mode, etc.).

Module map (internal to Renderer2)
- Theme: role → color/style; selection overlay (reverse video) layering.
- Roles: role helpers (is_toollike, label_of_role).
- Blocks: segmentation into Text and Code using Markdown_fences.split.
- Wrap: display-cell width-aware wrapping of styled spans into lines.
- Code_cache2: LRU image cache for code-block bodies keyed by (role_class, lang, digest, width_bucket).
- Render_context: immutable context per message render (width, selected, role, highlighter engine); computes prefix strings.
- Paint:
  - render_paragraph: markdown highlight, fallback bold detection, wrap, compose prefix+content rows.
  - render_code_block: syntax highlight per line, width-bucket cache when not selected, compose vertical prefix image + content image.
- Message: sanitize, apply developer label trimming, segment into blocks, render block-by-block with message-level first-line discipline, append one-row gap.
- Viewport: virtualized history renderer using prefix sums and binary search; manages per-message image/height caches.
- Status_bar: mode and RAW indicator.
- Input_box: bordered multi-line editor view, selection overlay, cursor coordinate computation.
- Compose: orchestrates a full frame (history viewport + status + input), returns image + cursor.

Data flow: message → image
1) Sanitize once (Util.sanitize ~strip:false). Empty/whitespace-only text yields empty image.
2) Segment text: Blocks.of_message_text → [Text s | Code {lang; code}] list.
3) For each block:
   - Text: highlight as markdown; if highlighter falls back, apply bold heuristic for **...** and __...__; wrap to width; draw prefix on the first physical row of the paragraph, indent on continuations.
   - Code: highlight per line; compute body width from message width minus prefix; cache non-selected body images at bucketed widths; draw a vertical prefix image (row0 = prefix or indent; rows 1..h-1 = indent) and compose with the body image.
4) Append a one-row gap after the message body.

Wrapping algorithm (Wrap.wrap_runs)
- Input: runs = (attr * string) list, limit = available columns.
- Steps:
  - Iterate by UTF-8 scalar length (1–4 bytes) and compute display width per scalar via Notty.I.width on a single-character image.
  - If adding the next scalar would exceed limit and the current line is not empty, flush the line and start a new one; otherwise append in-place.
  - Merge adjacent runs with identical attributes.
- Notes:
  - Uses Notty’s geometry to match its cropping behavior and terminal width heuristics.
  - Keeps behavior consistent with the legacy renderer for wide/combining glyphs and emoji.

Code-block image caching (Code_cache2)
- Key: role_class (‘T’ for toollike, ‘U’ for userlike), lang (or “-”), digest = md5(lang ^ NUL ^ code), width_bucket (size 8; rounded up).
- Entry: { last_used; img } with a global tick used as an LRU timestamp.
- Capacity: 128 entries; evict the entry with the smallest last_used when exceeding capacity.
- Selection policy: selected code blocks are not cached (render at exact width to preserve overlay accurately).

Virtualization (Viewport)
- Goal: render only the visible part of history; keep consistent scroll semantics.
- Caches in Model.t:
  - msg_img_cache[idx] → { width; text; img_unselected; height_unselected; img_selected?; height_selected? }.
  - msg_heights: heights per message.
  - height_prefix: prefix sums; length = len+1; prefix.(i+1) = prefix.(i) + msg_heights.(i).
  - dirty_height_indices: indices needing re-measurement (text mutated).
- Algorithm:
  1) ensure_arrays:
     - If lengths mismatch, rebuild msg_heights & height_prefix from scratch, caching unselected images.
     - Else, for each dirty index, re-render to get new height, compute delta, update heights.(idx) and prefix.(j) for j ≥ idx+1.
  2) Compute total_height = prefix.(len); determine scroll offset:
     - If auto_follow then scroll = max 0 (total_height - viewport_height).
     - Else clamp the scroll-box’s offset to [0, max_scroll].
  3) Binary searches on prefix to locate visible [start_idx..last_idx].
  4) For each visible index, reuse cached images when width and text match; otherwise re-render and cache. Render selected variant lazily when needed.
  5) Compose top pad, body (vcat of visible images), bottom pad.

Selection overlay semantics
- Selection is a reversible style overlay (reverse video) stacked on top of the base attributes. It must not affect geometry or height.
- Code blocks apply the overlay to each span when selected and skip the width-bucket cache.

Prefix/indent discipline
- First rendered physical row of a message has label_of_role role ^ “: ”; continuation rows (including later blocks) use spaces of equal width.
- Parity quirk preserved: for tool-like messages, a topmost code block does not flip the “first-line” flag, so a following text paragraph is prefixed again (matches legacy behavior).

Error handling & fallbacks
- safe_string wraps Notty.I.string; on error, logs and substitutes “[error: invalid input]”.
- Markdown fallback: if the highlighter falls back, treat **...** and __...__ as bold spans; otherwise draw plain.
- Narrow widths: if body width is ≤ 0, render a per-row path with the correct prefix and Notty cropping.

Performance model (summary)
- Per-frame work is O(visible_messages); dirty height updates are O(#changes) with O(n) worst-case tail updates when needed.
- Code-block LRU avoids rehighlighting/rendering for repeated widths; selection bypasses cache.
- Using Notty geometry aligns wrapping and cropping.

Testing strategy
- During transition: cross-renderer parity tests serialize full frames with Notty.Render to ensure strict equality across corpuses, sizes, capabilities, and selection. We also assert equality of msg_heights and height_prefix arrays.
- After cutover: retain Renderer2-only property tests (prefix/height invariants, scroll clamping, selection invariants) and add snapshot tests for a few Cap.dumb fixtures.

Cutover and maintenance
- Cutover: once parity is green across test suites and manual inspection, switch call sites to Renderer2 and remove the legacy renderer and its cross-parity tests.
- Maintenance knobs:
  - code-bucket size and capacity in Code_cache2.
  - per-frame highlighter engine reuse policy (currently: single engine per frame).
  - block segmentation in Blocks and fallback bold heuristic in Paint.

File references
- lib/chat_tui/renderer2.ml – implementation
- lib/chat_tui/renderer2.mli – public entry point
- lib/chat_tui/renderer.ml – legacy renderer kept during transition
- test/chat_tui_renderer_parity_test.ml – targeted cross-parity tests
- test/chat_tui_renderer_parity_fuzz_test.ml – fuzzed cross-parity tests
- test/chat_tui_renderer_heights_parity_test.ml – height/prefix array parity

Future extensions
- Snapshot suite for Renderer2 (Cap.dumb) to lock UI surfaces.
- Optional highlighter result caching keyed by (lang, code digest) to avoid re-tokenization across widths.
- Configurable theme and role labels at runtime.
- Grapheme-cluster-aware wrapping if/when Notty provides cluster width helpers.

