# Application-led website iteration

The user approved the six UI/content recommendations after the Graphite redesign. This iteration implements them without advancing P10 or reopening the P09 API deferral.

## Visitor experience

| Approved direction | Implementation |
|---|---|
| Explain useful outcomes | Homepage supporting copy names project understanding, review, research, and coordination. The hero explicitly says no OCaml is required for a first agent. Its secondary action opens applications. |
| Demonstrate an inspectable result | Homepage and documentation-review guide share a Result / Execution / Agent files viewer. The capture contains actual Ochat file-tool and specialist execution with live OpenAI output. Playback is user-initiated, pausable, and stops when the page becomes hidden. |
| Application gallery | Six canonical guides, grouped by Code, Documentation, Research, and Automation. URL-backed filters support reload/back navigation. Cards identify illustrative previews and link to inputs, outputs, setup, source, and adaptation guidance. |
| Give the design character through the work | A featured review artifact, output previews, consistent native SVG icons, restrained card states, and a clickable agent-composition diagram use the existing Graphite palette. No decorative animation or new image-generation dependency. |
| Make discovery and learning intuitive | A dedicated ten-lesson curriculum with three learning paths; outcome/setup summaries and explained next steps on every lesson; task-oriented docs entrances; visible Tutorial / Guide / Explanation / Reference / Overview search labels. |
| Explain practical differentiators | All fourteen framework capabilities now lead with a benefit and example. Their setup/limits remain accessible in disclosures. Homepage benefits connect versioned definitions, reusable specialists, and optional workflow control to practical work. |
| Improve inline source navigation | Desktop file list beside the code, compact mobile picker, collision-resistant file permalinks, initial-fragment and subsequent-fragment opening, exact copy, wrapping, and native disclosures without JavaScript. ChatML remains OCaml-highlighted. |

The selected Start here sequence remains Explore Ochat → Build and configure Ochat → Build troubleshooting → Run your first local agent. Installation and troubleshooting continue to the requested next destinations; the first lesson continues to the file-tool lesson.

## Content ownership and scope

Eight new canonical Markdown pages live in `docs-src/applications/` and `docs-src/tutorials/README.md`, with explicit manifest routes. Six applications cover repository onboarding, documentation review, supplied-note research, change review, headless reports, and host-owned background workflows. The background guide uses the existing complete timer source as its reproducible starting point and explicitly explains that background research is an extension requiring a configured job recipe and completion handling; the timer does not perform research.

Four new complete source bundles add a documentation-review pair, supplied-note researcher, patch reviewer, and headless report agent. All inputs are synthetic public examples. The catalog now has fourteen entries: eight complete examples, five configured templates, and one illustration. Thirteen plain `.tar` bundles and 39 individual destinations represent 27 unique source files. The existing extensionless `dune.txt` URL and archive/download filename behavior remain intact.

The three non-recorded new bundles pass the actual captured-loader parse/closure checks; they are labeled checked offline, not live-verified output. Other application previews are explicitly illustrative. Technical instructions and limitations remain canonical Markdown; presentation metadata owns discovery summaries. The importer validates application/example/tutorial relationships before publication.

## Recording and provenance

The flagship starts from `docs-src/examples/applications/docs-review/explorer.chatmd`. It reads `reference/project.txt`, calls `review_docs` using `docs-reviewer.chatmd`, and returns a report. The specialist has no file, editing, or shell tools. The capture includes four model requests against OpenAI `gpt-4.1`, and the recorded response identifies a resolved model version. This is a synthetic-document demonstration, not a quality benchmark or a claim of general correctness.

`website/scripts/record-showcase.py` reproduces real runtime execution against a deterministic local provider by default. Its explicit `--live` option forwards at most six bounded requests to OpenAI. The capture proxy converts complete responses into stream events and normalizes response-envelope metadata for the checkout's decoder; model output items are preserved. Initial capture attempts exposed adapter problems; those incomplete captures were not published. A separate direct CLI run with the same source bundle against OpenAI, without the proxy, also completed successfully.

`recording.json` keeps captured requests/responses, the displayed final result, source hashes, runtime-source hashes, revision, timestamp, redaction description, and transport scope. The importer rejects mismatched source/runtime hashes, changed final output, missing actual file-tool output, a specialist with tools, or inconsistent provider labeling. The downloadable `recorded-run.chatmd` preserves the redacted original transcript. Its Markdown hard breaks and indentation are intentional, so `.gitattributes` disables only end-of-line whitespace warnings for that single captured file.

No authentication headers or configured provider key are included. Private temporary directory paths are replaced by `<example-directory>`. The viewer exposes provenance and reproduction details. Playback timing is illustrative and does not claim to reproduce measured execution latency. A rerun can produce different model output. Recording generation is explicit and is never part of the website build or CI model execution.

## Validation

Current evidence is in `scratch/ochat-applications-evidence/`; logs use `scratch/applications-` and `scratch/showcase-` prefixes.

- `npm run check`: 64 passing tests and zero Astro errors, warnings, or hints.
- Offline Dune documentation gate: 308 pages and 38 methods pass, including actual source-loader checks for the new bundles.
- Full Chromium / Firefox / WebKit matrix: 208 passes and two existing non-Chromium clipboard skips, in 3.9 minutes with two workers. This run uses artifact `5dd90dd28524767b61bb52348ebaa662f934e7023b9dc426b135cee69eecba5a`, preserved as `full-matrix-build.json`.
- Initial expanded performance sampling caught a gallery layout shift of 0.142 while JavaScript revealed its filters. The final implementation reserves the controls’ actual wrapped dimensions before enhancement, with a native no-JavaScript fallback. Final affected browser, visual, search, and performance results are recorded below against the rebuilt artifact.
- Expanded catalog/search fixtures were updated to fourteen examples and 21 unique benchmark queries. Earlier exploratory failures are retained in local logs; only completed passing runs are counted as acceptance evidence.
- All 115 previously rendered canonical source hashes and all ten previous example records remain unchanged. Both specification snapshots retain the same 122 task states. Formatting and Git whitespace checks pass; the main-checkout HEAD and empty staged diff remain unchanged.

Final rebuilt artifact: 125 HTML pages, 575 files, 22,233,113 bytes; SHA-256 `f2c86e4f79f83308777d3ba5960e260db4b5e4b816f428d7b7a076998f93db56`. The final affected application suite passes all 18 cases across Chromium, Firefox, and WebKit (17.7 seconds). The seven-route throttled performance gate passes; the gallery’s sampled layout shift fell to 0.079. These are diagnostic local samples, not hosted Web Vitals. The homepage loads approximately 66 KB initially under the report’s compressed-resource accounting. Search passes all 21 benchmark queries across 114 indexed pages and 1,860 checked anchors. The final visual audit passes 16 page/theme/viewport combinations with no document overflow or automated WCAG violations, plus four desktop/mobile and light/dark home source-link checks. It captures 24 screenshots, including file and execution views. Desktop gallery, dark desktop source reader, dark mobile curriculum, and light mobile execution screenshots were inspected. The source-link checks include execution-to-file navigation, reload, and specialist selection. The isolated local production fixture passes: 125 HTML pages, 577 files, and 111 sitemap URLs at the synthetic `https://docs.ochat.test` origin. Its artifact SHA-256 is `d439f0ad6f44decbb73bb2845069f9ed9a11e08a8812fc43b4f907347255a345`. Evidence is in `scratch/ochat-website-evidence/applications-production/`. This verifies production output policy in a temporary committed copy with installed dependencies; it does not verify a public domain, HTTPS serving, immutable public source links, or an actual deployment.

## Release boundary

This completes the application/content UI scope with the validation recorded above. P10 remains the next phase for release-candidate review, manual assistive-technology and device sampling, hosted rehearsal, rollback, and release enforcement. No main-checkout commit, public deployment, domain change, or GitHub settings change is performed. Keep the persistent implementation notes and both 122-task specification snapshots synchronized.
