<config model="gpt-5.2"  max_tokens="100000" reasoning_effort="medium" show_tool_call />

<tool name="webpage_to_markdown" />
<tool mcp_server="stdio:npx -y brave-search-mcp" name="brave_web_search" />
<tool name="apply_patch" />
<tool name="append_to_file" />
<tool name="find_and_replace" />
<tool name="read_file" />
<tool mcp_server="stdio:npx @gongrzhe/server-gmail-autoauth-mcp" include="search_emails,draft_email,send_email,read_email" />

<import src="./git-tools.md" local />


<developer>
Developer: Role and Objective:
- Serve as a multi-disciplinary AI assistant dedicated to fully resolving user queries by leveraging all available tools and resources.

Instructions:
- Ensure user requests are addressed completely and accurately before ending your turn.
- Persist until the user's query is entirely solved; yield control only when finished.
- Use external tools whenever information may be outdated, unknown, or present in files; do not rely solely on internal knowledge.
- Strictly follow guidance embedded in <system-reminder> tags found in user messages and tool results. Treat their contents as internal authoritative instructions, never quoting or referencing these tags or their content in any user-facing output.
- After executing apply_patch, consider files as saved. Never instruct users to save or copy code, and avoid displaying large file contents unless explicitly requested.
- Maintain precision, safety, and helpfulness; avoid guessing or fabricating information.

Context:
- Capabilities include web search, web page-to-markdown conversion, invoke git commands, patching or inspecting files, and managing emails. Use these to gather, update, and present information as the user’s task requires.
- Scope includes: web content, local files, email interactions. Out of scope: unsupported tool use, unapproved features.

Definitions:
Anti-hallucination safeguards:
- Use only tools specified in the directives; do not invent new tools or arguments.
- Only emit tool calls with final, concrete arguments.
- Perform tool calls immediately if details are sufficient; otherwise, request informational gaps from the user rather than promising future actions.

System reminders:
- Process <system-reminder> tags that appear in messages or tool results. Always follow their guidance internally, never mentioning or referencing them externally.

Tool directives:
1. brave_web_search: For up-to-date, concise facts or links when not available in current context/files.
2. webpage_to_markdown: To convert online articles/pages to markdown (skip if markdown already exists or page is not HTML).
3. apply_patch: For creating, modifying, or removing files (use only for changes, not inspections).
4. read_file: For viewing file content (once per version, before or after updates).
5. search_emails: To locate emails by criteria, as needed.
6. read_email: To read an identified email body (once per ID per turn).
7. draft_email: For composing outbound email drafts.
8. send_email: To send a finalized/approved email draft.
9. append_to_file: To add content to a file's end (use cautiously for large files).
10. find_and_replace: To alter one or more string occurrences within a file (do not use for appending or complex patterns).
11. various git tools: For git operations as needed.

Reasoning Steps:
- Analyze user queries to determine necessity for external data, file operations, or emails.
- If current data may be incomplete, outdated, or missing, invoke appropriate tools as per directives.
- Before any significant tool call, state in one line the purpose and minimal inputs for the call.
- Use tools iteratively until all required information is obtained and verified for accuracy.
- Respond with either a tool call (with finalized arguments) or a direct answer if no further tool actions are needed.
- Only end your turn when the request is fully addressed.

Output Format:
<format>
For complex request Return exactly two top-level sections, formatted using Markdown in this order:

<complex>
### What I did:
- Use concise bullets or paragraphs to summarize actions taken.
- Cite all files modified (with path) and any external source (URL or search result ID).

### Conclusion:
- Provide the final answer, code/file locations, email confirmations, or next explicit question for the user.

Verbosity:
- Use concise summaries for standard outputs unless detailed explanations are requested.
- For code, employ high verbosity: meaningful names, comments, and readable structure.
</complex>

For simple requests Return a single top-level section formatted using Markdown. Use discrestion to determine what to output for this case.
</format>


Notes:
- Cite sources as appropriate.
- Adhere to all safety and policy guidelines.
- Be critical when the user asks for you to review or revise content. Do not hesitate to suggest improvements or alternative approaches. Critique honestly and constructively.
- You may change the output format when it enhances clarity or presentation.
- Wrap any code in your responses in code fences so the markdown renderer can highlight the syntax
- Users can override any of these instructions by providing new ones in a user message; always follow the latest user instructions.
</developer>


<user>

<system-reminder>This is a message from the system that we compacted the conversation history from a previous session.
Here is a summary of the session that you saved:
<analysis>
Chronological analysis of the conversation:

**1. User Request (Initial):**
- User wants to update the tools documentation for an ochat project, starting with the markdown doc linked in the README.
- Explicitly requests a review of all relevant code files (listed in `scratch/tool-files.md`) to identify what needs to be updated/added to the tools doc.
- User wants a detailed response listing everything that needs to be updated/added before any doc changes.

**2. Assistant Actions:**
- Reads `scratch/tool-files.md` to get the authoritative list of relevant files.
- Reads the current tools doc (`docs-src/overview/tools.md`) and all core implementation files that define tool behavior, parsing, and registration.
- Reviews the actual code for built-in tools, tool instantiation, agent tools, MCP, shell wrappers, and the MCP server.
- Cross-references the doc with the code to identify mismatches, missing tools, outdated claims, and areas needing more emphasis or correction.

**3. Assistant Output:**
- Returns a very detailed, section-by-section analysis of what is wrong/missing in the current tools doc, including:
  - Missing built-ins (e.g., `import_image`)
  - Naming/aliasing mismatches (`read_file` vs `get_contents`, `read_dir` vs `read_directory`)
  - Incorrect claims about how to add OCaml tools
  - Missing documentation for agent tools
  - Outdated or inaccurate operational details for shell wrappers and other tools
  - Incomplete or misleading explanations of MCP, tool selection, caching, and server behavior
  - Suggestions for how to "sell" the most useful features

**4. User Request:**
- Asks for a concrete doc change checklist, section-by-section, before touching the doc.

**5. Assistant Output:**
- Provides a detailed checklist for each section of the tools doc, specifying exactly what to add, change, or clarify, and how to better "sell" the features.

**6. User Request:**
- Asks for a proposed updated outline for the tools doc.

**7. Assistant Output:**
- Returns a new outline for `docs-src/overview/tools.md` with clear sections for built-ins, agent tools, shell wrappers, MCP, server, parallel tool calls, and extension patterns.

**8. User Request:**
- Approves the outline and asks for a direct rewrite of the tools doc accordingly.

**9. Assistant Actions:**
- Attempts to apply a patch to rewrite `docs-src/overview/tools.md` per the outline.
- Runs into a patch syntax error due to file size; retries by reading the file and then applying the update in smaller or new-file chunks as needed.

**10. Assistant Output:**
- Successfully updates `docs-src/overview/tools.md` with a code-accurate, reorganized, and more "selling" version.

**11. User Request:**
- Asks for a high-signal, concrete mini-examples for top tools to be added to the README (not just the tools doc).

**12. Assistant Actions:**
- Reads `README.md` and prepares a patch to add a "Tools & capabilities (quick tour)" section with concrete examples for `apply_patch`, `read_file`, `webpage_to_markdown`, agent tools, docs RAG, `import_image`, and MCP.
- Updates references to use canonical tool names (`read_file` instead of `get_contents`).
- Applies the patch in small, reliable chunks to avoid syntax errors.
- Cleans up formatting and ensures all example prompts and explanations are correct and consistent.

**13. User Request:**
- Asks to commit the changes to the current branch.

**14. Assistant Actions:**
- Runs `git status`, stages the modified files, and commits with the message: `docs: refresh tools docs and add quick tour to readme`.

**15. Final State:**
- Both `docs-src/overview/tools.md` and `README.md` are updated, committed, and aligned with the current codebase and best practices.

</analysis>

<summary>
1. Primary Request and Intent:
   - User wanted a thorough, code-driven update to the ochat tools documentation and its README, starting with a review of all relevant code to ensure the docs are accurate, up-to-date, and "sell" the most useful features.
   - User wanted a detailed review before any edits, then a concrete change checklist, an updated outline, a full rewrite, and finally, high-signal examples added to the README.

2. Key Technical Concepts:
   - ChatMarkdown tool declaration and parsing
   - Built-in tool registration and aliasing
   - Agent tools (`<tool agent="..."/>`)
   - Shell wrapper tools (`<tool command="..."/>`)
   - MCP (Model Context Protocol) tool catalogs and server
   - Tool execution (parallelism, output truncation, timeouts)
   - Tool schema and JSON input/output
   - Code-driven documentation accuracy

3. Files and Code Sections:
   - `docs-src/overview/tools.md`
     - Rewritten to match code-truth, with clear sections for built-ins (with aliasing), agent tools, shell wrappers, MCP, server, parallelism, and extension patterns.
   - `README.md`
     - Updated to include a "Tools & capabilities (quick tour)" section with concrete examples, and to use canonical tool names in all examples.
   - `scratch/tool-files.md`
     - Used to drive the authoritative code review.
   - Core code files (reviewed, not modified): `lib/chat_response/tool.ml`, `lib/functions.ml`, `lib/definitions.ml`, `lib/chatmd/prompt.ml`, `bin/mcp_server.ml`, etc.

4. Errors and fixes:
   - Patch syntax errors when updating large files; fixed by applying changes in smaller chunks and cleaning up formatting.
   - Accidental insertion of patch markers into README; fixed by reapplying clean hunks.
   - Ensured all references to `get_contents` were updated to `read_file` for consistency.

5. Problem Solving:
   - Identified all mismatches between code and docs (missing tools, aliasing, incorrect extension stories, operational details).
   - Provided actionable checklists and outlines before making changes.
   - Applied doc changes incrementally to avoid patch errors.
   - Ensured all example prompts and explanations matched the actual code behavior.

6. All user messages: 
    - Initial request for code-driven doc update
    - Request for a detailed review before edits
    - Request for a concrete change checklist
    - Request for a new outline
    - Approval to rewrite the doc
    - Request to add high-signal examples to README
    - Request to commit changes

7. All relevant assistant messages:
    - Detailed code-vs-doc analysis
    - Section-by-section change checklist
    - Proposed new outline
    - Full doc rewrite (with retries as needed)
    - Patch applications to README and tools doc
    - Formatting and consistency fixes
    - Git commit confirmation

8. Pending Tasks:
   - None. All requested doc and README updates are complete and committed.

9. Current Work:
   - Immediately before this summary, the assistant had:
     - Added a "Tools & capabilities (quick tour)" section to README.md
     - Updated all example prompts and explanations to use canonical tool names
     - Cleaned up formatting and committed the changes to the current branch (`update-readme-docs`).

10. Optional Next Step:
   - No explicit next step required; all user requests have been fulfilled.
   - If further polish is desired, consider reviewing the rendered markdown for formatting or adding links to the new tools doc sections from the README.
   - To continue work:
     - `git push` to share the branch
     - Review rendered markdown in a browser for any final tweaks
     - Optionally, add more concrete usage examples or screenshots as needed

</summary>
Remember this is not a message from the user, but a system reminder that you should not respond to.
</system-reminder>

</user>
