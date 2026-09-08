# Library prose and API documentation

This directory holds free-form Markdown files that go **beyond inline
`*.mli` comments**.  They capture design notes, usage examples, historical
decisions, and any other background that helps a human (or an indexing tool)
understand the code-base.

## Choose an integration path

Start with a subsystem guide, then follow its exact interfaces. The complete
module index below also links to repository-only internals and historical notes;
those links are not a claim of website publication or runnable examples.

| Task | Entry point | Boundary |
|---|---|---|
| Embed an agent or client | [Agent-core integration](../agent-server/embedding.md), [client library](agent_client/architecture.doc.md) | Hosts own execution; client projections do not own durable state. |
| Register an OCaml tool | [Custom tools](gpt_function.doc.md) | Registration and progress callbacks do not create authorization or output filtering. |
| Consume MCP tools | [MCP client](mcp/mcp_client.doc.md), [HTTP transport](mcp/mcp_transport_http.doc.md), [OAuth cache](oauth/oauth2_manager.doc.md) | Maintained outbound tools are separate from deprecated prompt serving. |
| Compose ChatMD files | [Source loader](chatmd/source_loader.doc.md), [imports](chatmd/chatmd_import_expansion.doc.md) | Captured source closures and relative paths have host-specific semantics. |
| Program a moderator | [Runtime guide](../guide/chatml-moderator-runtime.md), [language internals](../guide/chatml-implementation-architecture.md) | UI capabilities and automatic work limits depend on the host. |
| Manage conversation size | [Compaction](../context_compaction/compactor.doc.md), [archives](agent_session/compaction_archive.doc.md) | Summaries are lossy; an archive is not a running continuation. |
| Add retrieval | [Search setup](../guide/search-and-indexing.md) | Embedding-backed agent retrieval is separate from website search. |
| Refine prompts | [mp-refine-run](../bin/mp_refine_run.doc.md) | Local and paid strategies differ; the broad older library overview needs API reconciliation. |
| Reuse additional components | [Embedding and caching](embedding.md) | Keep identity-bearing histories and host-owned resources. |
| Maintain older file-backed sessions | [Prompt sessions](prompt_session.doc.md), [snapshot store](session_store.doc.md) | Compatibility APIs do not administer daemon sessions. |
| Extend the terminal UI | [Application hosts](chat_tui/app.doc.md), [controller](chat_tui/controller.doc.md), [display types](chat_tui/types.doc.md) | Editor state is local; native/daemon mutations go through the actor. |

Naming rules
------------

* Use the same basename as the module you’re describing plus the suffix
  `.doc.md`  –  e.g.

      vector_db.doc.md   → relates to `vector_db.ml` / `vector_db.mli`

* Longer, thematic docs are welcome; pick a concise slug such as
  `embedding_pipeline.doc.md`.

* Keep language plain Markdown; no special tooling required.

Scope
-----

These Markdown sidecars are repository documentation for users and maintainers.
They are not automatically shipped as generated odoc API pages or installed
package docs. Public `.mli` comments define exact APIs; sidecars explain design and
integration. Historical/TODO material must be labelled, not presented as current
behavior. Start at the [documentation index](../README.md) or
[agent library map](../agent-server/embedding.md#library-map).

When adding a new module, consider whether a side-car `.doc.md` would help
future readers.  If so, drop it here under the same sub-directory structure
as the source code.

## Browse library references

### Additional topic references and historical notes

These pages live outside `lib/` but cover related implementation topics. Research
and older design notes are not current command or host-parity promises; prefer
the current guides and interfaces when they differ.

- Context compaction: [configuration](../context_compaction/config.doc.md),
  [compactor](../context_compaction/compactor.doc.md),
  [relevance judge](../context_compaction/relevance_judge.doc.md).
- Prompt refinement: [recursive flow](../meta_prompting/recursive_mp.doc.md),
  [evaluator](../meta_prompting/evaluator.doc.md).
- Provider tool outputs: [response output notes](../openai_responses_tool_output.md),
  [image output support](../response_api_tool_output_image_support.md).
- TUI rendering research: [whitepaper](../chat_tui_renderer2_whitepaper.md),
  [Notty examples](../notty_examples_research.md),
  [research report](../notty_examples_research.md.report.md).
- Type-ahead tests: [synchronous UI checks](../test/chat_tui_type_ahead_test.doc.md),
  [debounce and cancellation](../test/chat_tui_type_ahead_debounce_test.doc.md).
- Older TUI link locations (forward to canonical library pages):
  [app](../chat_tui/app.doc.md), [controller](../chat_tui/controller.doc.md),
  [renderer](../chat_tui/renderer.doc.md), [model](../chat_tui/model.doc.md),
  [types](../chat_tui/types.doc.md), [grammar registry](../chat_tui/highlight_registry.doc.md),
  [grammars](../chat_tui/highlight_grammars.doc.md), [theme](../chat_tui/highlight_theme.doc.md),
  [TextMate engine](../chat_tui/highlight_tm_engine.doc.md).
- Historical source-only commands: [gpt](../bin/gpt.doc.md),
  [mp_prompt](../bin/mp_prompt.doc.md), [eio_get](../bin/eio_get.doc.md).

### Current library pages

For agent integrations start with [agent-core embedding](../agent-server/embedding.md).
For other components, use the index below. These pages mix API explanations and
historical design notes; exact signatures are in the linked current interfaces.
A link here is navigation, not a claim that every legacy example was executed.

### Core tools and retrieval

- [Io](Io.doc.md)
- [apply_patch](apply_patch.doc.md)
- [apply_patch_error](apply_patch_error.doc.md)
- [bin_prot_utils_eio](bin_prot_utils_eio.doc.md)
- [bm25](bm25.doc.md)
- [definitions](definitions.doc.md)
- [dune_describe](dune_describe.doc.md)
- [embed_service](embed_service.doc.md)
- [embedding](embedding.md)
- [environment](environment.doc.md)
- [functions](functions.doc.md)
- [github](github.doc.md)
- [gpt_function](gpt_function.doc.md)
- [indexer](indexer.doc.md)
- [jsonaf_ext](jsonaf_ext.doc.md)
- [log](log.doc.md)
- [lru_cache](lru_cache.doc.md)
- [markdown_crawler](markdown_crawler.doc.md)
- [markdown_indexer](markdown_indexer.doc.md)
- [markdown_snippet](markdown_snippet.doc.md)
- [md_index_catalog](md_index_catalog.doc.md)
- [merlin](merlin.doc.md)
- [meta_prompting](meta_prompting.doc.md)
- [mime](mime.doc.md)
- [notty_scroll_box](notty_scroll_box.doc.md)
- [ocaml_parser](ocaml_parser.doc.md)
- [odoc_crawler](odoc_crawler.doc.md)
- [odoc_indexer](odoc_indexer.doc.md)
- [odoc_snippet](odoc_snippet.doc.md)
- [package_index](package_index.doc.md)
- [parallel_tool_calls](parallel_tool_calls.doc.md)
- [prompt_session](prompt_session.doc.md)
- [session](session.doc.md)
- [session_store](session_store.doc.md)
- [source](source.doc.md)
- [template](template.doc.md)
- [tikitoken](tikitoken.doc.md)
- [ttl_lru_cache](ttl_lru_cache.doc.md)
- [vector_db](vector_db.doc.md)

### agent_client

- [agent_client/architecture](agent_client/architecture.doc.md)

### agent_protocol

- [agent_protocol/architecture](agent_protocol/architecture.doc.md)

### agent_server

- [agent_server/architecture](agent_server/architecture.doc.md)

### agent_session

- [agent_session/architecture](agent_session/architecture.doc.md)
- [agent_session/compaction_archive](agent_session/compaction_archive.doc.md)

### agent_store

- [agent_store/architecture](agent_store/architecture.doc.md)

### agent_transport_client

- [agent_transport_client/architecture](agent_transport_client/architecture.doc.md)

### agent_transport_http

- [agent_transport_http/architecture](agent_transport_http/architecture.doc.md)

### agent_transport_socket

- [agent_transport_socket/architecture](agent_transport_socket/architecture.doc.md)

### agent_transport_stdio

- [agent_transport_stdio/architecture](agent_transport_stdio/architecture.doc.md)

### chat_response

- [chat_response/agent_runtime](chat_response/agent_runtime.doc.md)
- [chat_response/cache](chat_response/cache.doc.md)
- [chat_response/chatml_moderation](chat_response/chatml_moderation.doc.md)
- [chat_response/config](chat_response/config.doc.md)
- [chat_response/converter](chat_response/converter.doc.md)
- [chat_response/ctx](chat_response/ctx.doc.md)
- [chat_response/driver](chat_response/driver.doc.md)
- [chat_response/fetch](chat_response/fetch.doc.md)
- [chat_response/fork](chat_response/fork.doc.md)
- [chat_response/history_stream_event](chat_response/history_stream_event.doc.md)
- [chat_response/in_memory_stream](chat_response/in_memory_stream.doc.md)
- [chat_response/mcp_discovery_cache](chat_response/mcp_discovery_cache.doc.md)
- [chat_response/moderation](chat_response/moderation.doc.md)
- [chat_response/response_loop](chat_response/response_loop.doc.md)
- [chat_response/tool](chat_response/tool.doc.md)

### chat_tui

- [chat_tui/agent_event_apply](chat_tui/agent_event_apply.doc.md)
- [chat_tui/agent_history_layout](chat_tui/agent_history_layout.doc.md)
- [chat_tui/agent_permission_view](chat_tui/agent_permission_view.doc.md)
- [chat_tui/agent_projection](chat_tui/agent_projection.doc.md)
- [chat_tui/agent_security_projection](chat_tui/agent_security_projection.doc.md)
- [chat_tui/agent_session_client](chat_tui/agent_session_client.doc.md)
- [chat_tui/app](chat_tui/app.doc.md)
- [chat_tui/app_compaction](chat_tui/app_compaction.doc.md)
- [chat_tui/app_events](chat_tui/app_events.doc.md)
- [chat_tui/app_reducer](chat_tui/app_reducer.doc.md)
- [chat_tui/app_runtime](chat_tui/app_runtime.doc.md)
- [chat_tui/app_stream_apply](chat_tui/app_stream_apply.doc.md)
- [chat_tui/app_streaming](chat_tui/app_streaming.doc.md)
- [chat_tui/app_submit](chat_tui/app_submit.doc.md)
- [chat_tui/cmd](chat_tui/cmd.doc.md)
- [chat_tui/connection_status](chat_tui/connection_status.doc.md)
- [chat_tui/controller](chat_tui/controller.doc.md)
- [chat_tui/controller_cmdline](chat_tui/controller_cmdline.doc.md)
- [chat_tui/controller_normal](chat_tui/controller_normal.doc.md)
- [chat_tui/controller_shared](chat_tui/controller_shared.doc.md)
- [chat_tui/controller_shell_security](chat_tui/controller_shell_security.doc.md)
- [chat_tui/controller_types](chat_tui/controller_types.doc.md)
- [chat_tui/conversation](chat_tui/conversation.doc.md)
- [chat_tui/highlight_grammars](chat_tui/highlight_grammars.doc.md)
- [chat_tui/highlight_registry](chat_tui/highlight_registry.doc.md)
- [chat_tui/highlight_styles](chat_tui/highlight_styles.doc.md)
- [chat_tui/highlight_theme](chat_tui/highlight_theme.doc.md)
- [chat_tui/highlight_tm_engine](chat_tui/highlight_tm_engine.doc.md)
- [chat_tui/highlight_tm_loader](chat_tui/highlight_tm_loader.doc.md)
- [chat_tui/markdown_fences](chat_tui/markdown_fences.doc.md)
- [chat_tui/model](chat_tui/model.doc.md)
- [chat_tui/path_completion](chat_tui/path_completion.doc.md)
- [chat_tui/persistence](chat_tui/persistence.doc.md)
- [chat_tui/renderer](chat_tui/renderer.doc.md)
- [chat_tui/renderer2](chat_tui/renderer2.doc.md)
- [chat_tui/renderer_component_history](chat_tui/renderer_component_history.doc.md)
- [chat_tui/renderer_component_input_box](chat_tui/renderer_component_input_box.doc.md)
- [chat_tui/renderer_component_message](chat_tui/renderer_component_message.doc.md)
- [chat_tui/renderer_component_status_bar](chat_tui/renderer_component_status_bar.doc.md)
- [chat_tui/renderer_highlight_engine](chat_tui/renderer_highlight_engine.doc.md)
- [chat_tui/renderer_lang](chat_tui/renderer_lang.doc.md)
- [chat_tui/renderer_page_chat](chat_tui/renderer_page_chat.doc.md)
- [chat_tui/renderer_page_shell_security](chat_tui/renderer_page_shell_security.doc.md)
- [chat_tui/renderer_pages](chat_tui/renderer_pages.doc.md)
- [chat_tui/renderer_shell_approval](chat_tui/renderer_shell_approval.doc.md)
- [chat_tui/renderer_shell_security_palette](chat_tui/renderer_shell_security_palette.doc.md)
- [chat_tui/shell_management_service](chat_tui/shell_management_service.doc.md)
- [chat_tui/shell_security_page_state](chat_tui/shell_security_page_state.doc.md)
- [chat_tui/shell_security_snapshot](chat_tui/shell_security_snapshot.doc.md)
- [chat_tui/snippet](chat_tui/snippet.doc.md)
- [chat_tui/stream](chat_tui/stream.doc.md)
- [chat_tui/type_ahead_provider](chat_tui/type_ahead_provider.doc.md)
- [chat_tui/types](chat_tui/types.doc.md)
- [chat_tui/ui_helpers](chat_tui/ui_helpers.doc.md)
- [chat_tui/util](chat_tui/util.doc.md)
- [chat_tui/utf8_edit](chat_tui/utf8_edit.doc.md)

### chatmd

- [chatmd/chatmd_ast](chatmd/chatmd_ast.doc.md)
- [chatmd/chatmd_import_expansion](chatmd/chatmd_import_expansion.doc.md)
- [chatmd/chatmd_lexer](chatmd/chatmd_lexer.doc.md)
- [chatmd/chatmd_parser](chatmd/chatmd_parser.doc.md)
- [chatmd/chatmd_script_declaration](chatmd/chatmd_script_declaration.doc.md)
- [chatmd/prompt](chatmd/prompt.doc.md)
- [chatmd/source_loader](chatmd/source_loader.doc.md)

### chatmd_shell_spec

- [chatmd_shell_spec/architecture](chatmd_shell_spec/architecture.doc.md)

### chatml

- [chatml/chatml_builtin_modules](chatml/chatml_builtin_modules.doc.md)
- [chatml/chatml_lang](chatml/chatml_lang.doc.md)
- [chatml/chatml_lexer](chatml/chatml_lexer.doc.md)
- [chatml/chatml_parser](chatml/chatml_parser.doc.md)
- [chatml/chatml_resolver](chatml/chatml_resolver.doc.md)
- [chatml/chatml_typechecker](chatml/chatml_typechecker.doc.md)
- [chatml/frame_env](chatml/frame_env.doc.md)

### context_compaction

- [context_compaction/summarizer](context_compaction/summarizer.doc.md)

### mcp

- [mcp/mcp_client](mcp/mcp_client.doc.md)
- [mcp/mcp_prompt_agent](mcp/mcp_prompt_agent.doc.md)
- [mcp/mcp_server_core](mcp/mcp_server_core.doc.md)
- [mcp/mcp_server_http](mcp/mcp_server_http.doc.md)
- [mcp/mcp_server_router](mcp/mcp_server_router.doc.md)
- [mcp/mcp_tool](mcp/mcp_tool.doc.md)
- [mcp/mcp_transport](mcp/mcp_transport.doc.md)
- [mcp/mcp_transport_http](mcp/mcp_transport_http.doc.md)
- [mcp/mcp_transport_interface](mcp/mcp_transport_interface.doc.md)
- [mcp/mcp_transport_stdio](mcp/mcp_transport_stdio.doc.md)
- [mcp/mcp_types](mcp/mcp_types.doc.md)

### meta_prompting

- [meta_prompting/aggregator](meta_prompting/aggregator.doc.md)
- [meta_prompting/context](meta_prompting/context.doc.md)
- [meta_prompting/evaluator](meta_prompting/evaluator.doc.md)
- [meta_prompting/meta_prompting](meta_prompting/meta_prompting.doc.md)
- [meta_prompting/mp_flow](meta_prompting/mp_flow.doc.md)
- [meta_prompting/preprocessor](meta_prompting/preprocessor.doc.md)
- [meta_prompting/prompt_factory](meta_prompting/prompt_factory.doc.md)
- [meta_prompting/prompt_factory_online](meta_prompting/prompt_factory_online.doc.md)
- [meta_prompting/prompt_intf](meta_prompting/prompt_intf.doc.md)
- [meta_prompting/prompting_guides](meta_prompting/prompting_guides.doc.md)
- [meta_prompting/prompts](meta_prompting/prompts.doc.md)
- [meta_prompting/recursive_mp](meta_prompting/recursive_mp.doc.md)
- [meta_prompting/task_intf](meta_prompting/task_intf.doc.md)

### notty-eio

- [notty-eio/notty_eio](notty-eio/notty_eio.doc.md)

### oauth

- [oauth/oauth2_client_credentials](oauth/oauth2_client_credentials.doc.md)
- [oauth/oauth2_client_store](oauth/oauth2_client_store.doc.md)
- [oauth/oauth2_http](oauth/oauth2_http.doc.md)
- [oauth/oauth2_manager](oauth/oauth2_manager.doc.md)
- [oauth/oauth2_pkce](oauth/oauth2_pkce.doc.md)
- [oauth/oauth2_pkce_flow](oauth/oauth2_pkce_flow.doc.md)
- [oauth/oauth2_server_client_storage](oauth/oauth2_server_client_storage.doc.md)
- [oauth/oauth2_server_routes](oauth/oauth2_server_routes.doc.md)
- [oauth/oauth2_server_storage](oauth/oauth2_server_storage.doc.md)
- [oauth/oauth2_server_types](oauth/oauth2_server_types.doc.md)
- [oauth/oauth2_types](oauth/oauth2_types.doc.md)

### openai

- [openai/completions](openai/completions.doc.md)
- [openai/embeddings](openai/embeddings.doc.md)
- [openai/responses](openai/responses.doc.md)

### shell_access

- [shell_access/architecture](shell_access/architecture.doc.md)

### shell_runtime

- [shell_runtime/architecture](shell_runtime/architecture.doc.md)

### webpage_markdown

- [webpage_markdown/driver](webpage_markdown/driver.doc.md)
- [webpage_markdown/fetch](webpage_markdown/fetch.doc.md)
- [webpage_markdown/html_to_md](webpage_markdown/html_to_md.doc.md)
- [webpage_markdown/md_render](webpage_markdown/md_render.doc.md)
- [webpage_markdown/tool](webpage_markdown/tool.doc.md)
