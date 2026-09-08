# Understand an unfamiliar repository

Turn a named project file into an explanation you can check against the source.

Keep project-specific instructions in a reusable agent instead of repeating them in every conversation.

## The outcome

Input: A project file and a question.

Output: An explanation with file references.

## Run the starting example

Follow the [file-tool tutorial](../tutorials/file-tool.md). Its complete bundle
contains `reader.chatmd` and the fictional Lantern project reference. Ask the
agent to explain that file and identify information the file does not contain.

## Adapt it to a real repository

Replace the sample with a file you are authorized to share with the configured
provider. Keep questions concrete and ask for file references. Add directory
listing when the agent needs to discover files; add indexed retrieval when
reading named files is no longer enough.

The bundled example reads one reference file. It does not automatically map an
entire repository. The [tools guide](../overview/tools.md) explains additional
capabilities, and [search and indexing](../guide/search-and-indexing.md) explains
Markdown retrieval and OCaml-specific code indexing.

[Explore another application](README.md) or [follow the tutorial curriculum](../tutorials/README.md).
