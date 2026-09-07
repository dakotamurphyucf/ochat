# Turn scattered notes into a research brief

Compare supplied source notes, preserve attribution, and separate evidence from open questions.

Start with supplied notes, then add web-page ingestion and indexed retrieval when your research grows.

## The outcome

Input: Two labeled research notes.

Output: A comparison and questions to investigate.

## Run the supplied-notes example

Complete [installation and provider setup](../agent-server/quickstart.md), extract
the research-brief bundle, and run from that directory:

```sh
mkdir -p runs
ochat chat-completion -prompt-file agent.chatmd -output-file runs/brief-01.chatmd
```

The prompt already includes the question. The two notes describe a fictional
team's documentation needs. A useful brief cites `reference/notes-a.txt` and
`reference/notes-b.txt`, compares the supplied evidence, and leaves unresolved
questions open. These notes do not establish which hosting provider is best.

## Add real research inputs

Replace the sample notes with material you may share with your provider.
Keep a source title, URL where applicable, and capture date alongside each note.
For supplied public URLs, declare `webpage_to_markdown` using the
[built-in tool guide](../overview/tools.md). For a growing local collection,
follow [Markdown indexing and retrieval](../guide/search-and-indexing.md).
The included agent reads local files; it does not browse or build an index.

## Check and extend the result

Open the saved ChatMD transcript and check citations against the source notes.
Do not treat a fluent synthesis as independent verification. A specialist can
review attribution or missing evidence using the [agent composition pattern](../tutorials/specialist.md).

[Explore another application](README.md) or [follow the tutorial curriculum](../tutorials/README.md).
