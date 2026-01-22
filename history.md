# History & Motivation: how Ochat started, how it evolved, and why OCaml

Ochat didn’t begin as an attempt to build a “framework.” It started as a practical response to the constraints of the earliest GPT-era workflows: I wanted a tool I could run in a terminal, with the lowest-friction interface possible, and a development loop that made model behavior *debuggable*.

### 1) Origins: a tiny GPT‑3 wrapper + a file-based terminal workflow
In the early days (GPT‑3 era), the interaction model was essentially **unstructured text in / text out**. I built a small API wrapper around GPT‑3 mainly because I wanted:

- something I could run easily from the terminal
- an **uncomplicated, file-based UI**
- updates streamed directly into a file, where the “UI” was basically just my editor viewing it

At that point, reproducibility and auditability weren’t the primary goal—they were a side-effect of the constraint: *a file is the interface*. Files were the simplest way to stay flexible, scriptable, and editor-native.

### 2) The ecosystem “awakening”: prompting techniques forced real structure
As more people worked with these models, it became clear that better results came from techniques like:

- contextual few-shot learning / better context construction
- induced reasoning patterns
- early tool-like workflows (even before official function calling)

But the models were still fundamentally **text-only**. To take advantage of these emerging techniques, I needed capabilities that went beyond a raw wrapper:

- prompt templating and composable context construction
- parsing model output to reliably extract “final answers” vs intermediate reasoning
- parsing structured tool-call-like outputs (even when they were just conventions)

At that point I realized the real challenge: if you want robust workflows, you need a real abstraction boundary between:

- **prompt/program generation** (inputs)
- **validation + error handling**
- **output parsing** (extracting tool calls, decisions, final answers)
- **repeatable orchestration patterns**

### 3) The key abstraction shift: prompts need reusable structure, not ad hoc strings
I came to believe that “prompt engineering” at scale requires:

- reusable prompt components (like functions/modules, not giant string templates)
- strong validation and error handling (parsing failures shouldn’t silently degrade)
- a disciplined integration surface for tool calls and structured outputs

In other words: this needed to feel less like “string concatenation” and more like building a small programming language runtime for agent workflows.

### 4) Why OCaml, even though Python has more training data and libraries?
It’s true that Python has vastly more ecosystem coverage and training data in LLMs. At the time, the prevailing assumption was:

- models write better code in popular languages (Python/JS) because they “know” those ecosystems
- therefore a language like OCaml would be impractical for LLM-assisted development

But I approached it from a different angle.

#### 4.1 My hypothesis: OCaml’s *constraints* make it easier for models to stay correct
OCaml has less public code available, but the code that exists tends to be extremely high quality, and more importantly, the language itself strongly encourages explicit structure:

- a strong static type system
- a powerful module system
- explicit interfaces/signatures
- far less “magic” than many dynamic ecosystems

So even if the model didn’t “know” every library, I suspected it could more reliably learn what *valid OCaml* looks like—because OCaml code is forced into a smaller, clearer space of possibilities.

#### 4.2 The key practical discovery: types + compiler errors turn the model into a convergent system
Experimentally, I found something surprising:

- if I provided **type signatures** in-context and a few valid examples,
- the model could often generate correct OCaml code *unassisted* better than it could in dynamic languages,
- and when it failed, **compiler errors** were an incredibly strong corrective signal.

This created a feedback loop that gave the model “superpowers” it didn’t have in one-shot generation:

1) generate code  
2) compile  
3) feed errors back  
4) iterate until it typechecks  
5) run tests to catch logic errors

In dynamic languages, the feedback loop is weaker: you often only get runtime errors (later), and the space of possible “valid-looking” programs is much larger. OCaml’s typechecker collapses the search space.

#### 4.3 Why OCaml was the best substrate for the kind of system I wanted
Once I realized this, OCaml became the natural choice for what I was building:

- OCaml is excellent for building parsers and robust DSLs
- the type system makes it realistic to enforce “errors are handled”
- you can model prompts as typed functions and only allow composition when types align

Conceptually, I wanted something like:

- a prompt has an input type and an output type
- prompts can only be piped together when the “shape” matches
- composition can be represented as a principled interface (monadic composition was an inspiration: only compose `a -> b` into `b -> c` when that boundary is explicit and type-checked)

Even before those ideas became first-class in the repo, they shaped the architecture: I was building toward a runtime where “prompt pipelines” could be engineered with the same rigor we apply to software.

### 5) Context became the bottleneck: vector search + code parsing were added
Over time, it became clear that **context construction** was the key to making the model effective—especially when asking it to work in OCaml with less ecosystem familiarity.

So I started adding:
- vector search / embeddings
- code parsing and indexing
- retrieval utilities to make context targeted and high-signal

The goal was always: make it easy for the model to get the *right* information (types, interfaces, examples) so it can be correct, not just plausible.

### 6) GPT‑4 and the chat/tool-calling shift: ChatMD emerges from “keep the file UI”
When GPT‑4 arrived, the “chat” format and structured tool calling became the dominant interaction pattern. That forced an architectural change: the runtime needed to represent roles, tool calls, and structured messages.

But I still wanted to preserve the original constraint that made iteration fast: **a file-based interface** that stayed simple and adaptable.

That’s when the core representational idea crystallized:

- use an explicit delimiter format inside a Markdown file
- parse it into the runtime’s structured chat representation
- keep everything editor-native and diffable

This is where the XML-like markup approach came from: using tags to delineate system/developer/user/assistant messages and tool traces, while keeping the artifact as a plain text file. Over time, that evolved into ChatMarkdown (ChatMD).

### 7) The autonomy inflection: reliable successive tool calls + reliable file editing
For a long time—even with GPT‑4—tool calling was not reliably autonomous across many consecutive steps. I was still driving the model heavily: using it like an advanced “type-ahead” system while I manually handled many workflow steps.

Two bottlenecks were especially painful:

1) **consecutive tool-call autonomy** (multi-step plans often fell apart)
2) **file editing** (updating code through ad hoc insert/remove mechanisms was slow)

Then things changed with the arrival of models that could reliably chain tool calls (in my experience, the “o1” era was the inflection). Around the same time, I discovered the “git v4 patch schema” that these models were trained to use for file manipulation.

That was the defining moment:
- I could give the model a structured patch mechanism
- it could apply multi-file edits reliably
- and I could convert reference patch parsers into OCaml quickly with the model’s help

This removed the final bottleneck to rapid iteration. Once the model could **read/write/build/test** in a tight loop, it could bootstrap large amounts of correct OCaml code—even though OCaml has far less public training data.

### 8) Bootstrapping proved the thesis: building big systems in “low-data OCaml”
With the new workflow loop (read/write/build/test + compiler feedback), I was able to build substantial components that would have been difficult to bootstrap reliably in dynamic languages at the time, including:

- **ChatML**: a custom small expression-oriented ML dialect with Hindley–Milner inference (Algorithm W) extended with row polymorphism for records/variants (a non-trivial type system, not something the model could have memorized from training)
- an HTML → Markdown converter
- a full chat TUI using a bare-bones OCaml TUI library I had never used before, including text editing features, syntax highlighting, efficient scrolling, and token streaming
- gradual expansion into MCP integration, custom tools, embeddings, retrieval, etc.

The point wasn’t “OCaml is better than Python in general.” The point was: **OCaml + tight feedback loops + explicit types creates a development environment where LLMs converge to correct code extremely reliably**, even in an ecosystem they barely “know.”

### 9) From side-effect to identity: reproducible file-native workflows
Originally, the file-based interface was just a pragmatic choice. But over time, the side-effects became the identity:

- transcripts are artifacts
- workflows are diffable and reviewable
- tools and permissions can be explicitly declared
- the runtime can be hosted in different modes without changing the artifact

That evolution is what turned “a tiny GPT‑3 wrapper” into what Ochat is now: a file-native agent/workflow runtime with a clear trajectory.

### 10) Where this leaves Ochat today
At this point, the project has a clearer identity and direction. The combination of:

- artifact-first workflows,
- explicit tool control,
- strong feedback loops (build/test/typecheck),
- and OCaml’s suitability for building robust parsers and abstractions

means I can often move faster—and more safely—than using raw API access in popular ad hoc setups, and in many cases even faster than industry-leading agent CLIs because I can always build something narrowly optimized for the task at hand.