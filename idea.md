individual agents for ocaml files
    - answer questions about module
        - summerize
        - query availible functions/types/values
    - make updates to the module
        - comment
        - edit
        - extend
    

individual agents for dune files
    - answer questions
        -  get dependency graph of libs declared in the file
        - details aboutthe contents of the file
    - make updates
        - edit stanzas i.e add/remove dependencies, ect
        - extend
    

individual agent for dune-project and opam file
    - answer questions
    - make updates

individual agent for opam and dune questions / commands
    - how to run commands
    - how to install
    - how to pin


agent for orchistrating agent workflows between the agents above. i.e 
    - this one will require some thinking. because it will be quite combersome to have to have multiple conversations for each agent. meaning i want a query category agent that detenines which agent the question should go to and then foward the question to agent, but now how do I keep a continued conversation with that agent but then switch to tohe routing agent when the query is no longer relevant to the current agent. possiblle solution is to have a recursive routing in each agent and instruct the agent to route the query to a different agent based on the classiification of the request.

functionality for updating the context of the agents
    - should have a command for proccessing a repo to generate the context for prompts that answer repo wide questions



use dune describe to get dependency info/lib info/ file location/ ect
    - use root to get path/ use build context to get build patth to replace path in output so we can get the true location

create an agent whos whole purpose is to gather relavant context for queries based on 


answer questions abouy ocaml file as well as generate code for that file
 - needs context for libs availible in scope, ability to edit/insert/add code to file, 
   able to ask questions about relevant functions in modules dependencies in order 
   to get contect to answer user query. suggest adding functions to dependencies in order to anser a query

answer questions about dune files as well as editing that file

answer questions about whole project


@ocaml /module 




markdown features:
────────────────────────────────────────────────────────────────────────
“Template” DSL for Reusable Message Structures
────────────────────────────────────────────────────────────────────────
• Purpose: Let users define “templates” for common tasks (e.g., bug fix request template, code generation template).  
• Implementation Detail:
  – Define a <template id="genCodeSnippet">  
        <msg role="system">You are a coding assistant ...</msg>  
        <msg role="assistant">Describe your code generation needs.</msg>  
        <!-- Possibly more structure here -->  
    </template>  
  – To create a new conversation or a new section, a user can do: <use_template ref="genCodeSnippet"/>. The application will replicate all <msg> blocks from genCodeSnippet into the conversation.  

────────────────────────────────────────────────────────────────────
Macro
────────────────────────────────────────────────────────────────────
• <macro> Definitions:
  – Define a <macro id="..."> that captures a frequently used prompt pattern.  
  – Support placeholders within the macro that can be replaced with dynamic text.  
  – Example usage:
    <macro id="bugReport">
      I want you to act as a QA engineer. The following bug was discovered in the code:
      {{BUG_DESCRIPTION}}
      Please analyze the reasons and potential fixes in detail.
    </macro>

• <usemacro> Element for Instantiation:
  – Let the user instantiate a macro in a <msg>:
    <msg role="user">
      <usemacro ref="bugReport">
        <var name="BUG_DESCRIPTION">Stack overflow when calling the data import API with zero-length input</var>
      </usemacro>
    </msg>
  – The system would resolve the {{BUG_DESCRIPTION}} placeholder, merging it into the final message content automatically.  
────────────────────────────────────────────────────────────────────────
Context‐Building “Composer” Elements
────────────────────────────────────────────────────────────────────────
• Purpose: Let advanced users compose large prompts from multiple partial segments or from multiple documents without manually re‐copying.  
• Implementation Detail:
  – Introduce a <compose> element, which references multiple <doc> or <msg> segments. For instance:  
    <compose id="featurePrompt">  
      <include ref="requirementsDoc"/>  
      <include ref="previousUserMsg"/>  
      <section>My custom text or additional instructions.</section>  
    </compose>  
  – A single <compose> can gather text from multiple references to produce a final text output. Then a <msg role="user" compose="featurePrompt"/> can automatically expand the composed text into one user message.  

--------------------------------------------------------------------------------
Cached Content for Reusability & Efficiency
--------------------------------------------------------------------------------
• Rationale  
  – Large documents or images may be processed repeatedly (e.g. embeddings, summarization).  
  – Caching prevents unnecessary re‐fetching or re‐processing, saving tokens and time.  

• Proposal  
  – Introduce a local on‐disk or in‐memory cache that stores:  
    1) Downloaded documents and images (by URL).  
    2) Any derived “dynamic content,” such as vector embeddings or function call results from prior steps.  
  – Provide an attribute like cache="true" on <doc> or <img> or a more general <cache> element to specify caching logic.  
  – Example:  
      <doc src="/Users/alex/chatproj/files/example.md" cache="true" local="example-md-cached" />
    This indicates that the file is fetched once, then stored locally under example-md-cached (or some hashed file name).  

• Implementation Detail  
  – The application, before sending the conversation to the AI, checks if it has a local copy of the resource and if that copy is up to date.  
  – If cache="true", skip re‐downloading and re‐embedding on subsequent runs.  
  – For function calls that are especially expensive (like a vector search), store the results in a small JSON DB keyed by the function arguments (e.g. same query + same folder path → same result). 
--------------------------------------------------------------------------------
Helper Scripts / Agents
--------------------------------------------------------------------------------
• Rationale  
  – Sometimes you need specialized scripts or sub‐agents. For example, a code‐linting agent, a translation agent, or a summarizer agent.  
  – Let the user quickly call them within the same conversation workflow.  

• Proposal  
  – Extend the notion of <tool> to “agents.” Agents are specialized GPT or LLM sessions with specific prompts or roles. You can define them once, then route calls through them.  
  – Example:
      <agent name="markdownFormatter" system="You are an expert at formatting text in markdown...">
         <msg role="user">Format the following text as markdown: ...</msg>
      </agent>

    Within your main conversation, you can say:
      <msg role="user">
        <callAgent agent="markdownFormatter">Here is some text that needs markdown formatting</callAgent>
      </msg>

• Implementation Detail  
  – The application, upon encountering <callAgent agent="markdownFormatter">, spawns a new mini conversation with the agent’s system prompt and user text, then returns the result.  
  – The returned result can be appended as a standard <msg role="assistant">.  

--------------------------------------------------------------------------------
Multi‐File Conversation Linking
--------------------------------------------------------------------------------
• Rationale  
  – Some advanced users might want to split a single conversation across multiple files (or link in “sub‐conversations” for complex workflows).  

• Proposal  
  – Create a <msg role="system" include="filename.xml" /> attribute or a new <import file="..." /> element that pulls in messages from another file.  
  – This allows referencing existing conversation context or specialized templates from a library.  

• Implementation Detail  
  – The application, upon encountering <import file="..."/> or <msg role="system" include="..."/>, opens the target file, extracts certain messages, and merges them into the current conversation context.  
  – Possibly specify which message range or ID to import. Example: include only the last 5 messages from the external conversation.  

--------------------------------------------------------------------------------
“Meta” or “Pipeline” Messages for Composing Sub‐Tasks
--------------------------------------------------------------------------------
• Rationale  
  – Often you want the AI to perform multiple steps (e.g. pull data from a knowledge base, summarize, then refine).  
  – Instead of the user manually copy‐pasting partial results, you can define a meta or pipeline approach.  

• Proposal  
  – Add a specialized <msg role="meta"> or <pipeline> element that outlines a sequence of sub‐tasks or function calls. The application orchestrates them behind the scenes.  
  – Example:
      <pipeline name="summarizeAndRefine">
         <subtask function="searchVectorDB" args="query='my topic',folderPath='./db'" />
         <subtask function="summarize" inputFrom="previous" />
         <subtask function="transliterate" inputFrom="previous" language="French" />
      </pipeline>
    The application calls each subtask function in order, passing the output to the next. Finally, the pipeline results are appended as a new <msg role="assistant" />.  

• Implementation Detail  
  – The pipeline element can be read at runtime. The application handles the chain of function calls, storing each subtask’s result.  
  – inputFrom="previous" indicates that the argument is the output from the immediately preceding subtask.  
  – Possibly specify error handling, e.g. on failure of a subtask, halt or skip.  

────────────────────────────────────────────────────
Inline References to Documents and Images
────────────────────────────────────────────────────
• Expanded <doc> and <img> Syntax:
  – Currently you have <doc src="..." />. Add optional attributes or child elements that allow specifying:
    • how it should be injected (inline vs. appended),
    • optional transformations (e.g., summarization, snippet extraction, etc.) if you want to hook it into a “pre-processing” feature.

• Automatic Summaries of Large Documents:
  – If a <doc> is very large, the application can automatically chunk it and request a summary from the model (or another summarizing function) before injecting it into the conversation.  
  – Example usage in XML: <doc src="long_file.txt" summarize="true" chunk_size="2000" join="true" />  
    – summarize="true" signals the system to attempt an LLM-based summary.  
    – chunk_size="2000" indicates reading 2000-token chunks.  
    – join="true" indicates the final summarized chunks should be joined into a single summary message.

Implementation Detail:
• Pre-processing pipeline that checks each <doc>:
  – If summarize="true", read the file contents in chunks, send them to a summarizing function, and combine them into a single summarized string.  
  – Replace the <doc> or augment the conversation with that summarized text so it doesn’t exceed token limits or clutter the conversation.
