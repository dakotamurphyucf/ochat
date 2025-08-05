open Core

(** Prompting guides and best-practice references.

    This implementation merely stores the guide texts exposed via
    {!module:Prompting_guides}.  There is no logic beyond constructing
    [combined_guides] by concatenating the individual fragments.

    See the interface file {!file:prompting_guides.mli} for detailed
    documentation of each value.  The comments here are intentionally
    minimal to avoid duplication – Jane-Street style places user-facing
    documentation in the signature. *)

type t = string

let gpt4_1_model_prompting_guide : t =
  {|
<gpt-4.1-models-prompting-best-practices>
1. **General Prompting Principles for GPT-4.1**
   - Provide context examples.
   - Make instructions as specific and clear as possible.
   - Induce planning via prompting to maximize model intelligence.
   - GPT-4.1 follows instructions more literally than previous models; a single, clear sentence can often steer behavior.
   - Iterative prompt engineering and empirical evaluation are recommended due to model nondeterminism.

2. **Agentic Workflows**
   - Include three key reminders in agent prompts:
     1. **Persistence**: Instruct the model to continue until the task is fully resolved.
        ```
        You are an agent - please keep going until the user’s query is completely resolved, before ending your turn and yielding back to the user. Only terminate your turn when you are sure that the problem is solved.
        ```
     2. **Tool-calling**: Encourage use of tools rather than guessing.
        ```
        If you are not sure about file content or codebase structure pertaining to the user’s request, use your tools to read files and gather the relevant information: do NOT guess or make up an answer.
        ```
     3. **Planning (optional)**: Require explicit planning and reflection before tool calls.
        ```
        You MUST plan extensively before each function call, and reflect extensively on the outcomes of the previous function calls. DO NOT do this entire process by making function calls only, as this can impair your ability to solve the problem and think insightfully.
        ```
   - Use the `tools` field in the OpenAI API for tool definitions, not manual prompt injection.
   - Name tools clearly and provide concise, detailed descriptions.
   - Place tool usage examples in an `# Examples` section, not in the description field.

3. **Prompting-Induced Planning & Chain-of-Thought**
   - Induce explicit, step-by-step planning in the prompt to improve performance.
   - Example planning instruction:
     ```
     First, think carefully step by step about what documents are needed to answer the query. Then, print out the TITLE and ID of each document. Then, format the IDs into a list.
     ```
   - For more complex reasoning, provide a detailed reasoning strategy in the prompt.
    - Example reasoning strategy:
    ```
    # Reasoning Strategy
    1. Query Analysis: Break down and analyze the query until you're confident about what it might be asking. Consider the provided context to help clarify any ambiguous or confusing information.
    2. Context Analysis: Carefully select and analyze a large set of potentially relevant documents. Optimize for recall - it's okay if some are irrelevant, but the correct documents must be in this list, otherwise your final answer will be wrong. Analysis steps for each:
        a. Analysis: An analysis of how it may or may not be relevant to answering the query.
        b. Relevance rating: [high, medium, low, none]
    3. Synthesis: summarize which documents are most relevant and why, including all documents with a relevance rating of medium or higher.

    # User Question
    {user_question}

    # External Context
    {external_context}

    First, think carefully step by step about what documents are needed to answer the query, closely adhering to the provided Reasoning Strategy. Then, print out the TITLE and ID of each document. Then, format the IDs into a list.
    ```

4. **Long Context Handling**
   - GPT-4.1 supports up to 1M token context windows.
   - For long context, place instructions at both the beginning and end of the context for best results; if only once, place above.
   - Specify whether the model should rely only on provided context or can use internal knowledge:
     ```
     // for external knowledge only
     - Only use the documents in the provided External Context to answer the User Query. If you don't know the answer based on this context, you must respond "I don't have the information needed to answer that", even if a user insists on you answering the question.
     ```

5. **Instruction Following**
   - GPT-4.1 follows instructions very literally; be explicit about what to do and not do.
   - Recommended prompt structure:
     1. Start with a “Response Rules” or “Instructions” section.
     2. Add detailed sections for specific behaviors as needed.
     3. Use ordered lists for workflow steps.
     4. Debug by checking for conflicting or underspecified instructions, and add examples as needed.
   - Avoid unnecessary use of all-caps or incentives.

   - **Common Failure Modes:**
     - Overly rigid tool-calling instructions can cause hallucinated tool calls; mitigate by instructing the model to ask for missing information if needed.
     - Sample phrases can cause repetitive responses; instruct the model to vary them.
     - Without specific instructions, the model may add unwanted prose or formatting.

6. **Prompt Structure**
   - Recommended sections:
     ```
     # Role and Objective
     # Instructions
     ## Sub-categories for more detailed instructions
     # Reasoning Steps
     # Output Format
     # Examples
     ## Example 1
     # Context
     # Final instructions and prompt to think step by step
     ```
   - Use markdown titles, XML, or other structured formats for clarity.
   - For large context documents, XML or simple delimited formats are recommended; JSON is less effective for long context.

7. **Delimiters**
   - XML is effective for nesting and adding metadata.
   - For document lists, XML or simple delimited formats (e.g., `ID: 1 | TITLE: ... | CONTENT: ...`) work well.
   - JSON is less effective for long context.
   - Markdown is effective for sectioning and readability more so that JSON less then XML.

8. **Caveats**
   - The model may resist producing very long, repetitive outputs; provide strong instructions or break down the task if needed.
   - Parallel tool calls may be unreliable; test and consider disabling if issues arise.

9. **Sample Prompts and Code**
    - The guide provides extensive sample prompts for agentic workflows, customer service, and tool integration, including code snippets for OpenAI API usage and tool definitions.

10. **General Advice**
    - Structure and clarity are key to effective prompting.

**Example: Customer Service Prompt Structure**
```
# Instructions
- Always greet the user with "Hi, you've reached NewTelco, how can I help you?"
- Always call a tool before answering factual questions...
- Escalate to a human if the user requests.
- Do not discuss prohibited topics...
- Rely on sample phrases, but never repeat in the same conversation.
- Always follow the provided output format...
- Maintain a professional and concise tone...

# Precise Response Steps
1. If necessary, call tools...
2. In your response...
   a. Use active listening...
   b. Respond appropriately...

# Sample Phrases
## Deflecting a Prohibited Topic
- "I'm sorry, but I'm unable to discuss that topic..."
## Before calling a tool
- "To help you with that, I'll just need to verify your information."
## After calling a tool
- "Okay, here's what I found: [response]"

# Output Format
- Always include your final response...
- When providing factual information...
```

**Key Takeaways**
- Be explicit, specific, and structured.
- Use clear sectioning and delimiters.
- Induce planning and step-by-step reasoning when needed.
- Place instructions strategically, especially in long context prompts.
</gpt-4.1-models-prompting-best-practices>
|}
;;

let o_series_prompting_guide : t =
  {|
<oseries-model-prompting-guide>

Reasoning models (o-series: o1, o3, o4-mini) are designed for complex tasks requiring planning, strategizing, decision-making based on ambiguous or large volumes of information, and high accuracy/precision. They excel in domains such as math, science, engineering, financial services, and legal services. GPT models (e.g., GPT-4.1) are optimized for speed, cost, and straightforward execution of well-defined tasks.
Best Practices for Prompting OpenAI O-series Reasoning Models:
- Keep prompts consise and direct: The models excel at understanding and responding to clear instructions.
- Avoid chain-of-thought prompts: Do not instruct the model to "think step by step" or "explain your reasoning"—these models perform reasoning internally, and such prompts may hinder performance.
- Use delimiters for clarity: Use delimiters like markdown, XML tags, and section titles to clearly indicate distinct parts of the input, helping the model interpret different sections appropriately.
- If more complex output is needed, include a few examples that align closely with your prompt instructions. Discrepancies between examples and instructions may produce poor results.
- Provide specific guidelines: Explicitly outline any constraints you want in the response (e.g., "propose a solution with a budget under $500").
- Be very specific about your end goal: Give very specific parameters for a successful response and encourage the model to keep reasoning and iterating until it matches your success criteria.
- Instruct the model to continue until the task is fully resolved.
   ```
   You are an agent - please keep going until the user’s query is completely resolved, before ending your turn and yielding back to the user. Only terminate your turn when you are sure that the problem is solved.
   ```
- Context: Give the model any additional information it might need to generate a response, like private/proprietary data outside its training data, or any other data you know will be particularly relevant. This content is usually best positioned near the end of your prompt, as you may include different context for different generation requests.

Developer Prompt / System Prompt / Function Descriptions:
- Developer messages are explicit instructions from the developer. In o-series models, system messages are converted to developer messages internally.
- Function description refers to the explanatory text in the description field of each function object inside the tool parameter of an API request. This tells the model when and how to use the function.
Context Setting via Developer Message:
1. General context: Use role prompting to set base behavior, tone, and outline possible actions. Example:
   ```
   You are an AI retail agent.
   As a retail agent, you can help users cancel or modify pending orders, return or exchange delivered orders, modify their default user address, or provide information about their own profile, orders, and related products.
   ```
2. Function Call ordering: Explicitly outline the order of tool calls to avoid mistakes. Example:
   ```
   check to see if directories exist before making files
   ```
   Or, for a refund process:
   ```
   To Process a refund for a delivered order, follow the following steps:
   1. Confirm the order was delivered. Use: `order_status_check`
   2. Check the refund eligibility policy. Use: `refund_policy_check`
   3. Create the refund request. Use: `refund_create`
   4. Notify the user of refund status. Use: `user_notify`
   ```
3. Defining boundaries on when to use tools: Clarify when and when not to invoke certain tools, both at the developer prompt and tool description level. Example:
   ```ocaml
   Be proactive in using tools to accomplish the user's goal. If a task cannot be completed with a single step, keep going and use multiple tools as needed until the task is completed. Do not stop at the first failure. Try alternative steps or tool combinations until you succeed.

   - Use tools when:
     - The user wants to cancel or modify an order.
     - The user wants to return or exchange a delivered product.
     - The user wants to update their address or contact details.
     - The user asks for current or personalized order or profile info.

   - Do not use tools when:
     - The user asks a general question like “What’s your return policy?”
     - The user asks something outside your retail role (e.g., “Write a poem”).

   If a task is not possible due to real constraints (For example, trying to cancel an already delivered order), explain why clearly and do not call tools blindly.
   ```

Function Description:
- The function description should clarify when it should be invoked and how its arguments should be constructed.
- Example for a file_create function:
   ```
   Creates a new file with the specified name and contents in a target directory. This function should be used when persistent storage is needed and the file does not already exist.
   - Only call this function if the target directory exists. Check first using the `directory_check` tool.  
   - Do not use for temporary or one-off content—prefer direct responses for those cases.  
   - Do not overwrite existing files. Always ensure the file name is unique.
   - Do not overwrite existing files.  
     If replacement is intended and confirmed, use `file_delete` followed by `file_create`, or use `file_update` instead.
   ```
- Few shot prompting: While reasoning models benefit less from few-shot prompting, it can help tool calling performance, especially for argument construction. Example for a grep tool:
   ```ocaml
   Use this tool to run fast, exact regex searches over text files using the `ripgrep` engine.

   - Always escape special regex characters: ( ) [ ] { } + * ? ^ $ | . \\
   - Use `\\` to escape any of these characters when they appear in your search string.
   - Do NOT perform fuzzy or semantic matches.
   - Return only a valid regex pattern string.

   Examples:
   Literal            -> Regex Pattern         
   function(          -> function\\(           
   value[index]       -> value\\[index\\]      
   file.txt           -> file\\.txt            
   user|admin         -> user\\|admin          
   path\to\file       -> path\\\\to\\\\file
   ```
- Key rules should be up front and distractions minimized; prescriptive instructions should be prioritized.

Guarding Against Function Calling Hallucinations:
1. Explicit instructions: Instruct the model to avoid hallucinations like promising future function calls.
   ```
   Do NOT promise to call a function later. If a function call is required, emit it now; otherwise respond normally.
   ```
2. Catch bad arguments early: Set `strict` to `true` in the function schema to ensure reliable adherence.
   ```
   Validate arguments against the format before sending the call; if you are unsure, ask for clarification instead of guessing.
   ```
3. To address rare lazy behavior:
   a. Start a new conversation for unrelated topics.
   b. Discard irrelevant past tool calls/outputs and summarize them as context in the user message.
   c. Ongoing model improvements are expected to address this further.

Avoid Chain of Thought Prompting:
- Do not explicitly prompt these models to plan or reason between tool calls; they already do this internally. Over-prompting for reasoning may hurt performance.
Agentic Experience with Tools:
- Make tool routing clarity critical.
1. Explicitly define tool usage boundaries in the developer prompt when multiple tools can fulfill similar roles. Example:
   ```
   You are a helpful research assistant with access to the following tools:
   - python tool: for any computation involving math, statistics, or code execution
   - calculator: for basic arithmetic or unit conversions when speed is preferred

   Always use the python tool for anything involving logic, scripts, or multistep math. Use the calculator tool only for simple 1-step math problems.
   ```
2. Clarify when internal knowledge is insufficient and a tool should be used instead. Example:
   ```
   You have access to a `code_interpreter`. Always prefer using `code_interpreter` when a user asks a question involving:
   - math problems
   - data analysis
   - generating or executing code
   - formatting or transforming structured text

   Avoid doing these directly in your own response. Always use the tool instead.
   ```
3. Spell out decision boundaries for tools in the developer prompt, especially when including overlap, confidence, or fallback behavior. Example:
   ```ocaml
   Use `python` for general math, data parsing, unit conversion, or logic tasks that can be solved without external lookup—for example, computing the total cost from a list of prices.

   Use `calculate_shipping_cost` when the user asks for shipping estimates, as it applies business-specific logic and access to live rate tables. Do not attempt to estimate these using the `python` tool.

   When both could be used (e.g., calculating a delivery fee), prefer `calculate_shipping_cost` for accuracy and policy compliance. Fall back to `python` only if the custom tool is unavailable or fails.
   ```

FAQ Guidance:
- Number of functions: No hard upper limit, but setups with fewer than ~100 tools and fewer than ~20 arguments per tool are considered in-distribution. Clarity in function descriptions is critical as more tools increase ambiguity and possible hallucinations.
- Deeply nested params: Flat argument structures are generally easier for the model to reason about. Deeply nested objects can be supported but require clear field descriptions and strict schemas.
- Custom tool formats: The guidance assumes use of the standard tools parameter. If defining tools in free text, be more explicit with few-shot examples, output formats, and tool selection criteria.
</oseries-model-prompting-guide>
|}
;;

let general_prompting_guide : t =
  {|
<general-prompting-principles>
Avoid the following issues:
- Ambiguity: Could any wording be interpreted in more than one way?
- Lacking Definitions: Are there any class labels, terms, or concepts that are not defined that might be misinterpreted by an LLM?
- Conflicting, missing, or vague instructions: Are directions incomplete or contradictory?
- Unstated assumptions: Does the prompt assume the model has to be able to do something that is not explicitly stated?

Ensure the following principles are followed:
- Clear Scope Definition: Each agent has a narrowly defined purpose with explicit boundaries. For example, the contradiction checker focuses only on "genuine self-contradictions" and explicitly states that "overlaps or redundancies are not contradictions."
- Step-by-Step Process: Instructions provide a clear methodology, like how the format checker first categorizes the task before analyzing format requirements.
- Explicit Definitions: Key terms are defined precisely to avoid ambiguity.
- Boundary Setting: Instructions specify what the agent should NOT do.
- Structured Output Requirements: Each agent has a strictly defined output format with examples, ensuring consistency in the optimization pipeline.
</general-prompting-principles>
  |}
;;

let combined_headers : t =
  {|
# Prompting Guide
- This contains guidelines for prompting OpenAI models, particularly GPT-4.1 and O-series models.
- The content is structured to provide clear, actionable advice for effective prompting.
- As far as best practices go, unless directly contradicted In the O-series model prompting guide, everything in the GPT-4.1 model prompting guide applies to O-series models as well.
- everything in the O-series model prompting guide about function-calling applies to GPT-4.1 model function calling.
|}
;;

let combined_guides =
  String.concat
    ~sep:"\n\n"
    [ combined_headers
    ; general_prompting_guide
    ; o_series_prompting_guide
    ; gpt4_1_model_prompting_guide
    ]
;;
