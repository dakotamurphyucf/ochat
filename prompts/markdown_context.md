<import file="/Users/dakotamurphy/chatgpt/prompts/s.md" />
<msg role="user">
    here is an example chat interface of the current version of an openai chat interface application:
    <img src="/Users/dakotamurphy/Desktop/chat.png" local/>
    the following document has a  set of proposed feature enhancements to a openai chat interface application:
    ```doc
    <doc src="/Users/dakotamurphy/chatgpt/proposal.md" local/>
    ```
    
    The parser for the xml document uses the ocaml library Markup here is the mli for that lib:
    ```ocaml
    <doc src="/Users/dakotamurphy/chatgpt/prompts/markup.mli" local/>
    ```

    the current parser is located in the Chat_markdown module in this ocaml file prompt_template.ml:
     ```ocaml
    <doc src="/Users/dakotamurphy/chatgpt/lib/prompt_template.ml" local/>
    ```

     the parser being used to run chat completion is in this ocaml file in the run_completion function:
     ```ocaml
    <doc src="/Users/dakotamurphy/chatgpt/lib/chat_completion.ml" local/>
    ```
</msg>
