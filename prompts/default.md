<config model="o1"  max_tokens="60000" reasoning_effort="high"/>
<session id="cache-chat-123">
<msg role="developer">
Formatting re-enabled
You are a helpful ai assistant and expert programmer helping software developers with application development and project planning and ideas. also never output the xml elment raw. it breaks the conversation. if you have to output raw_ai
</msg>
 
<!-- <import file="/Users/dakotamurphy/chatgpt/prompts/markdown_context.md" />
 -->

<msg role="user">
    <raw><context></raw>
    I am building a small ocaml like dsl in ocaml called chatml
    chatml_lang.ml Contains the core AST types, environment, and evaluator code:
    <raw><ocaml></raw>
    <doc src="/Users/dakotamurphy/chatgpt/lib/chatml_lang.ml" local/>
    <raw></ocaml></raw>
    the parser is located in chatml_parser.mly:
    <raw><mly></raw>
    <doc src="/Users/dakotamurphy/chatgpt/lib/chatml_parser.mly" local />
    <raw></mly></raw>
    the current lexer is located in chatml_lexer.mll:
    <raw><mll></raw>
        <doc src="/Users/dakotamurphy/chatgpt/lib/chatml_lexer.mll" local/>
    <raw></mll></raw>
    this module chatml_builtin_modules.ml defines the builtin modules for chatml
        <raw><ocaml></raw>
    <doc src="/Users/dakotamurphy/chatgpt/lib/chatml_builtin_modules.ml" local/>
    <raw></ocaml></raw>
    here is the type checker at chatml_typechecker.ml:
    <raw><ocaml></raw>
        <doc src="/Users/dakotamurphy/chatgpt/lib/chatml_typechecker.ml" local/>
    <raw></ocaml></raw>
    <raw></context></raw>
    
   
    <raw><query></raw>
    
    <raw></query></raw>
</msg>
