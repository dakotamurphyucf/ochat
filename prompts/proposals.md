1. make the response api more feature complete to adhear to the entire openai api spec ```done```
2. make the chat markdown config more feature complete
3. make the chat_response handle no reasoning by making msg ids optional
4. add function tool imports to chat markdown configs ```done```
5. make the streaming optional, as well as make the model output to the chat markdown optional
6. make a stdio stdout interface for interacting with the model
7. integrate with codex cli ```postpone```
8. refactor the code in chat_completion, chat_response, openai api, prompt_template.ml
9. clean up functions and definitions```(done)```, remove doc.ml and move function to another file ```(not doing)```, update bin_prot_utils to use eio ```(done)```
10. think of ways that we can restructure the lib folder to make it organized in a more logical way ```postponed (need to do more research)```
11. re-build chat_ml typechecker, start from scratch with the basics and stub everything else then iterate piece by piece to get full coverage testing along the way
12. build tests for everything
13. document the code properly, following odoc standards
14. document the cli, the language, and the markdown language
    markdown language
    - agent
        - can have doc/img/text children. when running the agent the content of the agent element will be inserted as a message to the agent prompt and run and the agent element will be replaced with the results of running that prompt
        - an agent can be nested inside of a msg element as well and the ouput of that agent run will ultimatley be what is embedded in that msg
        - msg with role assistant typically do not have agent/doc/img elements nested in them, but it is technically possible that it could have agent elements nested in them but the use case might be niche or non-existent
    - doc
        - imports the contents of a doc url and replaces the doc elment with the doc url content
        - can be local or remote
    - img
        - imports an image and embedds it in the the message making it a multi modal message
        - can be local or remote
    - raw
        - this is used as an escape mechanism for nested xml elements in the msg content so that way it is not parsed and remains as raw text. Does not work for nested raw tags and you would need to escape using CDATA blocks to escape them or do a document import with the content that contains the raw tagged xml elements
    - config
        - this is where you set config data for the model api like reasoning level, max_tokens, model, ect
    - import
        - this acts as a raw xml element import so you could import a set of elements from another file and it will be like you copy and pasted those elements directly into the prompt file before running
    - tool
        - this is how you declare what tools you want availible to the model. There are in-built functions as well the ability to declare custom functions witch just act as a wrapper over running a shell command. You just declare a name, the command, and an optional description and the model can call the command and add optional shell parameters to the command. You can control how and what parameters the model uses via the description and via system prompting. All tools that read/write to the filesystem are sandboxed to the current working directory that the prompt is run in
    - reasoning
        - this tag just wraps reasoning summaries from the model when using a reasoning model
    - summary
        this element is nested in reasoning elments, it contains the summary of the model reasoning. A models reasoning response can have multiple summaries
    - msg
        - this is the element that wraps user/assistant/system messages. System and and user messages can have nested doc/img/agnet elements that resolve to the final message input sent to the model
        - assistant messages can not have img elements nested, and typically have no use for doc/agent nested elements but it is technically possible but the use case may be niche such as editing the ouput of a model to try and steer it in a different direction, but this might be better done with a user mesage that trys to steer the model
15. clean up the repo ```done```
16. standardize where we put the cache files, standardize logging, add better error handling
17. add better debugging facility for chatml and chatmd
18. add better error handling around the openai api's, the chat_response, completions, ect. Add rollback to markdown files when failures occur