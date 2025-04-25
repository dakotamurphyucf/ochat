1. make the response api more feature complete to adhear to the entire openai api spec
2. make the chat markdown config more feature complete
3. make the chat_response handle no reasoning by making msg ids optional
4. add function tool imports to chat markdown configs
5. make the streaming optional, as well as make the model output to the chat markdown optional
6. make a stdio stdout interface for interacting with the model
7. integrate with codex cli
8. refactor the code in chat_completion, chat_response, openai api, prompt_template.ml
9. clean up functions and definitions, remove doc.ml and move function to another file, update bin_prot_utils to use eio
10. think of ways that we can restructure the lib folder to make it organized in a more logical way
11. re-build chat_ml typechecker, start from scratch with the basics and stub everything else then iterate piece by piece to get full coverage testing along the way
12. build tests for everything
13. document the code properly, following odoc standards
14. document the cli, the language, and the markdown language
15. clean up the repo
16. standardize where we put the cache files, standardize logging, add better error handling
17. add better debugging facility for chatml and chatmd
18. add better error handling around the openai api's, the chat_response, completions, ect. Add rollback to markdown files when failures occur