<config model="o3"  max_tokens="100000" reasoning_effort="high" show_tool_call />

<tool name="apply_patch" />
<tool name="read_dir" />
<tool name="get_contents" />
<tool name="dune" command="dune" description="Use to run dune commands on on ocaml project. Dune build returns nothing if successful" />


<msg role="developer">
 <doc src="./prompt-examples/coding_assistant.txt" local/>
</msg>

<msg role="user">
RAW|<context>|RAW
Here is an overview of the important folders and files in the Ocaml Repo, called ochat, we are working on you should ignore everything else:

    /:
    ochat.opam - opam file
    dune-project - dune project file
    lib - folder where all the library modules are
    bin - folder where the project executable files are.
    Readme.md - README for the project

    lib/:
    apply_patch.ml
    bin_prot_utils.ml
    bin_prot_utils.mli
    chat_completion.ml
    chat_response.ml
    chatml_builtin_modules.ml
    chatml_lang.ml
    chatml_lexer.mll
    chatml_parser.mly
    chatml_typechecker.md
    chatml_typechecker.ml
    definitions.ml
    doc.ml
    doc.mli
    dune - dune file that declares all the library modules.
    dune_describe.ml
    filter_file.ml
    functions.ml
    github.ml
    github.mli
    ochat_function.ml
    indexer.ml
    indexer.mli
    io.ml
    io.mli
    jsonaf_ext.ml
    lru_cache.ml
    lru_cache.mli
    lru_cache_intf.ml
    merlin.ml
    ocaml_parser.ml
    ocaml_parser.mli
    openai.ml
    openai.mli
    openai_chat_completion.ml
    prompt_template.ml
    prompt_template.mli
    template.ml
    template.mli
    test - folder where all of the test are
    tikitoken.ml
    ttl_lru_cache.ml
    ttl_lru_cache.mli
    vector_db.ml
    vector_db.mli 

    lib/test:
    dune - dune file that declares all the test.
    apply_patch_test.ml
    filter_file_test.ml

    bin/:
    dune - dune file that declares all the executables.
    main.ml - the main executable for the command line app that the project builds
    dsl_script.ml - test script for experimental language, currently WIP and not availible in the projects main executable
RAW|</context>|RAW

RAW|<query>|RAW
Describe this repo for me
RAW|</query>|RAW
</msg>