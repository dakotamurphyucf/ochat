## Description

This OCaml project is a comprehensive toolkit designed for parsing OCaml code, managing vector databases, and integrating with OpenAI's API. Key features include code indexing, prompt generation, markdown style chat completions using GPT models, and tokenization using OpenAI's Tikitoken specification. This library contains alot of experimentation, and is intented to be more of a research project than a production ready app. Uses Effect based library Eio for concurrency and Owl for computing cosign similarity of openai embeddings. I recommmend installing in a local opam switch.

## Installation

To build this project, you can use OPAM:

```sh
opam install . --deps-only
# If owl has issues installing on new apple chips https://github.com/owlbarn/owl/issues/597#issuecomment-1119470934 
#  try this 
opam pin -n git+https://github.com/mseri/owl.git#arm64 --with-version=1.1.0
PKG_CONFIG_PATH="/opt/homebrew/opt/openblas/lib/pkgconfig" opam install owl.1.1.0
```

# Command Line Application Documentation

This command-line application is designed for indexing OCaml code, serving queries to a code vector search database using OpenAI embeddings, and interacting with gpt models through chat interface using OpenAI chat completion api. The application provides four main commands: `index`, `query`, `chat-completion`, and `tokenize`.

## Index Command

The `index` command indexes OCaml code in a specified folder for a code vector search database using OpenAI embeddings.

### Usage

```bash
index -folder-to-index <folder_path> -vector-db-folder <db_folder_path>
```

### Parameters

- `-folder-to-index`: Path to the folder containing OCaml code to index. Default is `./lib`.
- `-vector-db-folder`: Path to the folder to store vector database data. Default is `./vector`.

## Query Command

The `query` command queries the indexed OCaml code using natural language.

### Usage

```bash
query -vector-db-folder <db_folder_path> -query-text <query_text> -num-results <num_results>
```

### Parameters

- `-vector-db-folder`: Path to the folder containing vector database data. Default is `./vector`.
- `-query-text`: Natural language query text to search the indexed OCaml code. This is a required parameter.
- `-num-results`: Number of top results to return. Default is 5.

## Chat Completion Command

The `chat-completion` command calls the OpenAI API to provide chat completion based on the content of a prompt file.

### Usage

```bash
chat-completion -prompt-file <prompt_file_path> -output-file <output_file_path> -max-tokens <max_tokens>
```

### Parameters

- `-prompt-file`: Path to the file containing the initial prompt. This is optional. If provided, its content is loaded and appended to the output file. 
- `-output-file`: Path to the file to save the chat completion output. Default is `./prompts/default.md`. Only include this parameter and not -prompt-file if you wan to continue with a previous conversation
- `-max-tokens`: Maximum number of tokens to generate in the chat completion. Default is 600.

### Markup syntax of prompt file

The markup syntax used is designed to represent a conversation as a series of messages. Each message is represented as a `msg` element with attributes for the role of the message (e.g., "system", "user", "assistant", "function"), an optional name to represent messages from the user/assistant or name of the function called for results returned in a msg with role function, and an optional function_call when the assistant invokes a function_call. The content of the message is represented as the text content of the `msg` element. If a function call is present, it is represented as a `function_call` attribute with a `function_name` attribute and `arguments` set as the elements contents. Only msg elements with assistant role can have a function_call. The function name and arguments are used to call a function from the list of functions made availble to the prompt. You can view availible functions in lib/chat_completion.ml or just ask the model in the chat for a list of availble function. The results of a function call are put in a msg element with role function, name is the function used, and the contents of the element are the results. 

here's an example of a conversation using the specialized markup:

```html
<msg role="system">You are a helpful assistant.</msg>
<msg role="user" name="John">What's the weather like?</msg>
<msg role="assistant">I'm not sure, would you like me to look it up for you?</msg>
<msg role="user" name="John">Yes, please.</msg>
<msg role="assistant" function_call function_name="get_url_content">{"url": "http://api.weatherapi.com/v1/current.json?key=YOUR_API_KEY&q=London"}</msg>
<msg role="function" name="get_url_content">{"location":{"name":"London","region":"City of London, Greater London","country":"UK"},"current":{"temp_c":14.0,"condition":{"text":"Partly cloudy"}}}</msg>
<msg role="assistant">The current weather in London is partly cloudy with a temperature of 14 degrees Celsius.</msg>
<msg role="user" name="John">Thank you!</msg>
<msg role="assistant">You're welcome!</msg>
```

## Tokenize Command

The `tokenize` command tokenizes the provided file using the OpenAI Tikitoken spec.

### Usage

```bash
tokenize -file <file_path>
```

### Parameters

- `-file`: Path to the file to tokenize. Default is `bin/main.ml`.

## Examples

Indexing a folder:

```bash
index -folder-to-index ./my_ocaml_code -vector-db-folder ./my_vector_db
```

Querying the indexed code:

```bash
query -vector-db-folder ./my_vector_db -query-text "How to write a for loop in OCaml?" -num-results 10
```

Running a chat completion:

```bash
chat-completion -prompt-file ./my_prompt.md -output-file ./my_chat.md -max-tokens 1000
```

Tokenizing a file:

```bash
tokenize -file ./my_ocaml_code/my_file.ml
```

`note: If testing using dune prefix commands with`
```sh 
dune exe ./bin/main.exe --
```


## Contributing

Any contributions you make are **greatly appreciated**.

If you have a suggestion that would make this better, please fork the repo and create a pull request. You can also simply open an issue with the tag "enhancement".


## License

Distributed under the MIT License. See `LICENSE.txt` for more information.

## Status

This project is Highly Experimental