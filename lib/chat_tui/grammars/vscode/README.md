# Vendored VS Code TextMate grammars

These grammars are copied from Microsoft Visual Studio Code release `1.96.4`:

- `python.json`: `extensions/python/syntaxes/MagicPython.tmLanguage.json`
- `python-regexp.json`: `extensions/python/syntaxes/MagicRegExp.tmLanguage.json`
- `rust.json`: `extensions/rust/syntaxes/rust.tmLanguage.json`
- `javascript.json`: `extensions/javascript/syntaxes/JavaScript.tmLanguage.json`
- `javascript-react.json`: `extensions/javascript/syntaxes/JavaScriptReact.tmLanguage.json`
- `typescript.json`: `extensions/typescript-basics/syntaxes/TypeScript.tmLanguage.json`
- `typescript-react.json`: `extensions/typescript-basics/syntaxes/TypeScriptReact.tmLanguage.json`

Run `sh lib/chat_tui/grammars/vscode/update.sh` to download the pinned files.
Review the resulting diff before committing an update.

VS Code package metadata is not consumed. JSX and TSX are registered as
distinct root grammars. VS Code's TypeScript JSDoc injection grammars are not
vendored because the current `textmate-language` integration does not expose
injection registration. All required root and companion grammars at this pin
are JSON, so runtime XML plist loading is not required.

The files are distributed under the MIT license in `LICENSE.txt`. Their
`information_for_contributors` and `version` fields preserve upstream
provenance.
