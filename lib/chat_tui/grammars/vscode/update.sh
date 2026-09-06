#!/bin/sh
set -eu

version=1.96.4
base=https://raw.githubusercontent.com/microsoft/vscode/$version/extensions
dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)

fetch() {
  curl --fail --location --silent --show-error "$base/$1" --output "$dir/$2"
}

fetch python/syntaxes/MagicPython.tmLanguage.json python.json
fetch python/syntaxes/MagicRegExp.tmLanguage.json python-regexp.json
fetch rust/syntaxes/rust.tmLanguage.json rust.json
fetch javascript/syntaxes/JavaScript.tmLanguage.json javascript.json
fetch javascript/syntaxes/JavaScriptReact.tmLanguage.json javascript-react.json
fetch typescript-basics/syntaxes/TypeScript.tmLanguage.json typescript.json
fetch typescript-basics/syntaxes/TypeScriptReact.tmLanguage.json typescript-react.json
