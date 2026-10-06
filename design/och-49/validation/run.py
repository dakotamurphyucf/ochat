#!/usr/bin/env python3
"""Compile design-only interfaces and run PPX expect probes in isolated scratch.
Uses existing default opam switch read-only; never installs or changes packages.
No production Dune library is introduced. Run from any location with Python3.
"""
import pathlib
import shutil
import subprocess

review = pathlib.Path(__file__).resolve().parents[1]
repo = review.parents[1]
project = repo / 'scratch/agents/ochat_runtime_inventory/validation'
project.mkdir(parents=True, exist_ok=True)
(project / 'dune-project').write_text('(lang dune 3.21)\n(name och49_review_validation)\n')
(project / 'dune').write_text('''(library
 (name contract_review)
 (modules provider_contracts storage_contracts)
 (modules_without_implementation provider_contracts storage_contracts)
 (libraries core jsonaf eio)
 (preprocess (pps ppx_jane)))
(library
 (name presence_probe)
 (modules presence_probe)
 (libraries core jsonaf expect_test_helpers_core)
 (inline_tests)
 (preprocess (pps ppx_jane ppx_expect ppx_jsonaf_conv)))
''')
for name in ['provider_contracts.mli', 'storage_contracts.mli']:
    shutil.copyfile(review / name, project / name)
shutil.copyfile(review / 'validation/presence_probe.ml', project / 'presence_probe.ml')
for command in [
    ['opam', 'exec', '--switch=default', '--', 'dune', 'build', '--root', str(project), '@all'],
    ['opam', 'exec', '--switch=default', '--', 'dune', 'runtest', '--root', str(project)],
]:
    print(' '.join(command), flush=True)
    subprocess.run(command, cwd=repo, check=True)
