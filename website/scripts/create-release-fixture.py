"""Create a private, committed local candidate without changing the main checkout.

Uses installed dependencies; this is not a clean Linux install or a public commit.
"""
import argparse
import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('--record', required=True, help='Where to write fixture.json')
args = parser.parse_args()
root = Path(__file__).resolve().parents[2]
candidate = Path(tempfile.mkdtemp(prefix='ochat-p10-candidate-')) / 'repo'
candidate.mkdir()
files = subprocess.check_output(['git', 'ls-files', '-z'], cwd=root).decode().split('\0')
for name in files:
    if not name or name.startswith(('website/', '.github/')):
        continue
    source = root / name
    if source.is_file():
        destination = candidate / name
        destination.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(source, destination)
for name in ['dune', '.gitattributes']:
    if (root / name).is_file():
        shutil.copy2(root / name, candidate / name)
shutil.copytree(root / '.github', candidate / '.github')
shutil.copytree(root / 'website', candidate / 'website', ignore=shutil.ignore_patterns(
    'node_modules', 'dist', '.generated', '.astro', 'test-results', 'playwright-report',
    '.content-lock', '.generated-stage-*', '.generated-previous', '.release', '.wrangler', '__pycache__'))
subprocess.run(['git', 'init', '-q'], cwd=candidate, check=True)
subprocess.run(['git', 'add', '-A'], cwd=candidate, check=True)
subprocess.run(['git', '-c', 'user.name=Ochat local release fixture', '-c',
                'user.email=fixture@ochat.test', 'commit', '-qm',
                'P10 local validation fixture; not a public release commit'], cwd=candidate, check=True)
shutil.copytree(root / 'website/node_modules', candidate / 'website/node_modules',
                symlinks=True, ignore=shutil.ignore_patterns('.vite', '.astro'))
record = Path(args.record).resolve()
record.parent.mkdir(parents=True, exist_ok=True)
record.write_text(json.dumps({
    'candidate': str(candidate),
    'sourceRevision': subprocess.check_output(['git', 'rev-parse', 'HEAD'], cwd=root).decode().strip(),
    'fixtureRevision': subprocess.check_output(['git', 'rev-parse', 'HEAD'], cwd=candidate).decode().strip(),
    'origin': 'https://release.ochat.test',
    'scope': 'Isolated synthetic commit with copied installed dependencies. Not public source-link availability or a clean dependency install.',
}, indent=2) + '\n')
env = dict(os.environ, SITE_ENV='production', SITE_URL='https://release.ochat.test')
subprocess.run(['npm', 'run', 'check'], cwd=candidate / 'website', env=env, check=True)
subprocess.run(['npm', 'run', 'build'], cwd=candidate / 'website', env=env, check=True)
print(f'Production candidate ready: {candidate}')
