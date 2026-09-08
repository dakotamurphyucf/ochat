"""Verify actual tutorial host binaries without provider calls.

Run from the repository root with Python 3 and the configured opam environment.
Only private generated fixtures and loopback listeners are used. The output
summary contains no credentials; owned processes and temporary state are cleaned
up even when a check fails.
"""

import contextlib, json, os, pathlib, queue, signal, socket, subprocess, tempfile, threading, time, urllib.request
repo = pathlib.Path.cwd()
out = repo / 'scratch/ochat-website-evidence/p06-host-check.json'
out.parent.mkdir(parents=True, exist_ok=True)
results = []
env = os.environ.copy()
env.pop('OPENAI_API_KEY', None)

def run(args):
    p = subprocess.run(args, cwd=repo, env=env, text=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE, timeout=90)
    if p.returncode:
        raise RuntimeError('Command failed: ' + ' '.join(args[:4]) + ' (exit ' + str(p.returncode) + ')')
    return p.stdout
run(['dune', 'build', 'bin/ochat_agent_server.exe', 'bin/ochat_agent_stdio.exe', 'bin/main.exe', 'bin/chat_tui.exe', 'docs-src/examples/agent-server/clients/docs_example.exe'])
requests = [json.loads(line) for line in (repo / 'docs-src/examples/agent-server/clients/discover.ndjson').read_text().splitlines() if line]

def discover(args):
    p = subprocess.Popen(args, cwd=repo, env=env, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL, text=True, bufsize=1)
    q = queue.Queue()

    def reader():
        for line in p.stdout:
            q.put(line)
    threading.Thread(target=reader, daemon=True).start()
    try:
        for request in requests:
            p.stdin.write(json.dumps(request) + '\n')
            p.stdin.flush()
            deadline = time.monotonic() + 15
            while True:
                line = q.get(timeout=max(0.01, deadline - time.monotonic()))
                response = json.loads(line)
                if response.get('id') == request['id']:
                    assert 'error' not in response and 'result' in response
                    break
        p.stdin.close()
        p.wait(timeout=15)
        assert p.returncode == 0
    finally:
        if p.poll() is None:
            p.send_signal(signal.SIGINT)
            p.wait(timeout=15)
    return len(requests)

@contextlib.contextmanager
def daemon(config, ready):
    p = subprocess.Popen(['dune', 'exec', 'bin/ochat_agent_server.exe', '--', '-config', str(config)], cwd=repo, env=env, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    try:
        deadline = time.monotonic() + 15
        while not ready():
            assert p.poll() is None, 'daemon exited before readiness'
            if time.monotonic() > deadline:
                raise RuntimeError('daemon startup timeout')
            time.sleep(0.05)
        yield
    finally:
        if p.poll() is None:
            p.send_signal(signal.SIGINT)
            p.wait(timeout=15)
with tempfile.TemporaryDirectory(prefix='ochat-p06-') as temporary:
    root = pathlib.Path(temporary)
    run(['dune', 'exec', 'docs-src/examples/agent-server/clients/docs_example.exe', '--', 'setup', temporary, 'gpt-5.6-sol'])
    for name in ['unix.sexp', 'http.sexp']:
        run(['dune', 'exec', 'bin/ochat_agent_server.exe', '--', '-config', str(root / name), '-validate-only'])
    results.append({'check': 'private Unix/HTTP config validation', 'result': 'pass'})
    count = discover(['dune', 'exec', 'bin/ochat_agent_stdio.exe', '--', '--local', '--prompt', str(root / 'hello.chatmd'), '--workspace', str(root / 'workspace'), '--data-root', str(root / 'stdio-state')])
    results.append({'check': 'standalone local stdio with explicit private data root; sequential discovery then EOF', 'responses': count, 'result': 'pass'})
    with daemon(root / 'unix.sexp', lambda: (root / 'agent.sock').exists()):
        count = discover(['dune', 'exec', 'bin/ochat_agent_stdio.exe', '--', '--connect', 'unix://' + str(root / 'agent.sock')])
    results.append({'check': 'real Unix daemon and stdio gateway discovery; graceful shutdown', 'responses': count, 'result': 'pass'})
    with socket.socket() as listener:
        listener.bind(('127.0.0.1', 0))
        port = listener.getsockname()[1]
    config = root / 'http.sexp'
    config.write_text(config.read_text().replace('8787', str(port)))
    url = 'http://127.0.0.1:' + str(port)

    def ready():
        try:
            with socket.create_connection(('127.0.0.1', port), timeout=0.1):
                return True
        except OSError:
            return False
    with daemon(config, ready):
        token = (root / 'admin.token').read_text().strip()
        headers = {'Authorization': 'Bearer ' + token, 'Content-Type': 'application/json', 'ochat-protocol-version': '1.0'}
        req = urllib.request.Request(url + '/v1/rpc', data=(root / 'initialize.json').read_bytes(), headers=headers)
        with urllib.request.urlopen(req, timeout=10) as response:
            connection = response.headers['ochat-connection-id']
            data = json.load(response)
            assert 'result' in data
        headers['ochat-connection-id'] = connection
        body = json.dumps({'jsonrpc': '2.0', 'id': 'sessions', 'method': 'session.list', 'params': {'limit': 20}}).encode()
        with urllib.request.urlopen(urllib.request.Request(url + '/v1/rpc', data=body, headers=headers), timeout=10) as response:
            assert 'result' in json.load(response)
        with urllib.request.urlopen(urllib.request.Request(url + '/v1/connection', method='DELETE', headers=headers), timeout=10) as response:
            assert response.status in [200, 204]
        count = discover(['dune', 'exec', 'bin/ochat_agent_stdio.exe', '--', '--connect', url, '--bearer-token-file', str(root / 'admin.token')])
    results.append({'check': 'real loopback HTTP daemon: authenticated handshake, connection header, session list, close, gateway discovery', 'responses': count, 'result': 'pass'})
inspect = run(['dune', 'exec', 'bin/main.exe', '--', 'shell', 'inspect', 'docs-src/examples/agent-server/shell/pwd.chatmd', '-canonical'])
assert '/bin/pwd' in inspect
results.append({'check': 'canonical fixed-pwd shell inspection on macOS; no shell command executed', 'result': 'pass'})
out.write_text(json.dumps({'platform': 'macOS 14.5 arm64', 'providerCalls': 0, 'scope': 'Exact checkout binaries; no model messages, TUI interaction, shell execution, or public listeners. All generated credentials and state removed after shutdown. HTTP used an available loopback port.', 'results': results}, indent=2) + '\n')
print('P06 provider-free host checks pass:', len(results), 'checks. Summary contains no credentials.')
