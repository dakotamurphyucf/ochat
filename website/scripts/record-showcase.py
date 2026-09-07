"""Record a real Ochat batch/tool/specialist run against synthetic public input.

Default: deterministic loopback provider. --live forwards at most six requests
only to api.openai.com using the existing OPENAI_API_KEY. No headers/credentials
are recorded. Source files are copied to a fresh private directory; no project
files are exposed to the agent. Run from any directory with Python 3 and Dune.
"""
import argparse, datetime, hashlib, http.server, json, os, pathlib, shutil, subprocess, tempfile, threading, urllib.request
ROOT = pathlib.Path(__file__).resolve().parents[2]
SOURCE = ROOT / 'docs-src/examples/applications/docs-review'
p = argparse.ArgumentParser(); p.add_argument('--live', action='store_true'); args = p.parse_args()
key = os.environ.get('OPENAI_API_KEY') if args.live else None
if args.live and not key: raise SystemExit('Live recording requires OPENAI_API_KEY.')
subprocess.run(['dune','build','bin/main.exe'], cwd=ROOT, check=True, stdout=subprocess.DEVNULL)
requests = []; failures = []
feedback = ('1. Requirements: "a recent Node.js release" leaves contributors guessing. '
            'Document the tested Node version in reference/project.txt.\n'
            '2. Setup: "Install the dependencies, then start the preview" omits both commands. '
            'Add the actual package manager and preview command after checking the project.\n'
            '3. Check your changes: "the usual checks" does not identify a check or expected result. '
            'Name the checks and describe a successful run.')
def fixed_response(body, index):
    tools = [t.get('name') for t in body.get('tools', [])]
    inputs = body.get('input', [])
    outputs = [i for i in inputs if isinstance(i,dict) and i.get('type')=='function_call_output']
    def call(name, arguments):
        return {'type':'function_call','id':f'fc_{index}','call_id':f'call_{index}','name':name,'arguments':json.dumps(arguments),'status':'completed'}
    def message(text):
        return {'type':'message','id':f'msg_{index}','role':'assistant','status':'completed','content':[{'type':'output_text','text':text,'annotations':[]}]}
    if 'review_docs' not in tools: item = message(feedback)
    elif not outputs: item = call('read_file',{'root':'reference','file':'project.txt'})
    elif len(outputs)==1: item = call('review_docs',{'input':outputs[0]['output']})
    else: item = message('Documentation review · reference/project.txt\n\n'+feedback+'\n\nNext action: confirm the supported runtime and commands with the maintainer, then add an explicit setup and verification checklist. No files were changed.')
    return {'id':f'resp_{index}','object':'response','created_at':0,'model':'gpt-4.1','output':[item],'parallel_tool_calls':False,'tool_choice':'auto','tools':body.get('tools',[]),'temperature':0,'top_p':1,'status':'completed','usage':None,'user':None,'error':None,'incomplete_details':None,'instructions':None,'max_output_tokens':1200,'metadata':{},'previous_response_id':None,'reasoning':None,'service_tier':'default','store':False,'text':{'format':{'type':'text'}},'truncation':'disabled'}
class Handler(http.server.BaseHTTPRequestHandler):
    def log_message(self,*unused): pass
    def do_POST(self):
        try:
            if self.path!='/v1/responses' or len(requests)>=6: raise RuntimeError('Unexpected endpoint or request limit exceeded')
            length=int(self.headers.get('Content-Length',0))
            if length>100000: raise RuntimeError('Unexpected request size')
            body=json.loads(self.rfile.read(length)); streaming=body.get('stream',False); body['stream']=False
            index=len(requests); event={'request':body}; requests.append(event)
            if args.live:
                req=urllib.request.Request('https://api.openai.com/v1/responses',data=json.dumps(body).encode(),headers={'Content-Type':'application/json','Authorization':'Bearer '+key})
                with urllib.request.urlopen(req,timeout=60) as response: result=json.load(response)
            else: result=fixed_response(body,index)
            event['response']=result
            # Normalize only response-envelope metadata for this checkout's
            # stream decoder. Preserve every model output item verbatim.
            envelope=fixed_response(body,index)
            envelope.update(id=result['id'], model=result['model'], output=result['output'], status=result['status'], usage=result.get('usage'))
            result=envelope
            if streaming:
                events=[]
                for n,item in enumerate(result['output']):
                    events.append({'type':'response.output_item.added','output_index':n,'item':item})
                    if item.get('type')=='function_call':
                        events.append({'type':'response.function_call_arguments.done','output_index':n,'item_id':item['id'],'arguments':item['arguments'],'name':item['name']})
                    if item.get('type')=='message':
                        for c in item['content']:
                            if c.get('type')=='output_text':events.append({'type':'response.output_text.delta','item_id':item['id'],'output_index':n,'content_index':0,'delta':c['text']})
                    events.append({'type':'response.output_item.done','output_index':n,'item':item})
                events.append({'type':'response.completed','response':result})
                encoded=(''.join('data: '+json.dumps(event)+'\n\n' for event in events)+'data: [DONE]\n\n').encode()
            else: encoded=json.dumps(result).encode()
            self.send_response(200);self.send_header('Content-Type','text/event-stream' if streaming else 'application/json');self.send_header('Content-Length',str(len(encoded)));self.end_headers();self.wfile.write(encoded)
        except Exception as exc:
            failures.append(type(exc).__name__);self.send_error(502,'Recording request failed')
server=http.server.ThreadingHTTPServer(('127.0.0.1',0),Handler)
thread=threading.Thread(target=server.serve_forever,daemon=True);thread.start()
try:
    with tempfile.TemporaryDirectory(prefix='ochat-showcase-') as tmp:
        directory=pathlib.Path(tmp)
        for name in ['explorer.chatmd','docs-reviewer.chatmd','reference/project.txt']:
            target=directory/name;target.parent.mkdir(parents=True,exist_ok=True);shutil.copy2(SOURCE/name,target)
        env=dict(os.environ,API_URL=f'http://127.0.0.1:{server.server_port}',OPENAI_API_KEY='loopback-recording')
        result=subprocess.run([str(ROOT/'_build/default/bin/main.exe'),'chat-completion','-prompt-file','explorer.chatmd','-output-file','review.chatmd'],cwd=directory,env=env,stdout=subprocess.PIPE,stderr=subprocess.PIPE,timeout=180)
        (ROOT/'scratch/showcase-last-requests.json').write_text(json.dumps(requests,indent=2))
        if result.returncode or failures:
            (ROOT/'scratch/showcase-runtime-error.log').write_bytes(result.stderr)
            raise RuntimeError('Recording failed ('+', '.join(failures)+'); private diagnostic saved in scratch/showcase-runtime-error.log')
        transcript=(directory/'review.chatmd').read_text()
        def redact(text):
            for prefix in sorted({str(directory),str(directory.resolve())},key=len,reverse=True):text=text.replace(prefix,'<example-directory>')
            return text
        transcript=redact(transcript)
        (ROOT/'scratch/showcase-requests.json').write_text(json.dumps(requests,indent=2))
        calls=[i for e in requests for i in e['response'].get('output',[]) if i.get('type')=='function_call']
        assert [i['name'] for i in calls]==['read_file','review_docs'], 'Expected file read followed by specialist delegation'
        assert len(requests)==4, 'Expected four bounded provider requests'
        assert any(not e['request'].get('tools') for e in requests), 'Specialist must have no tools'
        assert 'Lantern' in json.dumps(requests) and 'reference/project.txt' in transcript
        (SOURCE/'recorded-run.chatmd').write_text(transcript)
        final=requests[-1]['response']['output']
        report='\n'.join(c['text'] for i in final if i.get('type')=='message' for c in i['content'] if c.get('type')=='output_text')
        hashes={name:hashlib.sha256((SOURCE/name).read_bytes()).hexdigest() for name in ['explorer.chatmd','docs-reviewer.chatmd','reference/project.txt','recorded-run.chatmd']}
        artifact={'version':1,'provider':'live OpenAI' if args.live else 'deterministic scripted provider','model':'gpt-4.1','liveProvider':args.live,'scope':'Real Ochat batch driver, scoped file read, and nested specialist; synthetic Lantern documentation only. '+('Model responses recorded from OpenAI. Results vary on a new run.' if args.live else 'Model responses are scripted fixtures, not evidence of model reasoning quality.'),'command':'ochat chat-completion -prompt-file explorer.chatmd -output-file review.chatmd','sourceHashes':hashes,'requests':json.loads(redact(json.dumps(requests))),'result':redact(report),'redactions':['Temporary example directory replaced by <example-directory>.'],'transport':'Recording proxy converts complete responses to stream events and normalizes response-envelope metadata; model output is unchanged.'}
        artifact['sourceRevision']=subprocess.check_output(['git','rev-parse','HEAD'],cwd=ROOT,text=True).strip()
        artifact['recordedAt']=datetime.datetime.now(datetime.timezone.utc).isoformat()
        artifact['runtimeSources']={name:hashlib.sha256((ROOT/name).read_bytes()).hexdigest() for name in ['bin/main.ml','lib/chat_response/driver.ml','lib/chat_response/tool.ml','lib/openai/responses.ml','lib/functions.ml','lib/io.ml']}
        (SOURCE/'recording.json').write_text(json.dumps(artifact,indent=2)+'\n')
        print('Recorded real runtime: file read → specialist → report;',artifact['provider'],';',len(requests),'requests. No credentials recorded.')
finally:
    server.shutdown();server.server_close();thread.join(timeout=5)
