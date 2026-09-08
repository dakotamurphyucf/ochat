// One reader process per generated snapshot; exiting clears renderer modules.
import { dev } from 'astro';
import { fileURLToPath } from 'node:url';
const root = fileURLToPath(new URL('../', import.meta.url));
const server = await dev({ root, server: { host: '127.0.0.1', port: 4321 } });
process.send?.('ready');
let stopping = false;
async function stop() {
  if (stopping) return;
  stopping = true;
  await server.stop();
  if (process.connected) process.disconnect();
}
for (const event of ['SIGINT', 'SIGTERM', 'disconnect'])
  process.on(event, stop);
