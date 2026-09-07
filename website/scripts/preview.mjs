import { preview } from 'astro';
const server = await preview({ server: { host: '127.0.0.1', port: 4321 } });
for (const signal of ['SIGINT', 'SIGTERM'])
  process.on(signal, async () => {
    await server.stop();
  });
