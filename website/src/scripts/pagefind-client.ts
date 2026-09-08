// This adapter changes only the error boundary. It neither reranks results nor
// stores queries outside the current browser session/history entry.
export function createPagefindClient() {
  const worker = new Worker(new URL('./pagefind-worker.ts', import.meta.url), {
    type: 'module',
  });
  let sequence = 0;
  const pending = new Map<
    number,
    { resolve: (value: any) => void; reject: (error: Error) => void }
  >();
  const destroy = () => {
    worker.terminate();
    for (const request of pending.values())
      request.reject(new Error('Search stopped'));
    pending.clear();
  };
  worker.onerror = (event) => {
    event.preventDefault();
    destroy();
  };
  worker.onmessageerror = destroy;
  worker.onmessage = ({ data }) => {
    const request = pending.get(data.id);
    if (!request) return;
    pending.delete(data.id);
    if (data.error) request.reject(new Error(data.error));
    else request.resolve(data.result);
  };
  const call = (method: string, value: unknown): Promise<any> =>
    new Promise((resolve, reject) => {
      const id = ++sequence;
      pending.set(id, { resolve, reject });
      worker.postMessage({ id, method, value });
    });
  return {
    options: (value: { excerptLength: number }) => call('options', value),
    search: async (query: string) => ({
      results: (await call('search', query)).map(
        (key: { sequence: number; index: number }) => ({
          data: () => call('data', key),
        }),
      ),
    }),
    destroy,
  };
}
