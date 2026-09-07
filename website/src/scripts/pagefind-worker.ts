/// <reference lib="webworker" />
// Pagefind 1.5.2 swallows index-chunk fetch failures. Run its supported API in
// this dedicated worker and observe fetch failures here, isolated from the page.
// Ranking, tokenization, indexing, and excerpt generation remain Pagefind's.
const scope = self as unknown as DedicatedWorkerGlobalScope;
const originalFetch = scope.fetch.bind(scope);
let fetchFailed = false;
scope.fetch = async (...args: Parameters<typeof fetch>) => {
  try {
    const response = await originalFetch(...args);
    if (!response.ok) throw new Error('Search asset unavailable');
    return response;
  } catch (error) {
    fetchFailed = true;
    throw error;
  }
};
interface EngineResult {
  data(): Promise<unknown>;
}
interface Engine {
  options(options: unknown): Promise<unknown>;
  search(query: string): Promise<{ results: EngineResult[] }>;
}
let engine: Engine;
let sequence = 0;
let active: EngineResult[] = [];
let queue = Promise.resolve();
scope.onmessage = ({ data: { id, method, value } }) => {
  // Serialize API operations so a failed fetch belongs to one request, including
  // failures Pagefind catches internally. The main thread discards stale queries.
  queue = queue.then(async () => {
    fetchFailed = false;
    try {
      let result: unknown;
      if (method === 'options') {
        const path = '/pagefind/pagefind.js';
        engine = await import(/* @vite-ignore */ path);
        await engine.options(value);
      } else if (method === 'search') {
        const found = await engine.search(value);
        active = found.results;
        sequence++;
        result = active.map((_, index) => ({ sequence, index }));
      } else if (method === 'data') {
        if (value.sequence !== sequence || !active[value.index])
          throw new Error('Obsolete search result');
        result = await active[value.index].data();
      } else {
        throw new Error('Unknown search operation');
      }
      if (fetchFailed) throw new Error('Search asset unavailable');
      scope.postMessage({ id, result });
    } catch {
      scope.postMessage({ id, error: 'Search could not load' });
    }
  });
};
export {};
