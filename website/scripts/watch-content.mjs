import chokidar from 'chokidar';
export async function watchContent(
  paths,
  { regenerate, onError, delay = 180 },
) {
  const watcher = chokidar.watch(paths, { ignoreInitial: true });
  let timer,
    closed = false,
    chain = Promise.resolve();
  const pending = new Set();
  watcher.on('all', (_event, file) => {
    if (closed) return;
    pending.add(file);
    clearTimeout(timer);
    timer = setTimeout(() => {
      const changed = [...pending];
      pending.clear();
      chain = chain
        .then(() => (closed ? undefined : regenerate(changed)))
        .catch(async (error) => {
          if (closed) return;
          closed = true;
          clearTimeout(timer);
          await watcher.close();
          await onError(error);
        });
    }, delay);
  });
  await new Promise((resolve, reject) => {
    watcher.once('ready', resolve);
    watcher.once('error', reject);
  });
  return {
    async close() {
      closed = true;
      clearTimeout(timer);
      await watcher.close();
      await chain;
    },
  };
}
