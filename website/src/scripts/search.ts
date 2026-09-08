import { safeResultUrl, decodeExcerpt } from './search-utils';
import { createPagefindClient } from './pagefind-client';
interface ResultData {
  url: string;
  meta: { title: string; section?: string; status?: string; kind?: string };
  plain_excerpt: string;
  sub_results?: { url: string; title: string; locations?: number[] }[];
}
interface Result {
  data(): Promise<ResultData>;
}
interface Pagefind {
  search(query: string): Promise<{ results: Result[] }>;
  options(options: { excerptLength: number }): Promise<unknown>;
}

function bounded<T>(work: Promise<T>): Promise<T> {
  return new Promise((resolve, reject) => {
    const timer = setTimeout(
      () => reject(new Error('Search timed out')),
      12000,
    );
    work.then(resolve, reject).finally(() => clearTimeout(timer));
  });
}

export function initializeSearch(root: HTMLElement) {
  if (root.dataset.ready) return;
  root.dataset.ready = 'true';
  const dialog = root.querySelector('dialog')!;
  const trigger = root.querySelector<HTMLButtonElement>('[data-open-modal]')!;
  const input = root.querySelector('input')!;
  const status = root.querySelector<HTMLElement>('[role="status"]')!;
  const results = root.querySelector<HTMLOListElement>('ol')!;
  const clear = root.querySelector<HTMLButtonElement>('[data-clear]')!;
  const more = root.querySelector<HTMLButtonElement>('[data-more]')!;
  const retry = root.querySelector<HTMLButtonElement>('[data-retry]')!;
  let returnFocus: HTMLElement = trigger;
  let generation = 0;
  let timer: ReturnType<typeof setTimeout>;
  let api: Promise<Pagefind> | undefined;
  let client: ReturnType<typeof createPagefindClient> | undefined;
  let matches: Result[] = [];
  let shown = 0;
  let navigating = false;
  const state = (name: string, message: string) => {
    root.dataset.state = name;
    status.textContent = message;
    results.setAttribute('aria-busy', String(name === 'loading'));
    retry.hidden = name !== 'error';
  };
  const load = () =>
    (api ||= bounded(
      (async () => {
        client = createPagefindClient();
        await client.options({ excerptLength: 28 });
        return client;
      })(),
    ));
  const reset = () => {
    matches = [];
    shown = 0;
    results.replaceChildren();
    more.hidden = true;
  };
  const render = (data: ResultData) => {
    const href = safeResultUrl(data.url, location.origin);
    if (!href) throw new Error('Unexpected search result URL');
    const li = document.createElement('li');
    const link = document.createElement('a');
    link.className = 'pagefind-ui__result-link';
    link.href = href;
    link.textContent = data.meta.title;
    const context = document.createElement('p');
    context.className = 'search-context';
    context.textContent = [
      ...new Set([data.meta.kind, data.meta.section, data.meta.status]),
    ]
      .filter(Boolean)
      .join(' · ');
    const excerpt = document.createElement('p');
    excerpt.className = 'pagefind-ui__result-excerpt';
    // Pagefind provides a plain excerpt. Never interpret source or query HTML.
    excerpt.textContent = decodeExcerpt(data.plain_excerpt);
    li.append(link, context, excerpt);
    const sections = document.createElement('div');
    sections.className = 'search-sections';
    const seen = new Set([href]);
    for (const sub of [...(data.sub_results || [])].sort(
      (a, b) => (b.locations?.length || 0) - (a.locations?.length || 0),
    )) {
      const url = safeResultUrl(sub.url, location.origin);
      if (!url || seen.has(url) || sub.title === data.meta.title) continue;
      seen.add(url);
      const a = document.createElement('a');
      a.href = url;
      a.textContent = sub.title;
      sections.append(a);
      if (sections.children.length === 2) break;
    }
    if (sections.children.length) li.append(sections);
    return li;
  };
  const showPage = async (id: number, append = false) => {
    const data = await bounded(
      Promise.all(
        matches.slice(shown, shown + 5).map((result) => result.data()),
      ),
    );
    if (id !== generation) return;
    const items = data.map(render);
    results.append(...items);
    shown += data.length;
    more.hidden = shown >= matches.length;
    state(
      matches.length ? 'results' : 'empty',
      matches.length
        ? `${matches.length} ${matches.length === 1 ? 'page' : 'pages'} found. Showing ${shown}.`
        : 'No results. Try a shorter term, or browse the topics below.',
    );
    if (append) items[0]?.querySelector('a')?.focus();
  };
  const fail = (id: number) => {
    if (id !== generation) return;
    client?.destroy();
    client = undefined;
    api = undefined;
    reset();
    state(
      'error',
      'Search could not load. Check your connection and retry, or browse the docs below.',
    );
  };
  const search = async (id: number) => {
    const query = input.value.trim();
    if (!query || id !== generation) return;
    state('loading', 'Searching documentation…');
    try {
      const engine = await load();
      if (id !== generation) return;
      const response = await bounded(engine.search(query));
      if (id !== generation) return;
      matches = response.results;
      await showPage(id);
    } catch {
      fail(id);
    }
  };
  const changed = () => {
    clearTimeout(timer);
    const id = ++generation;
    reset();
    clear.hidden = !input.value;
    state(
      'initial',
      input.value.trim()
        ? 'Searching documentation…'
        : 'Search by topic, command, or code identifier.',
    );
    if (input.value.trim()) {
      state('loading', 'Searching documentation…');
      timer = setTimeout(() => search(id), 180);
    }
  };
  const open = (target: HTMLElement) => {
    returnFocus = target;
    const sidebar = document.querySelector<HTMLElement>(
      '#starlight__sidebar:popover-open',
    );
    if (sidebar) {
      returnFocus = trigger;
      sidebar.hidePopover();
    }
    if (!dialog.open) dialog.showModal();
    document.body.setAttribute('data-search-modal-open', '');
    input.focus();
  };
  const saveForBack = () => {
    try {
      history.replaceState({ ...history.state, ochatSearch: input.value }, '');
    } catch {
      /* Browsing still works if history storage is blocked. */
    }
  };
  trigger.disabled = false;
  if (/(Mac|iPhone|iPad|iPod)/i.test(navigator.platform))
    root.querySelector('kbd')!.textContent = '⌘ K';
  trigger.addEventListener('click', () => open(trigger));
  root
    .querySelector('[data-close-modal]')!
    .addEventListener('click', () => dialog.close());
  dialog.addEventListener('click', (event) => {
    if (event.target === dialog) {
      const box = dialog.getBoundingClientRect();
      if (
        event.clientX < box.left ||
        event.clientX > box.right ||
        event.clientY < box.top ||
        event.clientY > box.bottom
      )
        dialog.close();
    }
    if ((event.target as Element).closest('a')) {
      navigating = true;
      saveForBack();
      dialog.close();
    }
  });
  dialog.addEventListener('close', () => {
    document.body.removeAttribute('data-search-modal-open');
    if (!navigating) {
      try {
        const { ochatSearch: _query, ...rest } = history.state || {};
        history.replaceState(rest, '');
      } catch {
        /* Optional state only. */
      }
    }
    if (returnFocus.isConnected) returnFocus.focus({ preventScroll: true });
  });
  window.addEventListener('keydown', (event) => {
    const target = event.target as HTMLElement;
    if (
      event.isComposing ||
      event.repeat ||
      event.altKey ||
      event.shiftKey ||
      target.closest('input, textarea, select, [contenteditable="true"]')
    )
      return;
    if ((event.ctrlKey || event.metaKey) && event.key.toLowerCase() === 'k') {
      event.preventDefault();
      if (dialog.open) dialog.close();
      else
        open(
          document.activeElement instanceof HTMLElement &&
            document.activeElement !== document.body
            ? document.activeElement
            : trigger,
        );
    }
  });
  dialog.addEventListener('keydown', (event) => {
    if (
      event.key !== 'Tab' ||
      event.ctrlKey ||
      event.metaKey ||
      event.isComposing
    )
      return;
    const controls = [
      ...dialog.querySelectorAll<HTMLElement>(
        'a[href], button:not([disabled]), input:not([disabled]), [tabindex="0"]',
      ),
    ].filter((element) => element.getClientRects().length > 0);
    // Cycle every control explicitly: native WebKit Tab preferences can skip
    // links, otherwise moving focus into browser chrome before our last link.
    if (!controls.length) return;
    const current = controls.indexOf(document.activeElement as HTMLElement);
    const next =
      (current + (event.shiftKey ? -1 : 1) + controls.length) % controls.length;
    event.preventDefault();
    controls[next].focus();
  });
  input.addEventListener('input', changed);
  input.addEventListener('keydown', (event) => {
    if (event.key === 'ArrowDown') {
      const link = results.querySelector('a');
      if (link) {
        event.preventDefault();
        link.focus();
      }
    }
  });
  root.querySelector('form')!.addEventListener('submit', (event) => {
    event.preventDefault();
    if (root.dataset.state === 'results') results.querySelector('a')?.click();
    else if (root.dataset.state !== 'loading') {
      changed();
      clearTimeout(timer);
      void search(generation);
    }
  });
  clear.addEventListener('click', () => {
    input.value = '';
    changed();
    input.focus();
  });
  retry.addEventListener('click', () => {
    api = undefined;
    client?.destroy();
    client = undefined;
    changed();
    input.focus();
  });
  more.addEventListener('click', async () => {
    const id = generation;
    more.hidden = true;
    state('loading', 'Loading more results…');
    try {
      await showPage(id, true);
    } catch {
      fail(id);
    }
  });
  const restore = () => {
    navigating = false;
    if (typeof history.state?.ochatSearch === 'string') {
      input.value = history.state.ochatSearch.slice(0, 300);
      open(trigger);
      changed();
    }
  };
  state('initial', 'Search by topic, command, or code identifier.');
  restore();
  window.addEventListener('pageshow', (event) => {
    if (event.persisted) restore();
  });
}
