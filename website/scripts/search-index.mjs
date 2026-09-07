import fs from 'node:fs/promises';
import { fileURLToPath } from 'node:url';
import * as pagefind from 'pagefind';
import { assertApiReferenceExcluded } from '../config/api-reference.mjs';

// Own the single final-build index pass. Starlight's built-in pass is disabled
// because it does not expose Pagefind's includeCharacters indexing option.
export default function searchIndex() {
  return {
    name: 'ochat-search-index',
    hooks: {
      'astro:build:done': async ({ dir, logger }) => {
        const checked = (response) => {
          if (response.errors.length)
            throw new Error(response.errors.join('\n'));
          return response;
        };
        try {
          // Fail before indexing, even if an accidental API copy has no HTML.
          assertApiReferenceExcluded({ files: await fs.readdir(dir) });
          try {
            await fs.access(new URL('pagefind/', dir));
            throw new Error(
              'Another indexer already wrote pagefind/: expected one indexing owner.',
            );
          } catch (error) {
            if (error.code !== 'ENOENT') throw error;
          }
          const options = { includeCharacters: '${}' };
          const { index } = checked(await pagefind.createIndex(options));
          const { page_count } = checked(
            await index.addDirectory({ path: fileURLToPath(dir) }),
          );
          checked(
            await index.writeFiles({
              outputPath: fileURLToPath(new URL('pagefind/', dir)),
            }),
          );
          await fs.writeFile(
            new URL('../.generated/search-index-report.json', import.meta.url),
            JSON.stringify(
              {
                stage: 'astro:build:done',
                owner: 'ochat-search-index',
                passes: 1,
                htmlFilesProcessed: page_count,
                options,
              },
              null,
              2,
            ) + '\n',
          );
          logger.info(
            `Pagefind: one final-build pass over ${page_count} HTML files.`,
          );
        } finally {
          await pagefind.close();
        }
      },
    },
  };
}
