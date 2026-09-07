import fs from 'node:fs/promises';
import path from 'node:path';
import { createHash } from 'node:crypto';
import { fileURLToPath } from 'node:url';
const site = fileURLToPath(new URL('../', import.meta.url));
export async function inventory(directory) {
  const files = [];
  async function walk(dir) {
    for (const item of await fs.readdir(dir, { withFileTypes: true })) {
      const filename = path.join(dir, item.name);
      if (item.isSymbolicLink())
        throw new Error(`Artifact symlink: ${filename}`);
      if (item.isDirectory()) await walk(filename);
      else if (item.isFile()) files.push(filename);
      else throw new Error(`Unsupported artifact entry: ${filename}`);
    }
  }
  await walk(directory);
  const hash = createHash('sha256');
  const entries = [];
  for (const filename of files.sort()) {
    const relative = path.relative(directory, filename);
    const bytes = await fs.readFile(filename);
    hash.update(relative);
    hash.update(bytes);
    entries.push({
      path: relative,
      bytes: bytes.length,
      sha256: createHash('sha256').update(bytes).digest('hex'),
    });
  }
  return { sha256: hash.digest('hex'), files: entries };
}
export async function verifyArtifact(directory) {
  const manifest = JSON.parse(
    await fs.readFile(path.join(directory, 'artifact.json'), 'utf8'),
  );
  const actual = await inventory(path.join(directory, 'dist'));
  if (
    actual.sha256 !== manifest.build.sha256 ||
    JSON.stringify(actual.files) !== JSON.stringify(manifest.files)
  )
    throw new Error('Artifact contents differ from the retained manifest');
  const config = await fs.readFile(path.join(directory, 'wrangler.jsonc'));
  if (
    createHash('sha256').update(config).digest('hex') !== manifest.configSha256
  )
    throw new Error('Artifact Wrangler configuration changed');
  return manifest;
}
export async function retainArtifact(
  directory,
  evidenceDirectory = path.join(site, '.generated'),
) {
  const build = JSON.parse(
    await fs.readFile(
      path.join(evidenceDirectory, 'build-evidence.json'),
      'utf8',
    ),
  );
  const actual = await inventory(path.join(site, 'dist'));
  if (build.result !== 'pass' || actual.sha256 !== build.sha256)
    throw new Error('Build evidence does not match current output');
  await fs.mkdir(directory); // Refuse to overwrite a retained artifact.
  await fs.cp(path.join(site, 'dist'), path.join(directory, 'dist'), {
    recursive: true,
  });
  await fs.copyFile(
    path.join(site, 'wrangler.jsonc'),
    path.join(directory, 'wrangler.jsonc'),
  );
  await fs.cp(evidenceDirectory, path.join(directory, 'evidence'), {
    recursive: true,
    filter: (source) =>
      source === evidenceDirectory ||
      (path.dirname(source) === evidenceDirectory &&
        /\.(json|md)$/.test(source)),
  });
  const config = await fs.readFile(path.join(directory, 'wrangler.jsonc'));
  await fs.writeFile(
    path.join(directory, 'artifact.json'),
    JSON.stringify(
      {
        version: 1,
        retainedAt: new Date().toISOString(),
        build,
        configSha256: createHash('sha256').update(config).digest('hex'),
        packageLockSha256: createHash('sha256')
          .update(await fs.readFile(path.join(site, 'package-lock.json')))
          .digest('hex'),
        files: actual.files,
      },
      null,
      2,
    ) + '\n',
  );
  return verifyArtifact(directory);
}
if (
  process.argv[1] &&
  path.resolve(process.argv[1]) === fileURLToPath(import.meta.url)
) {
  const [command, target, evidence] = process.argv.slice(2);
  if (!target || !['retain', 'verify'].includes(command))
    throw new Error(
      'Usage: node scripts/release-artifact.mjs retain|verify DIRECTORY',
    );
  const result = await (command === 'retain'
    ? retainArtifact(
        path.resolve(target),
        evidence ? path.resolve(evidence) : undefined,
      )
    : verifyArtifact(path.resolve(target)));
  console.log(
    JSON.stringify({
      result: 'pass',
      environment: result.build.environment,
      sha256: result.build.sha256,
      directory: path.resolve(target),
    }),
  );
}
