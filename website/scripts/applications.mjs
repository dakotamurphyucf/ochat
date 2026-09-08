import fs from 'node:fs/promises';
import path from 'node:path';
import { z } from 'zod';
import { containedFile, rendered } from './content-lib.mjs';
import { digest } from './provenance.mjs';
const text = z.string().min(1);
const schema = z.array(
  z
    .object({
      id: text,
      title: text,
      category: z.enum(['Code', 'Documentation', 'Research', 'Automation']),
      summary: text,
      input: text,
      output: text,
      example: text,
      level: text,
      steps: z.array(text).length(3),
      preview: z.array(text).min(1),
      tutorial: text,
      benefit: text,
    })
    .strict(),
);
export async function applicationReport({
  root,
  siteRoot,
  entries,
  examples,
  facts,
  isProduction,
}) {
  const apps = schema.parse(
    JSON.parse(
      await fs.readFile(
        path.join(siteRoot, 'config/applications.json'),
        'utf8',
      ),
    ),
  );
  if (new Set(apps.map((a) => a.id)).size !== apps.length)
    throw new Error('Duplicate application');
  const source = 'docs-src/examples/applications/docs-review/recording.json';
  const raw = await fs.readFile(await containedFile(root, source), 'utf8');
  if (isProduction && facts.source(source, raw).sourceModified)
    throw new Error('Production requires committed recording bytes');
  const recording = JSON.parse(raw);
  if (
    recording.version !== 1 ||
    typeof recording.liveProvider !== 'boolean' ||
    !recording.result ||
    !Array.isArray(recording.requests)
  )
    throw new Error('Invalid showcase recording');
  for (const [source, expected] of Object.entries(
    recording.runtimeSources || {},
  )) {
    if (
      digest(await fs.readFile(await containedFile(root, source))) !== expected
    )
      throw new Error(`Stale recording runtime: ${source}`);
  }
  const example = examples.find((e) => e.id === 'docs-review');
  if (!example)
    throw new Error('Recording requires documentation-review source');
  for (const f of example.files.filter((f) => f.role !== 'notice')) {
    if (recording.sourceHashes[f.path] !== f.sha256)
      throw new Error(`Stale recording source: ${f.path}`);
  }
  const calls = recording.requests
    .flatMap((e) => e.response.output)
    .filter((i) => i.type === 'function_call');
  if (
    JSON.stringify(calls.map((i) => i.name)) !==
    JSON.stringify(['read_file', 'review_docs'])
  )
    throw new Error('Recording must show file read and specialist delegation');
  if (recording.requests.length !== 4)
    throw new Error('Unexpected showcase request count');
  const textOutput = (response) =>
    response.output
      .filter((i) => i.type === 'message')
      .flatMap((i) => i.content)
      .filter((c) => c.type === 'output_text')
      .map((c) => c.text)
      .join('\n');
  const fileOutput = recording.requests[1].request.input.find(
    (i) => i.type === 'function_call_output',
  )?.output;
  const fileBody = example.files.find(
    (f) => f.path === 'reference/project.txt',
  )?.content;
  if (
    typeof fileOutput !== 'string' ||
    !fileBody ||
    !fileOutput.includes(fileBody.trim())
  )
    throw new Error('Recording lost the actual file-tool output');
  if (recording.requests[2].request.tools?.length)
    throw new Error('Recorded specialist must have no tools');
  if (textOutput(recording.requests[3].response) !== recording.result)
    throw new Error('Recording result differs from captured response');
  if (recording.liveProvider !== (recording.provider === 'live OpenAI'))
    throw new Error('Recording provider label mismatch');
  const steps = [
    {
      title: 'Read the documentation',
      actor: 'Explorer · read_file',
      input: calls[0].arguments,
      output: fileOutput,
      file: 'explorer.chatmd',
    },
    {
      title: 'Ask the specialist',
      actor: 'Explorer → documentation reviewer',
      input: JSON.parse(calls[1].arguments).input,
      output: textOutput(recording.requests[2].response),
      file: 'docs-reviewer.chatmd',
    },
    {
      title: 'Return the report',
      actor: 'Explorer → maintainer',
      input: 'Synthesize the specialist’s findings and identify the source.',
      output: recording.result,
      file: 'recorded-run.chatmd',
    },
  ];
  return {
    applications: apps.map((a) => {
      const page = entries.find((e) => e.id === `applications/${a.id}`);
      const tutorial = entries.find((e) => e.id === a.tutorial);
      const example = examples.find((e) => e.id === a.example);
      if (
        !page ||
        !rendered(page) ||
        !tutorial ||
        !rendered(tutorial) ||
        !example
      )
        throw new Error(`Unpublished application dependency: ${a.id}`);
      return {
        ...a,
        route: page.route,
        tutorialRoute: tutorial.route,
        tutorialTitle: tutorial.title,
        sourceRoute: example.tutorialRoute,
        exampleTitle: example.title,
      };
    }),
    recording: { ...recording, steps, source, sha256: digest(raw) },
  };
}
