import { z } from 'zod';
const text = z.string().min(1);
const revision = z.string().regex(/^[a-f0-9]{40}$/);
export const manifestSchema = z.array(
  z
    .object({
      id: text,
      source: text,
      disposition: z.enum([
        'publish',
        'compatibility',
        'bridge',
        'repository-only',
        'deferred',
      ]),
      route: text.optional(),
      title: text,
      description: text.optional(),
      section: text.optional(),
      order: z.number().int().optional(),
      audience: z
        .array(z.enum(['author', 'operator', 'integrator', 'contributor']))
        .optional(),
      kind: z
        .enum(['tutorial', 'concept', 'guide', 'reference', 'index'])
        .optional(),
      status: z
        .enum(['current', 'experimental', 'compatibility', 'historical'])
        .optional(),
      navigation: z.boolean().optional(),
      search: z.boolean().optional(),
      sitemap: z.boolean().optional(),
      noindex: z.boolean().optional(),
      aliases: z.array(text).optional(),
      fragmentAliases: z.record(text, text).optional(),
      related: z.array(text).optional(),
      verifiedAt: z.iso.date().optional(),
      verifiedCommit: revision.optional(),
      reviewNote: text.optional(),
      provenance: z.enum(['authored', 'generated-from-code']),
      generatedBy: text.optional(),
      sourceCommit: revision.optional(),
      verification: z
        .enum([
          'offline-checked',
          'live-checked',
          'known-limitation',
          'not-checked',
        ])
        .optional(),
      limitationSource: text.optional(),
    })
    .strict(),
);
