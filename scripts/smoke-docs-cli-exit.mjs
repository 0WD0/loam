import { chmod, mkdir, mkdtemp, readFile, rm, writeFile } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import path from 'node:path';
import { spawnSync } from 'node:child_process';

const repoRoot = process.cwd();
const scratch = await mkdtemp(path.join(tmpdir(), 'loam-docs-cli-exit-'));
const binDir = path.join(scratch, 'bin');
const outputDir = path.join(scratch, 'output');
const fakeJj = path.join(binDir, 'jj');

try {
  await mkdir(binDir);
  await writeFile(fakeJj, '#!/bin/sh\nprintf "test-change\\ntest-commit\\n"\n');
  await chmod(fakeJj, 0o755);

  const startedAt = performance.now();
  const result = spawnSync(
    'clojure',
    [
      '-M',
      '-m',
      'loam.emit.starlight',
      '--repo-root',
      repoRoot,
      '--output-dir',
      outputDir,
      'test/fixtures/consumer-envelope-v1.edn',
    ],
    {
      cwd: repoRoot,
      encoding: 'utf8',
      timeout: 8_000,
      env: { ...process.env, PATH: `${binDir}${path.delimiter}${process.env.PATH}` },
    },
  );
  const elapsedMs = Math.round(performance.now() - startedAt);

  if (result.error && result.status !== 0) {
    throw new Error(`docs CLI did not exit promptly (${elapsedMs}ms): ${result.error.message}`);
  }
  if (result.status !== 0) {
    throw new Error(
      `docs CLI exited ${result.status}\nstdout:\n${result.stdout}\nstderr:\n${result.stderr}`,
    );
  }
  if (!result.stdout.includes('Compiled docs')) {
    throw new Error(`docs CLI omitted its success summary:\n${result.stdout}`);
  }

  const manifest = JSON.parse(await readFile(path.join(outputDir, 'manifest.json'), 'utf8'));
  if (manifest.build.vcs.changeId !== 'test-change' || manifest.pages.length !== 1) {
    throw new Error('docs CLI prompt-exit smoke test produced an unexpected manifest');
  }

  console.log(`docs CLI prompt-exit smoke passed in ${elapsedMs}ms`);
} finally {
  await rm(scratch, { recursive: true, force: true });
}
