#!/usr/bin/env node
import fs from 'node:fs';
import path from 'node:path';
import { spawnSync } from 'node:child_process';
import { fileURLToPath } from 'node:url';

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const target = path.join(root, 'public', 'assets', 'tree-sitter');

function hasRuntime(dir) {
  return fs.existsSync(path.join(dir, 'tree-sitter.js')) &&
    fs.existsSync(path.join(dir, 'tree-sitter.wasm'));
}

function resolveAssetsRoot(input) {
  const candidate = path.resolve(root, input);
  if (hasRuntime(candidate)) return candidate;

  const nested = path.join(candidate, 'assets', 'tree-sitter');
  if (hasRuntime(nested)) return nested;

  throw new Error(`not a Loam Tree-sitter assets directory: ${input}`);
}

function guixBuildAssets() {
  const args = ['build', '-f', 'guix/loam-tree-sitter-assets.scm'];
  const result = spawnSync('guix', args, {
    cwd: root,
    encoding: 'utf8',
    stdio: ['ignore', 'pipe', 'inherit']
  });

  if (result.error?.code === 'ENOENT') {
    throw new Error('guix not found; install Guix or set LOAM_TREE_SITTER_ASSETS=/path/to/assets/tree-sitter');
  }
  if (result.error) throw result.error;
  if (result.status !== 0) {
    throw new Error(`guix ${args.join(' ')} failed with status ${result.status}`);
  }

  const storePath = result.stdout.trim().split(/\r?\n/).filter(Boolean).at(-1);
  if (!storePath) throw new Error('guix build did not print an output path');
  return resolveAssetsRoot(storePath);
}

const source = process.env.LOAM_TREE_SITTER_ASSETS
  ? resolveAssetsRoot(process.env.LOAM_TREE_SITTER_ASSETS)
  : guixBuildAssets();

fs.rmSync(target, { recursive: true, force: true });
fs.mkdirSync(path.dirname(target), { recursive: true });
fs.cpSync(source, target, { recursive: true });

console.log(`Copied Tree-sitter assets from ${source}`);
console.log(`Wrote ${path.relative(root, target)}`);
