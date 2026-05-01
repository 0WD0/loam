#!/usr/bin/env node
import fs from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';
import { pathToFileURL, fileURLToPath } from 'node:url';

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
const assetsRoot = path.resolve(root, process.argv[2] || 'public/assets/tree-sitter');

async function exists(file) {
  try {
    await fs.access(file);
    return true;
  } catch {
    return false;
  }
}

function assetPath(url) {
  return path.join(assetsRoot, url.replace(/^\/assets\/tree-sitter\//, ''));
}

if (!await exists(path.join(assetsRoot, 'tree-sitter.js'))) {
  throw new Error(`missing Tree-sitter assets at ${assetsRoot}; run npm run copy-tree-sitter-assets`);
}

const runtimeModule = path.join(os.tmpdir(), `loam-tree-sitter-${process.pid}.mjs`);
await fs.copyFile(path.join(assetsRoot, 'tree-sitter.js'), runtimeModule);
const runtime = await import(pathToFileURL(runtimeModule));
await fs.rm(runtimeModule, { force: true });
const Parser = runtime.Parser ?? runtime.default ?? runtime;
const Language = runtime.Language ?? Parser.Language;
const Query = runtime.Query ?? Parser.Query;

if (!Parser?.init || !Language?.load) {
  throw new Error('Tree-sitter runtime does not expose Parser.init and Language.load');
}

await Parser.init({
  locateFile: () => path.join(assetsRoot, 'tree-sitter.wasm')
});

const manifest = JSON.parse(await fs.readFile(path.join(assetsRoot, 'manifest.json'), 'utf8'));

for (const [name, config] of Object.entries(manifest.languages || {})) {
  const language = await Language.load(assetPath(config.wasm));
  const parser = new Parser();
  parser.setLanguage(language);

  const tree = parser.parse(config.sample || '');
  const queryText = await fs.readFile(assetPath(config.query), 'utf8');
  const query = language.query ? language.query(queryText) : new Query(language, queryText);
  const captures = query.captures ? query.captures(tree.rootNode) : [];

  console.log(`${name}: parsed ${tree.rootNode.type}, ${captures.length} captures`);
}
