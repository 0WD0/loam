import { mkdir, rm, copyFile } from 'node:fs/promises';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const root = path.resolve(__dirname, '..');
const outRoot = path.join(root, 'public', 'assets', 'tree-sitter');

const assetMap = {
  'tree-sitter.js': 'node_modules/web-tree-sitter/web-tree-sitter.js',
  'tree-sitter.wasm': 'node_modules/web-tree-sitter/web-tree-sitter.wasm',

  'languages/tree-sitter-clojure.wasm': 'node_modules/@yogthos/tree-sitter-clojure/tree-sitter-clojure.wasm',
  'languages/tree-sitter-javascript.wasm': 'node_modules/@vscode/tree-sitter-wasm/wasm/tree-sitter-javascript.wasm',
  'languages/tree-sitter-typescript.wasm': 'node_modules/@vscode/tree-sitter-wasm/wasm/tree-sitter-typescript.wasm',
  'languages/tree-sitter-python.wasm': 'node_modules/@vscode/tree-sitter-wasm/wasm/tree-sitter-python.wasm',

  'queries/clojure/highlights.scm': 'node_modules/@yogthos/tree-sitter-clojure/queries/highlights.scm',
  'queries/javascript/highlights.scm': 'node_modules/tree-sitter-javascript/queries/highlights.scm',
  'queries/typescript/highlights.scm': 'node_modules/tree-sitter-typescript/queries/highlights.scm',
  'queries/python/highlights.scm': 'node_modules/tree-sitter-python/queries/highlights.scm',

  'licenses/web-tree-sitter-MIT.txt': 'node_modules/web-tree-sitter/LICENSE',
  'licenses/vscode-tree-sitter-wasm-MIT.txt': 'node_modules/@vscode/tree-sitter-wasm/LICENSE',
  'licenses/tree-sitter-clojure.txt': 'node_modules/@yogthos/tree-sitter-clojure/COPYING.txt',
  'licenses/tree-sitter-javascript-MIT.txt': 'node_modules/tree-sitter-javascript/LICENSE',
  'licenses/tree-sitter-typescript-MIT.txt': 'node_modules/tree-sitter-typescript/LICENSE',
  'licenses/tree-sitter-python-MIT.txt': 'node_modules/tree-sitter-python/LICENSE'
};

async function ensureParent(file) {
  await mkdir(path.dirname(file), { recursive: true });
}

async function main() {
  await rm(outRoot, { recursive: true, force: true });
  for (const [targetRel, sourceRel] of Object.entries(assetMap)) {
    const source = path.join(root, sourceRel);
    const target = path.join(outRoot, targetRel);
    await ensureParent(target);
    await copyFile(source, target);
  }
  console.log(`Synced Tree-sitter assets -> ${path.relative(root, outRoot)}`);
}

main().catch((error) => {
  console.error(error);
  process.exit(1);
});
