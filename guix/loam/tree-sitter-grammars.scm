;; Single source of truth for Loam's Tree-sitter browser asset set.
;; This file is intentionally plain Scheme data so it can be loaded both by
;; `guix/loam-tree-sitter-assets.scm` at package-evaluation time and by the
;; build script inside the derivation.

(define %loam-tree-sitter-version "0.26.8")
(define %loam-wasi-sdk-version "29.0")
(define %loam-wasi-sdk-release "29")

(define %loam-tree-sitter-cli-hash
  "0nx8s15nva51zqj4r0qm01idszvw11119dfjwzf0b3pfg4sdhxwk")

(define %loam-web-tree-sitter-hash
  "0cwz6gmch9r4jnlhrdy8k1la3nxjh1bw10yazdp3bjcxf8nswsha")

(define %loam-tree-sitter-license-hash
  "0y7w2jgv65aqif3805nrjcknsy57s0s7i6dsyi2j1dxn88qb9ky5")

(define %loam-wasi-sdk-hash
  "0w813bywm3n87bk9bzydg207paflsgx8x5jbcbf9q4wxhyid3lc7")

(define %loam-tree-sitter-grammars
  '((clojure
     (input . "grammar-clojure")
     (version . "0.0.14")
     (tag . #f)
     (commit . "8ec8407eada5f29728d746a46cbe6115938b5422")
     (source-hash . "1syn580wdkmk980bzmxnbr55dwq3pg6q94cbwn22qgjb3vp0mx0k")
     (repo . "https://github.com/yogthos/tree-sitter-clojure")
     (grammar-dir . ".")
     (query . "queries/highlights.scm")
     (license . "COPYING.txt")
     (license-output . "tree-sitter-clojure-CC0.txt")
     (aliases . ("clj" "cljs" "cljc" "edn"))
     (sample . "(defn foo [] :ok)\n"))

    (javascript
     (input . "grammar-javascript")
     (version . "0.25.0")
     (tag . "v0.25.0")
     (commit . "44c892e0be055ac465d5eeddae6d3e194424e7de")
     (source-hash . "1qdjpfxw9z1icx3jc3k006yj76lcqydkvbk4ji3wk4xy854zz66q")
     (repo . "https://github.com/tree-sitter/tree-sitter-javascript")
     (grammar-dir . ".")
     (query . "queries/highlights.scm")
     (license . "LICENSE")
     (license-output . "tree-sitter-javascript-MIT.txt")
     (aliases . ("js" "jsx" "node"))
     (sample . "const answer = 42;\n"))

    (typescript
     (input . "grammar-typescript")
     (version . "0.23.2")
     (tag . "v0.23.2")
     (commit . "f975a621f4e7f532fe322e13c4f79495e0a7b2e7")
     (source-hash . "0rlhhqp9dv6y0iljb4bf90d89f07zkfnsrxjb6rvw985ibwpjkh9")
     (repo . "https://github.com/tree-sitter/tree-sitter-typescript")
     (grammar-dir . "typescript")
     (query . "queries/highlights.scm")
     (license . "LICENSE")
     (license-output . "tree-sitter-typescript-MIT.txt")
     (aliases . ("ts"))
     (sample . "const answer: number = 42;\n"))

    (python
     (input . "grammar-python")
     (version . "0.25.0")
     (tag . "v0.25.0")
     (commit . "293fdc02038ee2bf0e2e206711b69c90ac0d413f")
     (source-hash . "05kk1wlm5fgpgwqxw3m68sipkinw0gf2jq19cgq9cgp3agdwg58p")
     (repo . "https://github.com/tree-sitter/tree-sitter-python")
     (grammar-dir . ".")
     (query . "queries/highlights.scm")
     (license . "LICENSE")
     (license-output . "tree-sitter-python-MIT.txt")
     (aliases . ("py"))
     (sample . "def answer():\n    return 42\n"))

    (scala
     (input . "grammar-scala")
     (version . "0.26.0")
     (tag . "v0.26.0")
     (commit . "38950b525c9dfc44c8b60d44bdd6e54217286ca8")
     (source-hash . "1g4ia61ibs66qchmwnddg9x02k4ix08jvma238msv9wqb90dqx0a")
     (repo . "https://github.com/tree-sitter/tree-sitter-scala")
     (grammar-dir . ".")
     (query . "queries/highlights.scm")
     (license . "LICENSE")
     (license-output . "tree-sitter-scala-MIT.txt")
     (aliases . ("sc"))
     (sample . "object Main extends App { println(42) }\n"))))
