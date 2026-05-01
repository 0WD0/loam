(use-modules (gnu packages base)
             (gnu packages bash)
             (gnu packages compression)
             (gnu packages elf)
             (gnu packages gcc)
             (guix build-system trivial)
             (guix download)
             (guix gexp)
             (guix git-download)
             ((guix licenses) #:prefix license:)
             (guix packages))

(define tree-sitter-version "0.26.8")
(define wasi-sdk-version "29.0")
(define here (dirname (current-filename)))

(define tree-sitter-javascript-commit
  "44c892e0be055ac465d5eeddae6d3e194424e7de")

(define tree-sitter-typescript-commit
  "f975a621f4e7f532fe322e13c4f79495e0a7b2e7")

(define tree-sitter-python-commit
  "293fdc02038ee2bf0e2e206711b69c90ac0d413f")

(define tree-sitter-scala-commit
  "38950b525c9dfc44c8b60d44bdd6e54217286ca8")

(define tree-sitter-clojure-commit
  "8ec8407eada5f29728d746a46cbe6115938b5422")

(define (github-source name url commit hash)
  (origin
    (method git-fetch)
    (uri (git-reference
           (url url)
           (commit commit)))
    (file-name (git-file-name name commit))
    (sha256 (base32 hash))))

(define asset-builder-source
  (local-file (string-append here "/loam/build-tree-sitter-assets.scm")
              "build-tree-sitter-assets.scm"))

(define tree-sitter-cli-source
  (origin
    (method url-fetch)
    (uri (string-append
          "https://github.com/tree-sitter/tree-sitter/releases/download/v"
          tree-sitter-version "/tree-sitter-cli-linux-x64.zip"))
    (sha256
     (base32 "0nx8s15nva51zqj4r0qm01idszvw11119dfjwzf0b3pfg4sdhxwk"))))

(define web-tree-sitter-source
  (origin
    (method url-fetch)
    (uri (string-append
          "https://github.com/tree-sitter/tree-sitter/releases/download/v"
          tree-sitter-version "/web-tree-sitter.tar.gz"))
    (sha256
     (base32 "0cwz6gmch9r4jnlhrdy8k1la3nxjh1bw10yazdp3bjcxf8nswsha"))))

(define tree-sitter-license-source
  (origin
    (method url-fetch)
    (uri (string-append
          "https://raw.githubusercontent.com/tree-sitter/tree-sitter/v"
          tree-sitter-version "/LICENSE"))
    (sha256
     (base32 "0y7w2jgv65aqif3805nrjcknsy57s0s7i6dsyi2j1dxn88qb9ky5"))))

(define wasi-sdk-source
  (origin
    (method url-fetch)
    (uri (string-append
          "https://github.com/WebAssembly/wasi-sdk/releases/download/wasi-sdk-29/"
          "wasi-sdk-" wasi-sdk-version "-x86_64-linux.tar.gz"))
    (sha256
     (base32 "0w813bywm3n87bk9bzydg207paflsgx8x5jbcbf9q4wxhyid3lc7"))))

(define tree-sitter-clojure-source
  (github-source
   "tree-sitter-clojure"
   "https://github.com/yogthos/tree-sitter-clojure"
   tree-sitter-clojure-commit
   "1syn580wdkmk980bzmxnbr55dwq3pg6q94cbwn22qgjb3vp0mx0k"))

(define tree-sitter-javascript-source
  (github-source
   "tree-sitter-javascript"
   "https://github.com/tree-sitter/tree-sitter-javascript"
   tree-sitter-javascript-commit
   "1qdjpfxw9z1icx3jc3k006yj76lcqydkvbk4ji3wk4xy854zz66q"))

(define tree-sitter-typescript-source
  (github-source
   "tree-sitter-typescript"
   "https://github.com/tree-sitter/tree-sitter-typescript"
   tree-sitter-typescript-commit
   "0rlhhqp9dv6y0iljb4bf90d89f07zkfnsrxjb6rvw985ibwpjkh9"))

(define tree-sitter-python-source
  (github-source
   "tree-sitter-python"
   "https://github.com/tree-sitter/tree-sitter-python"
   tree-sitter-python-commit
   "05kk1wlm5fgpgwqxw3m68sipkinw0gf2jq19cgq9cgp3agdwg58p"))

(define tree-sitter-scala-source
  (github-source
   "tree-sitter-scala"
   "https://github.com/tree-sitter/tree-sitter-scala"
   tree-sitter-scala-commit
   "1g4ia61ibs66qchmwnddg9x02k4ix08jvma238msv9wqb90dqx0a"))

(define loam-tree-sitter-assets
  (package
    (name "loam-tree-sitter-assets")
    (version "0.1.0")
    (source #f)
    (build-system trivial-build-system)
    (supported-systems '("x86_64-linux"))
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      '(begin
         (use-modules (guix build utils))
         (load (assoc-ref %build-inputs "asset-builder")))))
    (native-inputs
     `(("asset-builder" ,asset-builder-source)
       ("bash" ,bash-minimal)
       ("coreutils" ,coreutils)
       ("findutils" ,findutils)
       ("tar" ,tar)
       ("gzip" ,gzip)
       ("unzip" ,unzip)
       ("patchelf" ,patchelf)
       ("glibc" ,glibc)
       ("gcc:lib" ,gcc "lib")
       ("tree-sitter-cli" ,tree-sitter-cli-source)
       ("web-tree-sitter" ,web-tree-sitter-source)
       ("tree-sitter-license" ,tree-sitter-license-source)
       ("wasi-sdk" ,wasi-sdk-source)
       ("grammar-clojure" ,tree-sitter-clojure-source)
       ("grammar-javascript" ,tree-sitter-javascript-source)
       ("grammar-typescript" ,tree-sitter-typescript-source)
       ("grammar-python" ,tree-sitter-python-source)
       ("grammar-scala" ,tree-sitter-scala-source)))
    (home-page "https://github.com/tree-sitter/tree-sitter")
    (synopsis "Loam Tree-sitter browser assets")
    (description
     "Build the Tree-sitter runtime, grammar WebAssembly modules, highlight
queries, licenses, and manifests expected by Loam's browser highlighter.")
    (license (list license:expat license:cc0))))

loam-tree-sitter-assets
