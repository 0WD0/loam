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

(define here (dirname (current-filename)))
(load (string-append here "/loam/tree-sitter-grammars.scm"))

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

(define grammar-registry-source
  (local-file (string-append here "/loam/tree-sitter-grammars.scm")
              "tree-sitter-grammars.scm"))

(define tree-sitter-cli-source
  (origin
    (method url-fetch)
    (uri (string-append
          "https://github.com/tree-sitter/tree-sitter/releases/download/v"
          %loam-tree-sitter-version "/tree-sitter-cli-linux-x64.zip"))
    (sha256
     (base32 %loam-tree-sitter-cli-hash))))

(define web-tree-sitter-source
  (origin
    (method url-fetch)
    (uri (string-append
          "https://github.com/tree-sitter/tree-sitter/releases/download/v"
          %loam-tree-sitter-version "/web-tree-sitter.tar.gz"))
    (sha256
     (base32 %loam-web-tree-sitter-hash))))

(define tree-sitter-license-source
  (origin
    (method url-fetch)
    (uri (string-append
          "https://raw.githubusercontent.com/tree-sitter/tree-sitter/v"
          %loam-tree-sitter-version "/LICENSE"))
    (sha256
     (base32 %loam-tree-sitter-license-hash))))

(define wasi-sdk-source
  (origin
    (method url-fetch)
    (uri (string-append
          "https://github.com/WebAssembly/wasi-sdk/releases/download/wasi-sdk-"
          %loam-wasi-sdk-release "/wasi-sdk-" %loam-wasi-sdk-version
          "-x86_64-linux.tar.gz"))
    (sha256
     (base32 %loam-wasi-sdk-hash))))

(define (grammar-source-input grammar)
  (let* ((name (car grammar))
         (spec (cdr grammar))
         (input (assoc-ref spec 'input))
         (repo (assoc-ref spec 'repo))
         (commit (assoc-ref spec 'commit))
         (hash (assoc-ref spec 'source-hash)))
    (list input
          (github-source (string-append "tree-sitter-" (symbol->string name))
                         repo
                         commit
                         hash))))

(define grammar-source-inputs
  (map grammar-source-input %loam-tree-sitter-grammars))

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
       ("grammar-registry" ,grammar-registry-source)
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
       ,@grammar-source-inputs))
    (home-page "https://github.com/tree-sitter/tree-sitter")
    (synopsis "Loam Tree-sitter browser assets")
    (description
     "Build the Tree-sitter runtime, grammar WebAssembly modules, highlight
queries, licenses, and manifests expected by Loam's browser highlighter.")
    (license (list license:expat license:cc0))))

loam-tree-sitter-assets
