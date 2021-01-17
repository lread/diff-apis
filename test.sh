#/usr/bin/env bash
clojure -M -m diff-apis.main projects \
        rewrite-clj 0.4.13 clj \
        rewrite-clj 0.6.1 clj \
        --exclude-namespace rewrite-clj.custom-zipper.core \
        --exclude-namespace rewrite-clj.custom-zipper.core2 \
        --exclude-with :no-doc \
        --exclude-with :skip-wiki \
        --report-format :asciidoc \
        --arglists-by :arity-only \
        > test.adoc
