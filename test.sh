#/usr/bin/env bash
clojure -m diff-apis.main projects \
        rewrite-clj 0.5.0 clj \
        rewrite-clj 0.6.1 clj \
        --exclude-namespace rewrite-clj.custom-zipper.core \
        --exclude-namespace rewrite-clj.custom-zipper.core2 \
        --exclude-with :no-doc \
        --exclude-with :skip-wiki \
        --report-format :asciidoc \
        > test.adoc
