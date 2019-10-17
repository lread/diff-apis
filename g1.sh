#/usr/bin/env bash
clojure -m diff-apis.main projects \
        rewrite-clj 0.6.1 clj \
        rewrite-cljs 0.4.4 cljs \
        --report-format :asciidoc \
        --notes diff-notes/rewrite-cljs-and-rewrite-clj.adoc > g1.adoc
