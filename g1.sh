#/usr/bin/env bash
clojure -m diff-apis.main diff \
        rewrite-clj 0.6.1 clj \
        rewrite-cljs 0.4.4 cljs \
        --report-format :asciidoc > g1.adoc
