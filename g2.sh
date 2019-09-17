#/usr/bin/env bash
clojure -m diff-apis.main projects \
        rewrite-clj 0.6.1 clj \
        lread/rewrite-cljs-playground 1.0.0-alpha clj \
        --report-format :asciidoc > g2.adoc
