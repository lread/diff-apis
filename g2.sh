#/usr/bin/env bash
clojure -m diff-apis.main projects \
        rewrite-clj 0.6.1 clj \
        lread/rewrite-cljs-playground 1.0.0-alpha clj \
        --report-format :asciidoc \
        --notes diff-notes/rewrite-clj-and-rewrite-cljc-clj.adoc > g2.adoc
