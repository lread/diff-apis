#/usr/bin/env bash
clojure -m diff-apis.main diff \
        lread/rewrite-cljs-playground 1.0.0-alpha cljs \
        lread/rewrite-cljs-playground 1.0.0-alpha clj \
        --report-format :asciidoc > g4.adoc
