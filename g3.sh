#/usr/bin/env bash
clojure -m diff-apis.main diff \
        rewrite-cljs 0.4.4 cljs \
        lread/rewrite-cljs-playground 1.0.0-alpha cljs \
        --report-format :asciidoc > g3.adoc
