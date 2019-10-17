#/usr/bin/env bash
clojure -m diff-apis.main projects \
        rewrite-cljs 0.4.4 cljs \
        lread/rewrite-cljs-playground 1.0.0-alpha cljs \
        --report-format :asciidoc \
        --notes diff-notes/rewrite-cljs-and-rewrite-cljc-cljs.adoc > g3.adoc
