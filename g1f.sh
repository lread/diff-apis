#/usr/bin/env bash
clojure -m diff-apis.main files \
        rewrite-clj-0.6.1.pretty.edn clj \
        rewrite-cljs-0.4.4.pretty.edn cljs \
        --report-format :asciidoc \
        --notes diff-notes/rewrite-cljs-and-rewrite-clj.adoc > g1f.adoc
