#/usr/bin/env bash
clojure -m diff-apis.main projects \
        lread/rewrite-cljs-playground 1.0.0-alpha cljs \
        lread/rewrite-cljs-playground 1.0.0-alpha clj \
        --exclude-namespace rewrite-clj.potemkin.clojure \
        --exclude-with :no-doc \
        --exclude-with :skip-wiki \
        --report-format :asciidoc \
        --notes diff-notes/rewrite-cljc-nodoc.adoc > g4-nodoc.adoc
