#/usr/bin/env bash
clojure -m diff-apis.main diff rewrite-cljs-0.4.4.edn clj rewrite-cljc-1.0.0-alpha.edn cljs --report-format :asciidoc > g3.adoc
