#/usr/bin/env bash
clojure -m diff-apis.main diff rewrite-cljc-1.0.0-alpha.edn clj rewrite-cljc-1.0.0-alpha.edn cljs --report-format :asciidoc > g4.adoc
