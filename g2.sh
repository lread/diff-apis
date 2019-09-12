#/usr/bin/env bash
clojure -m diff-apis.main diff rewrite-clj-0.6.1.edn clj rewrite-cljc-1.0.0-alpha.edn clj --report-format :asciidoc > g2.adoc
