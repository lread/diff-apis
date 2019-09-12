(ns diff-apis.report
  (:require [lambdaisland.deep-diff :as deep-diff]
            [diff-apis.report.asciidoc :as asciidoc]))

(defn report [diff format]
  (case format
    :deep-diff (deep-diff/pretty-print diff)
    :asciidoc (println (asciidoc/as-asciidoc diff))
    (throw (ex-info (str "unsupported report format:" format) {}))))
