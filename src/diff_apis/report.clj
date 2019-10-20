(ns diff-apis.report
  (:require [lambdaisland.deep-diff :as deep-diff]
            [diff-apis.report.asciidoc :as asciidoc]))

(defn report [diff opts]
  (case (:report-format opts)
    :deep-diff (deep-diff/pretty-print diff)
    :asciidoc (println (asciidoc/as-asciidoc diff (:notes opts)))
    (throw (ex-info (str "unsupported report format:" (:report-format opts)) {}))))
