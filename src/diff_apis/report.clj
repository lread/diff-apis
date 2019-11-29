(ns diff-apis.report
  (:require [lambdaisland.deep-diff :as deep-diff]
            [diff-apis.report.asciidoc :as asciidoc]))

(defn- deep-diff-report [diff {:keys [report-filename]}]
  (deep-diff/pretty-print diff (deep-diff/printer {:output-file report-filename})))

;; TODO: maybe add an :ouput-file to asciidoc report opts
(defn- asciidoc-report [diff {:keys [notes report-filename]}]
  (let [r (asciidoc/as-asciidoc diff notes)]
    (spit (or report-filename *out*) r)))

(defn report [diff {:keys [report-format] :as opts}]
  (case report-format
    :deep-diff (deep-diff-report diff opts)
    :asciidoc (asciidoc-report diff opts)
    (throw (ex-info (str "unsupported report format:" (:report-format opts)) {}))))
