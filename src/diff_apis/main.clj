(ns diff-apis.main
  (:require [cli-matic.core :as climatic]
            [clojure.spec.alpha :as spec]
            [diff-apis.diff :as diff]
            [diff-apis.report :as report]))

(defn diff-cmd
  [{:keys [filename1 language1 filename2 language2 report-format include exclude-namespace]}]
  (-> (diff/diff-files [filename1 language1] [filename2 language2]
                       {:include include :exclude-namespaces exclude-namespace})
      (report/report report-format)))

(spec/def ::language #{"clj" "cljs"})
(spec/def ::include #{:all :changed-publics})
(spec/def ::report-format #{:asciidoc :deep-diff})

(def climatic-config
  {:app         {:command     "diff-apis"
                 :description "diff apis from cljdoc-analyzer outputs"
                 :version     "0.0.1"}

   :commands    [{:command     "diff"
                  :description "Diffs two apis"
                  :opts [{:option  "filename1"
                          :short   0
                          :as      "filename 1"
                          :type    :string
                          :default :present}
                         {:option  "language1"
                          :short   1
                          :as      "language 1 (clj or cljs)"
                          :type    :string
                          :spec    ::language
                          :default :present}
                         {:option  "filename2"
                          :short   2
                          :as      "filename 2"
                          :type    :string
                          :default :present}
                         {:option  "language2"
                          :short   3
                          :as      "language 2 (clj or cljs)"
                          :type    :string
                          :spec    ::language
                          :default :present}
                         {:option  "include"
                          :as      "Either :all or just :changed-publics defaults to :change-publics"
                          :type    :keyword
                          :spec    ::include
                          :default :changed-publics}
                         {:option   "exclude-namespace"
                          :as       "Fully qualified namespace to exclude from diff. Repeat for multiple"
                          :multiple true
                          :type     :string}
                         {:option  "report-format"
                          :as      "Either :asciidoc or :deep-diff, defaults to :deep-diff"
                          :type    :keyword
                          :spec    ::report-format
                          :default :deep-diff}]
                  :runs diff-cmd}]})


(defn -main [& args]
  (climatic/run-cmd args climatic-config))


(comment
  (require '[diff-apis/diff :as diff-apis])
  (require '[lambdaisland.deep-diff.diff :as deep-diff])

  ;; goal 1 diff rewrite-clj against rewrite-cljs
  (def g1 (diff-apis/diff-files ["rewrite-clj-0.6.1.edn" "clj"]
                                ["rewrite-cljs-0.4.4.edn" "cljs"]))
  (deep-diff/pretty-print g1)

  ;; goal 2 diff rewrite-cljc clj with rewrite-cljc cljs
  (def g2 (diff-apis/diff-files ["rewrite-cljc-1.0.0-alpha.edn" "clj"]
                                ["rewrite-cljc-1.0.0-alpha.edn" "cljs"]))
  (deep-diff/pretty-print g2)

  ;; goal 3 diff rewrite-clj with rewrite-cljc clj
  (def g3 (diff-apis/diff-files ["rewrite-clj-0.6.1.edn" "clj"]
                                ["rewrite-cljc-1.0.0-alpha.edn" "clj"]))

  (deep-diff/pretty-print g3)
  (spit "g3-test.adoc" (as-asciidoc g3))


  )
