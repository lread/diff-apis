(ns diff-apis.main
  (:require [cli-matic.core :as climatic]
            [clojure.spec.alpha :as spec]
            [diff-apis.diff :as diff]
            [diff-apis.report :as report]))

(defn diff-cmd
  [{:keys [project1 version1 language1 project2 version2 language2 report-format include exclude-namespace]}]
  (-> (diff/diff-projects {:project project1 :version version1 :lang language1}
                          {:project project2 :version version2 :lang language2}
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
                  :opts [{:option  "project1"
                          :short   0
                          :as      "project 1 - specify group-id/artifact-id of project 1"
                          :type    :string
                          :default :present}
                         {:option  "version1"
                          :short   1
                          :as      "version of project 1"
                          :type    :string
                          :default :present}
                         {:option  "language1"
                          :short   2
                          :as      "language for project 1 (clj or cljs)"
                          :type    :string
                          :spec    ::language
                          :default :present}
                         {:option  "project2"
                          :short   3
                          :as      "project 2 - specify group-id/artifact-id of project 2"
                          :type    :string
                          :default :present}
                         {:option  "version2"
                          :short   4
                          :as      "version of project 2"
                          :type    :string
                          :default :present}
                         {:option  "language2"
                          :short   5
                          :as      "language for project 2 (clj or cljs)"
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
