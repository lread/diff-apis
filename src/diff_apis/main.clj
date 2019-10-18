(ns diff-apis.main
  (:require [cli-matic.core :as climatic]
            [clojure.spec.alpha :as spec]
            [diff-apis.diff :as diff]
            [diff-apis.report :as report]))

(defn diff-api-projects-cmd
  [{:keys [project1 version1 language1
           project2 version2 language2
           report-format include exclude-namespace exclude-with notes]}]
  (-> (diff/diff-projects {:project project1 :version version1 :lang language1}
                          {:project project2 :version version2 :lang language2}
                          {:include include :exclude-namespaces exclude-namespace :exclude-with exclude-with})
      (report/report report-format notes)))

(defn diff-api-files-cmd
  [{:keys [filename1 language1
           filename2 language2
           report-format include exclude-namespace exclude-with notes]}]
  (-> (diff/diff-files {:filename filename1 :lang language1}
                       {:filename filename2 :lang language2}
                       {:include include :exclude-namespaces exclude-namespace :exclude-with exclude-with})
      (report/report report-format notes)))

(spec/def ::language #{"clj" "cljs"})
(spec/def ::include #{:all :changed-publics})
(spec/def ::report-format #{:asciidoc :deep-diff})

;; I don't want to make these global options to allow for future commands that may not care about these
(def api-diff-options
  [{:option  "include"
    :as      "Either :all or just :changed-publics defaults to :changed-publics"
    :type    :keyword
    :spec    ::include
    :default :changed-publics}
   {:option   "exclude-namespace"
    :as       "Fully qualified namespace to exclude from diff. Repeat for multiple"
    :multiple true
    :type     :string}
   {:option   "exclude-with"
    :as       "Exclude namespaces and publics with metadata key present"
    :type     :keyword
    :multiple true}
   {:option  "report-format"
    :as      "Either :asciidoc or :deep-diff, defaults to :deep-diff"
    :type    :keyword
    :spec    ::report-format
    :default :deep-diff}
   {:option  "notes"
    :as      "Filename containing notes to include in :asciidoc report"
    :type    :slurplines}] )

(def climatic-config
  {:app         {:command     "diff-api"
                 :description "diff apis from cljdoc-analyzer outputs"
                 :version     "0.0.1"}
   :commands    [{:command     "projects"
                  :description "Diffs two apis for projects fetched from maven repos"
                  :opts (into [{:option  "project1"
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
                                :default :present}]
                              api-diff-options)
                  :runs diff-api-projects-cmd}
                 {:command     "files"
                  :description "Diffs two apis from cljdoc-analyzer files"
                  :opts (into [{:option  "filename1"
                                :short   0
                                :as      "filename 1 - specify cljdoc-analyzer file 1"
                                :type    :string
                                :default :present}
                               {:option  "language1"
                                :short   1
                                :as      "language for project 1 (clj or cljs)"
                                :type    :string
                                :spec    ::language
                                :default :present}
                               {:option  "filename2"
                                :short   2
                                :as      "filename 2 - specify group-id/artifact-id of project 2"
                                :type    :string
                                :default :present}
                               {:option  "language2"
                                :short   3
                                :as      "language for project 2 (clj or cljs)"
                                :type    :string
                                :spec    ::language
                                :default :present}]
                              api-diff-options)
                  :runs diff-api-files-cmd}] })

(defn -main [& args]
  (climatic/run-cmd args climatic-config))
