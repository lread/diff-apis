(ns diff-apis.main
  (:require [cli-matic.core :as climatic]
            [expound.alpha :as expound]
            [clojure.spec.alpha :as spec]
            [clojure.set :as cset]
            [diff-apis.diff :as diff]
            [diff-apis.report :as report]))

(defn- diff-opts [args]
  (-> (select-keys args [:include :exclude-namespace :exclude-with :arglists-by
                         :replace-b-namespace])
      (cset/rename-keys {:exclude-namespace :exclude-namespaces})))

(defn- report-opts [args]
  (select-keys args [:report-format :report-filename :notes]))

(defn diff-api-projects-cmd
  [{:keys [project-a version-a language-a
           project-b version-b language-b] :as args}]
  (-> (diff/diff-projects {:project project-a :version version-a :lang language-a}
                          {:project project-b :version version-b :lang language-b}
                          (diff-opts args))
      (report/report (report-opts args))))

(defn diff-api-files-cmd
  [{:keys [filename-a language-a
           filename-b language-b] :as args}]
  (-> (diff/diff-files {:filename filename-a :lang language-a}
                       {:filename filename-b :lang language-b}
                       (diff-opts args))
      (report/report (report-opts args))))

(spec/def ::language #{"clj" "cljs"})
(spec/def ::include #{:all :changed-publics})
(spec/def ::report-format #{:asciidoc :deep-diff})
(spec/def ::arglists-by #{:arity-only :param-names})
;; expound does not seem to be able to handle spec/nilable so test this way:
(spec/def ::search-replace #(or (nil? %) (re-matches #"[^\/]+\/[^\/]*" %)))
(expound/defmsg ::search-replace "when provided, must specify search/replace where search can be a java regex")

(defn- option-help [& lines]
  (apply str (interpose (str "\n" (apply str (repeat 49 " ")))
                        lines)))


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
   {:option   "arglists-by"
    :as       (option-help "Compare arglists by :arity-only or :param-names."
                           "- when :arity-only is chosen, parameter names from project a will be shown."
                           "- defaults to :param-names")
    :type     :keyword
    :spec     ::arglists-by
    :default  :param-names}
   {:option  "notes"
    :as      "Filename containing notes to include in :asciidoc report"
    :type    :slurplines}
   {:option  "replace-b-namespace"
    :as      (option-help "Specify search/replace for b namespace name for comparisons."
                          "- for example: '^rewrite-cljc/rewrite-clj'"
                          "- :orig-b-name will appear for namespaces in diff result")
    :type    :string
    :spec    ::search-replace
    :default nil}
   {:option  "report-format"
    :as      "Either :asciidoc or :deep-diff, defaults to :deep-diff"
    :type    :keyword
    :spec    ::report-format
    :default :deep-diff}
   {:option  "report-filename"
    :as      "Filename to write report to, if absent report will be written to sdtout"
    :type    :string}])

(defn- project-option [qualifier short]
  {:option  (str "project-" qualifier)
   :short   short
   :as      (format "api %s - group-id/artifact-id for project" qualifier)
   :type    :string
   :default :present})

(defn- version-option [qualifier short]
  {:option  (str "version-" qualifier)
   :short   short
   :as      (format "api %s - version" qualifier)
   :type    :string
   :default :present})

(defn- lang-option [qualifier short]
  {:option  (str "language-" qualifier)
   :short   short
   :as      (format "api %s - language (clj or cljs)" qualifier)
   :type    :string
   :spec    ::language
   :default :present})

(defn- filename-option [qualifier short]
  {:option  (str "filename-" qualifier)
   :short   short
   :as      (format "api %s - cljdoc-analyzer filename" qualifier)
   :type    :string
   :default :present})

(def climatic-config
  {:app         {:command     "diff-api"
                 :description "diff apis from cljdoc-analyzer outputs"
                 :version     "0.0.1"}
   :commands    [{:command     "projects"
                  :description "Diffs apis for projects a and b fetched from maven repos"
                  :opts (into [(project-option "a" 0)
                               (version-option "a" 1)
                               (lang-option    "a" 2)
                               (project-option "b" 3)
                               (version-option "b" 4)
                               (lang-option    "b" 5)]
                               api-diff-options)
                  :runs diff-api-projects-cmd}
                 {:command     "files"
                  :description "Diffs apis from cljdoc-analyzer files a and b"
                  :opts (into [(filename-option "a" 0)
                               (lang-option     "a" 1)
                               (filename-option "b" 2)
                               (lang-option     "b" 3)]
                              api-diff-options)
                  :runs diff-api-files-cmd}]})

(defn -main [& args]
  (climatic/run-cmd args climatic-config))


(comment
  climatic-config)
