(ns diff-apis.diff
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.edn :as edn]
            [clojure.walk :as walk]
            [cljdoc-analyzer.main :as cljdoc]
            [lambdaisland.deep-diff :as deep-diff]
            [diff-apis.deep-diff-util :as dd-util]
            [diff-apis.stats :as stats]))

(defn- in?
  "true if coll contains elm"
  [coll elm]
  (some #(= elm %) coll))

(defn- contains-any-key? [m ks]
  (seq (select-keys m ks)))

(defn- api-essentials
  "Load api for signature comparison. We only include metadata of interest, effectively excluding :doc, :file :line."
  [m lang]
  (->> (get-in m [:analysis lang])
       (walk/postwalk #(if (map? %)
                         (select-keys % [:name :arglists :members
                                         :type :dynamic :publics
                                         :deprecated
                                         :no-doc :skip-wiki])
                         %))))

(defn- api [{:keys [cljdoc-analysis lang] :as _source}
            {:keys [exclude-namespaces exclude-with] :as _opts}]
  (->> (api-essentials cljdoc-analysis lang)
       (filter #(not (in? exclude-namespaces (str (:name %)))))
       (remove #(contains-any-key? % exclude-with))
       (map (fn [ns]
              (update ns :publics
                      (fn [vars]
                        (into () (remove #(contains-any-key? % exclude-with) vars))))))
       (into ())))

;; TODO: handle regex in arglists
(defn- load-analyzer-file [edn-filename]
  (edn/read-string (slurp edn-filename)))

(defn arity-comparator [a b]
  (let [a (if (and (keyword? a) (int? b)) Integer/MAX_VALUE a)
        b (if (and (keyword? b) (int? a)) Integer/MAX_VALUE b)]
        (compare a b)))

(defn- fn-arity [arglist]
  (if (= '& (last (butlast arglist)))
    :variadic
    (count arglist)))

(defn- raise-arglist-arity-as-key [arglists]
  (reduce (fn [acc al]
            (assoc acc (fn-arity al) al))
          {}
          arglists))

(defn- raise-arglists-arity-as-keys [api]
  (walk/postwalk
   (fn [x]
     (if (and (map? x) (:arglists x))
       (update x :arglists raise-arglist-arity-as-key)
       x))
   api))

(defn- lower-arglist-arity-map [raised-arity-map]
  (map (fn [[k v]]
         (with-meta
           (if (dd-util/diff? k)
             (dd-util/move-diff k v)
             v)
           {:diff-apis.diff/arity (dd-util/unwrap-elem k)}))
       raised-arity-map))

(defn- lower-arglists-arities [diff]
  (walk/postwalk
   (fn [x]
     (if (and (dd-util/has-key? x :arglists)
              (map? (second (dd-util/find x :arglists))))
       (dd-util/update x :arglists lower-arglist-arity-map)
       x))
   diff))

(defn- raise-name-as-key
  "As preparation for diffing convert [{:name a} {:name b}] to {a {:name a} {b {:name b}}} throughout `api`"
  [api]
  (walk/postwalk
   (fn [x]
     (if (and (list? x) (:name (first x)))
       (reduce (fn [acc m]
                 (assoc acc (:name m) m))
               {}
               x)
       x))
   api))

(defn- lower-name-map
  "Convert {:name {:name a}} back to {:name a} in `raised-name-map` preserving any special insertion/deletion records from deep-diff."
  [raised-name-map]
  (map (fn [[k v]]
         ;; TODO: check if it is a diff
         (if (and (map? k))
           (dd-util/move-diff k v)
           v))
       raised-name-map))

(defn- lower-names
  "Lower names raised for diffing throughout `diff` result."
  [diff]
  (walk/postwalk
   (fn [x]
     (if (and (map? x)
              (map? (first (vals x)))
              (:name (first (vals x))))
       (lower-name-map x)
       x))
   diff))

(defn- case-insensitive-comparator [a b]
  (let [la (and a (string/lower-case a))
        lb (and b (string/lower-case b))]
    (if (= la lb)
      (compare a b)
      (compare la lb))))

(defn- sort-result
  "Sorts all collections of maps with :name by :name.
  Sort is case insensitive with consistent sort order if :name is case sensitive unique across collection
  and all maps in collection have :name.

  Sorts arglists by arity."
  [namespaces]
  (clojure.walk/postwalk (fn [x]
                           (cond
                             ;; :name is always present so no need to worry about deletion or insertions on :name
                             (and (coll? x) (dd-util/has-key? (first x) :name))
                             (sort-by #(dd-util/get % :name) case-insensitive-comparator x)

                             ;; arglists can be present or not
                             (and (dd-util/has-key? x :arglists))
                             (dd-util/update x :arglists
                                             #(sort-by
                                               (fn [al] (:diff-apis.diff/arity (meta al)))
                                               arity-comparator %))
                             :else x))
                         namespaces))

(defn- raise-for-diff [api]
  (-> api
      raise-name-as-key
      raise-arglists-arity-as-keys))

(defn- lower-after-diff [diff]
  (-> diff
      lower-names
      lower-arglists-arities))

(defn- project-short-form [group-id artifact-id]
  (if (= group-id artifact-id)
    group-id
    (str group-id "/" artifact-id)))

(defn- project-summary [{:keys [cljdoc-analysis lang]}]
  (let [{:keys [group-id artifact-id version]} cljdoc-analysis]
    {:project (project-short-form group-id artifact-id) :version version :lang lang}))

(defn- projects-summary [a b]
  {:a (project-summary a)
   :b (project-summary b)})

(defn- revert-mismatches [result]
  (walk/postwalk #(if (dd-util/mismatch? %) (:- %) %) result))

(defn- tweak-namespace-for-comparison [api {:keys [replace-b-namespace]}]
  (let [ns-search-replace replace-b-namespace]
    (if (not ns-search-replace)
      api
      (let [[search replace] (string/split ns-search-replace #"\/")
            search-re (re-pattern search)]
        (into ()
              (map (fn [ns]
                     (with-meta
                         (update ns :name #(symbol (string/replace % search-re replace)))
                         {:orig-name (:name ns)}))
                   api))))))

(defn- include-untweaked-name [diff-result tweaked-api-b]
  (map (fn [diffed-ns]
         (let [ns-name (dd-util/get diffed-ns :name)
               orig-name (some-> (filter #(= ns-name (:name %)) tweaked-api-b)
                                 first
                                 meta
                                 :orig-name)]
           (if (or (nil? orig-name) (= ns-name orig-name))
             diffed-ns
             (dd-util/assoc diffed-ns :orig-b-name orig-name))))
       diff-result))

(defn diff-edn
  "Returns deep-diff of api `a` and api `b`.
  Use opts to:
  - `:include` `:all` or just `:changed-publics`
  - `:exlude-namespaces` a vector of namespaces to exclude
  - `:arglists-by` `:param-names` or `:arity-only` if latter, param names from a will be returned.
  - `:replace-b-namespace` - specify search/replace for namespace b name - for example ^rewrite-cljc/rewrite-clj"
  ([a b opts]
   (let [api-a (api a opts)
         api-b (api b opts)
         tweaked-api-b (tweak-namespace-for-comparison api-b opts)
         result (-> (deep-diff/diff (raise-for-diff api-a)
                                    (raise-for-diff tweaked-api-b))
                    (lower-after-diff)
                    (include-untweaked-name tweaked-api-b)
                    ;; mismatches only occur in arglists, if we are comparing by arity only revert them
                    (#(if (= :arity-only (:arglists-by opts)) (revert-mismatches %) %))
                    (#(if (= :changed-publics (:include opts)) (dd-util/changes-only %) %))
                    (sort-result))]
     {:diff result
      :stats (stats/count-diffs result)
      :projects (projects-summary a b)})))

(defn *diff-files
  [a b opts]
  (diff-edn {:cljdoc-analysis (load-analyzer-file (:filename a)) :lang (:lang a)}
            {:cljdoc-analysis (load-analyzer-file (:filename b)) :lang (:lang b)} opts))

(defn analysis-filename [coords]
  (str "./.diff-apis/.cache/"
       (.replace (str (:project coords) "-" (:version coords))
                 "/" "-")
       ".edn"))

(defn analyze [coords]
  (let [filename (analysis-filename coords)]
    (if (.exists (io/file filename))
      filename
      (do (cljdoc/analyze {:project (:project coords)
                           :version (:version coords)
                           :output-filename (analysis-filename coords)})
          filename))))

(defn diff-projects
  [a b opts]
  (let [a-filename (analyze a)
        b-filename (analyze b)
        result (*diff-files {:filename a-filename :lang (:lang a)}
                            {:filename b-filename :lang (:lang b)}
                            opts)]
    (assoc result :run-args {:diff-type :projects
                             :opts opts
                             :a a
                             :b b})))

(defn diff-files
  [a b opts]
  (let [result (*diff-files a b opts)]
    (assoc result :run-args {:diff-type :files
                             :opts opts
                             :a a
                             :b b})))

(comment
  (def d
    (diff-files {:filename "rewrite-clj-0.6.1.pretty.edn", :lang "clj"}
                {:filename "rewrite-cljs-0.4.4.pretty.edn", :lang "cljs"}
                {:include :changed-publics, :exclude-namespaces nil}))

  #_(def d
    (diff-files {:filename "rewrite-cljc-1.0.0-alpha.pretty.edn" :lang "cljs"}
                {:filename "rewrite-cljc-1.0.0-alpha.pretty.edn" :lang "clj"}
                {:include :changed-publics, :exclude-namespaces nil}))

  #_(def d
    (diff-files {:filename "rewrite-clj-0.6.1.pretty.edn", :lang "clj"}
                {:filename "rewrite-cljc-1.0.0-alpha.pretty.edn" :lang "clj"}
                {:include :changed-publics, :exclude-namespaces nil
                 :replace-b-namespace-for-compare "^rewrite-cljc/rewrite-clj"}))

  (def cljdoc-analysis (load-analyzer-file "rewrite-clj-0.6.1.pretty.edn"))
  (def t2 (api {:cljdoc-analysis cljdoc-analysis :lang "clj"} {:exclude-namespaces []}))

  (type t2)
  (edn/read-string (slurp "rewrite-clj-0.6.1.pretty.edn")))
