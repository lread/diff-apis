(ns diff-apis.diff
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.edn :as edn]
            [clojure.walk :as walk]
            [cljdoc-analyzer.main :as cljdoc]
            [lambdaisland.deep-diff :as deep-diff]
            [diff-apis.deep-diff-util :as dd-util]))

(defn in?
  "true if coll contains elm"
  [coll elm]
  (some #(= elm %) coll))

(defn- api-essentials
  "Load api for signature comparison. We only include metadata of interest, effectively excluding :doc, :file :line."
  [m lang]
  (->> (get-in m [:codox lang])
       (walk/postwalk #(if (map? %)
                         (select-keys % [:name :arglists :members :type :dynamic :publics :deprecated :no-doc :skip-wiki])
                         %))))

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
           (if (dd-util/is-diff? k)
             (dd-util/preserve-deep-diff-type k v)
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
           (dd-util/preserve-deep-diff-type k v)
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

(defn- changes-only
  "Returns only publics with changes in `diff` result.
  This includes all publics for ns when any change has been made to ns level attributes."
  [diff]
  (->> (map (fn [ns]
              (if (or (dd-util/is-diff? ns) (dd-util/any-diffs? (dissoc ns :publics)))
                ns
                (update ns :publics #(filter dd-util/any-diffs? %))))
            diff)
       (filter #(or (dd-util/is-diff? %) (seq (:publics %))))))

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

(defn project-summary [{:keys [analysis lang]}]
 {:project (str (:group-id analysis) "/" (:artifact-id analysis)) :version (:version analysis) :lang lang})

(defn projects-summary [a b]
  {:a (project-summary a)
   :b (project-summary b)})


;; TODO: implement
(defn diff-stats [diff]
  {:insertions {:namespaces -1
                :publics -2
                :arglists -3}
   :deletions {:namespaces -4
               :publics -5
               :arglists -6}
   :mismatches {:namespaces -7
                :publics -8
                :arglists -9}})

(defn diff-edn
  "Returns deep-diff of api `a` and api `b`.
  Use opts to:
  - `:include` `:all` or just `:changed-publics`
  TODO: implement exclude, will use it to exclude potemkin, I think
  - `:exlude-namespaces` a vector of namespaces to exclude"
  ([a b opts]
   (let [a-api (api-essentials (:analysis a) (:lang a))
         b-api (api-essentials (:analysis b) (:lang b))
         result (-> (deep-diff/diff (raise-for-diff a-api) (raise-for-diff b-api))
                    (lower-after-diff)
                    (#(if (= :changed-publics (:include opts)) (changes-only %) %))
                    (sort-result))]
     {:diff result
      :stats (diff-stats result)
      :projects (projects-summary a b)})))

(defn *diff-files
  [a b opts]
  (diff-edn {:analysis (load-analyzer-file (:filename a)) :lang (:lang a)}
            {:analysis (load-analyzer-file (:filename b)) :lang (:lang b)} opts))

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
  (diff-files {:filename "rewrite-clj-0.6.1.pretty.edn", :lang "clj"}
              {:filename "rewrite-cljs-0.4.4.pretty.edn", :lang "cljs"}
              {:include :changed-publics, :exclude-namespaces nil})

  (load-analyzer-file "rewrite-clj-0.6.1.pretty.edn")
  (edn/read-string (slurp "rewrite-clj-0.6.1.pretty.edn"))
  )
