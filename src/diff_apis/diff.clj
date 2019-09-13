(ns diff-apis.diff
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.edn :as edn]
            [clojure.walk :as walk]
            [cljdoc-analyzer.main :as cljdoc]
            [lambdaisland.deep-diff :as deep-diff]
            [diff-apis.deep-diff-util :as deep-diff-util]))

(defn in?
  "true if coll contains elm"
  [coll elm]
  (some #(= elm %) coll))

(defn- api-essentials
  "Load api for signature comparison. We also include metadata of interest.
  What am I excluding really? :doc :file :line"
  [m lang]
  (walk/postwalk #(if (map? %)
                    (select-keys % [:name :arglists :members :type :dynamic :publics :deprecated :no-doc :skip-wiki])
                    %)
                 (get-in m [:codox lang])))

(defn- load-api
  "Load cljdoc-analyzer generated edn from `edn-filename` for `lang` where `lang` is \"clj\" or \"cljs\"."
  [edn-filename lang]
  (api-essentials (edn/read-string (slurp edn-filename)) lang))

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
           (if (deep-diff-util/is-diff? k)
             (deep-diff-util/preserve-deep-diff-type k v)
             v)
           {:diff-apis.diff/arity (deep-diff-util/unwrap-elem k)}))
       raised-arity-map))

(defn- lower-arglists-arities [diff]
  (walk/postwalk
   (fn [x]
     (if (and (map? x) (:arglists x))
       (update x :arglists lower-arglist-arity-map)
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
           (deep-diff-util/preserve-deep-diff-type k v)
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
              (if (or (deep-diff-util/is-diff? ns) (deep-diff-util/any-diffs? (dissoc ns :publics)))
                ns
                (update ns :publics #(filter deep-diff-util/any-diffs? %))))
            diff)
       (filter #(or (deep-diff-util/is-diff? %) (seq (:publics %))))))

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
                             (and (coll? x) (deep-diff-util/diff-aware-get (first x) :name))
                             (sort-by #(deep-diff-util/diff-aware-get % :name) case-insensitive-comparator x)

                             ;; arglists can be present or not so we need to update carefully
                             (and (map? x) (deep-diff-util/diff-aware-get  x :arglists))
                             (deep-diff-util/diff-aware-update x :arglists
                                                               #(sort-by
                                                                 (fn [al] (:diff-apis.diff/arity (meta al)))
                                                                 arity-comparator %))
                             :else x))
                         namespaces))

(defn diff-edn
  "Returns deep-diff of api `a` and api `b`.
  Use opts to:
  - `:include` `:all` or just `:changed-publics`, defaults to `:changed-publics`
  TODO: implement exclude, will use it to exclude potemkin, I think
  - `:exlude-namespaces` `:none` or a vector of namespaces to exclude, defaults to `:none`"
  ([a b]
   (diff-edn a b {:include :changed-publics}))
  ([a b opts]
   (-> (deep-diff/diff (raise-name-as-key a) (raise-name-as-key b))
       (lower-names)
       (#(if (= :changed-publics (:include opts)) (changes-only %) %))
       (sort-result))))

(defn diff-files
  [a b opts]
  (diff-edn (apply load-api a) (apply load-api b) opts))

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
        result (diff-files [a-filename (:lang a)]
                           [b-filename (:lang b)]
                           opts)]
    {:diff result
     :run-args {:a a
                :b b
                :opts opts}}))

(comment
  (diff-projects {:project "rewrite-clj" :version "0.6.1" :lang "clj"}
                 {:project "rewrite-cljs" :version "0.4.4" :lang "cljs"}
                 {:include :changed-publics})


  (def dal '{ :name find-next, { :- :arglists } ( [ zloc p? ] [ zloc f p? ] ), :type :var })
  (deep-diff-util/diff-aware-get dal :arglists)

  (int? 3)
  (multitest "hi" "there" "joe" "blow" "go")
  (compare "smoke" :bork)
  (sort arity-comparator [1 11 13 2 :z :a ])
  (def alist '[[1 2 3]
               [4 5 & 6]
               [1 2]
               []])

  (def api {:a :b :arglists alist})

  (raise-arglist-arity-as-key alist)
  (lower-arglists-arities (raise-arglists-arity-as-keys api))
  ()

  (sort-by identity
           arity-comparator alist)

  (def x {:arglists alist})
  (update x :arglists sort-by #(if (deep-diff-util/is-diff? %)
                                 (val %)
                                 %)
          arity-comparator)
  (update x :arglists #(sort-by identity arity-comparator %))
  (diff-files ["rewrite-cljc-1.0.0-alpha.edn" "clj"] ["rewrite-cljc-1.0.0-alpha.edn" "cljs"] {:include :changed-publics})
  (diff-files ["rewrite-clj-0.6.1.edn" "clj"] ["rewrite-cljs-0.4.4.edn" "cljs"] {:include :changed-publics})

  )
