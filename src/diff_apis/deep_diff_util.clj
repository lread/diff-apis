(ns ^:no-doc diff-apis.deep-diff-util
  (:refer-clojure :exclude [get find update])
  (:require [lambdaisland.deep-diff.diff :as deep-diff]))

(defn inserted? [x]
  (= (type x) lambdaisland.deep_diff.diff.Insertion))

(defn deleted? [x]
  (= (type x) lambdaisland.deep_diff.diff.Deletion) )

(defn unary-diff? [x]
  (or (inserted? x) (deleted? x)))

(defn mismatch? [x]
  (= (type x) lambdaisland.deep_diff.diff.Mismatch))

(defn move-diff
  "Preserving the deep-diff type keeps pretty printing with deep-diff pretty wrt insertions an deletions."
  [from-elem new-elem]
  (cond
    (deleted? from-elem) (deep-diff/->Deletion new-elem)
    (inserted? from-elem) (deep-diff/->Insertion new-elem)
    :else (throw (ex-info (str "was expecting lambdaisland deep diff deletion or insertion but got:" from-elem) {}))))

(defn diff?
  "Returns true if `x` is a deep-diff marker."
  [x]
  (or (inserted? x) (deleted? x) (mismatch? x)))

(defn any-diffs?
  "Returns true if any deep-diff markers occur anywhere in `x` or its children."
  [x]
  (->> x
       (tree-seq #(or (map? %) (sequential? %)) seq)
       (some diff?)
       (some?)))

(defn changed?
  "Returns true if `x` is not a diff but a diff occurs somewhere in children of `x`."
  [x]
  (and (not (diff? x)) (any-diffs? x)))



(defn unwrap-elem [x]
  (cond
    (unary-diff? x)
    (val (first x))

    (mismatch? x)
    (throw (ex-info "programming error: cannot unwrap mismatch" {}))

    :else x))


(defn has-key? [m k]
  (and (map? m)
       (or (contains? m k)
           (contains? m (deep-diff/->Deletion k))
           (contains? m (deep-diff/->Insertion k)))))

(defn find
  "Returns k v pair matching key `k` in map `m` with matching for unary diffs. Returned k, when wrapped, will remain wrapped."
  [m k]
  (let [m (unwrap-elem m)]
    (and (or (map? m) nil)
         (or (clojure.core/find m (deep-diff/->Deletion k))
             (clojure.core/find m (deep-diff/->Insertion k))
             (clojure.core/find m k)))))

(defn find-all
  "Return vector of all k v pairs matching `keys` in `m` in same order as `keys`. See `find` for find behavior."
  [m keys]
  (reduce (fn [acc k]
            (if-let [found (find m k)]
              (into acc [found])
              acc))
          []
          keys))

(defn get
  "Return value in `m` for `k` unwrapping any diffs."
  [m k]
  (if-let [[_rk rv] (find m k)]
    (unwrap-elem rv)))


(defn update
  "Diff aware update. Handles wrapped keys and wrapped values. Preserves diff wrapper on m/key if present."
  [m k f]
  (let [rm (unwrap-elem m)
        [rk _rv] (find rm k)
        nrm (clojure.core/update rm rk f)]
    (if (= m rm)
      nrm
      (move-diff m nrm))))

(defn- diff-type [x]
  (when (diff? x)
    (if (mismatch? x)
      :!=
      (ffirst x))))

(defn calc-diff-type
  "Return diff-type for `elem` inheriting from `parent-diff-type`.

  `:-` deletion
  `:+` insertion
  `:=` no differences within
  `:!=`differences within

  A parent-difftype of `:!=` will can eventually be overriden by `:+` or `:-`.

  deep-diff wraps a key to indicate value is insertion or deletion so we treat it as the parent of the value. "
  [parent-difftype elem]
  (or (when-not (= parent-difftype :!=) parent-difftype)
      (if (map-entry? elem)
        (or
         (diff-type (key elem))
         (diff-type (val elem)))
        (diff-type elem))
      (if (any-diffs? (if (map-entry? elem)
                        (val elem)
                        elem)) :!= :=)))

(defn changes-only
  "Returns only publics with changes in `diff` result.
  This includes all publics for ns when any change has been made to ns level attributes, the idea
  being a change in attribute to a ns can affect the publics (ex deprecated, no-doc , etc)."
  [diff]
  (if (diff? diff)
    diff
    (->> (map (fn [ns]
                (let [[pubs-key pubs] (find ns :publics)]
                  (if (or (diff? ns)
                          (diff? pubs-key)
                          (diff? pubs)
                          (any-diffs? (dissoc ns :publics)))
                    ns
                    (update ns :publics #(filter any-diffs? %)))))
              diff)
         (filter #(any-diffs? %)))))

(comment
  (calc-diff-type nil (deep-diff/->Deletion "booya"))
  (calc-diff-type nil (deep-diff/->Insertion "booya"))

  (calc-diff-type nil (first {(deep-diff/->Insertion :booya) [{:name 1}]}))
  (calc-diff-type nil (first { :booya (deep-diff/->Insertion [{:name 1}])}))

  (calc-diff-type nil {:a 1})

  (first {:a 1})
  )
