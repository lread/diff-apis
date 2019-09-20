(ns ^:no-doc diff-apis.deep-diff-util
  (:refer-clojure :exclude [get find update])
  (:require [clojure.walk :as walk]
            [lambdaisland.deep-diff.diff :as deep-diff]))

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

(defn diff-type [x]
  (when (diff? x)
    (if (mismatch? x) (throw (ex-info "programming error: diff-type of mismatch unexpected" {})))
    (ffirst x)))

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
  "Returns k v pair matching `key` in map `m` with matching for unary diffs. Returned k, when wrapped, will remain wrapped."
  [x key]
  (let [x (unwrap-elem x)]
    (and (or (map? x) nil)
         (or (clojure.core/find x (deep-diff/->Deletion key))
             (clojure.core/find x (deep-diff/->Insertion key))
             (clojure.core/find x key)))))

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
