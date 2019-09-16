(ns ^:no-doc diff-apis.deep-diff-util
  (:refer-clojure :exclude [get find update])
  (:require [clojure.walk :as walk]
            [lambdaisland.deep-diff.diff :as deep-diff]))

(defn preserve-deep-diff-type
  "Preserving the deep-diff type keeps pretty printing with deep-diff pretty wrt insertions an deletions."
  [from-elem new-elem]
  (cond
    (= (type from-elem) lambdaisland.deep_diff.diff.Deletion) (deep-diff/->Deletion new-elem)
    (= (type from-elem) lambdaisland.deep_diff.diff.Insertion) (deep-diff/->Insertion new-elem)
    :else (throw (ex-info (str "was expecting lambdaisland deep diff deletion or insertion but got:" from-elem) {}))))

(defn is-diff?
  "Returns true if `x` is a deep-diff marker."
  [x]
  ;; TODO use deep-diff types, wrote this before I realized they were present
  (and (map? x) (or (:+ x) (:- x))))

(defn any-diffs?
  "Returns true if any deep-diff markers occur anywhere in `x`."
  [x]
  ;; TODO: maybe using atom not the most elegant way?
  (let [diffs-found (atom false)]
    (walk/postwalk
     (fn [n]
       (when (and (is-diff? n) (= false @diffs-found))
         (reset! diffs-found true))
       n)
     x)
    @diffs-found))

(defn is-mismatch?
  [elem]
  (= (type elem) lambdaisland.deep_diff.diff.Mismatch))

;; A map can be wrapped with an insertion or deletion record
;; a key can be wrapped with an insertion or deletion record
;; TODO: key-get... badly named.
(defn key-get [x key]
  (or (clojure.core/get x {:- key})
      (clojure.core/get x {:+ key})
      (clojure.core/get x key)))

;; for insertions and deletions only
(defn unwrap-elem [x]
  (if (is-diff? x)
    (val (first x))
    x))

(defn find [x key]
  (let [x (unwrap-elem x)]
    (and (or (map? x) nil)
         (or (clojure.core/find x (deep-diff/->Deletion key))
             (clojure.core/find x (deep-diff/->Insertion key))
             (clojure.core/find x key)))))

(defn find-all [elem keys]
  (reduce (fn [acc k]
            (if-let [found (find elem k)]
              (into acc [found])
              acc))
          []
          keys))

(comment
  (or (map? {:a 1}) nil)
  (or (map? 1) nil)

  )

;; for insertions and deletions only
(defn get [x key]
  (if (is-diff? x)
    (clojure.core/get (first (vals x)) key)
    (key-get x key)))



;; for insertions and deletions only
(defn diff-type [x]
  (when (is-diff? x)
    (ffirst x)))

;; we might be dealing with a wrapped map or a wrapped key
(defn update [m k f]
  (let [rm (unwrap-elem m)
        [rk _rv] (find rm k)
        nrm (clojure.core/update rm rk f)]
    (if (= m rm)
      nrm
      (preserve-deep-diff-type m nrm))))

(comment
  (def t {(deep-diff/->Deletion :arglists) '([a b c])})
  (:- (first (find t :arglists)))

  ()
  )
(comment
  (map? (second (find '{:name throw-reader, :arglists {:variadic [reader fmt & data]}, :type :var} :arglists)))
  (find (deep-diff/->Deletion :arglists)  :arglists)
  (any-diffs? [[(deep-diff/->Deletion :booya)]])
  (is-mismatch? (deep-diff/->Mismatch "old" "new"))
  )
