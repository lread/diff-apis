(ns diff-apis.report.asciidoc
  (:require [clojure.string :as string]
            [diff-apis.report.asciidoc.render :as render]
            [diff-apis.report.asciidoc.header :as header]
            [diff-apis.deep-diff-util :as dd-util]))

(defn- code-value [parent-difftype v]
  (if (dd-util/mismatch? v)
    (str (render/inline-change-prefix :-) " " (render/change-code :- (:- v)) " "
         (render/inline-change-prefix :+) " " (render/change-code :+ (:+ v)))
    (render/change-code parent-difftype v)))

(defn- arglist [parent-difftype al]
  (let [difftype (dd-util/calc-diff-type parent-difftype al)]
    (str
     (render/change-text difftype "[") " "
     (string/join " " (for [a al] (code-value difftype a)))
     " " (render/change-text difftype "]"))))

(defn- attributes
  "returns lines for attributes for `elem` which will be, for our purposes, one of namespace, public member"
  [parent-difftype elem]
  (into ["[unstyled]"]
        (for [a-entry (dd-util/find-all elem [:type :deprecated :no-doc :skip-wiki :dynamic])]
          (let [[a-name a-val] a-entry
                a-difftype (dd-util/calc-diff-type parent-difftype a-entry)]
            (str "* "
                 (str "*" (render/escape-text (dd-util/unwrap-elem a-name)) "*") " "
                 (render/change-prefix a-difftype) " "
                 (code-value a-difftype a-val))))))

(defn- arglists [parent-difftype argslist-entry]
  (into ["[unstyled]"]
        (let [argslist-difftype (dd-util/calc-diff-type parent-difftype argslist-entry)
              argslist (dd-util/unwrap-elem (val argslist-entry))]
          (for [al argslist]
            (let [al-difftype (dd-util/calc-diff-type argslist-difftype al)
                  al-elem (dd-util/unwrap-elem al)
                  elem-diff-type (dd-util/calc-diff-type al-difftype al-elem)]
              [ (str "* " (render/change-prefix elem-diff-type) " " (arglist al-difftype al-elem)) ])))))

(defn- members [parent-difftype members-entry]
  (let [members-difftype (dd-util/calc-diff-type parent-difftype members-entry)
        members (dd-util/unwrap-elem (val members-entry))]
    (for [m members]
      (let [member-difftype (dd-util/calc-diff-type members-difftype m)
            mname-difftype (dd-util/calc-diff-type member-difftype (dd-util/find m :name))]
        ["a|"
         (str (render/change-prefix mname-difftype) " " (render/change-code mname-difftype (dd-util/get m :name)))
         "a|"
         (arglists member-difftype (dd-util/find m :arglists))
         "a|"
         (attributes member-difftype m)
         ""]))))

(defn- publics [parent-difftype publics-entry]
  (let [publics-difftype (dd-util/calc-diff-type parent-difftype publics-entry)
        publics (dd-util/unwrap-elem (val publics-entry))]
    (for [p publics]
      (let [p-difftype (dd-util/calc-diff-type publics-difftype p)]
        [(str "=== " (render/change-prefix p-difftype) "" (render/change-text p-difftype (dd-util/get p :name)))
         (let [arglists-entry (dd-util/find p :arglists)
               members-entry (dd-util/find p :members)
               attributes-lines (attributes p-difftype p)
               arglists-lines (and arglists-entry (arglists p-difftype arglists-entry))
               members-lines (and members-entry (members p-difftype members-entry))]
           (cond
             (and arglists-lines members-lines)
             ["|==="
              ".2+h| attributes .2+h| arglists 3+h|members"
              "h|name h| arglists h| attributes"

              ".999+a|"
              attributes-lines
              ".999+a|"
              arglists-lines
              members-lines
              "|==="]
             members-lines
             ["|==="
              ".2+h| attributes 3+h| members"
              "h|name h| arglists h| attributes"
              ""
              ".999+a|"
              attributes-lines
              members-lines
              "|==="]

             arglists-lines
             ["|==="
              "| attributes | arglists"
              ""
              "a|"
              attributes-lines
              "a|"
              arglists-lines
              "|==="]

             :else
             ["|==="
              "| attributes"
              ""
              "a|"
              attributes-lines
              "|==="]))
         ""]))))


(defn namespaces [namespaces]
  (let [nses-diff-type (dd-util/calc-diff-type nil namespaces)]
    (for [ns namespaces]
      (let [ns-difftype (dd-util/calc-diff-type nses-diff-type ns)]
        [(str "== "
              (render/change-prefix ns-difftype) " "
              (render/change-text ns-difftype (dd-util/get ns :name)))
         ""
         (attributes nil ns)
         ""
         (publics ns-difftype (dd-util/find ns :publics))
         ""
         ""]))))

(defn as-lines [diff]
  (into []
        ;; I've heard the mantra against flatten, but seems appropriate here :-P
        (flatten (namespaces diff))))

(defn to-asciidoc [lines]
  (string/join "\n" lines))

(defn as-asciidoc [result]
  (str (header/header result)
       (-> (as-lines (:diff result))
           (to-asciidoc))))
