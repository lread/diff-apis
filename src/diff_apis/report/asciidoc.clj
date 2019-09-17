(ns diff-apis.report.asciidoc
  (:require [clojure.string :as string]
            [diff-apis.deep-diff-util :as dd-util]))

(defn- change-prefix [difftype]
  (case difftype
    :+ "[green]`*+*`"
    :- "[red]`*-*`"
    := "[black]`*=*`"
    "[black]`*≠*`"))

(defn- inline-change-prefix [difftype]
  (case difftype
    :+ "[green]`^*+*^`"
    :- "[red]`^*-*^`"
    := "[black]`^*=*^`"
    "[black]`^*≠*^`"))

(defn diff-type [parent-diff-type elem]
  (or parent-diff-type
      (dd-util/diff-type elem)
      (when (not (dd-util/any-diffs? elem)) :=)))

(defn diff-type-key [parent-diff-type key-elem]
  (or parent-diff-type
      (dd-util/diff-type key-elem)))

(defn escape-text [text]
  (str "pass:c[" text "]"))

(defn- change-text [difftype text]
  (case difftype
    :+ (str "[green]#pass:c[" text "]#")
    :- (str "[red]#pass:c[" text "]#")
    (str "[black]#pass:c[" text "]#")))

(defn- change-code [difftype code]
  (case difftype
    :+ (str "[green]`+" code "+`")
    :- (str "[red]`+" code "+`")
    (str "[black]`+" code "+`")))

(defn- code-value [parent-difftype v]
  (if (dd-util/is-mismatch? v)
    (str (inline-change-prefix :-) " " (change-code :- (:- v)) " "
         (inline-change-prefix :+) " " (change-code :+ (:+ v)))
    (change-code parent-difftype v)))

(defn- arglist [parent-difftype al]
  (let [difftype (diff-type parent-difftype al)]
    (str
     (change-text difftype "[") " "
     (string/join " " (for [a al] (code-value difftype a)))
     " " (change-text difftype "]"))))

(defn- arglists [parent-difftype [argslist-key argslist]]
  (into ["[unstyled]"]
        (let [argslist-difftype (diff-type-key parent-difftype argslist-key)]
          (for [al argslist]
            (let [al-difftype (diff-type argslist-difftype al)
                  al-elem (dd-util/unwrap-elem al)
                  elem-diff-type (diff-type al-difftype al-elem)]
              [ (str "* " (change-prefix elem-diff-type) " " (arglist al-difftype al-elem)) ])))))

(defn- attributes [parent-difftype elem]
  (into ["[unstyled]"]
        (for [[a-name a-val] (dd-util/find-all elem [:type :deprecated :no-doc :skip-wiki :dynamic])]
          (let [e-difftype (diff-type parent-difftype elem)
                a-name-difftype (diff-type-key e-difftype a-name)]
            (str "* " (change-prefix a-name-difftype) " "
                 (str "*" (change-text a-name-difftype (dd-util/unwrap-elem a-name)) "*") " "
                 (code-value a-name-difftype a-val))))))

(defn- members [parent-difftype [members-key members]]
  (let [members-difftype (diff-type-key parent-difftype members-key)]
    (for [m members]
      (let [member-difftype (diff-type members-difftype m)
            mname (dd-util/get m :name)
            mname-difftype (diff-type member-difftype mname)]
        ["a|"
         (str (change-prefix mname-difftype) " " (change-code mname-difftype (dd-util/get m :name)))
         "a|"
         (arglists member-difftype (dd-util/find m :arglists))
         "a|"
         (attributes member-difftype m)
         ""]))))

(defn as-lines [namespaces]
  (into []
        (flatten ;; I've heard the mantra against flatten, but seems appropriate here :-P
         (for [ns namespaces]
           (let [ns-difftype (dd-util/diff-type ns)]
             [(str "== " (change-prefix ns-difftype) " " (change-text ns-difftype (dd-util/get ns :name)))
              ""
              (attributes nil ns)
              ""
              (for [p (dd-util/get ns :publics)]
                (let [p-difftype (diff-type ns-difftype p)]
                  [(str "=== " (change-prefix p-difftype) "" (change-text p-difftype (dd-util/get p :name)))
                   (let [
                         arglists-key (first (dd-util/find p :arglists))
                         members-key (first (dd-util/find p :members))]
                     (cond
                       (and arglists-key members-key)
                       ["|==="
                        ".2+h| attributes .2+h| arglists 3+h|members"
                        "h|name h| arglists h| type"

                        ".999+a|"
                        (attributes ns-difftype p)
                        ".999+a|"
                        (arglists p-difftype (dd-util/find p :arglists))
                        (members p-difftype (dd-util/find p :members))
                        "|==="]
                       members-key
                       ["|==="
                        ".2+h| attributes 3+h| members"
                        "h|name h| arglists h| type"
                        ""
                        ".999+a|"
                        (attributes ns-difftype p)
                        (members p-difftype (dd-util/find p :members))
                        "|==="]

                       arglists-key
                       ["|==="
                        "| attributes | arglists"
                        ""
                        "a|"
                        (attributes ns-difftype p)
                        "a|"
                        (arglists p-difftype (dd-util/find p :arglists))
                        "|==="]

                       :else
                       ["|==="
                        "| attributes"
                        ""
                        "a|"
                        (attributes ns-difftype p)
                        "|==="]
                       ))
                   ""]))
              ""
              ""])))))


(defn to-asciidoc [lines]
  (string/join "\n" lines))

(defn- project-as-text [p]
  (str "++" (:project p) " " (:version p) " " (:lang p) "++"))

(defn- project-as-code [p]
  (str "`+" (:project p) "+` `+" (:version p) "+` `+" (:lang p) "+`"))

(defn header [{:keys [projects run-args]}]
  (str "// This file was auto-generated by diff-apis, best not to edit\n"
       "= Diff of " (project-as-text (:a projects)) " & " (project-as-text (:b projects)) "\n"
       ":toc: macro\n"
       ":toclevels: 5\n"
       ":!toc-title:\n\n"

       "**Diff of apis in:**\n\n"
       "A. " (project-as-code (:a projects)) "\n"
       "B. " (project-as-code (:b projects)) "\n\n"

       "With run-args:\n"
       "----\n"
       run-args "\n"
       "----\n"

       "**Legend:**\n\n"
       "* " (change-prefix :-) " " (change-text :- "A only ") "\n"
       "* " (change-prefix :+) " " (change-text :+ "B only") "\n"
       "* " (change-prefix :-) " " (change-text :- "A is" ) " " (change-prefix :+) " " (change-text :+ "different from B") "\n"
       "* " (change-prefix nil) " " (change-text nil "changes within A and B") "\n"
       "* " (change-prefix :=) " " (change-text := "equal")"\n\n"

       "toc::[]\n\n"))

(defn as-asciidoc [result]
  (str (header (select-keys result [:run-args :projects]))
       (-> (as-lines (:diff result))
           (to-asciidoc))))
