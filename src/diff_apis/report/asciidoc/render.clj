(ns diff-apis.report.asciidoc.render)

(defn change-prefix [difftype]
  (case difftype
    :+ "[green]`*+*`"
    :- "[red]`*-*`"
    := "[black]`*=*`"
    "[black]`*≠*`"))

(defn inline-change-prefix [difftype]
  (case difftype
    :+ "[green]`^*+*^`"
    :- "[red]`^*-*^`"
    := "[black]`^*=*^`"
    "[black]`^*≠*^`"))

(defn escape-text [text]
  (str "pass:c[" text "]"))

(defn change-text [difftype text]
  (case difftype
    :+ (str "[green]#pass:c[" text "]#")
    :- (str "[red]#pass:c[" text "]#")
    (str "[black]#pass:c[" text "]#")))

(defn change-code [difftype code]
  (case difftype
    :+ (str "[green]`+" code "+`")
    :- (str "[red]`+" code "+`")
    (str "[black]`+" code "+`")))
