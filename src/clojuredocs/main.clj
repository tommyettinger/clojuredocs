(ns clojuredocs.main
  (:require [clojure.repl :refer [source-fn]]
            [clojure.java.io :refer [writer]]
            [clojure.string :as s]
            [cd-client.core :refer [examples-core see-also-core]]))

(defn all-core-names [namespace]
  (require (symbol namespace))
  (->> namespace
       (symbol)
       (the-ns)
       (ns-publics)
       (map first)
       (map str)))

(defn fix-for-wiki [nm]
  (-> nm
      (s/replace #"<" "[LT]")
      (s/replace #">" "[GT]")
      (s/replace #"-" "[Dash]")
      (s/replace #"^/$" "[Slash]")
      (s/replace #"\?$" "[Q]")))

(defn fix-for-clojuredocs [nm]
  (cond
    (.endsWith nm "?") (str (subs nm 0 (- (count nm) 1)) "_q")
    (= nm "/") "_"
    ;; (= nm ".") "_dot"
    (= nm "..") "_dotdot"
    :else nm))

(defn fix-for-markdown [nm]
  (-> nm
      (s/replace #"\*" "\\\\*")))

(defn seed-page [short-name namespace]
  (let [filename (str "wiki/" namespace "-" (fix-for-wiki short-name) ".md")
        v (find-var (symbol namespace short-name))
        m (meta v)
        display-name (fix-for-markdown (:name m))]
    (with-open [w (writer filename)]
      (.write w "-\n\n")

      (if (empty? (:arglists m))
        (.write w (format "<b>%s</b>\n" display-name))
        (doseq [args (:arglists m)]
          (.write w (format "<b>(%s)</b><br>\n" (apply str (interpose " " (cons display-name args)))))))

      (.write w "\n\n")

      (when-let [doc (:doc m)]
        (.write w "```text\n  ")
        (.write w doc)
        (.write w "\n```\n"))

      (.write w "\n\n#### Exampes\n\n")
      (let [exs (->> (:examples (examples-core namespace short-name))
                     (map :body))]
        (if-not (empty? exs)
          (doseq [ex exs]
            (.write w "\n\n```clojure\n")
            (.write w ex)
            (.write w "\n```\n\n"))
          (.write w "No examples.\n\n")))

      (.write w "\n\n#### See also\n\n")
      (let [refs (see-also-core namespace short-name)]
        (if-not (empty? refs)
          (do (doseq [ref (map :name refs)]
                (let [fq-enqued-name (str namespace "-" (fix-for-wiki ref))
                      display-name (fix-for-markdown ref)]
                  (.write w (format "[%s](%s) &nbsp; &nbsp; " display-name fq-enqued-name))))
              (.write w "\n\n"))
          (.write w "Nothing linked.\n\n")))

      (.write w "#### Other docs\n\n")
      (.write w (str "[official docs](http://clojure.github.io/clojure/" namespace "-api.html#" namespace "/" short-name ") &mdash; "))
      (.write w (str "[clojuredocs.org](http://clojuredocs.org/clojure_core/" namespace "/" (fix-for-clojuredocs short-name) ")\n\n"))

      (.write w "\n#### Source\n\n")
      (if-let [src (source-fn (symbol short-name))]
        (do (.write w "```clojure\n")
            (.write w src)
            (.write w "\n```\n"))
        (.write w "No source available."))

      )))

(def all-namespaces-to-use
  ["clojure.core"
   "clojure.string"
   "clojure.set"
   "clojure.test"
   "clojure.core.protocols"
   "clojure.data"
   "clojure.inspector"
   "clojure.java.browse"
   "clojure.java.browse-ui"
   "clojure.java.io"
   "clojure.java.javadoc"
   "clojure.java.shell"
   "clojure.main"
   "clojure.pprint"
   "clojure.reflect"
   "clojure.repl"
   "clojure.stacktrace"
   "clojure.template"
   "clojure.test.junit"
   "clojure.test.tap"
   "clojure.walk"
   "clojure.xml"
   "clojure.zip"])

(defn -main []
  (println "Seeding wiki...")
  (with-open [w (writer "wiki/Home.md")]
    (.write w "Welcome to the experimental new Clojure docs site.\n\n")
    (.write w "It's just a wiki that was pre-seeded with Clojure 1.5.1 data and examples.\n\n")
    (.write w "Anyone and everyone is welcome to edit it. Enjoy!\n\n")
    (doseq [namespace all-namespaces-to-use]
      (println "Doing namespace" namespace)
      (let [names (all-core-names namespace)
            groups (group-by #(.charAt % 0) names)]
        (.write w (str "### " namespace "\n\n"))
        (.write w "char | names\n")
        (.write w "---- | -----\n")
        (doseq [[group-char names] (sort-by first groups)]
          (.write w (format "%c | " group-char))
          (doseq [short-name (sort names)]
            (let [fq-enqued-name (str namespace "-" (fix-for-wiki short-name))
                  display-name (fix-for-markdown short-name)]
              (.write w (format "[%s](wiki/%s) &nbsp; &nbsp; " display-name fq-enqued-name)))
            (seed-page short-name namespace))
          (.write w "\n"))
        (.write w "\n"))))
  (println "Done."))
