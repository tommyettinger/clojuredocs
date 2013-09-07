(ns clojuredocs.main
  (:require [ring.adapter.jetty :as jetty]
            [hiccup.core :refer [html]]
            [hiccup.page :refer [html5]]
            [garden.core :refer [css]]
            [compojure.core :refer :all]
            [ring.middleware.session :refer [wrap-session]]))

(def style
  (css
   [:#home
    [:td {:padding-bottom "20px"
          :vertical-align "top"}]
    [:#group-char {:width "50px"}]
    [:a {:text-decoration "none"
         :margin-right "5px"}]]
   [:#container {:margin "30px"}]
   [:#primary {:font-weight "bold"}]
   [:ul {:list-style-type "none"
         :padding-left "0px"}]
   [:body {:color "#555"
           :font-family "Menlo, monospace"}]
   [:hr {:height "1px"
         :border "0px"
         :background-color "#aaa"
         :width "375px"}]
   [:footer {:margin-top "70px"
             :font-size "75%"}]))

(defn enque [nm]
  (let [nm (str nm)]
    (cond
      (.endsWith nm "?") (str (subs nm 0 (- (count nm) 1)) "_q")
      (= nm "/") "_"
      ;; (= nm ".") "_dot"
      (= nm "..") "_dotdot"
      :else nm)))

(defn deque [nm]
  (cond
    (.endsWith nm "_q") (str (subs nm 0 (- (count nm) 2)) "?")
    (= nm "_") "/"
    ;; (= nm "_dot") "."
    (= nm "_dotdot") ".."
    :else nm))

(defn template [& body]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body (html
          (html5
           [:head [:style {:type "text/css"} style]]
           [:body
            [:div#container
             body
             [:footer
              [:hr {:align "left"}]
              [:p "Idea by "
               [:a {:href "https://github.com/TimMc"} "TimMc"]
               ". Webified by "
               [:a {:href "https://github.com/sdegutis"} "sdegutis"]
               ". Source "
               [:a {:href "https://github.com/sdegutis/clojuredocs"} "on github"]
               "."]]]]))})

(defn all-core-names []
  (->> clojure.core
       (quote)
       (the-ns)
       (ns-publics)
       (map first)
       (map str)))

(defn var-for [nm]
  (find-var (symbol "clojure.core" (str nm))))

(defroutes app
  (GET "/" [:as req]
    (let [names (all-core-names)
          groups (group-by #(.charAt % 0) names)]
      (template
       [:div#home
        [:table
         (for [[group-char names] (sort-by first groups)]
           [:tr
            [:td#group-char (format "%c" group-char)]
            [:td (interpose " "
                            (for [nm (sort names)]
                              [:a {:href (str "/" (enque (str nm)))}
                               nm]))]])]])))
  (GET "/:nm" [nm :as req]
    (let [nm (var-for (deque nm))
          name-meta (meta nm)
          fq-enqued-name (str (ns-name (:ns name-meta)) "/" (enque (:name name-meta)))
          fq-dequed-name (str (ns-name (:ns name-meta)) "/" (:name name-meta))]
      (template
       [:ul#primary
        (if (empty? (:arglists name-meta))
          [:li (:name name-meta)]
          (for [args (:arglists name-meta)]
            [:li "(" (interpose " " (cons (:name name-meta) args)) ")"]))]
       [:pre "  " (:doc name-meta)]
       [:p
        [:a {:href (str "/" (enque (:name name-meta)))}
         "permalink"]
        ", "
        [:a {:href (str "http://clojuredocs.org/clojure_core/" fq-enqued-name)}
         "clojuredocs"]
        ", "
        [:a {:href (str "http://clojure.github.io/clojure/clojure.core-api.html#" fq-dequed-name)}
         "official docs"]]))))

(defn -main [port]
  (jetty/run-jetty (-> app
                       (wrap-session))
                   {:port (Integer. port)
                    :join? false}))
