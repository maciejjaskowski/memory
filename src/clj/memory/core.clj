(ns memory.core
  (:require [compojure.handler :as handler]
            [compojure.route :as route]
            [compojure.core :refer [GET POST defroutes]]
            [ring.util.response :as resp]
            [ring.adapter.jetty :as ring]
            [cheshire.core :as json]
            [clojure.java.io :as io]))

(defroutes app-routes
  (GET "/" [] (resp/redirect "/index.html"))

  (route/resources "/")
  
  (route/not-found "Page not found"))

(def app
  (-> #'app-routes
      handler/api))

(defn start [port]
  (ring/run-jetty
    app
    {:port  port
     :join? false}))

(defn stop [server]
  (.stop server))

(defn -main []
  (let [port (Integer. (or (System/getenv "PORT") "8080"))]
    (start port)))
