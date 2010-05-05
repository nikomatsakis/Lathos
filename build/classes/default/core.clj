(comment
Sample clojure source file
)
(ns com.smallcultfollowing.lathos
    (:gen-class))

(defn -main
    ([greetee]
  (println (str "Hello " greetee "!")))
  ([] (-main "world")))
