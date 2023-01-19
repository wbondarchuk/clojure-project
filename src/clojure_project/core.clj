(ns clojure-project.core
  (:import [java.io PushbackReader])
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))


(defn check-correctness [expr]
  (if (and (seq? expr) (= 3 (count expr)))
    true
    false))

(defn read-forms [file]
  (let [rdr (-> file io/file io/reader PushbackReader.)
        sentinel (Object.)]
    (loop [forms []]
      (let [form (edn/read {:eof sentinel} rdr)]
        (if (= sentinel form)
          forms
          (recur (conj forms form)))))))

(defn read-sdata
  [path]
  (let [data-seq (try
                   (read-forms path)
                   (catch Exception e (list))
                   (finally))]
    (first (filter
             (fn [data]
               (check-correctness data))
             data-seq))))

(defn -main []
  (let [path-to-file1 "test/test-data/data1.txt"]
    (println (read-sdata path-to-file1))))


;Пример с выражения
(def example '(tag (nm :note)
                  (
                   (tag (nm :to) "Tove ")
                   (tag (nm :from) "Jani" )
                   (tag (nm :heading) "Reminder")
                   (tag (nm :body) "Don't forget me this weekend!")
                   )
                  ))

(def example-scheme '(tag (nm :note)
                          (sequence
                            (
                             (tag (nm :to) string)
                             (tag (nm :from) string )
                             (tag (nm :heading) string)
                             (tag (nm :body) string)
                             )
                            )
                          ))

