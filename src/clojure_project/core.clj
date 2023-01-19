(ns clojure-project.core
  (:import [java.io PushbackReader])
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

; Utils
(defn get-expr-type [expr]
  (if (seq? expr) (first expr) ::value))

(defn legal-expr? [expr expr-type]
  (= expr-type (get-expr-type expr)))

(defn expr-value [expr expr-type]
  (if (legal-expr? expr expr-type)
    (second expr)
    (throw (IllegalArgumentException. "Bad type"))))

(defn args [expr]
  (rest expr))

(defn nm [value]
  (list ::name value))

(defn tag [name & values]
  (concat (list ::tag name) values))

(defn tag? [expr]
  (= (first expr) ::tag))

(defn nm? [expr]
  (= (second expr) ::name))

(defmulti to-str (fn [expr] (get-expr-type expr)))
(defmethod to-str ::value [expr] (str expr))
(defmethod to-str ::name [expr] (str (expr-value expr ::name)))
(defmethod to-str ::tag [expr] (reduce (fn [acc val] (str acc " " (to-str val))) "" (args expr)))

(declare is-value)

(defn check-correctness [expr]
  (if (and (seq? expr) (= 3 (count expr)) (tag? expr))
    true
    false))

(defn is-value
  [data]
  (if (or (string? data) (number? data))
    true
    (check-correctness data)))


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



;Пример с-выражения
(def example (tag (nm :note)
                  '(
                   (tag (nm :to) "Tove ")
                   (tag (nm :from) "Jani" )
                   (tag (nm :heading) "Reminder")
                   (tag (nm :body) "Don't forget me this weekend!")
                   )
                  ))

;Пример схемы
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
