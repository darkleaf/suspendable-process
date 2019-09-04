(ns suspendable-process.b
  (:require
   [clojure.walk :as w]))

(defn- inject* [next form]
  (let [injected (w/prewalk-replace {'<> next} form)]
    (cond
      (not= form injected) injected
      (seq? form)          `(~@form ~next)
      :else                (throw (ex-info "This form must contain <> or be a seq")))))

(defn- linearize* [body]
  (reduce inject* (reverse body)))

(defmacro linearize [& body]
  (linearize* body))


(defn process [greeting]
  (linearize
   [[:print "What is your name?"]
    [:read (fn [name] <>)]]

   [[:print (str greeting " " name)]
    [:print "Done? (yes/no)"]
    [:read (fn [val] <>)]]

   (if (= val "yes")
     [[:print "Bye!"]])

   [[:next #(process greeting)]]))

(defmulti interpret (fn [[type & _ :as effect]] type))

(defmethod interpret :print [[_ value]]
  (prn value)
  [])

(defmethod interpret :read [[_ callback]]
  (callback (read-line)))

(defmethod interpret :next [[_ callback]]
  (callback))

(defn interpretator [interpret effects]
  (loop [[head & tail] effects]
    (when head
      (recur (concat (interpret head) tail)))))

(comment
  (interpretator interpret (process "Hi!")))
