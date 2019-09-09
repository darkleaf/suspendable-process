(ns suspendable-process.b
  (:require
   [clojure.pprint :as pp]))

(defn process [greeting]
  [[:do
    [:print "What is your name?"]
    [:read]]
   (fn [[_ name]]
     [[:do
       [:print (str greeting " " name)]
       [:print "Done? (yes/no)"]
       [:read]]
      (fn [[_ _ val]]
        (if (= val "yes")
          [[:print "Bye!"]
           (fn [_]
             [[:return "have a nice day"]])]
          [[:next greeting]
           process]))])])

(defmulti effect->coeffect first)

(defmethod effect->coeffect :print [[_ value]]
  (prn value))

(defmethod effect->coeffect :read [[_]]
  (read-line))

(defmethod effect->coeffect :next [[_ arg]]
  arg)

(defmethod effect->coeffect :return [[_ val]]
  val)

(defmethod effect->coeffect :do [[_ & effects]]
  (map effect->coeffect effects))

(defn interpret [f & args]
  (loop [[effect callback] (apply f args)]
    (let [coeffect (effect->coeffect effect)]
      (if (nil? callback)
        coeffect
        (recur (callback coeffect))))))

(defn logged-interpret [f & args]
  (loop [[effect callback] (apply f args)
         log               []]
    (let [coeffect (effect->coeffect effect)]
      (if (nil? callback)
        (do
          (pp/pprint log)
          coeffect)
        (recur (callback coeffect)
               (conj log [effect coeffect]))))))

(comment
  (interpret process "Hi!")
  (logged-interpret process "Hi!"))
