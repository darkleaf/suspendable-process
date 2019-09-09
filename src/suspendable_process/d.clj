(ns suspendable-process.d
  (:require
   [clojure.pprint :as pp]
   [clojure.walk :as w]))

(defn- inject* [next form]
  (let [expanded (w/macroexpand-all form)
        injected (w/prewalk-replace {'<> next} expanded)]
    (cond
      (not= expanded injected) injected
      (seq? expanded)          `(~@expanded ~next)
      :else                    (throw (ex-info "This form must contain <> or be a seq"
                                               {:form form, :next next})))))

(defn- linearize* [body]
  (reduce inject* (reverse body)))

(defmacro linearize [& body]
  (linearize* body))

(defn process [greeting]
  (linearize
   [[:print "Hi"] (fn [_] <>)]
   [[:next 0] (fn main-loop [n] <>)]
   [[:do
     [:print (str "iteration: " n)]
     [:print "What is your name?"]
     [:read]]
    (fn [[_ _ name]] <>)]
   [[:do
     [:print (str greeting " " name)]
     [:print "Done? (yes/no)"]
     [:read]]
    (fn [[_ _ val]] <>)]
   (if (= val "yes")
     (linearize
      [[:print "Bye!"] (fn [_] <>)]
      [[:return "have a nice day"]]))
   [[:next (inc n)] main-loop]))

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
