;; Copyright (C) 2013, 2015, Jozef Wagner. All rights reserved.
;;
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0
;; (http://opensource.org/licenses/eclipse-1.0.php) which can be
;; found in the file epl-v10.html at the root of this distribution.
;;
;; By using this software in any fashion, you are agreeing to be bound
;; by the terms of this license.
;;
;; You must not remove this notice, or any other, from this software.

(ns dunaj.type.validation
  "Type validation facilities.

  WORK IN PROGRESS"
  {:authors ["Jozef Wagner"]}
  (:api bare-ws)
  (:require
   [clojure.bootstrap :refer [v1 scratch]]
   [dunaj.type :refer [VariadicSignature Any]]
   [dunaj.boolean :refer [and or]]
   [dunaj.host :refer [class-instance? class?]]
   [dunaj.math :refer [inc dec <= - >= < >]]
   [dunaj.compare :refer [= nil?]]
   [dunaj.flow :refer [let cond when loop when-not]]
   [dunaj.function :refer [defn fn]]
   [dunaj.feature :refer [meta]]
   [dunaj.macro :refer [defmacro gensym]]
   [dunaj.poly :refer
    [defprotocol protocol? type? record? instance? satisfies?
     extend-protocol!]]
   [dunaj.coll :refer
    [list? first rest count map? seq nth empty? next red? second]]
   [dunaj.coll.recipe :refer [map repeatedly interleave filter]]
   [dunaj.coll.cons-seq :refer [cons]]
   [dunaj.coll.default :refer [vec]]
   [dunaj.coll.tuple :refer [tuple]]
   [dunaj.coll.util :refer [some last every?]]
   [dunaj.string :refer [->str]]
   [dunaj.error :refer [illegal-argument]]))

(defprotocol IRangeValidation
  (-validate-range [this from to]))

(defn validate-range
  [v from to]
  (-validate-range v from to))

(extend-protocol! IRangeValidation
  java.lang.Number
  (-validate-range [this from to]
    (and (or (nil? from) (>= this from))
         (or (nil? to) (< this to))))
  java.lang.String
  (-validate-range [this from to]
    (validate-range (count this) from to)))

(defprotocol ITypeValidation
  (-validate [this val]))

(defn validate-value
  [t v]
  (cond (class? t)
        (class-instance? ^java.lang.Class t ^java.lang.Object v)
        (protocol? t) (satisfies? t v)
        (or (type? t) (record? t)) (instance? t v)
        (-validate t v)))

(extend-protocol! ITypeValidation
  dunaj.type.UnionSignature
  (-validate [this val]
    (some #(validate-value % val) (:sigs this)))
  dunaj.type.IntersectionSignature
  (-validate [this val]
    (every? #(validate-value % val) (:sigs this)))
  dunaj.type.MaybeSignature
  (-validate [this val]
    (or (nil? val) (validate-value (:sig this) val)))
  dunaj.type.RangeSignature
  (-validate [this val]
    (and (validate-value (:sig this) val)
         (validate-range val (:from this) (:to this))))
  dunaj.type.VariadicSignature
  (-validate [this val]
    (or (nil? val) (validate-value (:sig this) val)))
  dunaj.type.AnySignature
  (-validate [this val] true)
  clojure.lang.IPersistentVector
  (-validate [this val]
    (let [vf (fn [item] (some #(validate-value % item) this))]
      (and (red? val) (or (empty? this) (every? vf val)))))
  clojure.lang.IPersistentMap
  (-validate [this val]
    (let [ivf (fn [k [tk tv]] (validate-value tk k))
          vf (fn [[k v]] (let [valid-vals (filter #(ivf k %) this)]
                          (some #(validate-value % v)
                                (map second valid-vals))))]
      (and (red? val) (or (empty? this) (every? vf val)))))
  java.lang.Object
  (-validate [this val] (= this val))
  nil
  (-validate [this val] (nil? val)))

(defn matched-sigs
  [msigs n]
  (let [ff #(or (= (count %) (inc n))
                (and (instance? VariadicSignature (last %))
                     (<= (dec (count %)) (inc n))))]
    (map vec (filter ff msigs))))

(defn validate-arg
  [msigs index arg]
  (seq (filter #(validate-value (nth % index) arg) msigs)))

(defn validate-args
  [tsig args]
  (when tsig
    (let [msigs (matched-sigs (:method-sigs tsig) (count args))]
      (when (empty? msigs)
        (throw (illegal-argument "wrong number of arguments")))
      (loop [args (seq args)
             index 1
             msigs (seq msigs)]
        (cond (nil? msigs)
              (throw (illegal-argument (->str "argument " (- index 2)
                                              " failed validation")))
              (nil? args) (map first msigs)
              (recur (next args) (inc index)
                     (validate-arg msigs index (first args))))))))

(defn validate-ret
  [rsigs ret]
  (when-not (some #(validate-value % ret) rsigs)
    (throw (illegal-argument "return value not valid"))))

(defn validate-fn
  [form]
  (let [fn-name (first form)
        args (rest form)
        arg-gensyms (seq (repeatedly (count args) gensym))
        arg-bindings (interleave arg-gensyms args)]
    `(let [tsig# (:tsig (meta (var ~fn-name)))
           ~@arg-bindings
           valid-rsigs# (validate-args tsig# (tuple ~@arg-gensyms))
           ret# (~fn-name ~@arg-gensyms)]
       (when tsig# (validate-ret valid-rsigs# ret#))
       ret#)))

(defn validate-form
  [form]
  (if (list? form)
    (validate-fn form)
    form))

;;;; Public API

(defmacro with-validation
  "Validates and evals all given forms."
  {:added v1}
  [opt-map & body]
  (let [body (if (map? opt-map) body (cons opt-map body))]
    `(do ~@(map validate-form body))))


;;;; Scratch

(scratch []

  []

  (defn foo :- {Any java.lang.String}
    [a :- dunaj.math/Integer,
     b :- (dunaj.type/Range dunaj.math/Float 1 10)]
    {:foo "bar"})

  (:tsig (meta (var foo)))

  (with-validation (foo 1 2.2))

)
