;; Copyright (C) 2013, 2015, Jozef Wagner. All rights reserved.
;;
;; Additional copyright for parts of documentation and/or
;; underlying implementation:
;; Copyright (C) 2008, 2015, Rich Hickey and Clojure contributors.
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

(ns dunaj.coll.batched-queue
  "Batched queue, a sequential FIFO persistent collection.

  Batched queue is an implementation of FIFO (conj onto rear,
  peek/pop from front)
  `<<dunaj.coll.spi.ad#IPersistentList,IPersistentList>>`
  based on http://okasaki.blogspot.sk/[Okasaki's] Batched Queues.

  IMPORTANT: Except for very special cases, it is idiomatic to use
  functions defined in `<<dunaj.coll.default.api.ad#,coll.default>>`
  rather than ones in this namespace."
  {:authors ["Jozef Wagner"]
   :additional-copyright true}
  (:refer-clojure :exclude
   [seq reduce first reverse reduced? deftype when-let reversible?
    conj let doto fn defn nil? loop cond next count apply defrecord])
  (:require
   [clojure.bootstrap :refer [v1]]
   [dunaj.host :refer [class-instance?]]
   [dunaj.compare :refer [IHash IEquiv nil?]]
   [dunaj.flow :refer [when-let cond loop let doto]]
   [dunaj.feature :refer [IMeta IPersistentMeta]]
   [dunaj.poly :refer [deftype defrecord]]
   [dunaj.coll :refer
    [IEmptyable IRed ISeq ISequential IPersistentCollection IStacked
     IPersistentList IEmptyAware IPeekable ICounted ICollectionFactory
     ISeqable collection first next reverse reversible? reduce conj
     seq postponed postponed? reduced? count]]
   [dunaj.function :refer [apply fn defn]]
   [dunaj.coll.helper :refer [reduce* advance-fn]]
   [dunaj.state.var :refer [def+]]
   [dunaj.coll.empty-list]
   [dunaj.coll.bvt-vector]))


;;;; Public API

(def+ ^:private br :- java.lang.reflect.Field
  (doto (.getDeclaredField clojure.lang.PersistentQueue "r")
    (.setAccessible true)))

(defn ^:private get-br
  [v :- clojure.lang.PersistentQueue]
  (.get br v))

(def+ ^:private bf :- java.lang.reflect.Field
  (doto (.getDeclaredField clojure.lang.PersistentQueue "f")
    (.setAccessible true)))

(defn ^:private get-bf
  [v :- clojure.lang.PersistentQueue]
  (.get bf v))

(deftype BatchedQueue
  "Batched Queue."
  clojure.lang.PersistentQueue
  ;; JVM
  ;; custom equiv and hash
  ;; j.l.Iterable, j.u.Collection, j.u.List
  ;; (CLJ BUG: only Collection for now)
  IMeta
  IPersistentMeta
  IRed
  (-reduce [this reducef init]
    (let [af (advance-fn [ret] (reduce* (get-br this) reducef ret))]
      (af (reduce* (get-bf this) reducef init))))
  ISeqable
  ICounted
  IEmptyable
  IPeekable
  ISequential
  IPersistentCollection
  IStacked

  ;; Abstract types
  IPersistentList)

(def+ empty-batched-queue :- BatchedQueue
  "An empty batched queue."
  {:added v1
   :see '[batched-queue-factory dunaj.coll.empty-list/empty-list
          dunaj.coll.default/empty-que dunaj.coll.util/into
          dunaj.coll/conj dunaj.coll/peek dunaj.coll/pop]}
  clojure.lang.PersistentQueue/EMPTY)

;;; Factory

(def+ ^:private qc :- java.lang.reflect.Constructor
  (doto (.getDeclaredConstructor
         clojure.lang.PersistentQueue
         (dunaj.host.array/array
          java.lang.Class
          [clojure.lang.IPersistentMap
           java.lang.Integer/TYPE
           clojure.lang.ISeq
           clojure.lang.PersistentVector]))
    (.setAccessible true)))

(defn ^:private invoke-qc :- clojure.lang.PersistentQueue
  [meta cnt s]
  (.newInstance qc (dunaj.host.array/array java.lang.Object
                                           [meta cnt s nil])))

(defrecord BatchedQueueFactory
  "Factory for batched queue."
  []
  ICollectionFactory
  (-from-coll [factory coll]
    ;; use coll as a rear seq
    (invoke-qc nil (count coll) (seq coll)))
  (-from-items [factory] empty-batched-queue)
  (-from-items [factory a] (conj empty-batched-queue a))
  (-from-items [factory a b] (conj empty-batched-queue a b))
  (-from-items [factory a b c] (conj empty-batched-queue a b c))
  (-from-items [factory a b c d]
    (conj empty-batched-queue a b c d))
  (-from-items [factory a b c d more]
    (apply conj empty-batched-queue a b c d more)))

(def+ batched-queue-factory :- ICollectionFactory
  "A Batched Queue factory instance.
  Currently there are no options.

  This factory implements
  `<<dunaj.coll.spi.ad#ICollectionFactory,ICollectionFactory>>`
  factory protocol.

  New instances of Batched Queue can be created with
  `<<dunaj.coll.api.ad#collection,collection>>` and
  `<<dunaj.coll.api.ad#{under}{under}GT{under}collection,-{gt}collection>>`."
  {:added v1
   :see '[dunaj.coll.linked-list/linked-list-factory
          dunaj.coll.default/que-factory
          dunaj.coll.default/->que
          dunaj.coll.default/que
          dunaj.coll/collection
          dunaj.coll/->collection]}
  (->BatchedQueueFactory))
