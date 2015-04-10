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

(ns dunaj.math.random
  "Random number generators.

  While convenience functions `<<rand,rand>>`,
  `<<rand-integer,rand-integer>>` and `<<rand-nth,rand-nth>>` are
  provided, this namespace's primary feature is the generation of
  reducible collections of random numbers.

  Random numbers are generated by a _random number generator_, which
  produces collection of random bytes. The random number generator
  is created with the `<<rng,rng>>` function. Dunaj provides
  four rng types, each having its own rng factory:

  * `<<secure_rng,secure-rng>>` - A thread safe secure rng with
    host-specific algorithm and providers

  * `<<seedable_rng,seedable-rng>>` - A thread safe rng with the
    ability to specify initial seed and with a guarantee that
    the same seed yields same sequence of items in the first
    reduction of the rng. Slower compared to other rngs.

  * `<<splittable_rng,splittable-rng>>` - A non thread safe rng
    which is however intended for fork-join tasks, as it is able to
    split into two separate rngs. Can be seeded with same guarantees
    as in seedable-rng.

  * `<<thread_local_rng,thread-local-rng>>` - A fast thread local rng.

  The `<<dunaj.math.random.spi.ad#IRngFactory,IRngFactory>>`
  protocol serves as an extension point for custom rngs.

  To generate numbers of other types, this namespace provides
  several <<Transducers,transducers>> to convert a collection of
  bytes to the collection of integers, floats, etc."
  {:authors ["Jozef Wagner"]
   :categories ["Primary" "Transducers"]}
  (:refer-clojure :exclude
   [rand-nth floats booleans rand seq satisfies? first boolean < neg?
    reduced? deftype <= let -> doto long double fn when > defn or
    name byte-array zero? rem nth nil? not defprotocol / >= loop
    merge cond reduced unchecked-byte next == count apply assoc
    defrecord and])
  (:require
   [clojure.bootstrap :refer [assert-primitive v1 not-implemented]]
   [dunaj.type :refer [Any Fn Va I U Maybe Predicate]]
   [dunaj.boolean :refer [Boolean+ and or not boolean]]
   [dunaj.math :refer [Integer+ Float+ >= rem < neg? <= zero? > == /]]
   [dunaj.math.unchecked :as mu]
   [dunaj.host :refer [Batch AnyBatch keyword->class]]
   [dunaj.host.int :refer
    [Int iint i0 idiv i>>> izero? iand iFF isub i8 i1 i3 iadd i<<
     idec irem ineg? i<= i< i>= i7 i== iinc inpos? iloop i4]]
   [dunaj.host.number :refer [long unchecked-byte double]]
   [dunaj.bit :as bit]
   [dunaj.compare :refer [nil?]]
   [dunaj.state :refer [ICloneable clone]]
   [dunaj.flow :refer [cond let loop when doto]]
   [dunaj.threading :refer [->]]
   [dunaj.poly :refer [defprotocol deftype defrecord satisfies?]]
   [dunaj.feature :refer [IConfig]]
   [dunaj.coll :refer
    [IRed IBatchedRed IHomogeneous IIndexed IReducing ISeqable
     count nth item-type assoc first reduced seq next
     reduced? postponed postponed? advance unsafe-advance!]]
   [dunaj.coll.helper :refer
    [reduce* reduce-batched* advance-fn finish-advance
     defxform defreducing reduced-advance strip-reduced
     reduce-with-batched* cloned-advance-fn red-to-seq]]
   [dunaj.function :refer [apply defn fn]]
   [dunaj.concurrent.thread :refer
    [Thread+ IThreadLocal current-thread ensure-thread-local]]
   [dunaj.host.array :refer [byte-array array-manager]]
   [dunaj.host.batch :refer [provide-batch-size batch-manager
                             select-item-type item-types-match?]]
   [dunaj.identifier :refer [Keyword name]]
   [dunaj.error :refer [index-out-of-bounds]]
   [dunaj.state.var :refer [Var def+]]
   [dunaj.coll.default :refer [->map]]
   [dunaj.coll.util :refer [merge]]
   [dunaj.coll.recipe :refer [->ObjectWrap cloning-advance]]))


;;;; Implementation details

(defn ^:private tlrng :- java.util.concurrent.ThreadLocalRandom
  "Returns a ThreadLocalRandom instance for the current thread."
  []
  (java.util.concurrent.ThreadLocalRandom/current))

(def+ ^:dynamic ^:private *default-rng-batch-size* :- Integer+
  32)

(defn ^:private reduce-batched-rng
  "Performs batched reduce with batches backed by array, filled in
  with nextf in each iteration."
  [nextf this requested-type size-hint reducef init]
  (let [batch-size (provide-batch-size size-hint)
        bm (batch-manager
            (select-item-type requested-type (item-type this)))
        arr (byte-array batch-size)
        batch :- AnyBatch (.wrap bm arr (i0) batch-size)
        af (fn af [ret batch :- AnyBatch]
             (cond (reduced? ret) ret
                   (postponed? ret)
                   (postponed @ret
                              #(af (advance ret) (clone batch))
                              #(af (unsafe-advance! ret) batch))
                   :else (do (.clear batch)
                             (nextf (.array batch))
                             (recur (reducef ret batch) batch))))]
    (af init batch)))

(deftype RWrap
  [ret :- Any, pval :- Integer+, left :- Int, other :- Any])

(deftype BRWrap
  [ret :- Any, pval :- Integer+, left :- Int,
   obatch :- Any, other :- Any])

(defn ^:private r-advance :- Any
  [ret :- Any, pval :- Integer+, left :- Int, other :- Any]
  (cond (reduced? ret) (reduced (->RWrap @ret pval left other))
        (postponed? ret)
        (postponed (->RWrap @ret pval left other)
                   #(r-advance (advance ret) pval left other)
                   #(r-advance (unsafe-advance! ret) pval left other))
        :else (->RWrap ret pval left other)))

;; ported from JDK sources
(defn ^:private iprocess
  [f ret val begin end]
  (if (nil? end)
    (f ret val)
    (let [bound (long end)
          origin (long (or begin 0))
          n (long (mu/subtract bound origin))
          xr (long val)
          m (long (mu/dec n))]
      (cond
       ;; range larger than int
       (neg? bound)
       (if (and (<= origin xr) (< xr end)) (f ret xr) ret)
       ;; bound is power of 2
       (zero? (bit/and n m))
       (f ret (mu/add origin (long (bit/and xr m))))
       ;; discard values which would corrupt uniformity
       :else (let [u (long (bit/>>> xr 1)), xr (long (rem u n))]
               (if (neg? (mu/add u (long (mu/subtract m xr))))
                 ret
                 (f ret (mu/add origin xr))))))))

(def+ ^:private double-unit :- java.lang.reflect.Field
  "Makes internal DOUBLE_UNIT field public. >= JDK8 only"
  (doto (.getDeclaredField java.util.Random "DOUBLE_UNIT")
    (.setAccessible true)))

(defn ^:private get-double-unit :- Float+
  "Returns DOUBLE_UNIT value"
  []
  (.get double-unit nil))

(def+ ^:private DOUBLE_UNIT
  (get-double-unit))

;; ported from JDK sources
(defn ^:private fprocess
  [f ret val begin end]
  (if (and (nil? end) (nil? begin))
    (f ret val)
    (let [bound (double (or end 1.0))
          origin (double (or begin 0.0))
          r val
          r (mu/add origin
                    (mu/multiply val (mu/subtract bound origin)))]
      (if (>= r bound)
        (f ret (java.lang.Double/longBitsToDouble
            (mu/subtract (java.lang.Double/doubleToLongBits bound)
                         1)))
        (f ret r)))))


;;;; Public API

(defprotocol IRngFactory
  "A factory protocol for random number generators."
  {:added v1}
  (-rng :- IRed
    "Returns a random number generator represented by a reducible
    collection that reduces infinite number of random bytes.

    Implementations usually return `IHomogeneous` collection with
    support for `IBatchedRed`. Some also support `IConfig` for
    storing the configuration of the rng."
    [this]))

(def+ default-rng-batch-size :- Var
  "A var holding a default rng batch size."
  {:added v1
   :see '[dunaj.coll/IBatchedRed]}
  (var *default-rng-batch-size*))

(deftype LocalRng
  "A type for thread local random number generator."
  [tlr :- java.util.Random, thread :- Thread]
  IRed
  (-reduce [this reducef init]
    (reduce-with-batched*
     nil *default-rng-batch-size* this reducef init))
  ISeqable
  (-seq [this] (red-to-seq this))
  IThreadLocal
  IHomogeneous
  (-item-type [this] (keyword->class :byte))
  IBatchedRed
  (-reduce-batched [this requested-type size-hint reducef init]
    (ensure-thread-local thread)
    (reduce-batched-rng
     #(.nextBytes tlr %) this requested-type size-hint reducef init)))

(defrecord LocalRngFactory
  "A factory record for thread local random number generator."
  []
  IRngFactory
  (-rng [this]
    (->LocalRng (java.util.concurrent.ThreadLocalRandom/current)
                (current-thread))))

(def+ thread-local-rng :- IRngFactory
  "A thread local random number generator factory.
  Has no options. Is thread local."
  {:added v1
   :see '[rng IRngFactory]
   :category "Primary"}
  (->LocalRngFactory))

(deftype SeedableRng
  "A type for seedable random number generator."
  [r :- java.util.Random]
  IRed
  (-reduce [this reducef init]
    (reduce-with-batched*
     nil *default-rng-batch-size* this reducef init))
  ISeqable
  (-seq [this] (red-to-seq this))
  IHomogeneous
  (-item-type [this] (keyword->class :byte))
  IBatchedRed
  (-reduce-batched [this requested-type size-hint reducef init]
    (reduce-batched-rng
     #(.nextBytes r %) this requested-type size-hint reducef init)))

(defrecord SeedableRngFactory
  "A factory record for seedable random number generator."
  [seed]
  IRngFactory
  (-rng [this]
    (->SeedableRng
     (if seed (java.util.Random. seed) (java.util.Random.)))))

(def+ seedable-rng :- IRngFactory
  "A seedable random number generator factory.

  Thread safe, but slower compared to other rngs. Supported options:

  * `:seed` - if not `nil`, used to seed the generator.
    It is guaranteed that same seed yields same set of items in the
    first reduction of the rng."
  {:added v1
   :see '[rng IRngFactory]
   :category "Primary"}
  (->SeedableRngFactory nil))

(deftype SplittableRng
  "A type for splittable random number generator."
  [sr :- java.util.SplittableRandom]
  IRed
  (-reduce [this reducef init]
    (let [ml (isub (idiv java.lang.Integer/SIZE java.lang.Byte/SIZE)
                   (i1))
          af (advance-fn [ret, left :- Int, val :- Int]
               (izero? left)
               (let [val (iint (.nextInt sr))]
                 (recur
                  (reducef ret (unchecked-byte (iand val (iFF))))
                  ml
                  (i>>> val (i8))))
               :else
               (recur (reducef ret (unchecked-byte (iand val (iFF))))
                      (isub left (i1))
                      (i>>> val (i8))))]
      (af init 0 0)))
  ISeqable
  (-seq [this] (red-to-seq this))
  ICloneable
  (-clone [this] (->SplittableRng (.split sr)))
  IHomogeneous
  (-item-type [this] (keyword->class :byte)))

(defrecord SplittableRngFactory
  "A factory record for splittable random number generator."
  [seed]
  IRngFactory
  (-rng [this]
    (->SplittableRng (if seed
                       (java.util.SplittableRandom. seed)
                       (java.util.SplittableRandom.)))))

(def+ splittable-rng :- IRngFactory
  "A splittable random number generator factory, useful in fork-join
  tasks. Split can be performed with
  `<<dunaj.state.api.ad#clone,dunaj.state/clone>>` function.
  Random number generators created with this factory are
  not thread safe.

  Supported options:

  * `:seed` - if not `nil`, used to seed the generator.
    It is guaranteed that same seed yields same set of items in the
    first reduction of the rng."
  {:added v1
   :see '[rng IRngFactory dunaj.state/clone]
   :category "Primary"}
  (->SplittableRngFactory nil))

(deftype SecureRng
  "A type for secure random number generator."
  [sr :- java.security.SecureRandom, config :- {}]
  IRed
  (-reduce [this reducef init]
    (reduce-with-batched*
     nil *default-rng-batch-size* this reducef init))
  ISeqable
  (-seq [this] (red-to-seq this))
  IConfig
  (-config [this]
    (merge config
           {:algorithm (.getAlgorithm sr)
            :provider (.getName (.getProvider sr))}))
  IHomogeneous
  (-item-type [this] (keyword->class :byte))
  IBatchedRed
  (-reduce-batched [this requested-type size-hint reducef init]
    (reduce-batched-rng
     #(.nextBytes sr %) this requested-type size-hint reducef init)))

(defrecord SecureRngFactory
  "A factory record for seedable random number generator."
  [strong algorithm provider]
  IRngFactory
  (-rng [this]
    (->SecureRng
     (cond strong (java.security.SecureRandom/getInstanceStrong)
           provider (java.security.SecureRandom/getInstance
                     (name algorithm) (name provider))
           algorithm (java.security.SecureRandom/getInstance
                      (name algorithm))
           :else (java.security.SecureRandom.))
     this)))

(def+ secure-rng :- IRngFactory
  "A secure random number generator factory.

  Is thread safe, may block if amount of entropy provided by system
  is not sufficient.

  Supported options:

  * `:strong` - if `true`, ignores `:algorithm` and `:provider` and
    uses default strong rng as specified by the host.
  * `:algorithm` - rng algorithm (e.g. `:NativePRNG` or `:SHA1PRNG`).
  * `:provider` - rng provider (e.g. `:SUN`). If set, `:algorithm`
    must also be non-nil.

  No support for `:seed`.
  In case no algorithm is provided, host's default is used instead.

  NOTE: Options used for the random number generator crated with
  this factory can be accessed with
  `<<dunaj.feature.api.ad#config,config>>`."
  {:added v1
   :see '[rng dunaj.feature/config IRngFactory]
   :category "Primary"}
  (->SecureRngFactory nil nil nil))

(defn rng :- IRed
  "Returns a newly created random number generator based on given
  `_factory_` and options (integer `_seed_` or `_key_`-`_val_`
  options). `_factory_` defaults to `_thread-local-rng_`.

  Returned random number generator is a homogeneous reducible
  collection, usually with support for batched reduce.

  The available options and support for `_seed_` is determined
  by a chosen `_factory_` implementation."
  {:added v1
   :see '[IRngFactory thread-local-rng secure-rng
          default-rng-batch-size integers floats rand
          dunaj.coll.util/reduce-batched dunaj.coll/item-type
          dunaj.coll/reduce]
   :category "Primary"}
  ([] (rng thread-local-rng))
  ([factory :- IRngFactory]
   (-rng factory))
  ([factory :- IRngFactory, seed :- Integer+]
   (-rng (assoc factory :seed seed)))
  ([factory :- IRngFactory, key :- Keyword, val :- Any
    & keyvals :- Any]
   (-rng (merge factory (apply ->map key val keyvals)))))

;;; Transformations

(deftype IntegerReducing
  "An augmented reducing type for integers* transducer."
  [r :- IReducing, begin :- (Maybe Integer+), end :- (Maybe Integer+)]
  IReducing
  (-init [this] (._init r))
  (-finish [this wrap]
    (-> (.-ret ^dunaj.math.random.RWrap (strip-reduced wrap))
        (reduced-advance (reduced? wrap))
        (finish-advance r)))
  (-wrap [this ret]
    (->RWrap (._wrap r ret) 0 (i7) 0))
  (-unwrap [this wrap]
    (._unwrap r (.-ret ^dunaj.math.random.RWrap wrap)))
  (-step [this wrap val]
    (let [ret (.-ret ^dunaj.math.random.RWrap wrap)
          pval (.-pval ^dunaj.math.random.RWrap wrap)
          left (.-left ^dunaj.math.random.RWrap wrap)
          other (.-other ^dunaj.math.random.RWrap wrap)]
      (let [nval (mu/add (bit/and 255 (long val))
                         (bit/<< (long pval) 8))]
        (cond (i== left (i4))
              (r-advance ret 0 (idec left) nval)
              (izero? left)
              (let [nval (iadd (iand (iFF) (iint val))
                               (i<< (iint pval) (i8)))
                    nval (mu/add (long nval)
                                 (bit/<< (long other) 32))]
                (r-advance
                 (iprocess #(._step r % %2) ret nval begin end)
                 0 (i7) 0))
              :else (r-advance ret nval (idec left) other))))))

(defxform integers*
  "Returns a transducer for converting collection of random bytes into
  a collection of random integers from the `[_begin_ _end_)`
  interval, both of which can be `nil`. Ignores `_begin_` if `_end_`
  is `nil`."
  {:added v1
   :see '[integers rand-integer rng booleans]
   :category "Transducers"}
  [begin :- (Maybe Integer+), end :- (Maybe Integer+)]
  ([r]
   (when (and begin end (>= begin end)) (throw (index-out-of-bounds)))
   (->IntegerReducing r begin end))
  :count false
  :fold false
  :section false
  :unpack false)

(deftype BatchableIntegers
  "A type for batchable collection of random integers."
  [coll begin end]
  IRed
  (-reduce [this reducef init]
    (reduce-with-batched* (keyword->class :long)
                          *default-rng-batch-size* this reducef init))
  ISeqable
  (-seq [this] (red-to-seq this))
  IHomogeneous
  (-item-type [this] nil)
  IBatchedRed
  (-reduce-batched [this requested-type size-hint reducef init]
    (let [batch-size (provide-batch-size size-hint)
          t (select-item-type requested-type (item-type this))
          bm (batch-manager t)
          batch :- AnyBatch (.allocate bm batch-size)
          other :- AnyBatch (.allocate bm batch-size)
          af (cloned-advance-fn
              [ret, pval, left :- Int, obatch :- AnyBatch,
               other :- AnyBatch, ibatch :- (Batch java.lang.Byte),
               idx :- Int]
              ;; output batch full, do the reduction step
              (not (.hasRemaining obatch))
              (let [nret (reducef ret (.flip obatch))]
                (recur nret pval left
                       (.clear other) obatch ibatch idx))
              ;; next number ready
              (ineg? left)
              (let [hval (bit/and (long pval)
                                  (bit/not (long 0xffffffff)))
                    lval (iint (bit/and (long pval)
                                        (long 0xffffffff)))
                    pval (mu/add hval (long lval))
                    nbatch (iprocess
                            #(.put bm % %2) obatch pval begin end)]
                (recur ret 0 (i7) nbatch other ibatch idx))
              ;; input batch empty
              (i>= idx (.limit ibatch))
              (->BRWrap ret pval left obatch other)
              ;; can read whole long
              (and (i== left (i7)) (< 7 (isub (.limit ibatch) idx)))
              (let [hval (.getInt ibatch idx)
                    lval (.getInt ibatch (iadd idx (i4)))
                    pval (mu/add (bit/<< (long hval) 32) (long lval))
                    nbatch (iprocess
                            #(.put bm % %2) obatch pval begin end)]
                (recur ret 0 (i7) nbatch other
                       ibatch (iadd idx (i8))))
              ;; read one byte
              :else
              (let [nb (.get ibatch idx)
                    nval (mu/add (bit/and 255 nb) (bit/<< pval 8))]
                (recur ret nval (idec left)
                       obatch other ibatch (iinc idx))))
          rf (fn [wrap batch :- AnyBatch]
               (let [ret (.-ret ^dunaj.math.random.BRWrap wrap)
                     pval (.-pval ^dunaj.math.random.BRWrap wrap)
                     left (.-left ^dunaj.math.random.BRWrap wrap)
                     obatch (.-obatch ^dunaj.math.random.BRWrap wrap)
                     other (.-other ^dunaj.math.random.BRWrap wrap)]
                 (af ret pval left obatch other batch (i0))))
          wrap (->BRWrap init 0 (i7) batch other)]
      (reduce-batched* (keyword->class :byte)
                       (mu/multiply batch-size 8) coll rf wrap))))

(defn integers :- IRed
  "Returns a transducer for converting collection of random bytes into
  a collection of random integers from the `[_begin_ _end_)`
  inteval, both of which can be `nil`. Default `_begin_` is set
  to 0. Ignores `_begin_` if `_end_` is `nil`."
  {:added v1
   :see '[integers* rand-integer rng booleans]
   :transducer true
   :category "Transducers"}
  ([] (integers* nil nil))
  ([coll :- []] (integers nil nil coll))
  ([end :- (Maybe Integer+), coll :- []] (integers nil end coll))
  ([begin :- (Maybe Integer+), end :- (Maybe Integer+), coll :- []]
   (if (and
        (satisfies? IBatchedRed coll)
        (item-types-match? (keyword->class :byte) (item-type coll)))
     (->BatchableIntegers coll begin end)
     (integers* begin end coll))))

(deftype FloatReducing
  "An augmented reducing type for floats* transducer."
  [r :- IReducing, begin :- (Maybe Integer+), end :- (Maybe Integer+)]
  IReducing
  (-init [this] (._init r))
  (-finish [this wrap]
    (-> (.-ret ^dunaj.math.random.RWrap (strip-reduced wrap))
        (reduced-advance (reduced? wrap))
        (finish-advance r)))
  (-wrap [this ret]
    (->RWrap (._wrap r ret) 0 (i7) nil))
  (-unwrap [this wrap]
    (._unwrap r (.-ret ^dunaj.math.random.RWrap wrap)))
  (-step [this wrap val]
    (let [ret (.-ret ^dunaj.math.random.RWrap wrap)
          pval (.-pval ^dunaj.math.random.RWrap wrap)
          left (.-left ^dunaj.math.random.RWrap wrap)]
      (cond
       (i== left (i7))
       (r-advance ret (bit/and 3 val) (idec left) nil)
       (i== left (i3))
       (let [nval (mu/add (bit/and 7 val) (bit/<< pval 3))]
         (r-advance ret nval (idec left) nil))
       (izero? left)
       (let [nval (mu/add (bit/and 255 val) (bit/<< pval 8))
             nval (mu/* nval DOUBLE_UNIT)]
         (r-advance (fprocess #(._step r % %2) ret nval begin end)
                     0 (i7) nil))
       :else
       (let [nval (mu/add (bit/and 255 val) (bit/<< pval 8))]
         (r-advance ret nval (idec left) nil))))))

(defxform floats*
  "A transducer for converting collection of random bytes into
  a collection of random floats from the `[_begin_ _end_)`
  inteval, both of which can be `nil`.
  Default `begin` value is 0.0 and default `end` value is 1.0."
  {:added v1
   :see '[floats rand gaussian]
   :category "Transducers"}
  [begin :- (Maybe Float+), end :- (Maybe Float+)]
  ([r]
   (when (and begin end (>= begin end)) (throw (index-out-of-bounds)))
   (->FloatReducing r begin end))
  :count false
  :fold false
  :section false
  :unpack false)

(deftype BatchableFloats
  "A type for batchable collection of random floats."
  [coll begin end]
  IRed
  (-reduce [this reducef init]
    (reduce-with-batched* (keyword->class :double)
                          *default-rng-batch-size* this reducef init))
  ISeqable
  (-seq [this] (red-to-seq this))
  IHomogeneous
  (-item-type [this] nil)
  IBatchedRed
  (-reduce-batched [this requested-type size-hint reducef init]
    (let [batch-size (provide-batch-size size-hint)
          t (select-item-type requested-type (item-type this))
          bm (batch-manager t)
          batch :- AnyBatch (.allocate bm batch-size)
          other :- AnyBatch (.allocate bm batch-size)
          af (cloned-advance-fn
              [ret, pval, left :- Int, obatch :- AnyBatch,
               other :- AnyBatch, ibatch :- (Batch java.lang.Byte),
               idx :- Int]
              ;; output batch full, do the reduction step
              (not (.hasRemaining obatch))
              (let [nret (reducef ret (.flip obatch))]
                (recur nret pval left
                       (.clear other) obatch ibatch idx))
              ;; next number ready
              (ineg? left)
              (let [nbatch (fprocess
                            #(.put bm % %2) obatch pval begin end)]
                (recur ret 0 (i7) nbatch other ibatch idx))
              ;; input batch empty
              (i>= idx (.limit ibatch))
              (->BRWrap ret pval left obatch other)
              ;; read one byte
              (i== left (i7))
              (let [nb (.get ibatch idx)
                    nval (bit/and 3 nb)]
                (recur ret nval (idec left)
                       obatch other ibatch (iinc idx)))
              (i== left (i3))
              (let [nb (.get ibatch idx)
                    nval (mu/add (bit/and 7 nb) (bit/<< pval 3))]
                (recur ret nval (idec left)
                       obatch other ibatch (iinc idx)))
              (izero? left)
              (let [nb (.get ibatch idx)
                    nval (mu/add (bit/and 255 nb) (bit/<< pval 8))
                    nval (mu/* nval DOUBLE_UNIT)]
                (recur ret nval (idec left)
                       obatch other ibatch (iinc idx)))
              :else
              (let [nb (.get ibatch idx)
                    nval (mu/add (bit/and 255 nb) (bit/<< pval 8))]
                (recur ret nval (idec left)
                       obatch other ibatch (iinc idx))))
          rf (fn [wrap batch :- AnyBatch]
               (let [ret (.-ret ^dunaj.math.random.BRWrap wrap)
                     pval (.-pval ^dunaj.math.random.BRWrap wrap)
                     left (.-left ^dunaj.math.random.BRWrap wrap)
                     obatch (.-obatch ^dunaj.math.random.BRWrap wrap)
                     other (.-other ^dunaj.math.random.BRWrap wrap)]
                 (af ret pval left obatch other batch (i0))))
          wrap (->BRWrap init 0 (i7) batch other)]
      (reduce-batched* (keyword->class :byte)
                       (mu/multiply batch-size 8) coll rf wrap))))

(defn floats :- IRed
  "A transducer for converting collection of random bytes into
  a collection of random floats from the `[_begin_ _end_)`
  inteval, both of which can be `nil`.
  Default `begin` value is 0.0 and default `end` value is 1.0."
  {:added v1
   :see '[floats* rand gaussian]
   :transducer true
   :category "Transducers"}
  ([] (floats*))
  ([coll :- []] (floats nil nil coll))
  ([end :- (Maybe Float+), coll :- []] (floats nil end coll))
  ([begin :- (Maybe Float+), end :- (Maybe Float+), coll :- []]
   (if (and
        (satisfies? IBatchedRed coll)
        (item-types-match? (keyword->class :byte) (item-type coll)))
     (->BatchableFloats coll begin end)
     (floats* begin end coll))))

(deftype GaussianReducing
  "An augmented reducing type for gaussian transducer."
  [r :- IReducing]
  IReducing
  (-init [this] (._init r))
  (-finish [this wrap]
    (-> (.-ret ^dunaj.math.random.RWrap (strip-reduced wrap))
        (reduced-advance (reduced? wrap))
        (finish-advance r)))
  (-wrap [this ret]
    (->RWrap (._wrap r ret) 0 (i1) nil))
  (-unwrap [this wrap]
    (._unwrap r (.-ret ^dunaj.math.random.RWrap wrap)))
  (-step [this wrap val]
    (let [ret (.-ret ^dunaj.math.random.RWrap wrap)
          pval (.-pval ^dunaj.math.random.RWrap wrap)
          left (.-left ^dunaj.math.random.RWrap wrap)]
      (if (izero? left)
        (let [v1 (double pval)
              v2 (double val)
              s (mu/add (mu/* v1 v1) (mu/* v2 v2))]
          (if (or (== s 0) (>= s 1))
            (r-advance ret 0 (i1) 0)
            (let [multiplier
                  (java.lang.StrictMath/sqrt
                   (/ (mu/multiply
                       -2 (java.lang.StrictMath/log s))
                      s))
                  ng (mu/multiply multiplier v2)
                  g  (mu/multiply multiplier v1)
                  nret (._step r ret g)
                  af (advance-fn [ret] (._step r ret ng))]
              (r-advance (af nret) 0 (i1) 0))))
        (r-advance ret val (i0) 0)))))

(defxform gaussian
  "Returns a transducer for converting collection of random bytes into
  a collection of random floats in normal distribution."
  {:added v1
   :see '[rand sample floats]
   :category "Transducers"}
  []
  ([r] ((floats* -1.0 1.0) (->GaussianReducing r)))
  :count false
  :fold false
  :section false
  :unpack false)

(defreducing BooleanReducing
  "An augmented reducing type for booleans transducer."
  [r :- IReducing]
  IReducing
  (-step [this wrap val]
    (let [af (advance-fn [ret val :- Int i :- Int]
               (ineg? i) ret
               :else (let [nb (i== (i1) (iand (i1) val))
                           nval (i>>> val (i1))]
                       (recur (._step r ret nb) nval (idec i))))]
      (af wrap (iint val) (i7)))))

(defxform booleans
  "Returns a transducer for converting collection of random bytes into
  a collection of random booleans."
  {:added v1
   :see '[rand-integer integers]
   :category "Transducers"}
  []
  ([r] (->BooleanReducing r))
  :count false
  :fold false
  :section false
  :unpack false)


;;; Convenience functions

(defn rand :- Float+
  "Returns a random floating point number, with optional
  `_begin_` (inclusive) and `_end_` (exclusive) bounds. Default value
  for `_begin_` is 0.0 and default value for `_end_` is 1.0."
  {:added v1
   :see '[rand-integer rand-nth floats gaussian]
   :category "Primary"
   :inline
   (fn
     ([]
      `(.nextDouble (java.util.concurrent.ThreadLocalRandom/current)))
     ([end]
      `(.nextDouble (java.util.concurrent.ThreadLocalRandom/current)
                    ~end))
     ([begin end]
      `(.nextDouble (java.util.concurrent.ThreadLocalRandom/current)
                    ~begin ~end)))}
  ([]
   (.nextDouble (tlrng)))
  ([end :- Float+]
   (.nextDouble (tlrng) end))
  ([begin :- Float+, end :- Float+]
   (.nextDouble (tlrng) begin end)))

(defn rand-integer :- Integer+
  "Returns a random integer number, with optional
  `_begin_` (inclusive) and `_end_` (exclusive) bounds.
  One arg version sets `_begin_` to 0."
  {:added v1
   :see '[rand rand-nth integers booleans]
   :category "Primary"
   :inline
   (fn
     ([]
      `(.nextLong (java.util.concurrent.ThreadLocalRandom/current)))
     ([end]
      `(.nextLong (java.util.concurrent.ThreadLocalRandom/current)
                  ~end))
     ([begin end]
      `(.nextLong (java.util.concurrent.ThreadLocalRandom/current)
                  ~begin ~end)))}
  ([] (.nextLong (tlrng)))
  ([end :- Integer+] (.nextLong (tlrng) (long end)))
  ([begin :- Integer+, end :- Integer+]
   (.nextLong (tlrng) (long begin) (long end))))

(defn rand-nth :- Any
  "Returns a random item of an `IIndexed` `_coll_`.
  Can supply a custom `_rng_`."
  {:added v1
   :see '[rand rand-integer sample rng]
   :category "Primary"}
  ([coll :- IIndexed]
   (nth coll (rand-integer (count coll))))
  ([coll :- IIndexed, rng :- IRed]
   (nth coll (first (integers (count coll) rng)))))

(deftype SampleReducing
  "Reducing type for sample."
  [prob :- Float+, sample-rng :- [], r :- IReducing]
  IReducing
  (-init [this] (._init r))
  (-finish [this wrap]
    (-> (.-ret ^dunaj.coll.recipe.ObjectWrap (strip-reduced wrap))
        (reduced-advance (reduced? wrap))
        (finish-advance r)))
  (-wrap [this ret]
    (->ObjectWrap (._wrap r ret) (seq (floats (clone sample-rng)))))
  (-unwrap [this wrap]
    (._unwrap r (.-ret ^dunaj.coll.recipe.ObjectWrap wrap)))
  (-step [this wrap val]
    (let [ret (.-ret ^dunaj.coll.recipe.ObjectWrap wrap)
          rs (.-x ^dunaj.coll.recipe.ObjectWrap wrap)
          nrs (next rs)]
      (cloning-advance (if (< (first rs) prob) (._step r ret val) ret)
                       (next rs))))
  (-step [this wrap val val2]
    (let [ret (.-ret ^dunaj.coll.recipe.ObjectWrap wrap)
          rs (.-x ^dunaj.coll.recipe.ObjectWrap wrap)
          nrs (next rs)]
      (cloning-advance
       (if (< (first rs) prob) (._step r ret val val2) ret)
       (next rs))))
  (-step [this wrap val val2 val3]
    (let [ret (.-ret ^dunaj.coll.recipe.ObjectWrap wrap)
          rs (.-x ^dunaj.coll.recipe.ObjectWrap wrap)
          nrs (next rs)]
      (cloning-advance
       (if (< (first rs) prob) (._step r ret val val2 val3) ret)
       (next rs))))
  (-step [this wrap val val2 val3 val4]
    (let [ret (.-ret ^dunaj.coll.recipe.ObjectWrap wrap)
          rs (.-x ^dunaj.coll.recipe.ObjectWrap wrap)
          nrs (next rs)]
      (cloning-advance
       (if (< (first rs) prob) (._step r ret val val2 val3 val4) ret)
       (next rs))))
  (-step [this wrap val val2 val3 val4 more]
    (let [ret (.-ret ^dunaj.coll.recipe.ObjectWrap wrap)
          rs (.-x ^dunaj.coll.recipe.ObjectWrap wrap)
          nrs (next rs)]
      (cloning-advance (if (< (first rs) prob)
                         (._step r ret val val2 val3 val4 more)
                         ret)
                       (next rs)))))

(defxform sample
  "Returns a transducer that randomly filters out step values with
  probability `_prob_`, a value from the interval `[0.0 - 1.0]`."
  {:added v1
   :see '[rand rand-integer rand-nth gaussian]
   :category "Transducers"}
  [prob :- Float+]
  ([r] (->SampleReducing prob (rng splittable-rng) r))
  :count false
  :section false)

;;; Assertions for primitives

(assert-primitive
 (rand-integer)
 (rand-integer 500)
 (rand-integer 3 6)
 (rand)
 (rand 500)
 (rand 3 6))
