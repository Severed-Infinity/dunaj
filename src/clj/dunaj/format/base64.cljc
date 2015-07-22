;; (C) 2013, 2015, Jozef Wagner. All rights reserved.
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

(ns dunaj.format.base64
  "Base64 formatter.

  Base64 factory support following options:

  * `:mode` - `Keyword`, `:basic`, `:safe`
  * `:padding` - `Boolean`, defaults to `true`"
  {:authors ["Jozef Wagner"]}
  (:refer-clojure :exclude
   [reduce satisfies? map < comp reduced? deftype * let -> identity fn
    when-not when defn or counted? nil? not identical? print / loop
    merge condp cond ex-info reduced defmacro case max count defrecord
    and])
  (:require
   [clojure.bootstrap :refer [v1]]
   [dunaj.type :refer [Maybe Va Fn Any AnyFn U I Signature]]
   [dunaj.boolean :refer [Boolean+ or not and]]
   [dunaj.host :refer
    [Array BatchManager keyword->class Batch class-instance?]]
   [dunaj.host.array :as dha]
   [dunaj.host.int :refer
    [Int iint iadd i0 imul i3 i4 ipos? imin idiv izero? i<= i2]]
   [dunaj.math :refer [Integer+ max < * ceil /]]
   [dunaj.compare :refer [identical? nil?]]
   [dunaj.state :refer [clone]]
   [dunaj.flow :refer [let when cond when-not loop condp case]]
   [dunaj.threading :refer [->]]
   [dunaj.poly :refer [deftype defrecord satisfies?]]
   [dunaj.coll :refer
    [IReducing reduced IRed IHomogeneous reduced? item-type reducing
     IBatchedRed postponed postponed? advance unsafe-advance! reduce
     ISeqable ICounted section sectionable? counted? count]]
   [dunaj.concurrent.forkjoin :refer
    [IFoldable -fold fork join invoke fold]]
   [dunaj.function :refer [Function fn defn identity comp]]
   [dunaj.coll.helper :refer
    [defxform cloned-advance-fn reduce-batched* reduced-advance
     reduce-augmented* reduce* finish-advance strip-reduced
     reduce-with-batched* red-to-seq]]
   [dunaj.error :refer [illegal-argument ex-info]]
   [dunaj.feature :refer [IConfig]]
   [dunaj.host.batch :refer [batch-manager item-types-match? batch]]
   [dunaj.host.array :refer [array-manager]]
   [dunaj.macro :refer [defmacro]]
   [dunaj.identifier :refer [Keyword]]
   [dunaj.state.var :refer [Var def+]]
   [dunaj.coll.util :refer [merge recipe]]
   [dunaj.coll.recipe :refer [map concat*]]
   [dunaj.format :refer [IParserFactory IPrinterFactory parse print
                         -parse -print default-formatter-batch-size]]
   [dunaj.format.charset :refer [utf-8]]
   [dunaj.format.helper :refer [prepend-unread]]))


;;;; Implementation details

(def+ Base64Batch :- Signature
  (Batch java.lang.Byte))

(defn ^:private get-decoder :- java.util.Base64$Decoder
  "Returns new decoder instance based on given input args."
  [mode :- Keyword]
  (if (identical? :safe mode)
    (java.util.Base64/getUrlDecoder)
    (java.util.Base64/getDecoder)))

(defn ^:private get-encoder :- java.util.Base64$Encoder
  "Returns new encoder instance based on given input args."
  [mode :- Keyword, padding :- Boolean+]
  (let [enc (if (identical? :safe mode)
              (java.util.Base64/getUrlEncoder)
              (java.util.Base64/getEncoder))]
    (if padding enc (.withoutPadding enc))))

(defn ^:private get-coder
  :- (U java.util.Base64$Encoder
        java.util.Base64$Decoder)
  "Returns encoder or decoder based on given input args."
  [code-mode :- Keyword, mode :- Keyword, padding :- Boolean+]
  (if (identical? :encode code-mode)
    (get-encoder mode padding)
    (get-decoder mode)))

(defn code
  [coder :- (U java.util.Base64$Encoder
               java.util.Base64$Decoder)
   input :- Base64Batch]
  (if (class-instance? java.util.Base64$Encoder coder)
    (.encode ^java.util.Base64$Encoder coder input)
    (.decode ^java.util.Base64$Decoder coder input)))

(defn block-length :- Int
  [code-mode :- Keyword]
  (if (identical? :encode code-mode) (i3) (i4)))

(defn batch-size-hint
  [block-length :- Integer+]
  (imul (iint @default-formatter-batch-size) block-length))

(deftype B64Wrap [ret :- Any, unread-batch :- (Maybe Base64Batch)])

(deftype BatchedBase64CoderReducing
  [r :- IReducing,
   coder :- (U java.util.Base64$Encoder java.util.Base64$Decoder),
   block-length :- Integer+, code-mode :- Keyword,
   batch-size :- Integer+, fbm :- BatchManager, tbm :- BatchManager]
  IReducing
  (-init [this] (._init r))
  (-finish [this wrap]
    (let [w (strip-reduced wrap)
          ret (.-ret ^dunaj.format.base64.B64Wrap w)
          unread-batch (.-unread_batch ^dunaj.format.base64.B64Wrap w)
          af (fn af [ret :- Any, unread-batch :- Base64Batch]
               (cond
                 (reduced? ret) ret
                 (postponed? ret)
                 (postponed @ret
                            #(af (advance ret) (clone unread-batch))
                            #(af (unsafe-advance! ret) unread-batch))
                 (nil? unread-batch) ret
                 (ipos? (.position unread-batch))
                 (let [to-batch (code coder (.flip unread-batch))
                       _ (.clear unread-batch)]
                   (recur (._step r ret to-batch) nil))
                 ret))]
      (-> (af ret unread-batch)
          (reduced-advance (reduced? wrap))
          (finish-advance r))))
  (-wrap [this ret]
    (let [unread-batch (.allocate fbm batch-size)]
      (->B64Wrap (._wrap r ret) unread-batch)))
  (-unwrap [this wrap]
    (._unwrap r (.-ret ^dunaj.format.base64.B64Wrap wrap)))
  (-step [this wrap batch]
    (let [ret (.-ret ^dunaj.format.base64.B64Wrap wrap)
          unread-batch
          (.-unread_batch ^dunaj.format.base64.B64Wrap wrap)
          af (fn af [ret :- Any, from-batch :- Base64Batch,
                    unread-batch :- Base64Batch]
               (cond (reduced? ret)
                     (reduced (->B64Wrap @ret nil))
                     (postponed? ret)
                     (postponed
                      (->B64Wrap @ret unread-batch)
                      #(af (advance ret) (clone from-batch)
                           (clone unread-batch))
                      #(af (unsafe-advance! ret) from-batch
                           unread-batch))
                     ;; block is ready, process
                     (not (.hasRemaining unread-batch))
                     (let [to-batch (code coder (.flip unread-batch))
                           _ (.clear unread-batch)]
                       #_(clojure.core/println
                        "got back" (.limit to-batch))
                       (recur (._step r ret to-batch)
                              from-batch unread-batch))
                     ;; TODO: shortcut if from-batch size is nice
                     ;; has data to put into unread-batch
                     (.hasRemaining from-batch)
                     (let [old-lim (.limit from-batch)
                           max-size (imin (.remaining unread-batch)
                                          (.remaining from-batch))
                           lim (iadd max-size (.position from-batch))]
                       (.limit from-batch lim)
                       (.put unread-batch from-batch)
                       (.limit from-batch old-lim)
                       (recur ret from-batch unread-batch))
                     :else (->B64Wrap ret unread-batch)))]
      (af ret batch unread-batch))))

(defxform batched-base64-coder*
  [coder :- (U java.util.Base64$Encoder java.util.Base64$Decoder),
   block-length :- Integer+, code-mode :- Keyword]
  ([r] (let [from-type (keyword->class :byte)
             to-type (keyword->class :byte)
             batch-size (batch-size-hint block-length)
             fbm (batch-manager from-type)
             tbm (batch-manager to-type)]
         (->BatchedBase64CoderReducing
          r coder block-length code-mode batch-size fbm tbm)))
  :count false
  :unpack false
  :fold false
  :section false)

(deftype BatchedBase64Coder
  "A type for base64 encoder and decoder."
  [coll :- (Maybe IRed),
   coder :- (U java.util.Base64$Encoder java.util.Base64$Decoder),
   block-length :- Integer+, code-mode :- Keyword, config :- {}]
  IRed
  (-reduce [this reducef init]
    (let [to-type (keyword->class :byte)
          tbm (batch-manager to-type)
          size-hint (batch-size-hint block-length)]
      (reduce-with-batched* to-type size-hint this reducef init)))
  IHomogeneous
  (-item-type [this] (keyword->class :byte))
  IBatchedRed
  (-reduce-batched [this item-type size-hint reducef init]
    (let [from-type (keyword->class :byte)]
      (reduce-augmented*
       coll
       #(reduce-batched* from-type size-hint % %2 %3)
       ((batched-base64-coder* coder block-length code-mode)
        (reducing reducef init)))))
  IFoldable
  (-fold [this reduce-fn pool n combinef reducef]
    (cond
      (or (not (sectionable? coll))
          (not (counted? coll)))
      (reduce-fn this reducef (combinef))
      :let [count (count coll)]
      (izero? count) (combinef)
      (i<= count (iint n)) (reduce-fn this reducef (combinef))
      [split (if (identical? :encode code-mode)
               (imul (idiv (idiv count (i2)) (i3)) (i3))
               (imul (idiv (idiv count (i2)) (i4)) (i4)))]
      (let [c1 (->BatchedBase64Coder (section coll 0 split) coder
                                     block-length code-mode config)
            c2 (->BatchedBase64Coder (section coll split) coder
                                     block-length code-mode config)
            fc (fn [child]
                 #(-fold child reduce-fn pool n combinef reducef))]
        (invoke pool #(let [f1 (fc c1), t2 (fork (fc c2))]
                        (combinef (f1) (join t2)))))
      :else (reduce-fn this reducef (combinef))))
  IConfig
  (-config [this]
    (merge config {:block-length block-length :code-mode code-mode})))

(defrecord Base64FormatterFactory
  [mode :- Keyword, padding :- Boolean+]
  IParserFactory
  (-parse [this]
    (let [coder (get-coder :decode mode padding)
          bl (block-length :decode)]
      (comp (batch (keyword->class :byte) (batch-size-hint bl))
            (batched-base64-coder* coder bl :decode)
            (concat*))))
  (-parse [this coll]
    (if (and (satisfies? IBatchedRed coll)
             (item-types-match? (keyword->class :byte)
                                (item-type coll)))
      (->BatchedBase64Coder
       coll (get-coder :decode mode padding)
       (block-length :decode) :decode this)
      (recipe (-parse this) coll)))
  IPrinterFactory
  (-print [this]
    (let [coder (get-coder :encode mode padding)
          bl (block-length :encode)]
      (comp
       (batch (keyword->class :byte) (batch-size-hint bl))
       (batched-base64-coder* coder bl :encode)
       (concat*))))
  (-print [this coll]
    (if (and (satisfies? IBatchedRed coll)
             (item-types-match? (keyword->class :byte)
                                (item-type coll)))
      (->BatchedBase64Coder
       coll (get-coder :encode mode padding)
       (block-length :encode) :encode this)
      (recipe (-print this) coll))))


;;;; Public API

(def+ base64 :- (I IParserFactory IPrinterFactory)
  "Basic base64 formatter factory."
  {:added v1
   :see '[base64-safe]}
  (->Base64FormatterFactory :basic true))

(def+ base64-safe :- (I IParserFactory IPrinterFactory)
  "Safe base64 formatter factory."
  {:added v1
   :see '[base64]}
  (->Base64FormatterFactory :safe true))
