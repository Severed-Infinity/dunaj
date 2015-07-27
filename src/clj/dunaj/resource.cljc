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

(ns dunaj.resource
  "Computer and network resources, systems, scopes.

  WARNING: dunaj.resource API is a very early experiment with
           incomplete functionality. Expect rough edges.

  Basic characteristics of a resource is its limited availability
  within a computer system. Additional informations are available in
  the <<resources.ad#,resource specification>>.

  User acquires a resource with acquire! function. Resource
  is a stateful object and is released within a given scope.
  Resource can be in one of following states:

  * OPEN (dunaj.state/open?)
  * FAILED (dunaj.error/error)
  * CANCELLED (dunaj.state/cancelled?)
  * CLOSED (not observable)
  * RELEASING (not observable)

  State transitions:

  * after acquiring: OPEN or FAILED
  * when working with resource: OPEN -> OPEN, OPEN -> FAILED
  * evaluation hits end of scope: OPEN -> RELEASING
  * after release is done: RELEASING -> CLOSED, RELEASING -> FAILED
  * cancelling the release process: RELEASING -> CANCELLED.

  NOTE: Resources with composite bands implement classic collection
  protocols (IIndexed, ILookup, ICounted). Resources where
  individuals items can be sent/received implement ISourcePort
  and ITargetPort. If resource contains single value, it can
  implement IReference or IBlockingReference.
  Resources can implement custom error handling."
  {:authors ["Jozef Wagner"]
   :categories ["Primary" "Scope" "System"]}
  (:refer-clojure :exclude
   [slurp format read seq reduce contains? butlast satisfies? first
    atom = map rest cons pos? if-not sequential? doseq neg? reduced?
    deftype when-let zipmap min conj let map? get into meta var?
    future fn empty? key string? when-not vec when defn declare or
    name some nil? update reify update-in instance? val not type
    identical? defprotocol print loop merge integer? condp cond
    ex-info partial reduced defmacro odd? keyword next if-let io! max
    == count apply assoc defrecord and ->> get-in])
  (:require
   [clojure.bootstrap :refer [v1]]
   [clojure.core.async]
   [dunaj.type :refer [Fn AnyFn Any Maybe U I]]
   [dunaj.boolean :refer [Boolean+ or and not]]
   [dunaj.host :refer [keyword->class class-instance?]]
   [dunaj.host.int :refer [i0]]
   [dunaj.math :refer [Integer+ integer? pos? odd? max neg? == min]]
   [dunaj.state :refer
    [IAtomic cancel! alter! cas! reference? IOpenAware adjust! io!
     atomic? switch! IAdjustable IReference IMutable ensure-io
     ensure-open open?]]
   [dunaj.compare :refer [identical? = nil? sentinel]]
   [dunaj.flow :refer
    [when let when-not when-let loop cond condp if-let if-not when]]
   [dunaj.threading :refer [->>]]
   [dunaj.buffer :refer [buffer]]
   [dunaj.poly :refer
    [Record instance? reify satisfies? defprotocol deftype defrecord
     type]]
   [dunaj.feature :refer [IConfig meta]]
   [dunaj.coll :refer
    [count contains? next first IRed IBatchedRed sequential? empty?
     -reduce-batched reduced? ICounted get reduce Postponed full? seq
     map? assoc update-in conj postponed? unsafe-advance! postponed
     lookup? rest reduced update provide-collection get-in]]
   [dunaj.function :refer [apply defn invocable? fn partial]]
   [dunaj.host.batch :refer [select-item-type batch-manager]]
   [dunaj.concurrent.thread :refer [current-thread sleep]]
   [dunaj.concurrent :refer [future]]
   [dunaj.concurrent.port :as dp :refer
    [ISourcePort chan <!! thread >!! timeout alts!!]]
   [dunaj.string :as ds :refer [String+ string? ->str]]
   [dunaj.time :refer [IDuration milliseconds]]
   [dunaj.macro :refer [defmacro]]
   [dunaj.identifier :refer [Keyword keyword name]]
   [dunaj.state.weak :refer [weak]]
   [dunaj.state.basic :refer [atom]]
   [dunaj.state.var :refer [Var var? def+ declare alter-root!]]
   [dunaj.error :refer [IException ex-info illegal-argument
                        unsupported-operation npe fail-aware? error]]
   [dunaj.uri :refer [Uri uri uri?]]
   [dunaj.coll.tuple :refer [tuple val key pair]]
   [dunaj.coll.util :refer
    [into revlist doseq merge some recipe butlast]]
   [dunaj.coll.recipe :refer [map]]
   [dunaj.coll.cons-seq :refer [cons]]
   [dunaj.coll.default :refer [->map zipmap vec]]
   [dunaj.format :refer [IParserFactory IPrinterFactory print parse]]
   [dunaj.format.charset :refer [utf-8]]
   [dunaj.type.validation :refer [validate-value]]))


;;;; Implementation details

(def+ ^:dynamic *resource-providers*
  "Registered resource providers."
  {})

(def+ ^:dynamic *scope*
  "Dynamic Var for current scope."
  nil)

(def+ ^:dynamic *scope-thread*
  "Dynamic Var for current scope home thread."
  nil)


;;;; Public API

(defprotocol IReleasable
  "A state protocol for releasable objects. Note that there are no
  public functions for this protocol, as the release process should
  be handled through scopes. Types implementing `IReleasable` must
  also call `-release!` in their `finalize` method.
  `defreleasable` macro handles this transparently."
  {:added v1
   :category "Scope"
   :see '[release-scope! with-scope]
   :predicate 'releasable?}
  (-release! :- nil
    "Releases this object, blocking until the release is not
    finished (either successfully or not). Releasing an object not
    in OPEN state must be a nop. Returns nil. Throws if
    release has failed. Object must not be left in OPEN state both
    during the release process is running and after the release
    process is done (either successfully or not).
    Thread safe resources must manage their state so that several
    threads calling -release!, open?, etc. will work fine."
    [this]))

;;; Scopes

(defn deref-scope! :- Any
  "Returns `_scope_` if it is map, safely nilify and deref if
  `_scope_` is an atomic reference."
  [scope :- Any]
  (if (atomic? scope) (switch! scope nil) scope))

(defn merge-ex! :- java.lang.Throwable
  "Merge exceptions with the help of a supressed feature."
  [old-ex :- (Maybe java.lang.Throwable), ex :- java.lang.Throwable]
  (if old-ex (do (.addSuppressed old-ex ex) old-ex) ex))

(defmacro release-throw
  ([s val opts]
   `(throw
     (dunaj.error/ex-info ~s {:value ~val :opts ~opts})))
  ([s val opts cause]
   `(throw
     (dunaj.error/ex-info
      ~s {:value ~val :opts ~opts :cause ~cause}))))

(defn release-releasable! :- nil
  "Releases a releasable `val`. Returns nil."
  [val :- IReleasable, opts :- {}]
  (let [t (:timeout opts)]
    (cond (not (and (integer? t) (pos? t))) (-release! val)
          :let [c (thread (try (-release! val)
                               :done
                               (catch java.lang.Exception e e)))
                tc (timeout t)
                [v p] (alts!! [c tc])]
          ;; releasing has timed out
          (identical? p tc)
          (condp identical? (:timeout-mode opts)
            :throw
            (release-throw
             "Release timeout has been reached for a scoped item."
             val opts)
            :cancel
            (do (cancel! val)
                (when-let [ex (and (fail-aware? val) (error val))]
                  (release-throw
                   "Scoped item has failed to cancel."
                   val opts ex)))
            nil)
          ;; releasing has finished or failed
          (not (identical? :done v)) (throw v))))

(defn release-item! :- nil
  "Releases one scope item and returns nil. May throw."
  [val :- Any, opts :- {}, exr :- IAtomic]
  (loop [val val]
    (cond (releasable? val) (release-releasable! val opts)
          (nil? val) nil
          (sequential? val) (doseq [v val]
                              (try (release-item! v opts exr)
                                   (catch java.lang.Exception ex
                                     (alter! exr merge-ex! ex))))
          (reference? val) (recur @val)
          (invocable? val) (recur (val opts))
          :else (release-throw "unknown item in scope." val opts))))

(defn in-scope? :- Boolean+
  "Returns `true` if there is a current scope present,
  `false` otherwise."
  {:added v1
   :category "Scope"
   :see '[with-scope]}
  []
  (not (or (nil? *scope*) (nil? @*scope*))))

(defn release-scope! :- {}
  "Releases `_scope_` based on `_mode_` and returns scope map.
  `_scope_` can either be a normal scope (map), or an atom holding
  scope map. `_mode_` is one of `:exit`, `:success`, `:fail` and
  defaults to `:exit`.

  Throws if releasing has failed. Puts additional exceptions as
  supressed into throwed one, if multiple items have throwed.
  For failed release, initial exception may be provided in `_ex_`.

  Mutates `_scope_` if it was an atom."
  {:added v1
   :category "Scope"
   :see '[grab-scope]}
  ([scope :- (U {} IAtomic)]
   (release-scope! scope :exit))
  ([scope :- (U {} IAtomic), mode :- Keyword]
   (release-scope! scope mode nil))
  ([scope :- (U {} IAtomic), mode :- Keyword,
    ex :- (Maybe java.lang.Throwable)]
   (let [exr (atom ex)
         scope (deref-scope! scope)]
     (doseq [item (:items scope)
             :when (or (not (:mode item))
                       (identical? :exit (:mode item))
                       (identical? mode (:mode item)))]
       (try
         (release-item! (:value item) (merge scope item) exr)
         (catch java.lang.Exception e (alter! exr merge-ex! e))))
     (when-let [new-ex @exr] (throw new-ex))
     scope)))

(defn scope-push! :- {}
  "Pushes item or scope into current scope. Returns scope map.
  Item is a map with following keys:

  * `:value` - fn or releasable or weak reference to the releasable
  * `:mode` - optional - `:exit`/`:success`/`:fail`
  * `:timeout` - optional - `nil` (blocks)/`IDuration`
  * `:timeout-mode` - optional - `nil` (ignore)/`:cancel`/`:throw`

  Scope defaults (see docs for `with-scope`) are used when some
  optional entries are not specified.
  If `_value-or-map_` is a scope, all its options, including those
  passed as additional args, are ignored.
  Mutates `_value-or-map_` if it was an atom."
  {:added v1
   :category "Scope"
   :see '[grab-scope with-scope]}
  [value-or-map & options]
  (when-not (in-scope?) (throw (npe "No scope defined.")))
  (when (and (not (:pervasive @*scope*))
             (not (identical? (current-thread) *scope-thread*)))
    (throw (unsupported-operation "Scope is not pervasive.")))
  (let [item (if (odd? (count options))
               (->> options
                    (cons value-or-map)
                    (apply clojure.core/hash-map))
               (let [m (cond (atomic? value-or-map)
                             (deref-scope! value-or-map)
                             (map? value-or-map)
                             value-or-map
                             :else {:value value-or-map})]
                 (merge m (apply clojure.core/hash-map options))))
        item (if (releasable? (:value item))
               (update-in item [:value] weak)
               item)]
    (if (and (contains? item :items) (not (contains? item :value)))
      ;; push whole scope, ignoring options
      (alter! *scope* update-in [:items] into (revlist (:items item)))
      ;; push one item
      (alter! *scope* update-in [:items] conj item))))

(defmacro grab-scope
  "Executes `_body_` in a new scope. Returns pair `[ret scope-atom]`.
  Takes an optional existing scope map or scope atom
  or defaults map as first arg. Following keys are valid:

  * `:pervasive` - Boolean, defaults to `nil`
  * `:timeout` - `nil` (blocks, default)/`IDuration`
  * `:timeout-mode` - `nil` (ignore, default)/`:cancel`/`:throw`

  Note if pervasive is enabled, the returned atom can be updated
  by child threads even after `grab-scope` finishes.
  Mutates first arg if it is atom and there are more than one args.
  Throws when `_body_` throws, releasing scope with `:fail` mode."
  {:added v1
   :category "Scope"
   :see '[with-scope release-scope! scope-push! in-scope?]}
  [& body]
  (let [scope {:items ()}
        defaults? (and (next body) (or (map? (first body))
                                       (atomic? (first body))))
        scope (if defaults?
                (merge scope (deref-scope! (first body)))
                scope)
        body (if defaults? (next body) body)]
    `(let [scope# (dunaj.state.basic/atom ~scope)
           ret# (try
                  (dunaj.state.var/with-bindings
                    {(var dunaj.resource/*scope*)
                     scope#
                     (var
                      dunaj.resource/*scope-thread*)
                     (dunaj.concurrent.thread/current-thread)}
                    ~@body)
                  (catch java.lang.Throwable t#
                    (release-scope! scope# :fail t#)))]
       (pair ret# scope#))))

(defmacro with-scope
  "Executes `_body_` in a new scope. Returns result from evaluating
  `_body_`. Takes an optional existing scope map or scope atom
  or defaults map as first arg. Following keys are valid:

  * `:pervasive` - Boolean, defaults to `nil`
  * `:timeout` - `nil` (blocks, default)/`IDuration`
  * `:timeout-mode` - `nil` (ignore, default)/`:cancel`/`:throw`

  Note if pervasive is enabled, user must manually handle waiting
  for child threads or else scoped items used in child threads gets
  released too early.

  Mutates first arg if it is atom and there are more than one args.
  Automatically releases scope after `_body_` is evaluated, with
  `:success` mode, throwing if release fails.
  Throws when `_body_` throws, releasing scope with `:fail` mode."
  {:added v1
   :category "Scope"
   :see '[with-io-scope grab-scope scope-push! release-scope!
          in-scope?]}
  [& body]
  (let [scope {:items ()}
        defaults? (and (next body) (or (map? (first body))
                                       (atomic? (first body))))
        scope (if defaults?
                (merge scope (deref-scope! (first body)))
                scope)
        body (if defaults? (next body) body)]
    `(let [scope# (dunaj.state.basic/atom ~scope)
           ret# (try
                  (dunaj.state.var/with-bindings
                    {(var dunaj.resource/*scope*)
                     scope#
                     (var
                      dunaj.resource/*scope-thread*)
                     (dunaj.concurrent.thread/current-thread)}
                    ~@body)
                  (catch java.lang.Throwable t#
                    (release-scope! scope# :fail t#)))]
       (release-scope! scope# :success)
       ret#)))

(defmacro with-io-scope
  "Like `with-scope`, but additionally puts body inside `io!` block."
  {:added v1
   :category "Scope"
   :see '[with-scope in-scope? dunaj.state/io!]}
  [& body]
  `(dunaj.state/io! (with-scope ~@body)))

;;; Resource Factories

(defprotocol IConfigurable
  "A feature protocol for objects that can be automatically
  configured, either fully or partially, from external configuration
  map."
  {:added v1
   :see '[system start!]
   :category "Configuration"}
  (-autoconf :- []
    "Returns collection of maps that specifies how individual
    fields should be configured. Example:
    `[{:key :field-name :path [:path :in :cfg] :type ValueType}]`
    When no type is specified, configuration framework should use
    field's type signature for that. Can return just path instead, if
    all fields are on the same path and default types should be used.
    Can return `:default` in which case the configuration framework
    constructs path from type's namespace."
    [this]))

(defprotocol IAcquirableFactory
  "A factory protocol for objects that can be acquired.
  By convention, resource uri (if object to be acquired is a
  resource) is stored under `:uri` key and scope options, if any,
  are stored under `:scope` key.

  If a factory object also implements `ISystem` protocol,
  `start!` should be used instead, which implicitly calls
  `acquire!` when all dependencies are resolved."
  {:added v1
   :category "Primary"
   :predicate 'acquirable?
   :see '[acquire! resource]
   :forbids-extensions true}
  (-acquire! :- Any
    "Returns an acquired object. Does not neccessarily have to
    be releasable."
    [factory]))

(defn resource :- (Maybe IAcquirableFactory)
  "Returns an acquirable factory based on given `_x_`, passing
  `_opts_` into it. Uses uri scheme to map resource type from
  currently registered resource providers.
  Returns `nil` if resource type cannot be found."
  {:added v1
   :category "Primary"
   :see '[dunaj.resource.helper/register-factory! acquire!]}
  [x :- Any, & {:as opts}]
  (cond
    (not (or (string? x) (uri? x))) (merge x opts)
    :let [uri (uri x)]
    [factory (*resource-providers* (.getScheme ^java.net.URI uri))]
    (merge factory (assoc opts :uri x))))

(defn acquire! :- Any
  "Acquires object from a given `_factory_`, pushes it
  into current scope if it is releasable and returns it.

  Uses `:scope` entry in `_factory_` to customize scope pushing
  process. Classic scope keys are `:timeout`, `:timeout-mode` and
  `:mode`.

  Special `:scope` option called `:release-fn` overrides releasing.
  If set, it must contain two args fn accepting weak reference to
  acquired object and scope options. This custom function will then
  be called when releasing instead of standard release process, and
  it can return `nil` or other releasable object (or reference to
  one), which will then be released.

  Throws if object cannot be acquired."
  {:added v1
   :category "Primary"
   :see '[with-scope resource IAcquirableFactory]}
  ([factory :- IAcquirableFactory]
   (when-not (in-scope?) (throw (npe "No scope defined.")))
   (let [res (-acquire! factory)
         wres (weak res)
         rfn (:release-fn (:scope factory))
         val (if rfn (partial rfn wres) wres)]
     (when (or (releasable? res) rfn)
       (scope-push! (merge (:scope factory) {:value val})))
     res))
  ([factory :- IAcquirableFactory, uri-or-map :- Any & options :- Any]
   (if (odd? (count options))
     (->> options
          (cons uri-or-map)
          (apply clojure.core/hash-map)
          (merge factory)
          acquire!)
     (let [mm (if (map? uri-or-map) uri-or-map {:uri uri-or-map})]
       (acquire! (merge factory mm
                        (apply clojure.core/hash-map options)))))))

;;; Resource protocols

(defprotocol IImmutableReadable
  "A protocol for objects that support immutable reads.
  This protocol should be implemented on resource factories, not
  on acquired resources."
  {:added v1
   :category "Primary"
   :see '[read read!]}
  (-read :- IRed
    "Returns finite collection recipe attached to `_this_`."
    [this]))

(defn read :- IRed
  "Returns finite collection recipe attached to the object `_x_`."
  {:added v1
   :category "Primary"
   :see '[read! readable? slurp]}
  [x :- (U String+ Uri IImmutableReadable)]
  (-read (resource x)))

(defprotocol IReadable
  "A protocol for readable objects. May return postponed object
  if object is in non-blocking mode. It is a conscious decision
  to not support timeouts. Such logic should be handled up in the
  abstraction by e.g. channels."
  {:added v1
   :category "Primary"
   :see '[read! read]
   :predicate 'readable?
   :forbid-extensions true}
  (-read! :- IRed
    "Returns collection recipe attached to `_this_`.
    Returned collection recipe must be reduced inside `io!` block.
    The returned collection recipe may be infinite and may block.

    Reducing collection while it is being released may end
    by reduction premature end or by an I/O exception.
    It is thus advised to either expect an exception or to
    stop reducing before releasing the resource.

    May return postponed object if `_this_` is in a
    non-blocking mode."
    [this]))

(defn read! :- IRed
  "Returns collection recipe attached to the object `_x_`.
  Returned collection recipe must be reduced inside `io!` block.
  The returned collection recipe may be infinite and may block.

  If the resource is seekable, the position after reducing collection
  recipe from `read!` is undefined.

  Reducing collection while it is being released may end by reducing
  ending or by I/O exception. It is thus advised to either expect an
  exception or to stop reducing before releasing the resource.
  May return postponed object if `_x_` is in a non-blocking mode."
  {:added v1
   :category "Primary"
   :see '[readable? read-one! read slurp exchange! transform format]}
  [x :- IReadable]
  (let [res (if (readable? x) x (acquire! (resource x)))]
    (-read! res)))

(defn read-one! :- Any
  "Returns first read item from `_x_`, potentionally discarding
  other items. If the `_x_` is seekable,
  the position after `read-one!` is undefined.
  May return postponed object if `_x_` is in non-blocking mode."
  {:added v1
   :category "Primary"
   :see '[read! readable?]}
  [x :- IReadable]
  (io! (first (read! x))))

(defprotocol IWritable
  "A protocol for writable object. It is a conscious decision to not
  support timeouts. Such logic should be handled up in the
  abstraction tree by e.g. channels."
  {:added v1
   :category "Primary"
   :see '[write! write-one!]
   :predicate 'writable?
   :forbid-extensions true}
  (-write! :- (U Integer+ Postponed)
    "Writes `_coll_` into `_this_` and returns number of items
    written. Blocks or returns postponed object if in non-blocking
    mode. Throws if I/O error occurs."
    [this coll :- IRed]))

(defn write! :- (U Integer+ Postponed)
  "Writes `_coll_` into object `_x_` and returns number of items
  written. Takes an optional transducer `_xf_`.

  Blocks or returns postponed object if in non-blocking mode.
  Throws if I/O error occurs. Writing while the resource is being
  released ends by I/O exception too. It is thus advised to expect
  an exception or to finish writing before releasing the resource."
  {:added v1
   :category "Primary"
   :see '[writable? write-one! spit! exchange! transform format]}
  ([x :- Any, coll :- IRed]
   (let [res (if (writable? x) x (acquire! (resource x)))]
     (-write! res coll)))
  ([x :- Any, xform :- Any, coll :- IRed]
   (write! x (recipe xform coll))))

(defn write-one! :- (U Integer+ Postponed)
  "Writes `_val_` into object `_x_` and returns number of items
  written. Takes an optional transducer `_xf_`.
  Blocks or returns postponed object if in non-blocking mode.

  Throws if I/O error occurs. Writing while the resource is being
  released ends by I/O exception too. It is thus advised to expect
  an exception or to finish writing before releasing the resource."
  {:added v1
   :category "Primary"
   :see '[writable? write!]}
  [x :- Any, val :- Any]
  (write! x (tuple val)))

;;; Control and Status

(defprotocol IControllable
  "A protocol for resources with controllers."
  {:added v1
   :see '[controller control!]
   :predicate 'controllable?}
  (-controller :- IAdjustable
    "Returns mutable reference to the controller map."
    [this]))

(defn controller :- IAdjustable
  "Returns a controller for `_x_`, a mutable reference to the map-like
  structure containing resource controls."
  {:added v1
   :see '[control! controllable?]}
  [x :- IControllable]
  (-controller x))

(defn control! :- IAdjustable
  "Adjusts the part of the controller and returns it (the controller
  for `_x_`)."
  {:added v1
   :see '[controller controllable?]}
  ([x :- IControllable, k :- Any, v :- Any]
   (let [c (-controller x)] (adjust! c k v)))
  ([x :- IControllable, k :- Any, v :- Any & keyvals :- Any]
   (let [c (-controller x)] (apply adjust! c k v keyvals))))

(defprotocol IStatusable
  "A protocol for statusable objects."
  {:added v1
   :see '[status status-of]
   :predicate 'statusable?}
  (-status :- IReference
    "Returns an immutable reference to the status map.
    Returned reference usually also behaves as a Mult."
    [this]))

(defn status :- IReference
  "Returns an immutable reference to a the status map.
  Returned reference usually also behaves as a Mult."
  {:added v1
   :see '[statusable? status-of]}
  [x :- IStatusable]
  (-status x))

(defn status-of :- Any
  "Returns current value of a status property under the key `_k_`
  for the object `_x_`."
  {:added v1
   :see '[status statusable?]}
  [x :- IStatusable, k :- Any]
  (get @(-status x) k))

(defprotocol ISeekable
  "A protocol for seekable objects with fixed size."
  {:added v1
   :see '[size position]
   :predicate 'seekable?}
  (-size :- Integer+
    "Returns the current size of `_this_`."
    [this])
  (-position :- IMutable
    "Returns a mutable reference to the current position."
    [this]))

(defn size :- Integer+
  "Returns the current size of a seekable resource `_x_`,
  in resource specific units."
  {:added v1
   :see '[position seekable?]}
  [x :- ISeekable]
  (-size x))

(defn position :- IMutable
  "Returns mutable reference to the current position."
  {:added v1
   :see '[seekable? size]}
  [x :- ISeekable]
  (-position x))

(defprotocol IFlushable
  "A protocol for flushable objects."
  {:added v1
   :see '[flush]
   :predicate 'flushable?}
  (-flush! :- nil
    "Flushes any pending operations, returns `nil`. May block."
    [this]))

(defn flush! :- nil
  "Flushes any pending operations and returns `nil`. May block."
  {:added v1
   :see '[flushable?]}
  [x :- IFlushable]
  (-flush! x))

(defprotocol IRequestable
  "A protocol for synchronized requestable objects.

  WARNING: experimental, subject to change."
  {:added v1
   :see '[request!]}
  (-request! :- Any
    "Returns a response to the `request`. Blocks."
    [this request :- Any]))

(defn request! :- Any
  "Returns a response to the `_request_`. Blocks.

  WARNING: experimental, subject to change."
  {:added v1
   :see '[requestable?]}
  [x :- Any, request :- Any]
  (if (satisfies? IRequestable x)
    (-request! x request)
    (do (write! x request) (read! x))))

;; TODO: efficient data transfer between two java nio channels
;; some protocol that exposes ability of a channel to receive
;; transfer or to send transfer.
;; this protocol is meant for both collection recipe and writable
;; resource. Based on the combination of features the right way
;; should be selected.

(defn slurp :- IRed
  "Returns a utf-8 collection recipe of data read from resource
  factory `_res_`, which was opened for immutable reading. May
  supply custom `_parser_` if encoding other than utf-8 is needed."
  {:added v1
   :category "Primary"
   :see '[read read! spit! exchange format transform]}
  ([res :- IImmutableReadable]
   (parse utf-8 (read res)))
  ([res :- IImmutableReadable, parser :- IParserFactory]
   (parse parser (read res))))

(defn spit-with! :- Integer+
  "Writes `_coll_` to the `_res_` using `_printer_` for printing,
  returning number of bytes written. Default mode is `:append`."
  {:added v1
   :category "Primary"
   :see '[spit! write! exchange format transform]}
  ([res :- Any, printer :- IPrinterFactory, coll :- IRed]
   (spit-with! res utf-8 coll [:append]))
  ([res :- Any, printer :- IPrinterFactory, coll :- IRed, mode :- Any]
   (let [mode (conj (provide-collection mode) :write :create)
         res (if (writable? res)
               res
               (assoc (resource res) :mode mode))]
     (write! res (print printer coll)))))

(defn spit! :- Integer+
  "Writes `_coll_` to the `_res_`, returning number of bytes written.
  Uses utf-8 printer. Default `_mode_` is `:append`."
  {:added v1
   :category "Primary"
   :see '[spit-with! write! exchange format transform]}
  ([res :- Any, coll :- IRed]
   (spit-with! res utf-8 coll))
  ([res :- Any, coll :- IRed, mode :- Any]
   (spit-with! res utf-8 coll mode)))

(defn exchange! :- IRed
  "Writes `_coll_` into `_resource_`. Without waiting for `write!` to
  finish, returns result of `read!`-ing the `_resource_`.

  NOTE: While the `write!` exception is eaten, the write error
  results in the resource going into failed state with exception
  retrievable with `error` fn and returned reducible throwing when
  reduced.

  Should not be used for non-blocking resource, as the write could
  postpone without a possibility to advance."
  {:added v1
   :category "Primary"
   :see '[spit! slurp write! read! format transform]}
  [resource :- IWritable, coll :- IRed]
  (future (write! resource coll))
  (read! resource))

(defn format :- (I IReadable IWritable)
  "Returns a readable and writable `_resource_` formatted with a given
  `_formatter_` factory for both reading and writing. May also supply
  parser and printer factories separately."
  {:added v1
   :category "Primary"
   :see '[exchange transform read! write! slurp spit!]}
  ([resource :- (I IReadable IWritable),
    formatter-factory :- (I IParserFactory IPrinterFactory)]
   (format resource formatter-factory formatter-factory))
  ([resource :- (I IReadable IWritable),
    parser-factory :- IParserFactory,
    printer-factory :- IPrinterFactory]
   (reify
     IReadable
     (-read! [this] (parse parser-factory (-read! resource)))
     IWritable
     (-write! [this coll]
       (-write! resource (print printer-factory coll))))))

(defn transform :- (I IReadable IWritable)
  "Returns a readable and writable `_resource_` transformed with given
  transducer `_xform_` for both reading and writing. May also supply
  reading and writing transducer separately."
  {:added v1
   :category "Primary"
   :see '[format exchange read! write!]}
  ([resource :- (I IReadable IWritable), xform :- Any]
   (transform resource xform xform))
  ([resource :- (I IReadable IWritable),
    read-xform :- Any, write-xform :- Any]
   (reify
     IReadable
     (-read! [this] (recipe read-xform (-read! resource)))
     IWritable
     (-write! [this coll]
       (-write! resource (recipe write-xform coll))))))

(defn deps* :- {}
  "Returns generated dependency map for a given factory."
  [factory :- {}]
  (reduce (fn [m kv] (if (nil? (val kv)) (conj m kv) m))
          {} (seq factory)))

(defn deps :- {}
  "Returns a dependency map for a given object `_x_`."
  {:added v1
   :category "System"
   :see '[assoc-deps system start!]}
  [x :- Record]
  (cond (not (lookup? x)) nil
        (contains? x ::deps) (::deps x)
        :else (deps* x)))

(defn assoc-deps :- Record
  "Returns `_factory_` with `_deps_` associated as its
  dependency map."
  {:added v1
   :category "System"
   :see '[deps system start!]}
  [factory :- Record, deps :- {}]
  (assoc factory ::deps deps))

(defprotocol ISystem
  "A marker protocol for system objects which should be started
  with `start!` function."
  {:added v1
   :category "System"
   :see '[start! acquire! system]
   :predicate 'system?})

(defrecord System+
  []
  ISystem)

(defn system :- ISystem
  "Returns a system map."
  {:added v1
   :category "System"
   :see '[start! deps assoc-deps]}
  [& {:as keyvals}]
  (map->System+ keyvals))

(declare start!*)

(defn autoconf*
  ([cfg autoconf]
   (autoconf* cfg autoconf nil))
  ([cfg autoconf cast-fn]
   (let [bf (fn [m {:keys [:key :path :type] :as spec}]
              (let [sen (sentinel)
                    val (get-in cfg path sen)]
                (cond
                  (identical? sen val) m
                  (or (not (contains? spec :type))
                      (validate-value type val))
                  (assoc m key val)
                  cast-fn (assoc m key (cast-fn type val))
                  (throw (illegal-argument "cannot cast value")))))]
     (reduce bf {} autoconf))))

(defn normalized-autoconf
  ([x]
   (normalized-autoconf x (-autoconf x)))
  ([x autoconf]
   (let [dpath (when-let [sym (:on (type x))]
                 (->> (name sym)
                      (ds/split #(= % \.))
                      (map #(ds/replace % \_ \-))
                      (map keyword)
                      butlast
                      vec))
         autoconf (if (identical? :default autoconf) dpath autoconf)
         fields (map keyword (:fields (type x)))
         tsigs (:tsig (type x))
         type-map (zipmap fields tsigs)
         mf #(if (nil? type-map)
               (->map :key % :path (conj autoconf %))
               (->map :key % :path (conj autoconf %)
                      :type (get type-map %)))
         amf #(if (or (contains? % :type) (nil? type-map))
                %
                (assoc % :type (type-map (:key %))))
         apf #(if (or (contains? % :path) (nil? dpath))
                %
                (assoc % :path (conj dpath (:key %))))]
     (if (map? (first autoconf))
       (map apf map amf)
       (map mf fields)))))

(defn autoconf
  [x cfg cast-fn]
  (if (satisfies? IConfigurable x)
    (merge x (autoconf* cfg (normalized-autoconf x) cast-fn))
    x))

(defn resolve-run
  "Perform one run of dependency resolution.
  Returns pair [done outstanding] with some system components
  resolved. Dependency resolution is finished if returned set of
  outstanding component keys empty,
  otherwise this function should be called again.
  Caller should assume circular dependency when returned done has not
  changed and `outstanding` is not empty."
  [done :- {}, system :- IRed, cfg :- {}, cast-fn :- AnyFn]
  (loop [done done, pending (seq system), k nil, v nil, d nil, o nil]
    (cond
      ;; no current component, no pending components
      (and (nil? k) (nil? pending)) (pair done o)
      ;; no current components, but some pending components
      (nil? k)
      (let [[nk nv] (first pending), np (next pending)
            cnv (autoconf nv cfg cast-fn)]
        ;; was first pending component present in the original done?
        (if (contains? done nk)
          (recur done np nil nil nil o)
          (recur done np nk cnv (seq (deps cnv)) o)))
      ;; current component has no unresolved dependencies
      (nil? d)
      (recur (assoc done k (start!* v cfg cast-fn))
             pending nil nil nil o)
      ;; pick current component's first dependency
      :let [dep (first d), dep-key (key dep), dep-val (val dep)
            nd (next d), x (or dep-val dep-key)]
      ;; current dependency is already available for use
      (contains? done x)
      (recur done pending k (assoc v dep-key (get done x)) nd o)
      ;; current dependency is not present in the system at all
      (not (contains? system x))
      (if (nil? dep-val) ;; optional dependency?
        (recur done pending k v nd o)
        (throw (illegal-argument
                "no component found for a required dependency")))
      ;; current component cannot be resolved in the current run
      :else (recur done pending nil nil nil (conj o k)))))

(defn resolve-system
  [system cfg cast-fn]
  (loop [done {}]
    (let [[new-done outstanding]
          (resolve-run done system cfg cast-fn)]
      (cond
        (empty? outstanding) new-done
        (identical? done new-done)
        (throw (illegal-argument
                (->str "circular dependency detected" outstanding)))
        :else (recur new-done)))))

(defn start!* :- {}
  [system :- ISystem, cfg :- {}, cast-fn :- AnyFn]
  (let [resolved (if (system? system)
                   (resolve-system system cfg cast-fn)
                   system)]
    (if (satisfies? IAcquirableFactory system)
      (acquire! (merge system resolved)) ;; retain factory type
      resolved)))

(defn start! :- {}
  "Starts the `_system_` and returns map of started components.
  If the system object is also an acquirable factory, performs
  `acquire!` and returns a resource object.
  Configures `_system_` and all its dependencies, if configurable,
  with `_cfg_` and `_cast-fn_`."
  {:added v1
   :category "System"
   :see '[acquire! deps assoc-deps system]}
  ([system :- ISystem]
   (start! system nil))
  ([system :- ISystem, cfg :- {}]
   (start! system cfg (:cast-fn (meta cfg))))
  ([system :- ISystem, cfg :- {}, cast-fn :- AnyFn]
   (start!* (autoconf system cfg cast-fn) cfg cast-fn)))
