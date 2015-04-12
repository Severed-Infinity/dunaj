(ns lonocloud.synthread-last)

(defn copy-ns-from [from-ns]
  (doseq [[sym target] (ns-map from-ns)]
    (if (var? target)
      (when-not (::isolated (meta target))
        (.refer *ns* sym target))
      (.importClass *ns* sym target)))
  (doseq [[sym ns] (ns-aliases from-ns)]
    (alias sym (symbol (str ns)))))

(defn split-def [orig-ns def-macro [def-name & def-tail]]
  (let [tmp (with-meta (symbol (str "__" def-name))
              (meta def-name))]
    `(do
       (~def-macro ~tmp ~@def-tail)
       (ns-unmap '~orig-ns '~def-name)
       (intern '~orig-ns (with-meta '~def-name
                           (assoc (meta (var ~tmp))
                             ::isolated true))
               (deref (var ~tmp))))))

(defmacro isolate-ns
  "Isolate the body of each definition (defaulting to defn's and
  defmacro's) in the rest of this namespace from the vars being
  defined. Access to these vars is available through the alias given
  to the optional :as parameter. Useful only for namespaces that
  provide many vars whose names conflict with clojure.core."
  [& {:keys [as macros]}]
  (let [orig-ns (symbol (str *ns*))
        macros (or macros `[defn defmacro])]
    `(do
       (in-ns '~(symbol (str *ns* ".isolated")))
       (copy-ns-from '~orig-ns)
       ~(when as
          `(alias '~as '~(symbol (str *ns*))))
       ~@(for [macro macros
               :let [unqualified-macro (symbol (name macro))]]
           `(do
              (ns-unmap *ns* '~unqualified-macro)
              (clojure.core/defmacro ~unqualified-macro [& everything#]
                (split-def '~orig-ns '~macro everything#)))))))

(isolate-ns :as ->>)

(defn replace-content
     [o n]
     (condp instance? o
       (type n)                           (if (instance? clojure.lang.IObj o)
                                            (with-meta n (meta o))
                                            n)
       clojure.lang.IMapEntry             (vec n)
       clojure.lang.IRecord               (with-meta
                                            (merge o (if (map? n) n
                                                         (into {} (map vec n))))
                                            (meta o))
       clojure.lang.IPersistentList       (with-meta (apply list n) (meta o))
       clojure.lang.IPersistentMap        (into (empty o) (map vec n))

       clojure.lang.ISeq                  (with-meta (doall n) (meta o))
       clojure.lang.IPersistentCollection (into (empty o) n)

       clojure.lang.IObj                  (with-meta n (meta o))
       n))

;; Section 0: special syntax support for updating and getting from a
;; sub-path.
(defn- expand-by-form
  [[label expr :as binding]]
  (if (and (list? expr)
           (= 'by (first expr)))
    (let [[_ path updater & [getter]] expr
          getter (if (nil? getter) `identity getter)]
      `[path# [~path]
        ~'<> (update-in ~'<> path# #(->> % ~updater))
        ~label (->> (get-in ~'<> path#) ~getter)])
    binding))

(defn- expand-by-forms
  "Look for special 'by' forms in binding pairs to expand them into multiple binding pairs"
  [bindings]
  (->> bindings
       (partition 2)
       (mapcat expand-by-form)))

;; Section 1: macros that do not update the topic.
;;            Generally control flow macros.

(defmacro do
  "Thread x through body. Semantically identical to ->> with the
  extra feature that the symbol <> is bound to the new value of x
  between each form.

  Note that the old marking constraint has been removed as it was
  unduly restrictive."
  [x & body]
  (if (empty? body)
    x
    `(let [~'<> ~x]
       (->>/do (->> ~'<> ~(first body))
               ~@(rest body)))))

;; (oh yeah. we're messing with if :-)
(defmacro if
  "If pred is true, thread x through the then form, otherwise through
  the else form.
  (->> 5 (->>/if should-inc? inc dec))"
  [pred then else x]
  `(let [~'<> ~x]
     (if ~pred
       (->>/do ~'<> ~then)
       (->>/do ~'<> ~else))))

(defmacro when
  "If pred is true, thread x through body, otherwise return x unchanged.
  (->> 5 (->>/when should-inc? inc))"
  [pred & body]
  (let [x (last body)
        body (butlast body)]
    `(let [~'<> ~x]
       (if ~pred
         (->>/do ~'<> ~@body)
         ~'<>))))

(defmacro when-not
  "If pred is false, thread x through body, otherwise return x unchanged.
  (->> 5 (->>/when should-inc? inc))"
  [pred & body]
  (let [x (last body)
        body (butlast body)]
   `(let [~'<> ~x]
      (if ~pred
        ~'<>
        (->>/do ~'<> ~@body)))))

(defmacro cond
  "EXPERIMENTAL Thread x through forms in each clause. Return x if no test matches.
  (->>/cond [1 2] true (conj 3) false pop)"
  [& body]
  (let [x (last body)
        body (butlast body)]
    `(let [~'<> ~x]
       (cond ~@(mapcat (fn [[test form]] `[~test (->>/do ~'<> ~form)])
                       (partition 2 body))
             :else ~'<>))))

(defmacro for
  "Thread x through each iteration of body. Uses standard looping
  binding syntax for iterating.
  (->>/for 4 [x [1 2 3]] (+ x)) ;; returns 10"
  [seq-exprs & body]
  (let [x (last body)
        body (butlast body)]
    `(let [box# (clojure.lang.Box. ~x)
           ~'<> (.val box#)]
       (doseq ~seq-exprs
         (set! (.val box#) (->>/do (.val box#) ~@body)))
       (.val box#))))

(defmacro let
  "Thread x through body (with bindings available as usual).
  (->>/let 4 [x 3] (+ x) (- x)) ;; returns 4"
  [bindings & body]
  (let [x (last body)
        body (butlast body)]
    `(let [~'<> ~x
           ~@(expand-by-forms bindings)]
       (->>/do ~'<> ~@body))))

(defmacro if-let
  "Thread x through then or else depending on the value of pred. If
  pred is true, bind local to pred.
  (->> {}
    (->>/if-let [x :bar]
      (assoc :foo x)
      (assoc :was-bar false)))
  ;; returns {:foo :bar}"
  [[local pred :as binding] then else x]
  `(let [~'<> ~x]
     (if-let [~@(expand-by-forms binding)]
       (->>/do ~'<> ~then)
       (->>/do ~'<> ~else))))

(defmacro when-let
  "If bound values are true in bindings, thread x through the body,
  otherwise return x unchanged.
  (->> 5 (->>/when-let [amount (:amount foo)] (+ amount)))"
  [[local pred :as binding] & forms]
  (let [x (last forms)
        forms (butlast forms)]
    `(let [~'<> ~x
           ~@(expand-by-forms binding)]
       (if ~local
         (->>/do ~'<> ~@forms)
         ~'<>))))

(defmacro fn
  "Thread x into body of fn. (inspired by Prismatic's fn->).
  (let [add-n (->/fn [n] (+ n))]
    (->> 1 (add-n 2))) ;; returns 3"
  [args & body]
  (let [x (last body)
        body (butlast body)]
    `(fn [~'<> ~@args] (->>/do ~'<> ~@body))))

;; Section 2: Macros that access or update the topic.

;; TODO review performance of nth and last.
;; TODO Preserve the type of topic when digging into it with each,
;; each-as, first, second, etc

;; Labeling forms (as, as-do, as-to)
;; +----- label value x
;; | +--- modify value x
;; | | +- thread value x
;; | | |
;; 0 0 0 (doto x (do ...))     ;; for side effects, no access to the topic: (prn "hello")
;; 0 0 1 (doto x (-> ...))     ;; almost but not quite (doto x ...) threading but with chained side-effects?
;; 0 1 0 (do x ...)
;; 0 1 1 (-> x ...)
;; 1 0 0 (->/aside x ...)      ;; equivilent to (doto (->/as x (do ...)))
;; 1 0 1 (doto x (->/as ...))
;; 1 1 0 (->/as x (do .... ))  ;; mention in style guide
;; 1 1 1 (->/as x ...)

(defmacro as
  "Bind value of x and thread x through body.
   EXPERIMENTALLY supports arbitrary threading form in place of binding form."
  [x binding & body]
  (if (seq? binding)
    `(let [~'<> ~x
           ~(last binding) (-> ~'<> ~(drop-last binding))]
       (->/do ~'<> ~@body))
    `(let [~'<> ~x
           ~binding ~'<>]
       (->/do ~'<> ~@body))))

(defmacro aside
  "Bind value of x, evaluate unthreaded body and return x."
  [x binding & body]
  `(doto ~x (->/as ~binding (do ~@body))))

(defmacro side
  "Evaluate unthreaded body and return unchanged x."
  [& body]
  (let [x (last body)
        body (butlast body)]
    `(let [~'<> ~x]
       ~@body
       ~'<>)))

(defmacro first
  "Thread the first element of x through body.
  (->>/first [1 2 3] inc -) ;; returns [-2 2 3]"
  [& body]
  (let [x (last body)
        body (butlast body)]
    `(let [x# ~x]
       (impl/replace-content x# (cons (->>/do (first x#) ~@body)
                                      (rest x#))))))

(defmacro second
  "Thread the second element of x through body.
  (->>/second [1 2 3] inc -) ;; returns [1 -3 3]"
  [& body]
  (let [x (last body)
        body (butlast body)]
    `(let [x# ~x]
       (impl/replace-content x# (cons (first x#)
                                      (cons (->>/do (second x#) ~@body)
                                            (drop 2 x#)))))))

(defmacro nth
  "EXPERIMENTAL Thread the nth element of x through body.
  (->>/nth [1 2 3] 1 inc -) ;; returns [1 -3 3]"
  [n & body]
  (let [x (last body)
        body (butlast body)]
    `(let [x# ~x
           n# ~n]
       (impl/replace-content x# (concat (take n# x#)
                                        (cons (->>/do (nth x# n#) ~@body)
                                              (drop (inc n#) x#)))))))

(defmacro last
  "EXPERIMENTAL Thread the last element of x through body.
  (->>/last [1 2 3] inc -) ;; returns [1 2 -4]"
  [& body]
  (let [x (last body)
        body (butlast body)]
    `(let [x# ~x]
       (impl/replace-content x# (concat (drop-last 1 x#)
                                        [(->>/do (last x#) ~@body)])))))

(defmacro rest
  "EXPERIMENTAL Thread the rest of items in x through body."
  [& body]
  (let [x (last body)
        body (butlast body)]
    `(let [x# ~x]
       (impl/replace-content x# (cons (first x#)
                                      (->>/do (rest x#) ~@body))))))

(defmacro update
  "Thread the value at each key through the pair form."
  [& body]
  (let [x (last body)
        body (butlast body)]
    (let [xx (gensym)]
      `(let [~xx ~x]
         (assoc ~xx
           ~@(->> body
                  (partition 2)
                  (mapcat (fn [[key form]]
                            [key `(->>/do (get ~xx ~key) ~form)]))))))))

(defmacro in
  "Thread the portion of x specified by path through body.
  (->>/in {:a 1, :b 2} [:a] (+ 2)) ;; = {:a 3, :b 2}"
  [path & body]
  (let [x (last body)
        body (butlast body)]
    `(if (empty? ~path)
       (->>/do ~x ~@body)
       (update-in ~x ~path (fn [x#] (->>/do x# ~@body))))))

(defmacro each
  "EXPERIMENTAL Thread each item in x through body."
  [& body]
  (let [x (last body)
        body (butlast body)]
    `(let [x# ~x]
       (impl/replace-content x# (map #(->>/do % ~@body) x#)))))

(defmacro each-as
  "EXPERIMENTAL Thread each item in x through body and apply binding to each item."
  [binding & body]
  (let [x (last body)
        body (butlast body)]
    `(->>/each ~x (->>/as ~binding ~@body))))

(defmacro key
  "Thread the key in x through body (x must be a MapEntry)."
  [& body]
  (let [x (last body)
        body (butlast body)]
    `(let [x# ~x] (clojure.lang.MapEntry. (->>/do (key x#) ~@body) (val x#)))))

(defmacro val
  "Thread the value in x through body (x must be a MapEntry)."
  [& body]
  (let [x (last body)
        body (butlast body)]
    `(let [x# ~x] (clojure.lang.MapEntry. (key x#) (->>/do (val x#) ~@body)))))

;; Section 3: Additional helper functions.

(defn apply
  "Apply f to x and args."
  [& body]
  (let [x (last body)
        body (butlast body)]
    (let [[f & args] (concat (drop-last body) (last body))]
      (apply f x args))))

(defn reset
  "Replace x with y."
  [y x] y)
