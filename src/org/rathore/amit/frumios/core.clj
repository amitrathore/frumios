(ns org.rathore.amit.frumios.core)

(def resolve-from-keyword (comp resolve symbol name))

(defn new-object [klass]
  (let [state (ref {})]
    (fn [command & args]
      (cond
        (= :set! command)
          (let [[k v] args]
            (dosync (alter state assoc k v))
            nil)
        (= :get command)
          (let [[key] args]
            (state key))
        (= :class command)
          klass
        :else
          (let [method (klass :method command)]
	    (if method 
	      (apply method args)))))))

(defn find-method [method-name instance-methods parent-class]
  (let [method (instance-methods method-name)]
    (if method
      method
      (if-not (= #'org.rathore.amit.frumios.core/OBJECT parent-class)
	(find-method method-name (parent-class :methods) (parent-class :parent))))))

(defn new-class [class-name parent methods]
  (let [klass (resolve-from-keyword class-name)]
    (fn [command & args]
      (cond
	(= :parent command) parent
	(= :name command) klass
	(= :method-names command) (keys methods)
	(= :methods command) methods
	(= :new command) (new-object klass)
	(= :method command) 
          (let [[method-name] args]
	    (find-method method-name methods parent))
	:else (throw (RuntimeException. (str "Unknown message: " command)))))))

(def OBJECT (new-class :org.rathore.amit.frumios.core/OBJECT nil {}))
(def this)

(defn parent-class-spec [sexprs]
  (let [extends-spec (filter #(= :extends (first %)) sexprs)
        extends (first extends-spec)]
    (if (empty? extends)
      'org.rathore.amit.frumios.core/OBJECT
      (do 
	(if-not (= 1 (count extends-spec))
	  (throw (RuntimeException. "defclass only accepts a single extends clause")))
	(if-not (= 2 (count extends))
	  (throw (RuntimeException. "the extends clause only accepts a single parent class")))
	(last extends)))))

(defn method [sexpr]
  (let [name (keyword (second sexpr))
	remaining (next sexpr)]
    {name (conj remaining 'fn)}))

(defn methods-as-hash [sexprs]
  (apply merge (map method sexprs)))

(defn method-specs [sexprs]
  (let [method? #(= 'method (first %))]
    (filter method? sexprs)))

(defmacro defclass [class-name & specs]
  (let [parent-class-symbol (parent-class-spec specs)
        this-class-name (keyword class-name)
	fns (methods-as-hash (method-specs specs))
	fns (or fns {})]
    `(def ~class-name 
	  (new-class ~this-class-name (var ~parent-class-symbol) ~fns))))