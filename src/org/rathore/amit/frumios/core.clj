(ns org.rathore.amit.frumios.core)

(def k-resolve (comp resolve symbol name))

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
          ((klass :method command) args)))))

(defn new-class [class-name parent methods]
  (let [klass (k-resolve class-name)]
    (fn [command]
      (cond
	(= :parent command) parent
	(= :name command) klass
	(= :new command) (new-object klass)
	:else (throw (RuntimeException. (str "Unknown message: " command)))))))

(def OBJECT (new-class :org.rathore.amit.frumios.core/OBJECT nil []))

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

(defmacro defclass [class-name & specs]
  (let [parent-class-symbol (parent-class-spec specs)
        this-class-name (keyword class-name)]
    `(def ~class-name 
	  (new-class ~this-class-name (var ~parent-class-symbol) []))))