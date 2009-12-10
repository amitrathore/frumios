(ns frumios-spec)

(use 'org.rathore.amit.frumios.core)

(defclass animal 
  (method sound []
    "grr")
  (method say-something []
    (str (this :sound) ", I say!"))
  (method move []
    "going!"))

(defclass cat
  (:extends animal)
  (method sound []
    "meow"))

(def a (animal :new))
(def c (cat :new))


(deftest this-is-dynamic
  (is (= "meow, I say!") (c :say)))

;frumios-spec> (a :sound)
;"grr"
;frumios-spec> (c :sound)
;"meow!"
;frumios-spec> (c :move)
;"going!"
;frumios-spec> (c :say-something)
;";meow!, I say!"
;frumios-spec> (a :say-something)
;"grr, I say!"frumios-spec> (a :sound)
;"grr"
;frumios-spec> (c :sound)
;"meow!"
;frumios-spec> (c :move)
;"going!"
;frumios-spec> (a :say-something)
;"grr, I say!"
;frumios-spec> (c :say-something)
;"meow!, I say!"