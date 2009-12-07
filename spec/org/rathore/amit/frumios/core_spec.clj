(ns frumios-spec
  (:use [clojure.test :only [run-tests deftest is]])
  (:use org.rathore.amit.frumios.core))

(defclass person
  (defmethod greet [visitor]
    (println "Hi" visitor ", I'm" first-name last-name))

  (defmethod dob []
    (println "I was born on " birth-date)))

(defclass employee
  (:extends person))

(def adi (employee :new))

(deftest can-define-classes
  (is (fn? person)))

(deftest can-create-objects
  (is (fn? adi)))

(deftest can-create-properties
  (adi :set! :age 2)
  (adi :set! :iq 210)
  (is (= 210 (adi :get :iq)))
  (is (= 2 (adi :get :age))))