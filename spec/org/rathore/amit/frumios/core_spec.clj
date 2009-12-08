(ns frumios-spec
  (:use [clojure.test :only [run-tests deftest is]])
  (:use org.rathore.amit.frumios.core))

(defclass person
  (method greet [visitor]
    (println "Hi" visitor ", I'm here!"))

  (method dob []
    (println "I was born!"))

  (method age []
    2)

  (method experience [years]
    (str years " years")))

(defclass employee
  (:extends person))

(def kyle (person :new))
(def adi (employee :new))

(deftest can-define-classes
  (is (fn? person)))

(deftest can-define-parent-class
  (is (= (var-get (employee :parent)) person)))

(deftest can-create-objects
  (is (fn? adi)))

(deftest can-create-properties
  (adi :set! :age 2)
  (adi :set! :iq 210)
  (is (= 210 (adi :get :iq)))
  (is (= 2 (adi :get :age))))

(deftest can-specify-methods
  (= '(:experience :age :dob :greet) (person :methods)))

(deftest can-call-methods
  (is (= 2 (kyle :age)))
  (is (= "12 years" (kyle :experience 12))))