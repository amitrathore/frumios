(ns frumios-spec
  (:use [clojure.test :only [run-tests deftest is]])
  (:use org.rathore.amit.frumios.core))

(defclass person
  (method greet [visitor]
    (println "Hi" visitor ", I'm here!"))

  (method dob []
    (str "I was born on " (this :get :birth-date)))

  (method age []
    2)

  (method experience [years]
    (str years " years"))

  (method bio []
    (let [msg (str (this :dob) ", and have " (this :experience (this :age)) " of experience.")]
      (println msg))))

(defclass employee
  (:extends person)
  (method experience [years]
    (str "Grade " years)))

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
  (is (= '(:bio :experience :age :dob :greet) (person :method-names))))

(deftest can-call-methods
  (is (= 2 (kyle :age)))
  (is (= "12 years" (kyle :experience 12))))

(deftest can-call-parent-methods
  (is (= 2 (adi :age))))

(deftest can-override-parent-methods
  (is (= "Grade 12" (adi :experience 12))))