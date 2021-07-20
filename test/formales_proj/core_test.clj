(ns formales-proj.core-test
  (:require [clojure.test :refer :all]
            [formales-proj.core :refer :all]))


(deftest a-mayusculas-salvo-strings-test
  (testing "Prueba de la funcion: a-mayusculas-salvo-strings"
    (is (= '" " (a-mayusculas-salvo-strings " ")))
    (is (= '"  CONST Y = 2;" (a-mayusculas-salvo-strings "  const Y = 2;")))
    (is (= '"  WRITELN ('Se ingresa un valor, se muestra su doble.');" (a-mayusculas-salvo-strings "  writeln ('Se ingresa un valor, se muestra su doble.');")))
    )
  )

(deftest palabra-reservada-test
  (testing "Prueba de la funcion: palabra-reservada-test"
    (is (= true (palabra-reservada? 'CALL)))
    (is (= true (palabra-reservada? '"CALL")))
    (is (= false (palabra-reservada? '"ASIGNAR")))
    (is (= false (palabra-reservada? 'ASIGNAR )))
    ))

(deftest identificador-test
  (testing "Prueba de la funcion: identificador-test"
    (is (= false (identificador? 2)))
    (is (= true (identificador? 'V2)))
    (is (= true (identificador? "V2")))
    (is (= false (identificador? 'CALL)))
  ))

(deftest cadena?-test
  (testing "Prueba de la funcion: cadena?"
    (is (= true (cadena? "'Hola'")))
    (is (= false (cadena? "Hola")))
    (is (= false (cadena? "'Hola")))
    (is (= false (cadena? 'Hola)))))
