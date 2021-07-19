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