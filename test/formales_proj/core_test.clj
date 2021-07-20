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

(deftest ya-declarado-localmente?-test
  (testing "Prueba de la funcion: ya-declarado-localmente?"
    (is (= false (ya-declarado-localmente? 'Y '[[0] []])))
    (is (= true (ya-declarado-localmente? 'Y '[[0] [[X VAR 0] [Y VAR 1]]])))
    (is (= false (ya-declarado-localmente? 'Z '[[0] [[X VAR 0] [Y VAR 1]]])))
    (is (= false (ya-declarado-localmente? 'Y '[[0 3 5] [[X VAR 0] [Y VAR 1] [INICIAR PROCEDURE 1] [Y CONST 2] [ASIGNAR PROCEDURE 2]]])))
    (is (= true (ya-declarado-localmente? 'Y '[[0 3 5] [[X VAR 0] [Y VAR 1] [INICIAR PROCEDURE 1] [Y CONST 2] [ASIGNAR PROCEDURE 2] [Y CONST 6]]])))))

(deftest generar-test
  (testing "Prueba de la funcion: generar"
    (is (= '[nil () [VAR X] :sin-errores [[0] []] 0 [[JMP ?] HLT]] (generar '[nil () [VAR X] :sin-errores [[0] []] 0 [[JMP ?]]] 'HLT)))
    (is (= '[nil () [VAR X] :sin-errores [[0] []] 0 [[JMP ?] [PFM 0]]] (generar '[nil () [VAR X] :sin-errores [[0] []] 0 [[JMP ?]]] 'PFM 0)))
    (is (= '[nil () [VAR X] :error [[0] []] 0 [[JMP ?]]] (generar '[nil () [VAR X] :error [[0] []] 0 [[JMP ?]]] 'HLT)))
    (is (= '[nil () [VAR X] :error [[0] []] 0 [[JMP ?]]] (generar '[nil () [VAR X] :error [[0] []] 0 [[JMP ?]]] 'PFM 0)))
    ))

(deftest buscar-coincidencias-test
  (testing "Prueba de funcion: buscar-coincidencias"
    (is (= '([X VAR 0] [X VAR 2]) (buscar-coincidencias '[nil () [CALL X] :sin-errores [[0 3] [[X VAR 0] [Y VAR 1] [A PROCEDURE 1] [X VAR 2] [Y VAR 3] [B PROCEDURE 2]]] 6 [[JMP ?] [JMP 4] [CAL 1] RET]])))
    ))
