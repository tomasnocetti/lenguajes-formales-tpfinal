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
    (is (= false (identificador? (symbol "("))))
    (is (= false (identificador? (symbol ")"))))
    (is (= false (identificador? (symbol "."))))
    (is (= false (identificador? (symbol ";"))))
    (is (= false (identificador? (symbol "<>"))))
    (is (= false (identificador? (symbol "="))))
    (is (= false (identificador? (symbol ">="))))
    (is (= false (identificador? (symbol "<="))))
    (is (= false (identificador? (symbol "+"))))
    (is (= false (identificador? (symbol "-"))))
    (is (= false (identificador? (symbol "*"))))
    (is (= false (identificador? (symbol "/"))))
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

(deftest aplicar-relacional-test
  (testing "Prueba de funcion: aplicar-relacional"
    (is (= 'nil (aplicar-relacional > nil)))
    (is (= '[] (aplicar-relacional > [])))
    (is (= '[7] (aplicar-relacional > [7])))
    (is (= '[1] (aplicar-relacional > [7 5])))
    (is (= '[4 1] (aplicar-relacional > [4 7 5])))
    (is (= '[4 1] (aplicar-relacional >= [4 7 5])))
    (is (= '[4 1] (aplicar-relacional >= [4 7 7])))
    (is (= '[4 0] (aplicar-relacional >= [4 7 8])))
    (is (= '[4 0] (aplicar-relacional = [4 7 5])))
    (is (= '[4 1] (aplicar-relacional not= [4 7 5])))
    (is (= '[4 0] (aplicar-relacional < [4 7 5])))
    (is (= '[4 1] (aplicar-relacional <= [4 6 6])))
    (is (= '[a b c] (aplicar-relacional <= '[a b c])))
))

(deftest aplicar-aritmetico-test
  (testing "Prueba de funcion: aplicar-aritmetico"
    (is (= '[3] (aplicar-aritmetico + [1 2])))
    (is (= '[1 3] (aplicar-aritmetico - [1 4 1])))
    (is (= '[1 8] (aplicar-aritmetico * [1 2 4])))
    (is (= '[1 0] (aplicar-aritmetico / [1 2 4])))
    (is (= 'nil (aplicar-aritmetico + nil)))
    (is (= '[] (aplicar-aritmetico + [])))
    (is (= '[1] (aplicar-aritmetico + [1])))
    (is (= '[1 2 4] (aplicar-aritmetico 'hola [1 2 4])))
    (is (= '[1 2 4] (aplicar-aritmetico count [1 2 4])))
    (is (= '[a b c] (aplicar-aritmetico + '[a b c])))
  ))

(deftest inicializar-contexto-local-test
  (testing "Prueba de funcion: aplicar-aritmetico"
    (is (= '[nil () [] :error [[0] [[X VAR 0] [Y VAR 1] [INI PROCEDURE 1]]] 2 [[JMP ?]]] (inicializar-contexto-local '[nil () [] :error [[0] [[X VAR 0] [Y VAR 1] [INI PROCEDURE 1]]] 2 [[JMP ?]]])))
    (is (= '[nil () [] :sin-errores [[0 3] [[X VAR 0] [Y VAR 1] [INI PROCEDURE 1]]] 2 [[JMP ?]]] (inicializar-contexto-local '[nil () [] :sin-errores [[0] [[X VAR 0] [Y VAR 1] [INI PROCEDURE 1]]] 2 [[JMP ?]]])))
    ))

(deftest dump-test
  (testing "Prueba de funcion: dump"
    (is (= 'nil (dump '[[PFM 0] [PFI 2] MUL [PFI 1] ADD NEG])))
    (is (= 'nil (dump '[HLT])))
    (is (= 'nil (dump nil)))))

(deftest cargar-var-en-tabla-test
  (testing "Prueba de funcion: cargar-var-en-tabla"
    (is (= '[nil () [VAR X] :error [[0] []] 0 [[JMP ?]]] (cargar-var-en-tabla '[nil () [VAR X] :error [[0] []] 0 [[JMP ?]]])))
    (is (= '[nil () [VAR X] :sin-errores [[0] [[X VAR 0]]] 1 [[JMP ?]]] (cargar-var-en-tabla '[nil () [VAR X] :sin-errores [[0] []] 0 [[JMP ?]]])))
    (is (= '[nil () [VAR X Y] :sin-errores [[0] [[X VAR 0] [Y VAR 1]]] 2 [[JMP ?]]] (cargar-var-en-tabla '[nil () [VAR X , Y] :sin-errores [[0] [[X VAR 0]]] 1 [[JMP ?]]])))))

(deftest declaracion-var-test
  (testing "Prueba de funcion: declaracion-var-tabla"
    (is (= ['VAR (list 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X (symbol ":=") 7 (symbol ";") 'Y (symbol ":=") 12 (symbol ";") 'END (symbol ".")) [] :error [[0] []] 0 '[[JMP ?]]] (declaracion-var ['VAR (list 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X (symbol ":=") 7 (symbol ";") 'Y (symbol ":=") 12 (symbol ";") 'END (symbol ".")) [] :error [[0] []] 0 '[[JMP ?]]])))
    (is (= ['BEGIN (list 'X (symbol ":=") 7 (symbol ";") 'Y (symbol ":=") 12 (symbol ";") 'END (symbol ".")) ['VAR 'X (symbol ",") 'Y (symbol ";")] :sin-errores [[0] ['[X VAR 0] '[Y VAR 1]]] 2 '[[JMP ?]]] (declaracion-var ['VAR (list 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X (symbol ":=") 7 (symbol ";") 'Y (symbol ":=") 12 (symbol ";") 'END (symbol ".")) [] :sin-errores [[0] []] 0 '[[JMP ?]]])))
))

(deftest fixup-test
  (testing "Prueba de funcion: fixup"
    (is (= '[WRITELN (END .) [] :error [[0 3] []] 6 [[JMP ?] [JMP ?] [CAL 1] RET]] (fixup ['WRITELN (list 'END (symbol ".")) [] :error [[0 3] []] 6 '[[JMP ?] [JMP ?] [CAL 1] RET]] 1)))
    (is (= '[WRITELN (END .) [] :sin-errores [[0 3] []] 6 [[JMP ?] [JMP 4] [CAL 1] RET]] (fixup ['WRITELN (list 'END (symbol ".")) [] :sin-errores [[0 3] []] 6 '[[JMP ?] [JMP ?] [CAL 1] RET]] 1)))
    (is (= '[WRITELN (END .) [] :sin-errores [[0 3] []] 6 [[JMP 8] [JMP 4] [CAL 1] RET [PFM 2] OUT NL RET]] (fixup ['WRITELN (list 'END (symbol ".")) [] :sin-errores [[0 3] []] 6 '[[JMP ?] [JMP 4] [CAL 1] RET [PFM 2] OUT NL RET]] 0)))
    ))

(deftest generar-operador-relacional-test
  (testing "Prueba de funcion: generar-operador-relacional"
    (is (= '[WRITELN (END .) [] :error [[0 3] []] 6 [[JMP ?] [JMP ?] [CAL 1] RET]] (generar-operador-relacional ['WRITELN (list 'END (symbol ".")) [] :error [[0 3] []] 6 '[[JMP ?] [JMP ?] [CAL 1] RET]] '=)))
    (is (= '[WRITELN (END .) [] :sin-errores [[0 3] []] 6 [[JMP ?] [JMP ?] [CAL 1] RET]] (generar-operador-relacional ['WRITELN (list 'END (symbol ".")) [] :sin-errores [[0 3] []] 6 '[[JMP ?] [JMP ?] [CAL 1] RET]] '+)))
    (is (= '[WRITELN (END .) [] :sin-errores [[0 3] []] 6 [[JMP ?] [JMP ?] [CAL 1] RET EQ]] (generar-operador-relacional ['WRITELN (list 'END (symbol ".")) [] :sin-errores [[0 3] []] 6 '[[JMP ?] [JMP ?] [CAL 1] RET]] '=)))
    (is (= '[WRITELN (END .) [] :sin-errores [[0 3] []] 6 [[JMP ?] [JMP ?] [CAL 1] RET GTE]] (generar-operador-relacional ['WRITELN (list 'END (symbol ".")) [] :sin-errores [[0 3] []] 6 '[[JMP ?] [JMP ?] [CAL 1] RET]] '>=)))
    (is (= '[WRITELN (END .) [] :sin-errores [[0 3] []] 6 [[JMP ?] [JMP ?] [CAL 1] RET GT]] (generar-operador-relacional ['WRITELN (list 'END (symbol ".")) [] :sin-errores [[0 3] []] 6 '[[JMP ?] [JMP ?] [CAL 1] RET]] '>)))
    (is (= '[WRITELN (END .) [] :sin-errores [[0 3] []] 6 [[JMP ?] [JMP ?] [CAL 1] RET LTE]] (generar-operador-relacional ['WRITELN (list 'END (symbol ".")) [] :sin-errores [[0 3] []] 6 '[[JMP ?] [JMP ?] [CAL 1] RET]] '<=)))
    (is (= '[WRITELN (END .) [] :sin-errores [[0 3] []] 6 [[JMP ?] [JMP ?] [CAL 1] RET LT]] (generar-operador-relacional ['WRITELN (list 'END (symbol ".")) [] :sin-errores [[0 3] []] 6 '[[JMP ?] [JMP ?] [CAL 1] RET]] '<)))
    (is (= '[WRITELN (END .) [] :sin-errores [[0 3] []] 6 [[JMP ?] [JMP ?] [CAL 1] RET NEQ]] (generar-operador-relacional ['WRITELN (list 'END (symbol ".")) [] :sin-errores [[0 3] []] 6 '[[JMP ?] [JMP ?] [CAL 1] RET]] '<>)))
  ))

(deftest procesar-unario-test
  (testing "Prueba de funcion: generar-operador-relacional"
    (is (= ['+ (list 7 (symbol ";") 'Y ':= '- 12 (symbol ";") 'END (symbol ".")) ['VAR 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X (symbol ":=")] :error '[[0] [[X VAR 0] [Y VAR 1]]] 2 []] (procesar-signo-unario ['+ (list 7 (symbol ";") 'Y ':= '- 12 (symbol ";") 'END (symbol ".")) ['VAR 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X (symbol ":=")] :error '[[0] [[X VAR 0] [Y VAR 1]]] 2 []])))
    (is (= [7 (list (symbol ";") 'Y ':= '- 12 (symbol ";") 'END (symbol ".")) ['VAR 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X (symbol ":=")] :sin-errores '[[0] [[X VAR 0] [Y VAR 1]]] 2 []] (procesar-signo-unario [7 (list (symbol ";") 'Y ':= '- 12 (symbol ";") 'END (symbol ".")) ['VAR 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X (symbol ":=")] :sin-errores '[[0] [[X VAR 0] [Y VAR 1]]] 2 []])))
    (is (= [7 (list (symbol ";") 'Y ':= '- 12 (symbol ";") 'END (symbol ".")) ['VAR 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X (symbol ":=") '+] :sin-errores '[[0] [[X VAR 0] [Y VAR 1]]] 2 []] (procesar-signo-unario ['+ (list 7 (symbol ";") 'Y ':= '- 12 (symbol ";") 'END (symbol ".")) ['VAR 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X (symbol ":=")] :sin-errores '[[0] [[X VAR 0] [Y VAR 1]]] 2 []])))
    (is (= [7 (list (symbol ";") 'Y ':= '- 12 (symbol ";") 'END (symbol ".")) ['VAR 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X (symbol ":=") '-] :sin-errores '[[0] [[X VAR 0] [Y VAR 1]]] 2 []] (procesar-signo-unario ['- (list 7 (symbol ";") 'Y ':= '- 12 (symbol ";") 'END (symbol ".")) ['VAR 'X (symbol ",") 'Y (symbol ";") 'BEGIN 'X (symbol ":=")] :sin-errores '[[0] [[X VAR 0] [Y VAR 1]]] 2 []])))
    ))

(deftest generar-signo-test
  (testing "Prueba de funcion: generar-signo"
    (is (= '[nil () [] :error [[0] [[X VAR 0]]] 1 [MUL ADD]] (generar-signo [nil () [] :error '[[0] [[X VAR 0]]] 1 '[MUL ADD]] '-)))
    (is (= '[nil () [] :error [[0] [[X VAR 0]]] 1 [MUL ADD]] (generar-signo [nil () [] :error '[[0] [[X VAR 0]]] 1 '[MUL ADD]] '+)))
    (is (= '[nil () [] :sin-errores [[0] [[X VAR 0]]] 1 [MUL ADD]] (generar-signo [nil () [] :sin-errores '[[0] [[X VAR 0]]] 1 '[MUL ADD]] '+)))
    (is (= '[nil () [] :sin-errores [[0] [[X VAR 0]]] 1 [MUL ADD]] (generar-signo [nil () [] :sin-errores '[[0] [[X VAR 0]]] 1 '[MUL ADD]] '*)))
    (is (= '[nil () [] :sin-errores [[0] [[X VAR 0]]] 1 [MUL ADD NEG]] (generar-signo [nil () [] :sin-errores '[[0] [[X VAR 0]]] 1 '[MUL ADD]] '-)))
    ))

(deftest termino-test
  (testing "Prueba de funcion: termino"
    (is (= ['X (list '* 2 'END (symbol ".")) ['VAR 'X (symbol ";") 'BEGIN 'X (symbol ":=")] :error '[[0] [[X VAR 0]]] 1 []] (termino ['X (list '* 2 'END (symbol ".")) ['VAR 'X (symbol ";") 'BEGIN 'X (symbol ":=")] :error '[[0] [[X VAR 0]]] 1 []])))
    (is (= ['END (list (symbol ".")) ['VAR 'X (symbol ";") 'BEGIN 'X (symbol ":=") 'X '* 2] :sin-errores '[[0] [[X VAR 0]]] 1 '[[PFM 0] [PFI 2] MUL]] (termino ['X (list '* 2 'END (symbol ".")) ['VAR 'X (symbol ";") 'BEGIN 'X (symbol ":=")] :sin-errores '[[0] [[X VAR 0]]] 1 []])))
    ))

(deftest expresion-test
  (testing "Prueba de funcion: expresion"
    (is (= ['- (list (symbol "(") 'X '* 2 '+ 1 (symbol ")") 'END (symbol ".")) ['VAR 'X (symbol ";") 'BEGIN 'X (symbol ":=")] :error '[[0] [[X VAR 0]]] 1 []] (expresion ['- (list (symbol "(") 'X '* 2 '+ 1 (symbol ")") 'END (symbol ".")) ['VAR 'X (symbol ";") 'BEGIN 'X (symbol ":=")] :error '[[0] [[X VAR 0]]] 1 []])))
    (is (= ['END (list (symbol ".")) ['VAR 'X (symbol ";") 'BEGIN 'X (symbol ":=") '+ (symbol "(") 'X '* 2 '+ 1  (symbol ")")] :sin-errores '[[0] [[X VAR 0]]] 1 ['[PFM 0] '[PFI 2] 'MUL '[PFI 1] 'ADD]] (expresion ['+ (list (symbol "(") 'X '* 2 '+ 1 (symbol ")") 'END (symbol ".")) ['VAR 'X (symbol ";") 'BEGIN 'X (symbol ":=")] :sin-errores '[[0] [[X VAR 0]]] 1 []])))
    (is (= ['END (list (symbol ".")) ['VAR 'X (symbol ";") 'BEGIN 'X (symbol ":=") '- (symbol "(") 'X '* 2 '- 1  (symbol ")")] :sin-errores '[[0] [[X VAR 0]]] 1 ['[PFM 0] '[PFI 2] 'MUL '[PFI 1] 'SUB 'NEG]] (expresion ['- (list (symbol "(") 'X '* 2 '- 1 (symbol ")") 'END (symbol ".")) ['VAR 'X (symbol ";") 'BEGIN 'X (symbol ":=")] :sin-errores '[[0] [[X VAR 0]]] 1 []])))))


(deftest interpretar-test
  (testing "Prueba de funcion: interpretar"
    (is (= ['[1] 2 [] []] (interpretar '[[PFI 1] [POP 0] RHLT] '[0] 0 [] [])))
    ))