#lang racket
(provide (all-defined-out))
(require "TDA_Documento.rkt")
(require "TDA_ParadigmaDocs.rkt")
;Representación '((idDoc idVer (doc) (...))
;Ejemplo '((list idDoc (selectIdVer idDoc historialList) documento) (2 1 '(2 "driques" (30 10 2020) "otroTest" "doc2")) (1 1 '(1 "driques" (30 10 2020) "test 1" "doc1")))
;Dominio: int X documento X list
;Recorrido : historial

(define (historial id documento historialList)
      (if (null? historialList)
      (list (list (docs->idDoc (docs->selectDoc id documento)) 1 (docs->selectDoc id documento)))
      (if (isInHist? (docs->idDoc (docs->selectDoc id documento)) historialList) ;Se pregunta si el documento estaba anteriormente editado.
          (append (list (list (docs->idDoc (docs->selectDoc id documento)) (selectIdVer (docs->idDoc (docs->selectDoc id documento)) historialList) (docs->selectDoc id documento))) historialList); Si el doc fue editado, se crea un append con lo anterior
          (append (list (list (docs->idDoc (docs->selectDoc id documento)) 1 (docs->selectDoc id documento))) historialList) ;Sino, se crea uno nuevo con idVer 1.

          )
      )

  )





;Pertenencia
;Pregunta si el id del doc está ya anteriormente en el historial.
;dom: int X list
;rec: boolean
(define (isInHist? idDoc historialList)
  (if (null? historialList)
      #f
      (if (eq? idDoc (historial->idDoc historialList))
          #t
          (isInHist? idDoc (cdr historialList))
          )
      )
  )

;Funcion que pregunta si es de autoria el doc.
;dom: int X str X list
;rec: boolean

(define (autoriaHistorialDoc? idDoc activeUser historial)
          (if (null? historial)
              #f
              (if (and (eq? idDoc (historial->idDoc historial)) (eq? activeUser (historial->owner historial)))
                  #t
                  (autoriaHistorialDoc? idDoc activeUser (cdr historial))
               )
      )
  )

;Funcion que pregunta si el id ingresado existe.
;;dom: int X int X list
;rec: boolean
(define (existenId? idDoc idVer historial)
    (if (null? historial)
        #f
        (if (and (eq? (historial->idDoc historial) idDoc) (eq? (historial->idVer historial) idVer))
            #t
            (existenId? idVer idDoc (cdr historial))
            )
        )
     )



;Selector
;descripción: Función que retorna el id del doc.
;dom: historial
;rec: entero
(define (historial->idDoc historial)
  (car(car historial))
  )
;Selector
;descripción: Función que retorna el id de la version del doc.
;dom: historial
;rec: entero
(define (historial->idVer historial)
  (car(cdr(car historial)))
  )
;Selector
;descripción: Función que retorna el doc seleccionado.
;dom: historial
;rec: documento
(define (historial->docSelect historial)
  (cddr(car historial))
  )
;Selector
;descripción: Función que retorna el propietario.
;dom: historial
;rec: str
(define (historial->owner historial)
      (cadr(car (historial->docSelect historial)))
  )
;Modificador

;descripción: Función que restablece a una version especifica
;dom: int X int X historial
;rec: list
(define (restore idDoc idVer historial)
         (if (null? historial)
             "error"
             (if (and (eq? (historial->idDoc historial) idDoc) (eq? (historial->idVer historial) idVer))
                 (car (historial->docSelect historial))
                 (restore idDoc idVer (cdr historial))
              )
          )   
     )



;Funciones adicionales
;Función que toma el id actual y suma 1.

;dom: int X list
;rec: entero
(define (selectIdVer idDoc historialList)
  (if (eq? idDoc (historial->idDoc historialList))
      (+ (historial->idVer historialList) 1)
      (selectIdVer idDoc (cdr historialList))
      )
 )



;Tests
;DEL MOMENTO, FUNCIONALES, FALTA IMPLEMENTARLOS DENTRO DE MAIN.
;(define testDoc1 '(1 "driques" (30 10 2020) "test 2 del doc1" "doc1"))
;(define testDoc2 '(1 "driques" (30 10 2020) "test 3 del doc1" "doc1"))
;(define testHist1 '((1 2 (3 "driques" (30 10 2020) "otnemucod omitlu" "docFinal")) (2 1 (2 "pepe" (30 10 2020) "otnemucod ooodnuges" "doc2")) (1 1 (1 "driques" (30 10 2020) "otnemucod remirp" "doc1"))) )
;(define testHist2 '())

;(define test1 (historial testDoc1 testHist2))
;(define test2 (historial testDoc2 test1))


