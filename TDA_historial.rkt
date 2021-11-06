#lang racket
(provide (all-defined-out))
(require "TDA_Documento.rkt")
(require "TDA_ParadigmaDocs.rkt")
;Representación


;Me falta reconstruir el constructor anterior, para llevarlo a la siguiente forma,
;asi pregunto por el idVer máx (que se encontrará más a la izquierda), para luego, en caso
;de hacer un cambio, agregar un 1 al idVer del idDoc, y asi ir incrementando el historial para
;un documento en especifico.

;Constructor '( (idDoc idVer (doc)) (idDoc2 idVer2 (doc)) )
;Ejemplo '((list idDoc (selectIdVer idDoc historialList) documento) (2 1 '(2 "driques" (30 10 2020) "otroTest" "doc2")) (1 1 '(1 "driques" (30 10 2020) "test 1" "doc1")))


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
(define (isInHist? idDoc historialList)
  (if (null? historialList)
      #f
      (if (eq? idDoc (historial->idDoc historialList))
          #t
          (isInHist? idDoc (cdr historialList))
          )
      )
  )

;Selector
(define (historial->idDoc historial)
  (car(car historial))
  )
(define (historial->idVer historial)
  (car(cdr(car historial)))
  )


;Funciones adicionales
;Función que toma el id actual y suma 1.
(define (selectIdVer idDoc historialList)
  (if (eq? idDoc (historial->idDoc historialList))
      (+ (historial->idVer historialList) 1)
      (selectIdVer idDoc (cdr historialList))
      )
 )




;Tests
;DEL MOMENTO, FUNCIONALES, FALTA IMPLEMENTARLOS DENTRO DE MAIN.
(define testDoc1 '(1 "driques" (30 10 2020) "test 2 del doc1" "doc1"))
(define testDoc2 '(1 "driques" (30 10 2020) "test 3 del doc1" "doc1"))
(define testHist1 (list '(2 1 '(2 "driques" (30 10 2020) "otroTest" "doc2")) '(1 1 '(1 "driques" (30 10 2020) "test 1" "doc1"))) )
(define testHist2 '())

;(define test1 (historial testDoc1 testHist2))
;(define test2 (historial testDoc2 test1))


