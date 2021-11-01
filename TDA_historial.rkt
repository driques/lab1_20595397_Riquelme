#lang racket
(provide (all-defined-out))
(require "TDA_Documento.rkt")
(require "TDA_ParadigmaDocs.rkt")
;Representación



;Me falta reconstruir el constructor anterior, para llevarlo a la siguiente forma,
;asi pregunto por el idVer máx (que se encontrará más a la izquierda), para luego, en caso
;de hacer un cambio, agregar un 1 al idVer del idDoc, y asi ir incrementando el historial para
;un documento en especifico.

;Constructor '( (idVer idDoc (doc)) (idVer2 idDoc2 (doc)) )


(define (historial documento historial)
  (if (null? historial)
      (list 1 (car documento))
      (append (list (+ (historial->idHist historial) 1) (car documento)) historial)
     )
 
  )



;Selector
(define (historial->idHist historial)
  (car historial)
  )