#lang racket
(provide (all-defined-out))
(require "TDA_Fecha.rkt")
;Representación
;'(idDoc Autor Fecha Contenido NombreDoc)


;Constructor de nuevo documento.
;Dominio: string X string X Fecha X lista X string
;Recorrido : nuevoDoc
(define (nuevoDoc contenido autor date documentos nombreDoc)
   (if(string? contenido)
     (append (list (newId documentos) autor date contenido nombreDoc) documentos)
     null
     )
  )



;Funciones adicionales

(define (newId documentos)
  (if(null? documentos)
     1
     (+ (/ (length documentos) 5) 1)
     )
  )


;Test
;(define test1 (nuevoDoc "heyhey" "pepe" (fecha 10 12 2020) '() "docTest"))