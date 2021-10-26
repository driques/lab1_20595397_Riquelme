#lang racket
(provide (all-defined-out))

;Representación (la haré luego)


;Constructor
(define (nuevoDoc contenido autor date documentos nombreDoc)
   (if(string? contenido)
     (list (newId documentos) autor date contenido nombreDoc)
     null
     )
  )



















;Funciones adicionales

(define (newId documentos)
  (if(null? documentos)
     0
     (+ (length documentos) 1)
     )
  )