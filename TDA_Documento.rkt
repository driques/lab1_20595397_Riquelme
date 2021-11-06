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
     (append documentos (list (list (newId documentos) autor date contenido nombreDoc)) )
     null
     )
  )
;Pertenencia
;Ímplementación función adicional para verificar que el documento seleccionado a compartir sea de la
;autoria del usuario logeado.
;Dominio: string X int X listaString
;Recorrido: Booleano
(define(autoriaDoc? userLog idDoc docs)
        (if(null? docs)
           #f
           (if (eq? idDoc (car (car docs)))
               (if (eq? userLog (car (cdr (car docs))))
                   #t
                   #f)
               (autoriaDoc? userLog idDoc (cdr docs))
            )
         )
     )

;Selector
(define (docs->idDoc doc)
  (list-ref doc 0)
  )
(define (docs->selectAutor doc)
   (list-ref doc 1)
  )
(define (docs->selectDate doc)
   (list-ref doc 2)
  )
(define (docs->selectContent doc)
   (list-ref doc 3)
  )
(define (docs->selectNombreDoc doc)
   (list-ref doc 4)
  )
;Selector especial
(define (docs->selectDoc id documentos)
   (list-ref documentos (- id 1))
  )

;Modificador
(define (actualizaDoc id contenido documentos)
     (reemplaza documentos id (list id
          (docs->selectAutor (docs->selectDoc id documentos))
          (docs->selectDate (docs->selectDoc id documentos))
          (~a (docs->selectContent (docs->selectDoc id documentos)) contenido)
          (docs->selectNombreDoc (docs->selectDoc id documentos))
          )
       )
  )
;Funciones adicionales

(define (newId documentos)
  (if(null? documentos)
     1
     (+ (length documentos)  1)
     )
  )



(define reemplaza (lambda (docs id docAct)
                    (if (null? docs)
                        '()
                        (if(=  (car (car docs)) id)
                           (cons docAct (reemplaza (cdr docs) id docAct))
                           (cons (car docs) (reemplaza (cdr docs) id docAct))
                        )
                    )
                    )
 )


;Test
;(define test1 (nuevoDoc "heyhey" "pepe" (fecha 10 12 2020) '() "docTest"))
;(define test2 (nuevoDoc "heyhey" "pepe" (fecha 10 12 2020) test1 "docTest"))
;(define listaTest '((1 "driques" (30 10 2020) "primer documento" "doc1") (2 "pepe" (30 10 2020) "segundooo documento" "doc2")))