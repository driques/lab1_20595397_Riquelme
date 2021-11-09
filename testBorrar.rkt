#lang racket


(define listaTest '((1 "driques" (30 10 2020) "primer documento" "doc1") (3 "driques" (30 10 2020) "tercer documento" "doc3") (2 "pepe" (30 10 2020) "segundooo documento" "doc2")))

(define listaAccess '((2 (("driques" #\w) ("pepe3" #\r)) 1 (("pepe3" #\w)))))
(define listaAccess2 '((1 (("pepe3" #\w)))))

(define (nameEq? user)
            (lambda (listaDocs)
              (if (eq? (cadr listaDocs) user)
                  #t
                  #f))
  )




(define (filtraId listaDocs user)
       (map car (filter (nameEq? user) listaDocs))
  )

;------------------------------------------------------------------------------
(define (idEq? id)
          (lambda (listaAccess)
            (if (eq? (car listaAccess) id)
               null
               (list ( (car listaAccess) (car (cdr listaAccess))))
               
               )
            )
  )
(define (idEq?2 id)
   (lambda (listaAccess)
     (if (eq? id (car listaAccess))
         '()
         (list id (car listaAccess));(list (car listaAccess) (cadr listaAccess))
       )
     )
  )

(define (revoke listaAccess ids listaFinal)
               (if (< 2 (length listaAccess))
                  (revoke (cddr listaAccess) (cdr ids) (append ((idEq?2 (car ids)) listaAccess) listaFinal)) ;(revoke (cddr listaAccess) (cdr ids) (append (filter (idEq? (car ids)) listaAccess) listaFinal) )
                  ((idEq?2 ids) listaAccess) ; listaFinal)
               
         )
  )
  


;Lo que busco hacer es volver a crear listaAccess pero con un parentesis más, es decir '(((1 (("pepe3" #\w)))))) esto para encapsular la lista donde se encuentren los usuarios, para luego
;Con filter descartarlo y hacer una lista nueva, despues de eso, recursivamente, volveré a preguntar sobre la lista que ya se filtró pero ahora con el siguiente id.


;> (filtraNombre listaTest "driques")
;'((1 "driques" (30 10 2020) "primer documento" "doc1") (3 "driques" (30 10 2020) "tercer documento" "doc3"))
;(revoke (car listaAccess) (filtraId listaTest "driques") '())