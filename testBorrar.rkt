#lang racket


(define listaTest '((1 "driques" (30 10 2020) "primer documento" "doc1") (3 "driques" (30 10 2020) "tercer documento" "doc3") (2 "pepe" (30 10 2020) "segundooo documento" "doc2")))

(define listaAccess '((2 (("driques" #\w) ("pepe3" #\r)) 1 (("pepe5" #\w)) 4 (("driques" #\w) ("pepe3" #\r)) 3 (("pepe3" #\r)))))
(define listaAccess2 '((1 (("pepe3" #\w)))))
(define listaAccessTest '( (2 (("driques" #\w) ("pepe3" #\r))) (1 (("pepe3" #\w))) (4 (("driques" #\w) ("pepe3" #\r))) (3 (("pepe3" #\r)))) )

;-----------------------------------------------------------------------------
(define (nameEq? user)
            (lambda (listaDocs)
              (if (eq? (cadr listaDocs) user)
                  #t
                  #f)
              )
  )


(define (filtraId listaDocs user)
       (map car (filter (nameEq? user) listaDocs))
       
  )
;------------------------------------------------------------------------------

(define (usuariosRemover id listaAccess)
         (if (null? listaAccess)
             '()
               (if (eq? (car listaAccess) id)
                   (cadr listaAccess)
                   (usuariosRemover id (cddr listaAccess))
                   )
             )
         )


(define (filtraIdDado id listaAccess)
           (remove (usuariosRemover id listaAccess) listaAccess)
         
          
  )

(define (eliminaId id ListaAccess)
   (remq id (filtraIdDado id (car listaAccess)) )
  ) 
;------------------------------------------------------------------------------

(define (revokeAllAccesses listaAccess listaId verif listaComp)
   null
       
  )

;(map (lambda (id listaAccess) (remq id (filtraIdDado id (car listaAccess)) )) (car listaAccess))

;(revokeAllAccessess listaAccess '(1 3))


