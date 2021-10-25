#lang racket
(provide (all-defined-out))
(require "TDA_Fecha.rkt")
(require "TDA_Usuarios.rkt")

;Definición encryptFn
(define encryptFn (lambda (s) (list->string (reverse (string->list s)))))

;Representación paradigmaDocs
;( (name) (date) (encryptFn) (decryptFn) '() '() '()) 

;Constructor paradigmaDocs
(define (paradigmaDocs name date encryptFn decryptFn)
  (if (string? name)
      (if (fecha? date)
          (list name date encryptFn decryptFn '() '() '())
          null)
      null)
  )

(define a (paradigmaDocs "gDocs" (fecha 12 1 2020) encryptFn encryptFn))

;Selectores
(define (pDocs->usersList pDocs)
    (list-ref pDocs 4)
)

;Funciones extras

(define (existeUserDoc? listaUsuarios username)
    (if (null? listaUsuarios)
        #f
        (if (equal? (car listaUsuarios) username)
            #t 
            (existeUserDoc? (cdr listaUsuarios) username);Llamado recursivo
        )
    )
  )

