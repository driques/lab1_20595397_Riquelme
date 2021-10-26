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
(define (pDocs->name pDocs)
  (list-ref pDocs 0)
  )
(define (pDocs->date pDocs)
  (list-ref pDocs 1)
  )
(define (pDocs->encryptFn pDocs)
  (list-ref pDocs 2)
  )

(define (pDocs->decryptFn pDocs)
  (list-ref pDocs 3)
  )
(define (pDocs->usersList pDocs)
    (list-ref pDocs 4)
)
(define (pDocs->activeUser pDocs)
    (list-ref pDocs 5)
)
(define (pDocs->docs pDocs)
    (list-ref pDocs 6)
)
;Modificadores
(define (actualizarDocs name date encryptFn decryptFn listaUsuarios usuariosActivos listaDocs)
  (list name date encryptFn decryptFn listaUsuarios usuariosActivos listaDocs)
 )


;Funciones extras

(define (existeUserDoc? listaUsuarios username)
    (if (null? listaUsuarios)
        #f
        (if (equal? (car (car (list (car listaUsuarios)))) username)
            #t 
            (existeUserDoc? (cdr listaUsuarios) username);Llamado recursivo
        )
    )
  )


(define (autenticacion listaUsuarios user password)
    (if (null? listaUsuarios)
        #f
        (if (equal? (car listaUsuarios) (list user password))
            #t 
            (autenticacion (cdr listaUsuarios) user password);Llamado recursivo
        )
    )
  )

(define (logeado? pDocs)
  (not (null? (pDocs->activeUser pDocs)))
 )

(define listaUsuarios '(("driques" "contrasenia123") ("pedroPPerez" "password") ("tesUser" "testContrasenia")))
(define user '("driques"))
(define password '("contrasenia123"))
(define testAuto (autenticacion listaUsuarios "driques" "contrasenia123"))
(define testAutoF (autenticacion listaUsuarios "driques" "contraseniaasd123"))