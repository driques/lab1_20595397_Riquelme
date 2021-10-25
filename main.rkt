#lang racket
(require "TDA_Fecha.rkt")
(require "TDA_ParadigmaDocs.rkt")
(require "TDA_Usuarios.rkt")
;Implementación de función register.


(define (registrarUsuario pDocs username password)
    (if (user? (list username password)
        (if (not (existeUserDoc? (pDocs->userList pDocs) username))
            (agregaUsuario (list username password) (pDocs->userList pDocs))
            pDocs
        )
        pDocs
        )
    )
 
)




      