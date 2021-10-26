#lang racket
(require "TDA_Fecha.rkt")
(require "TDA_ParadigmaDocs.rkt")
(require "TDA_Usuarios.rkt")
(require "TDA_Documento.rkt")
;Implementación de función register.


(define (register pDocs date username password)
       (if (and (string? username) (string? password))
          (if (not (existeUserDoc? (pDocs->usersList pDocs) username))
           (actualizarDocs (pDocs->name pDocs)
                           (pDocs->date pDocs)
                           (pDocs->encryptFn pDocs)
                           (pDocs->decryptFn pDocs)
                           (agregaUsuario (newUser username password) (pDocs->usersList pDocs))
                           (pDocs->activeUser pDocs)
                           (pDocs->docs pDocs)
                          )
            pDocs
           )
          pDocs
       )
      
      
  )

(define (login pDocs user password function)
     (if (existeUserDoc? (pDocs->usersList pDocs) user)
         (if (autenticacion (pDocs->usersList pDocs) user password)
             (function (actualizarDocs (pDocs->name pDocs)
                           (pDocs->date pDocs)
                           (pDocs->encryptFn pDocs)
                           (pDocs->decryptFn pDocs)
                           (pDocs->usersList pDocs)
                           (agregaUsuario (newUser user password) (pDocs->activeUser pDocs))
                           (pDocs->docs pDocs)
                           ))
             (function pDocs))
         (function pDocs))
    
  )

(define (create pDocs)
         (lambda (date nombre contenido)
           (if (logeado? pDocs)
               (if(and (string? contenido) (fecha? date))
                  (actualizarDocs (pDocs->name pDocs)
                                  (pDocs->date pDocs)
                                  (pDocs->encryptFn pDocs)
                                  (pDocs->decryptFn pDocs)
                                  (pDocs->usersList pDocs)
                                  (pDocs->activeUser pDocs)
                                  (nuevoDoc contenido (car (pDocs->activeUser pDocs)) date (pDocs->docs pDocs) nombre))
                  pDocs)
               pDocs
               )  
        )
  )



(define pDocs (paradigmaDocs "pDocs" (fecha 20 11 2020) encryptFn encryptFn))
(define pDocsRegister1 (register pDocs (fecha 20 11 2021) "pepe" "qwertyy1234"))
(define pDocsRegister2 (register pDocsRegister1 (fecha 10 11 2020) "pepe321321" "qwertyy1234"))
(define pDocsRegister3 (register pDocsRegister2 (fecha 12 01 2023) "driques" "contrasenia321"))
(define pDocsError (register pDocsRegister3 (fecha 20 11 2020) "driques" "qwertyy1234"));Hace falta que solo el username sea identico para
                                                                                        ;no poder tomarlo
(define pDocsLogin1 ((login pDocsRegister1  "pepe"  "qwertyy1234" create) (fecha 30 10 2020) "doc1" "primer documento")) 
;(define pDocsLogin2  ((((login pDocsRegister1 "pepe" "qwertyy1234" create) (fecha 20 10 2020)) "doc2" )"este es mi primer docum22ento"))
