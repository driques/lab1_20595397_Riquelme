#lang racket
(provide (all-defined-out))
(require "TDA_Fecha.rkt")
(require "TDA_Usuarios.rkt")

;Definición encryptFn
(define encryptFn (lambda (s) (list->string (reverse (string->list s)))))

;Representación paradigmaDocs
;( (name) (date) (encryptFn) (decryptFn) '(listaRegistrados) '(usuarioActivo) '(listaDocumentos) '(listaAccess) '(historial)) 

;Constructor paradigmaDocs, crea la base de todo.
;Dominio: string X string X fecha X encrypt X decrypt
;Recorrido: lista paradigmaDocs
(define (paradigmaDocs name date encryptFn decryptFn)
  (if (string? name)
      (if (fecha? date)
          (list name date encryptFn decryptFn '() '() '() '() '())
          null)
      null)
  )
;Pertenencia
;Función que pregunta si es que existe un usuario x dentro de la lista de usuarios.
;Dominio: listaString X string
;Recorrido: booleano
(define (existeUserDoc? listaUsuarios username)
    (if (null? listaUsuarios)
        #f
        (if (equal? (car (car (list (car listaUsuarios)))) username)
            #t 
            (existeUserDoc? (cdr listaUsuarios) username);Llamado recursivo
        )
    )
  )


;Selectores
;Función selectora del nombre del programa.
;Dominio:lista paradigmaDocs
;Recorrido: string
(define (pDocs->name pDocs)
  (list-ref pDocs 0)
  )
;Función selectora de la fecha de creación.
;Dominio: lista paradigmaDocs
;Recorrido: lista enteros
(define (pDocs->date pDocs)
  (list-ref pDocs 1)
  )
;Función selectora de la función encrypt.
;Dominio:lista paradigmaDocs
;Recorrido: función encrypt.
(define (pDocs->encryptFn pDocs)
  (list-ref pDocs 2)
  )
;Función selectora de la función decrypt.
;Dominio:lista paradigmaDocs
;Recorrido: función decrypt.
(define (pDocs->decryptFn pDocs)
  (list-ref pDocs 3)
  )
;Función selectora de la lista de usuarios registrados.
;Dominio:lista paradigmaDocs
;Recorrido: lista strings.
(define (pDocs->usersList pDocs)
    (list-ref pDocs 4)
)
;Función selectora del usuario activo.
;Dominio:lista paradigmaDocs
;Recorrido: lista string.
(define (pDocs->activeUser pDocs)
    (list-ref pDocs 5)
)
(define (pDocs->onlyOnlineUser pDocs)
  (car(car(pDocs->activeUser pDocs)))
  )
;Función selectora de los documentos creados.
;Dominio:lista paradigmaDocs
;Recorrido: lista strings.
(define (pDocs->docs pDocs)
    (list-ref pDocs 6)
)
;Función selectora de los accesos para cada id de documento.
;Dominio:lista paradigmaDocs
;Recorrido: lista strings.
(define (pDocs->access pDocs)
  (list-ref pDocs 7)
 )
;Función selectora del historial.
;Dominio:lista paradigmaDocs
;Recorrido: lista strings.
(define (pDocs->history pDocs)
  (list-ref pDocs 8)
 )




;Modificadores
;La siguiente función actualiza paradigmadocs cada vez que se ejerce un cambio sobre él.
;Dominio: string X string X fecha X encrypt X decrypt X string X string X string X string
;Recorrido: lista paradigmadocs
(define (actualizarDocs name date encryptFn decryptFn listaUsuarios usuariosActivos listaDocs access historial)
  (list name date encryptFn decryptFn listaUsuarios usuariosActivos listaDocs access historial)
 )


;Funciones extras

;Función que permite autentifiacar al usuario para poder iniciar sesión.
;Dominio: listaString X string X string
;Recorrido: Booleano 
(define (autenticacion listaUsuarios user password)
    (if (null? listaUsuarios)
        #f
        (if (equal? (car listaUsuarios) (list user password))
            #t 
            (autenticacion (cdr listaUsuarios) user password);Llamado recursivo
        )
    )
  )
;Función que permite saber si el usuario ingresado está logeado.
;Dominio: paradigmaDocs
;Recorrido: booleano
(define (logeado? pDocs)
  (not (null? (pDocs->activeUser pDocs)))
 )
;Función que permite realizar un logout
;Dominio: no presenta
;Recorrido: lista
(define (logOut)
  '()
  )

;Tests
;(define a (paradigmaDocs "gDocs" (fecha 12 1 2020) encryptFn encryptFn))
;(define listaUsuarios '(("driques" "contrasenia123") ("pedroPPerez" "password") ("tesUser" "testContrasenia")))
;(define user '("driques"))
;(define password '("contrasenia123"))
;(define testAuto (autenticacion listaUsuarios "driques" "contrasenia123"))
;(define testAutoF (autenticacion listaUsuarios "driques" "contraseniaasd123"))