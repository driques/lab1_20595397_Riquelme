#lang racket
(require "TDA_Fecha.rkt")
(require "TDA_ParadigmaDocs.rkt")
(require "TDA_Usuarios.rkt")
(require "TDA_Documento.rkt")
(require "TDA_Access.rkt")
(require "TDA_historial.rkt")



;Implementación de función register. Crea una nueva cuenta dentro de paradigmaDocs.
;Utiliza recursion natural en agregaUsuario, si hay un dato invalido, retorna paradigmadocs sin cambios. 
;Dominio: paradigmadocs X date X string X string
;Recorrido: paradigmaDocs

;OJO FALTA AUN IMPLEMENTAR LA PARTE DE LA FECHA QUE NO SE ME OLVIDE!!!
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
                           (pDocs->access pDocs)
                           (pDocs->history pDocs)
                          )
            pDocs
           )
          pDocs
       )
      
      
  )
;Implementación de login
;Utiliza una funcion de orden superior como parametro para poder realizar las demas funciones correspondientes
;Retorna una funcion function, y emplea recursion natural en agregaUsuario.
;Dominio: paradigmadocs X string X string X function
;Recorrido: function & paradigmadocs

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
                           (pDocs->access pDocs)
                           (pDocs->history pDocs)
                           )
                       )
              (function pDocs)
             )
      (function pDocs))
    
  )
;Implementación de función create, cumple con el prerequisito de login.
;NO OLVIDAR ME FALTA ENCRIPTAR CONTENIDO


;Dominio: paradigmadocs X date X String (nombreDoc) X String  (contenido)
;Recorrido: Paradigmadocs
;Cabe destacar el uso de currificación para hacer posible la función, en caso de no cumplir
;las condiciones necesarias se retorna paradigmadocs sin modificar.
(define (create pDocs)
         (lambda (date nombre contenido)
           (if (logeado? pDocs)
               (if(and (string? contenido) (fecha? date))
                  (actualizarDocs (pDocs->name pDocs)
                                  (pDocs->date pDocs)
                                  (pDocs->encryptFn pDocs)
                                  (pDocs->decryptFn pDocs)
                                  (pDocs->usersList pDocs)
                                  (logOut)
                                  (nuevoDoc contenido (car (car (pDocs->activeUser pDocs))) date (pDocs->docs pDocs) nombre)
                                  (pDocs->access pDocs)
                                  (historial (docs->idDoc (list-ref (reverse(nuevoDoc contenido (car (car (pDocs->activeUser pDocs))) date (pDocs->docs pDocs) nombre)) 0));Por la naturaleza de nuevoDoc, se invierte.
                                             (nuevoDoc contenido (car (car (pDocs->activeUser pDocs))) date (pDocs->docs pDocs) nombre)
                                             (pDocs->history pDocs))
                                  )
                   
                  
                  pDocs)
               pDocs
               )  
        )
  )


;Implementación de share
;Se creó con anterioridad create, y no arroja resultado si no se ocupa con login
;ya que aprovecha la currificación de la función creada anteriormente.
;Dominio: paradigmadocs X int X access List
;Recorrido: paradigmadocs
(define (share pDocs)
       (lambda (id access . accesses)
          (if (not(null? (pDocs->activeUser pDocs)))
             (if (autoriaDoc? (car ( car(pDocs->activeUser pDocs))) id (pDocs->docs pDocs))
               (actualizarDocs
                            (pDocs->name pDocs)
                            (pDocs->date pDocs)
                            (pDocs->encryptFn pDocs)
                            (pDocs->decryptFn pDocs)
                            (pDocs->usersList pDocs)
                            (logOut)
                            (pDocs->docs pDocs)
                            (list (daAcceso (append (list access) accesses) (pDocs->access pDocs) id (pDocs->usersList pDocs)))
                            (pDocs->history pDocs)
                            )
             
                pDocs
             ) 
              pDocs
           )
             
         )
  )



;Implementación de add, no arroja resultados si no se aplica login
;Solo propietarios o usuarios con permiso write pueden agregar
;FALTA AÑADIR LA ENCRIPTACIÓN

;Dominio: paradigmadocs X int X date X String
;Recorrido: paradigmadocs 
(define (add pDocs)
        (lambda (id fecha content)
        (if (not (null? (pDocs->activeUser pDocs)))
            (if (or (permisoEditar? (car(car (pDocs->activeUser pDocs))) id (car (pDocs->access pDocs)))  (autoriaDoc? (car ( car(pDocs->activeUser pDocs))) id (pDocs->docs pDocs)))
                  (actualizarDocs
                            (pDocs->name pDocs)
                            (pDocs->date pDocs)
                            (pDocs->encryptFn pDocs)
                            (pDocs->decryptFn pDocs)
                            (pDocs->usersList pDocs)
                            (logOut)
                            (actualizaDoc id content (pDocs->docs pDocs))
                            (pDocs->access pDocs)
                            (historial id
                                       (actualizaDoc id content (pDocs->docs pDocs))
                                       (pDocs->history pDocs))
                            )
                 pDocs
                 )
            pDocs
            )
      )
      
  )




;RestoreVersion
;Dominio: paradigmadocs X int X int
;Recorrido: paradigmadocs 

(define (restoreVersion pDocs)
         (lambda (idDoc idVersion)
           (if (not (null? (pDocs->activeUser pDocs)))
              (if (autoriaHistorialDoc? idDoc (car(car(pDocs->activeUser pDocs))) (pDocs->history pDocs))
                 (if (existenId? idDoc idVersion (pDocs->history pDocs))
                     (actualizarDocs
                            (pDocs->name pDocs)
                            (pDocs->date pDocs)
                            (pDocs->encryptFn pDocs)
                            (pDocs->decryptFn pDocs)
                            (pDocs->usersList pDocs)
                            (logOut)
                            (reemplaza (pDocs->docs pDocs) idDoc (restore idDoc idVersion (pDocs->history pDocs)));Aqui actualizar
                            (pDocs->access pDocs)
                            (pDocs->history pDocs)
                            )
                     pDocs)
                 pDocs
                 )

              pDocs
            )
           )
  )


;RevokeAllAccesses

(define (revokeAllAccesses pDocs)
           (if (not (null? (pDocs->activeUser pDocs)))
            null
            pDocs
            )
  )




;Los siguientes son funciones "test" para probar las anteriores funciones, probar uno a uno para entender la traza.

(define pDocs (paradigmaDocs "pDocs" (fecha 20 11 2020) encryptFn encryptFn)) ;Se crea el editor de texto.
(define pDocsRegister1 (register pDocs (fecha 20 11 2021) "pepe" "qwertyy1234")) ;Se regitra usuario
(define pDocsRegister2 (register pDocsRegister1 (fecha 10 11 2020) "pepe3" "qwertyy1234")) ;Se regitra usuario
(define pDocsRegister3 (register pDocsRegister2 (fecha 12 01 2023) "driques" "contrasenia321")) ;Se regitra usuario
(define pDocsError (register pDocsRegister3 (fecha 20 11 2020) "driques" "qwertyy1234"));Hace falta que solo el username sea identico para
                                                                                        ;no poder tomarlo
(define pDocsLogin1 ((login pDocsRegister3  "driques"  "contrasenia321" create) (fecha 30 10 2020) "doc1" "primer documento"));Se inicia sesión y se crea un doc. 
(define pDocsLogin2 ((login pDocsLogin1  "pepe"  "qwertyy1234" create) (fecha 30 10 2020) "doc2" "segundooo documento")) ;Se inicia sesión y se crea un doc.
(define pDocsLogin3 ((login pDocsLogin1  "usuarioError"  "qwertyy1234" create) (fecha 30 10 2020) "doc2" "segundooo documento")) ;No existe el usuario
(define pDocsLogin4 ((login pDocsLogin2  "pepe3"  "qwertyy1234" create) (fecha 30 10 2020) "doc3" "tercer documento")) ;Se inicia sesión y se crea un doc.


(define pDocsShare ((login pDocsLogin2  "pepe"  "qwertyy1234" share) 2 (newAccess "driques" #\w) (newAccess "pepe3" #\r))) ;Se dan accesos nuevos
(define pDocsShare2 ((login pDocsShare  "juana"  "qwertyy1234" share) 2 (newAccess "driques" #\w) ));Juana no existe en los usuarios registrados, retorna pDocs sin cambios
(define pDocsShare3 ((login pDocsShare  "driques"  "contrasenia321" share) 1 (newAccess "pepe3" #\w) )) ;Comparte otro usuario otro doc
(define pDocsShare4 ((login pDocsShare  "driques"  "contrasenia321" share) 2 (newAccess "pepe3" #\c) )) ;No permite compartir documentos que no sean de la autoria del usuario.


(define pDocsAdd ((login pDocsShare3  "driques"  "contrasenia321" add) 2 (fecha 1 11 2020) " este es un comentario")) ;driques tiene permisos write en el doc 2, por lo que puede editar
(define pDocsAdd2 ((login pDocsAdd  "pepe3"  "qwertyy1234" add) 1 (fecha 1 11 2020) " este es un comentario en el documento 1")) ;pepe3 tiene permisos write en el doc 1, por lo que puede editar
(define pDocsAdd3 ((login pDocsAdd2  "driques"  "contrasenia321" add) 3 (fecha 1 11 2020) " Aqui no hay comentario, doc no existe.")) ;No existe el doc 3.

(define pDocsRestore ((login pDocsAdd2 "driques" "contrasenia321" restoreVersion) 1 1))
(define pDocsRestore2 ((login pDocsAdd2 "pepe3" "qwertyy1234" restoreVersion) 1 1))
(define pDocsRestore3 ((login pDocsRestore "driques" "contrasenia321" restoreVersion) 1 2))
;Nota, ya se guardan dentro del historial las versiones distintas de un mismo documento, falta crear la función que devuelva al documento principal el que se quiere volver.