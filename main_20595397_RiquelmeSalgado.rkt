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
;Autoevaluacion: 100%

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
                           (agregaUsuarioHist (newHistUser username date) (pDocs->usersHistory pDocs))
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
;Autoevaluacion: 100%

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
                           (pDocs->usersHistory pDocs)
                           )
                       )
              (function pDocs)
             )
      (function pDocs))
    
  )
;Implementación de función create, cumple con el prerequisito de login.

;Dominio: paradigmadocs X date X String (nombreDoc) X String  (contenido)
;Recorrido: Paradigmadocs
;Cabe destacar el uso de currificación para hacer posible la función, en caso de no cumplir
;las condiciones necesarias se retorna paradigmadocs sin modificar.
;Autoevaluacion: 100%
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
                                  (nuevoDoc contenido (car (car (pDocs->activeUser pDocs))) date (pDocs->docs pDocs) nombre pDocs)
                                  (pDocs->access pDocs)
                                  (historial (docs->idDoc (list-ref (reverse(nuevoDoc contenido (car (car (pDocs->activeUser pDocs))) date (pDocs->docs pDocs) nombre pDocs)) 0));Por la naturaleza de nuevoDoc, se invierte.
                                             (nuevoDoc contenido (car (car (pDocs->activeUser pDocs))) date (pDocs->docs pDocs) nombre pDocs)
                                             (pDocs->history pDocs))
                                  (pDocs->usersHistory pDocs)
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
;Autoevaluacion: 100%
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
                            (pDocs->usersHistory pDocs)
                            )
             
                pDocs
             ) 
              pDocs
           )
             
         )
  )



;Implementación de add, no arroja resultados si no se aplica login
;Solo propietarios o usuarios con permiso write pueden agregar

;Dominio: paradigmadocs X int X date X String
;Recorrido: paradigmadocs
;Autoevaluacion: 100%
(define (add pDocs)
        (lambda (id fechaEnt content)
        (if (not (null? (pDocs->activeUser pDocs)))
            (if (or (permisoEditar? (car(car (pDocs->activeUser pDocs))) id (car (pDocs->access pDocs)))  (autoriaDoc? (car ( car(pDocs->activeUser pDocs))) id (pDocs->docs pDocs)))
                  (actualizarDocs
                            (pDocs->name pDocs)
                            (pDocs->date pDocs)
                            (pDocs->encryptFn pDocs)
                            (pDocs->decryptFn pDocs)
                            (pDocs->usersList pDocs)
                            (logOut)
                            (actualizaDoc id ((pDocs->encryptFn pDocs)content) (pDocs->docs pDocs))
                            (pDocs->access pDocs)
                            (historial id
                                       (actualizaDoc id ((pDocs->encryptFn pDocs)content) (pDocs->docs pDocs))
                                       (pDocs->history pDocs))
                            (pDocs->usersHistory pDocs)
                            )
                 pDocs
                 )
            pDocs
            )
      )
      
  )




;RestoreVersion, se encarga de reestablecer una version anterior de pdocs.
;Dominio: paradigmadocs X int X int
;Recorrido: paradigmadocs 
;Autoevaluacion: 100%
(define (restoreVersion pDocs)
         (lambda (idDoc idVersion)
           (if (not (null? (pDocs->activeUser pDocs)))
              (if (autoriaHistorialDoc? idDoc (pDocs->onlyOnlineUser pDocs) (pDocs->history pDocs))
                 (if (existenId? idDoc idVersion (pDocs->history pDocs))
                     (actualizarDocs
                            (pDocs->name pDocs)
                            (pDocs->date pDocs)
                            (pDocs->encryptFn pDocs)
                            (pDocs->decryptFn pDocs)
                            (pDocs->usersList pDocs)
                            (logOut)
                            (reemplaza (pDocs->docs pDocs) idDoc (restore idDoc idVersion (pDocs->history pDocs)));Aqui actualiza
                            (pDocs->access pDocs)
                            (pDocs->history pDocs)
                            (pDocs->usersHistory pDocs)
                            )
                     pDocs)
                 pDocs
                 )

              pDocs
            )
           )
  )


;RevokeAllAccesses, no retorna resultado si el user activo no tiene documentos, o no hay users activos.
;Solo hace cambios en los documentos del propietario del doc.
;Utiliza funciones al estilo declarativo, con map y filter (revisar TDA_Access).
;Dom: paradigmaDocs
;Rec: paradigmaDocs
;Autoevaluacion: 100%
(define (revokeAllAccesses pDocs)
           (if (not (null? (pDocs->activeUser pDocs)))
               (actualizarDocs
                            (pDocs->name pDocs)
                            (pDocs->date pDocs)
                            (pDocs->encryptFn pDocs)
                            (pDocs->decryptFn pDocs)
                            (pDocs->usersList pDocs)
                            (logOut)
                            (pDocs->docs pDocs)
                            (revokeAllAccessesNoEncp (car (pDocs->access pDocs)) (filtraId (pDocs->docs pDocs) (pDocs->onlyOnlineUser pDocs)))
                            (pDocs->history pDocs)
                            (pDocs->usersHistory pDocs)
                            )
            pDocs
            )
  )



;----------------------------------------------------------------------------------------------------------------------------
;Función auxiliar para search, permite saber si se contiene una palabra dentro de un doc, y si el user es, propietario, lector
;o escritor.
;Dom: stringXstringXchar
;Rec:bool
(define (contPalabra? palabra user accesos)
  (lambda (docs)
    (if (or (autoriaDoc? user (docs->idDoc docs) (list docs)) (permisoSearch? user (docs->idDoc docs) accesos))
        (string-contains? (cadddr docs) palabra)
        #f)
        
    )
 )

;Implementación de función search, retorna una lista nula en caso de no cumplir ser propietario, lector o escritor.
;Dom: paradigmaDocs
;Rec: document list
;Autoevaluacion: 100%
(define (search pDocs)
           (lambda (palabra)
            (filter (contPalabra? palabra (pDocs->onlyOnlineUser pDocs) (car(pDocs->access pDocs))) (listaDocTotal (pDocs->history pDocsAdd) '()))
        )
  )


;Se implementa listaDocTotal con el fin de recaudar todos los documentos, tanto en versiones activas como antiguas.
;Dom: lista X lista
;Rec: lista
(define (listaDocTotal historial listasAcum)
     (if (null? historial)
         listasAcum
         (listaDocTotal (cdr historial) (append listasAcum (cddr(car historial))))
         )
  )

  
;

;paradigmaDocs->string se encarga de recibir un editor paradigmaDocs y transformarlo a un str comprensible por el
;usuario.
;Dom: paradigmaDocs
;Rec: String
;Autoevaluacion: 100%
(define (paradigmaDocs->string pDocs)
        (if (not (null? (pDocs->activeUser pDocs)))
            (~a (~a (string->activeUser (pDocs->activeUser pDocs) (pDocs->usersHistory pDocs))
            (string->activeDocs (pDocs->docs pDocs) (pDocs->decryptFn pDocs) (user->username (car (pDocs->activeUser pDocs))) (pDocs->history pDocs) (if (null? (pDocs->access pDocs))
                                                                                   null
                                                                                   (car (pDocs->access pDocs))   ) )
             ) (if (null? (pDocs->access pDocs))
                      "NO HAY DOCUMENTOS COMPARTIDOS."
                     (~a (~a (~a "Documentos compartidos con " (user->username (car (pDocs->activeUser pDocs)))"\n") )(entregaDocs (entregaIDDocsCom (user->username (car (pDocs->activeUser pDocs))) (car (pDocs->access pDocs))) (pDocs->docs pDocs) (pDocs->decryptFn pDocs)))  ))
           ;De aqui sin login 
            (~a (~a (string->registerUser (pDocs->usersList pDocs))
                  (string->docs (pDocs->docs pDocs) (pDocs->decryptFn pDocs))) (if (null? (pDocs->access pDocs))
                                                                                   "SIN COMPARTIR DOCS"
                                                                                   (string->access (car (pDocs->access pDocs))))))

        )
               ;
  ; 
;-----------------------------------------------------------------------------------------------------------------------------



;Los siguientes son funciones "test" para probar las anteriores funciones, probar uno a uno para entender la traza.

(define pDocs (paradigmaDocs "pDocs" (fecha 20 11 2020) encryptFn encryptFn)) ;Se crea el editor de texto.
(define pDocs1 (paradigmaDocs "pDocs1" (fecha 41 11 2020) encryptFn encryptFn)) ;Se crea el editor de texto, pero la fecha tiene error. Retorna nulo.
(define pDocs2 (paradigmaDocs "pDocs2" (fecha 02 04 2021) encryptFn encryptFn)) ;Se crea el editor de texto.


(define pDocsRegister1 (register pDocs (fecha 12 11 2021) "pepe" "qwertyy1234")) ;Se regitra usuario
(define pDocsRegister2 (register pDocsRegister1 (fecha 10 11 2020) "pepe3" "qwertyy1234")) ;Se regitra usuario
(define pDocsRegister3 (register pDocsRegister2 (fecha 10 11 2020) "driques" "contrasenia321")) ;Se registra usuario
(define pDocsError (register pDocsRegister3 (fecha 20 11 2020) "driques" "qwertyy1234"));Hace falta que solo el username sea identico para
                                                                                        ;no poder tomarlo



(define pDocsLogin1 ((login pDocsRegister3  "driques"  "contrasenia321" create) (fecha 30 10 2020) "doc1" "primer documento"));Se inicia sesión y se crea un doc. 
(define pDocsLogin2 ((login pDocsLogin1  "pepe"  "qwertyy1234" create) (fecha 30 10 2020) "doc2" "segundooo documento")) ;Se inicia sesión y se crea un doc.
(define pDocsLogin3 ((login pDocsLogin2  "usuarioError"  "qwertyy1234" create) (fecha 30 10 2020) "doc2" "segundooo documento")) ;No existe el usuario
(define pDocsLogin4 ((login pDocsLogin2  "pepe3"  "qwertyy1234" create) (fecha 30 10 2020) "doc3" "tercer documento")) ;Se inicia sesión y se crea un doc.


(define pDocsShare ((login pDocsLogin2  "pepe"  "qwertyy1234" share) 2 (newAccess "driques" #\w) (newAccess "pepe3" #\r))) ;Se dan accesos nuevos
(define pDocsShare2 ((login pDocsShare  "juana"  "qwertyy1234" share) 2 (newAccess "driques" #\w) ));Juana no existe en los usuarios registrados, retorna pDocs sin cambios
(define pDocsShare3 ((login pDocsShare  "driques"  "contrasenia321" share) 1 (newAccess "pepe3" #\w) )) ;Comparte otro usuario otro doc
(define pDocsShare4 ((login pDocsShare  "driques"  "contrasenia321" share) 2 (newAccess "pepe3" #\c) )) ;No permite compartir documentos que no sean de la autoria del usuario.


(define pDocsAdd ((login pDocsShare3  "driques"  "contrasenia321" add) 2 (fecha 1 11 2020) " este es un comentario")) ;driques tiene permisos write en el doc 2, por lo que puede editar
(define pDocsAdd2 ((login pDocsAdd  "pepe3"  "qwertyy1234" add) 1 (fecha 1 11 2020) " este es un comentario en el documento 1")) ;pepe3 tiene permisos write en el doc 1, por lo que puede editar
(define pDocsAdd3 ((login pDocsAdd2  "driques"  "contrasenia321" add) 3 (fecha 1 11 2020) " Aqui no hay comentario, doc no existe.")) ;No existe el doc 3.

(define pDocsRestore ((login pDocsAdd2 "driques" "contrasenia321" restoreVersion) 1 1)) ;Doc 1 se restaura a la version 1
(define pDocsRestore2 ((login pDocsAdd2 "pepe3" "qwertyy1234" restoreVersion) 1 1)) ;Doc 1 no se restaura ya que pepe3 no es el admin.
(define pDocsRestore3 ((login pDocsRestore "driques" "contrasenia321" restoreVersion) 1 2)) ;Doc 1 se restaura a la version 2.

(define pDocsRevoke (login pDocsShare3 "driques" "contrasenia321" revokeAllAccesses));Introducción correcta, procede a eliminar los permisos del documento de "driques"
(define pDocsRevoke2 (login pDocsShare3 "pepe" "contrasenia321" revokeAllAccesses));Usuario invalido, solo devuelve paradigmaDocs.
(define pDocsRevoke3 (login pDocsShare3 "pepe" "qwertyy1234" revokeAllAccesses));Introducción correcta, elimina los permisos de los documentos de "pepe"

;--------------------------------------
(define pDocsSearch ((login pDocsShare3 "pepe" "qwertyy1234" search)"documento"));"pepe" solo tiene permiso para ver su propio doc, ya que el permiso que tiene es de comentario, no de escritor o lector.
(define pDocsSearch2 ((login pDocsShare3 "driques" "contrasenia321" search)"documento"));"driques" tiene permiso de escritor en el documento 2, y es propietario del documento 1, por lo que puede ver ambos.
(define pDocsSearch3 ((login pDocsShare3 "driques" "contrasenia321" search)"noExistePalabra"));Busca una palabra que no existe, entonces, retorna una lista nula.


(define pDocsLoginFinal ((login pDocsAdd2 "driques"  "contrasenia321" create) (fecha 30 10 2020) "docFinal" "ultimo documento")) ;Creacion de un doc.
(define pDocsShare5 ((login pDocsLoginFinal  "driques"  "contrasenia321" share) 3 (newAccess "pepe3" #\c) )) ;Comparte doc 3.

(define pDocsStrActive (login pDocsShare5 "driques" "contrasenia321" paradigmaDocs->string)) ;Creacion del str de paradigmadocs a través de login.
(define pDocsStr (paradigmaDocs->string pDocsShare5)) ;Creacion del str de paradigmadocs sin login.
(define pDocsStr2 (paradigmaDocs->string pDocs)) ;Creacion de str sin documentos.
