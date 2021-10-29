#lang racket
(require "TDA_Fecha.rkt")
(require "TDA_ParadigmaDocs.rkt")
(require "TDA_Usuarios.rkt")
(require "TDA_Documento.rkt")
(require "TDA_Access.rkt")

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
                                  (pDocs->access pDocs))
                  
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
                            )
             
                pDocs
             ) 
              pDocs
           )
             
         )
  )

;Ímplementación función adicional para verificar que el documento seleccionado a compartir sea de la
;autoria del usuario logeado.
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

;PermisoEditar?
(define (permisoEditar? userLog idDoc accesos)
  (recorreEditores (car (cdr accesos)) userLog)
   

  )
;Funcion que recorre los editores y pregunta si es que el nombre del user está dentro de estos y si tiene permisos write

(define (recorreEditores listaEditores nombreEditor)
  
;(car (car listaEditores))
 nombreEditor   
  )





;((add pDocsShare3) 2 (fecha 2 1 2020) "heyhey")

;Implementación de add
(define (add pDocs)
        (lambda (id fecha content)
          (print (car(car (pDocs->activeUser pDocs))))
          (print  id )
           (print (cdr (car (pDocs->access pDocs))))
          (permisoEditar? (car(car (pDocs->activeUser pDocs))) id (car (pDocs->access pDocs))
         

      )
      
  ))



;Los siguientes son funciones "test" para probar las anteriores

(define pDocs (paradigmaDocs "pDocs" (fecha 20 11 2020) encryptFn encryptFn))
(define pDocsRegister1 (register pDocs (fecha 20 11 2021) "pepe" "qwertyy1234"))
(define pDocsRegister2 (register pDocsRegister1 (fecha 10 11 2020) "pepe3" "qwertyy1234"))
(define pDocsRegister3 (register pDocsRegister2 (fecha 12 01 2023) "driques" "contrasenia321"))
;(define pDocsError (register pDocsRegister3 (fecha 20 11 2020) "driques" "qwertyy1234"));Hace falta que solo el username sea identico para
                                                                                        ;no poder tomarlo
(define pDocsLogin1 ((login pDocsRegister3  "driques"  "contrasenia321" create) (fecha 30 10 2020) "doc1" "primer documento")) 
(define pDocsLogin2 ((login pDocsLogin1  "pepe"  "qwertyy1234" create) (fecha 30 10 2020) "doc2" "segundooo documento"))

(define pDocsShare ((login pDocsLogin2  "pepe"  "qwertyy1234" share) 2 (newAccess "driques" #\w) (newAccess "pepe" #\r) (newAccess "pepe3" #\w)))
;(define pDocsShare2 ((login pDocsShare  "juana"  "qwertyy1234" share) 2 (newAccess "driques" #\w) ));Juana no existe en los usuarios registrados, retorna pDocs sin cambios
(define pDocsShare3 ((login pDocsShare  "driques"  "contrasenia321" share) 1 (newAccess "pepe3" #\c) )) ;Comparte otro usuario otro doc
;(define pDocsShare4 ((login pDocsShare  "driques"  "contrasenia321" share) 2 (newAccess "pepe3" #\c) )) ;No permite compartir documentos que no sean de la autoria del usuario.


(define pDocsAdd ((login pDocsShare  "pepe3"  "qwertyy1234" add) 2 (fecha 1 11 2020) "asdasd")) ;Tiene permisos w en el doc 2

;(define testAutoria (autoriaDoc? "driques" 1 (pDocs->docs pDocsLogin2)))
;((add pDocsLogin3) 1 (fecha 2 1 2020) "heyhey")

;(car (pDocs->access pDocsShare))
;(((("driques" #\r) ("pepe" #\r) ("pepe3" #\r)) 2))
;(((("driques" #\r) ("pepe" #\r) ("pepe3" #\r)) 2 (("pepe3" #\c)) 1))