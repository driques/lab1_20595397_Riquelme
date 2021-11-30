#lang racket
(provide (all-defined-out))

;Representacion
;'("user" #\modoEdicion)

;Constructor, permite crear un nuevo usuario que pueda tener un tipo de acceso en especifico
;Dominio: string X char
;Recorrido: lista
(define (newAccess user modo)
  (list user modo)
  )


;Pertenencia
;Función access nos permite saber si es que el modo en que ingresó un usuario es correcot
;Dominio: char
;Recorrido: Booleano
(define (access? modo)
         (if (or (eq? modo #\r) (eq? modo #\w) (eq? modo #\c))
             #t
             #f)
  )



;PermisoEditar? sirve para verificar que un usuario tenga permisos write de un documento en especifico.
;Dominio: stringXintXlistaString
;Recorrido: Booleano
(define (permisoEditar? userLog idDoc accesos)
(if(null? accesos)
           #f
           (if (eq? idDoc  (car accesos))
               (if (recorreEditores (car(cdr accesos)) userLog)
                   #t
                   #f)
               (permisoEditar? userLog idDoc (cdr accesos))
            )
         )
  )

;Selectores
;Funcion que selecciona el id
;Dom: list
;Rec: list
(define (access->id accessList)
  (car accessList))

;Funcion que selecciona los editores
;Dom: list
;Rec: list
(define (access->editores accessList )
     (cadr accessList)
  )



;Funcion que recorre los editores y pregunta si es que el nombre del user está dentro de estos y si tiene permisos write.
;Dominio: listaString X string
;Recorrido: Booleano

(define (recorreEditores listaEditores nombreEditor)

  (if (null? listaEditores)
             #f
             (if (and (eq? (car(car listaEditores)) nombreEditor) (char=? (car(cdr (car listaEditores))) #\w))
                 #t
                 (recorreEditores (cdr listaEditores) nombreEditor)
                 )
             )
  )
;-------------------------------------------------------------------------------------------------------------------



;PermisoEditar? sirve para verificar que un usuario tenga permisos write de un documento en especifico.
;Dominio: stringXintXlistaString
;Recorrido: Booleano
(define (permisoSearch? userLog idDoc accesos)
(if(null? accesos)
           #f
           (if (eq? idDoc  (car accesos))
               (if (recorreSearch (car(cdr accesos)) userLog)
                   #t
                   #f)
               (permisoSearch? userLog idDoc (cdr accesos))
            )
         )



   

  )

;Funcion que recorre los editores y pregunta si es que el nombre del user está dentro de estos y si tiene permisos write.
;Dominio: listaString X string
;Recorrido: Booleano

(define (recorreSearch listaEditores nombreEditor)
  (if (null? listaEditores)
             #f
             (if (and (eq? (car(car listaEditores)) nombreEditor) (or (char=? (car(cdr (car listaEditores))) #\w) (char=? (car(cdr (car listaEditores))) #\r)))
                 #t
                 (recorreEditores (cdr listaEditores) nombreEditor)
                 )
             )
  )


;-------------------------------------------------------------------------------------------------------------------

;Selectores
;Función que nos permite obtener el usuario de access
;Dominio: access
;Recorrido: string
(define (access->user access)
         (car access)
 )
;Selectores
;Función que nos permite obtener el modo de access
;Dominio: access
;Recorrido: char
(define (access->mode access)
         (cadr access)
 )





;Modificador, da acceso a los documentos dependiendo de si están registrados o no, y si todos sus
;datos son validos.
;Dominio: listaStrings X listaStrings X entero X listaStings
;Recorrido: listaString
(define (daAcceso usuarioAccess listaAccess id listaRegistrados)
     (if (eq? (estaRegistradoListas? usuarioAccess listaRegistrados #t) #t) ;Que todos los usuarios que requieran accesos esten
                                                                              ;registrados en la plataforma.
           (if (null? listaAccess) ;no sea nulo
             (list id usuarioAccess )
           (if (equal? 1 (length listaAccess))
                 (append (car listaAccess) (list id) (list usuarioAccess) )
                 (append  (car listaAccess) (list id) (daAcceso usuarioAccess (cdr listaAccess) (list id)) )
                 )
     
         )
         listaAccess ;Se retorna lista sin cambios
      )
  
  )



;Funciones extras

;Las funciones estaRegistrado? y estaRegistradoListas? van de la mano, ya que una nos permite saber si un usuario
;en especifico está registrado, y la otra saber si una lista de usuarios están registrados dentro de una lista de registros.
;Dominio: string X listaString X Bool
;Recorrido: Bool
(define (estaRegistrado? usuario listaRegistrados booleano)
        (if (eq? booleano #f)
            (if (null? listaRegistrados)
                #f
                (if (eq? usuario (car (car listaRegistrados)))
                    (estaRegistrado? usuario (cdr listaRegistrados) #t)
                    (estaRegistrado? usuario (cdr listaRegistrados) #f))
                )
            #t
       )
)
;estaRegistradoListas? nos permite preguntar por cada user la lista de registrados correspondientes.
;Dominio: listaString X listaString X Bool
;Recorrido: Bool
(define (estaRegistradoListas? listaUsers listaRegistrados boolSi)
         (if  (eq? boolSi #t)
             (if (null? listaUsers)
                 boolSi
                 (if (eq? (estaRegistrado? (car (car listaUsers)) listaRegistrados #f) #t)
                     (estaRegistradoListas? (cdr listaUsers) listaRegistrados #t)
                     (estaRegistradoListas? (cdr listaUsers) listaRegistrados #f)
                     )
               )
             
           #f)
  )


;-----------------------------------------------------------------------------
;nameEq? es una función de pertenencia que pregunta si un user coincide con la listaDocs.
;Dom: strXlist
;Rec: bool
(define (nameEq? user)
            (lambda (listaDocs)
              (if (eq? (cadr listaDocs) user)
                  #t
                  #f)
              )
  )

;FilraId nos permite filtrar todos los ids pertenecientes a un user.
;Dom: listXstr
;Rec: list
(define (filtraId listaDocs user)
       (map car (filter (nameEq? user) listaDocs))
       
  )
;------------------------------------------------------------------------------
;UsuarioRemover nos permite remover un usuario de la lista de accesos dependiendo
;del id asignado
;Dom: intXlist
;Rec: list
(define (usuariosRemover id listaAccess)
         (if (null? listaAccess)
             '()
               (if (eq? (car listaAccess) id)
                   (cadr listaAccess)
                   (usuariosRemover id (cddr listaAccess))
                   )
             )
         )

;FiltraIdDado nos permite remover los id.
;Dom: intXlist
;Rec: list
(define (filtraIdDado id listaAccess)
           (remove (usuariosRemover id listaAccess) listaAccess)
         
          
  )
;eliminaId nos permite eliminar completamente tanto el nombre como el id de la lista
;de accesos.
;Dom: intXlist
;Rec: list
(define (eliminaId id listaAccess)
   (remq id (filtraIdDado id  listaAccess) )
  ) 
;------------------------------------------------------------------------------
;revokeAllAccessesNoEncp es la función no encapsulada de revokeAllAccesses, nos permite
;recursivamente eliminar uno a uno los usuarios de un doc especifico.
;Dom: listXlist
;Rec: list
(define (revokeAllAccessesNoEncp listaAccess listaId)
     (if (null? listaId)
         listaAccess
         (revokeAllAccessesNoEncp (eliminaId (car listaId) listaAccess) (cdr listaId))
      )
  )

;string->access nos permite transformar el accessList a un str comprensible.
;Dom: list
;Rec: str
(define (string->access accessList)
       (if(null? accessList)
          "--------------------------\n"
          (string-append (~a (~a (~a ( ~a "ID documento: " (access->id accessList)) " compartido con: ") (access->editores accessList)) "\n") (string->access (cddr accessList)))
          )
  )
;string->activeAccess nos permite transformar los documentos con un user activo a un str comprensible.
;Dom: list
;Rec: str
(define (string->activeAccess accessList id)
    (if(null? accessList)
          "\n"
          (if (eq? id (access->id accessList))
              (~a (~a (~a ( ~a "ID documento: " (access->id accessList)) " compartido con: ") (access->editores accessList)) "\n") 
              (string->access (cddr accessList))
              )
          
          )


  )

;Función que pregunta si un documento se compartió con un user.
;Dom: listXstr
;Rec: Bool

(define (shareWithMe accessList user)
      (if (null? accessList)
          #t
          #f
       )
  )


;Funcion que recorre los editores y pregunta si es que el nombre del user está dentro de estos
;(se diferencia con la otra funcion ya que esta solo pide que exista, y no que tenga un modo de edicion especifico).
;Dominio: listaString X string
;Recorrido: Booleano

(define (recorreEditoresUser listaEditores nombreEditor)
  (if (null? listaEditores)
             #f
             (if (eq? (car (car listaEditores)) nombreEditor) 
                 (append '(#t) (cadr (car listaEditores)))
                 (recorreEditores (cdr listaEditores) nombreEditor)
                 )
             )
  )
;Entrega una lista de los id donde se hayan compartido los documentos con el usuario.
;Dom: strXlist
;Rec: list
(define (entregaIDDocsCom user listaAccess)
        (if (null? listaAccess)
            null
            (if (recorreEditoresUser (cadr listaAccess) user)
                (append (append (list (car listaAccess)) (list (cdr (recorreEditoresUser (cadr listaAccess) user))))  (entregaIDDocsCom user (cddr listaAccess)))
                (entregaIDDocsCom user (cddr listaAccess))
                )
            )
  )




;Tests
;(define testAccess (list (newAccess "pepe1" #\r) (newAccess "pepe2" #\w) (newAccess "pepe3" #\c) (newAccess "pepe4" #\r) (newAccess "pepe5" #\r)))
;(define listaRe(list "driques" "pepe" "pepwe321321" "pepe321321"))
;(define listaAcc(list "dris" "dros" "drus"))


;(define listaTest '(("pepe" "qwertyy1234") ("pepe3" "qwertyy1234") ("driques" "contrasenia321")("khj" "contrasenia321")))
;(define listaTestUsuarios '(("driques" #\r) ("pepe" #\r) ("pepe3" #\w)))

;(define listaTestUsuarios2 '(("driques" #\r) ("pepe3" #\w)))

;(define AccesoTest (daAcceso listaTestUsuarios testAccess 1 listaTest))
;(define testAutoF '((2 (("driques" #\c) ("pepe3" #\r)) 1 (("pepe" #\c)) 5 (("driques" #\w) ("pepe3" #\r)) 7 (("driques" #\w) ("pepe3" #\r)) 9 (("driques" #\w) ("pepe3" #\r)))))


;(define testEntregaDocs (entregaIDDocsCom "driques" (car testAutoF)))
