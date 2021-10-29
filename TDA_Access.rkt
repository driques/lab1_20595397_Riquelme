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




;No sé si debo borrar el siguiente código

;(define (accessos access.accesses count)
;         (if (null? access.accesses)
;             count
;             (accessos (cdr access.accesses) (+ count 1))
;             )
;  )

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


;Tests
;(define testAccess (list (newAccess "pepe1" #\r) (newAccess "pepe2" #\w) (newAccess "pepe3" #\c) (newAccess "pepe4" #\r) (newAccess "pepe5" #\r)))
;(define listaRe(list "driques" "pepe" "pepwe321321" "pepe321321"))
;(define listaAcc(list "dris" "dros" "drus"))


;(define listaTest '(("pepe" "qwertyy1234") ("pepe3" "qwertyy1234") ("driques" "contrasenia321")("khj" "contrasenia321")))
;(define listaTestUsuarios '(("driques" #\r) ("pepe" #\r) ("pepe3" #\w)))

;(define listaTestUsuarios2 '(("driques" #\r) ("pepe3" #\w)))

;(define AccesoTest (daAcceso listaTestUsuarios testAccess 1 listaTest))