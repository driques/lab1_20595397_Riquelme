#lang racket
(provide (all-defined-out))
(require "TDA_Fecha.rkt")
;Representación
;'() <- TDA usuario vacio
;'("JuanSanchez" "contrasenia") <- TDA usuario compuesto por su usuario y contrasenia

;Constructores

;Función creadora de user
;Dominio: string string
;Recorrido: lista vacía

(define (newUser user password)
  (list user password)
  )

(define (newHistUser user date)
  (list user date)
  )
;Pertenencia

;La siguiente funcion nos permite determinar si los datos introducidos son correctos
;Para la generacion de un nuevo usuario, se pedirá como minimo un usuario de 4 caracteres y una
;contrasenia de 8.
;Dominio: lista usuario
;Recorrido: bool.
(define (user? user)
  (if (null? user)
  #f
  (if (list? user)
      (if (= 2 (length user))
          (if (and (string? (car user))
                   (string? (car (cdr user))))
               (if(<= 4 (string-length (car user)))
                  (if(<= 8 (string-length (cadr user)))
                     #t
                     #f)
                  #f)
              #f
           )
          #f
          )
      #f
      )
   
   )
  )

;La siguiente funcion nos permite determinar si los datos introducidos son correctos
;Para la generacion del historial del registro de users.
;Dominio: lista usuario hist.
;Recorrido: bool.

(define (userHist? userHist)
  (if (null? userHist)
      #f
      (if (fecha? (user->date userHist))
          #t
          #f)
      )
)
;Selectores

;Funcion selectora del usuario
;Dominio:lista User
;Recorrido: string
(define (user->username user)
  (if (null? user)
      null
  (car user))
  )
;Funcion selectora de contrasenia
;Dominio:lista user
;Recorrido: string
(define (user->password user)
  (car (cdr user))
  )
(define (user->date user)
  (car (cdr user))
  )
;Modificadores
;Aniade un nuevo usuario a la lista de users disponibles
;Dominio: lista de usuarios
;Recorrido: lista de usuarios nueva, lista de usuarios antigua.

(define (agregaUsuario usuarioAgregar listaUsuarios)
  ;Se verifica que el usuario sea del tipo usuario
  (if(user? usuarioAgregar)
     (if (null? listaUsuarios) ;Y que no sea nulo
       (list usuarioAgregar)
       (if (equal? 1 (length listaUsuarios))
          (append (list (car listaUsuarios)) (list usuarioAgregar))
          (append (list (car listaUsuarios)) (agregaUsuario usuarioAgregar (cdr listaUsuarios)))
       )
      )
      listaUsuarios)
  )
;Aniade un nuevo usuario a la lista de historial de usuarios registrados.
;Dominio: lista de usuarios
;Recorrido: lista de usuarios nueva, lista de usuarios antigua.

(define (agregaUsuarioHist usuarioAgregar listaUsuarios)
  ;Se verifica que el usuario sea del tipo usuario
  (if(userHist? usuarioAgregar)
     (if (null? listaUsuarios) ;Y que no sea nulo
       (list usuarioAgregar)
       (if (equal? 1 (length listaUsuarios))
          (append (list (car listaUsuarios)) (list usuarioAgregar))
          (append (list (car listaUsuarios)) (agregaUsuarioHist usuarioAgregar (cdr listaUsuarios)))
       )
      )
      listaUsuarios)
  )


;Funciones extras
;string->activeUser nos permite obtener un str leible por el user.
;Dom: str X fecha
;Rec: str
(define (string->activeUser user fechas)
  (if (null? fechas)
      "ERROR CON ACTIVE USER\n"
      (if (eq? (user->username (car user)) (car(car fechas)))
           (~a ( ~a "Usuario activo: " (user->username (car user))) (~a (~a "\nFecha creación usuario : " (cdr(car fechas))) "\n"))
          (string->activeUser user (cdr fechas))))
  
  )




;(define listTest '(("pepe" (12 11 2021)) ("pepe3" (10 11 2020)) ("driques" (10 11 2020))))






