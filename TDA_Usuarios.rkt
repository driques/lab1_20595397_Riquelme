#lang racket

;Representación
;'() <- TDA usuario vacio
;'("JuanSanchez" "contrasenia") <- TDA usuario compuesto por su usuario y contrasenia

;Constructores

;Función creadora de user
;Dominio: string-string
;Recorrido: lista vacía

(define (newUser user password)
  (list user password)
  )
;Pertenencia

;La siguiente funcion nos permite determinar si los datos introducidos son correctos
;Para la generacion de un nuevo usuario, se pedirá como minimo un usuario de 4 caracteres.
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



