#lang racket

;Representación
;'(DD MM AA)

;Constructor
;Funcion que construye una fecha
;Dominio: Enteros
;Recorrido: Lista fecha
(define (construFecha DD MM AA)
  (list DD MM AA)
  )

;Pertenencia
;Funcion que permite saber si se ingresa una fecha valida
;Dominio: lista de fecha
;Recorrido: Bool
(define (fecha? listaFecha)
  (if (null? listaFecha)
      #f
      (if(and (< 0 (car listaFecha))(> 32 (car listaFecha)))
         (if(and (< 0 (cadr listaFecha))(> 13 (cadr listaFecha)))
            (if (and (< 1900 (caddr listaFecha))(> 2100 (caddr listaFecha)))
                #t
                #f
             )
            #f
            )
         #f
         )
      )
  )

;Selectores

;Funcion selectora del dia de la fecha entregada
;Domino: lista de fecha
;Recorrido: Entero
(define (selectDia fecha)
  (if (fecha? fecha)
      (car fecha)
      null)
  )
;Funcion selectora del dia de la fecha entregada
;Domino: lista de fecha
;Recorrido: Entero
(define (selectMes fecha)
  (if (fecha? fecha)
      (cadr fecha)
      null)
  )

;Funcion selectora del dia de la fecha entregada
;Domino: lista de fecha
;Recorrido: Entero
(define (selectAnio fecha)
  (if (fecha? fecha)
      (caddr fecha)
      null)
  )




;Modificadores


;Exmple
(define testFecha (construFecha 21 5 2000))