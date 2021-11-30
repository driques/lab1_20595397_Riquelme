#lang racket
(provide (all-defined-out))
(require "TDA_ParadigmaDocs.rkt")
(require "TDA_Usuarios.rkt")
(require "TDA_Fecha.rkt")
(require "TDA_Access.rkt")


;Representación
;'(idDoc Autor Fecha Contenido NombreDoc)


;Constructor de nuevo documento.
;Dominio: string X string X Fecha X lista X string
;Recorrido : nuevoDoc
(define (nuevoDoc contenido autor date documentos nombreDoc pDocs)
   (if(string? contenido)
     (append documentos (list (list (newId documentos) autor date ((pDocs->encryptFn pDocs)contenido) nombreDoc)) )
     null
     )
  )
;Pertenencia
;Ímplementación función adicional para verificar que el documento seleccionado a compartir sea de la
;autoria del usuario logeado.
;Dominio: string X int X listaString
;Recorrido: Booleano
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

;Selector
;Función selectora del id del documento.
;Dominio:lista doc.
;Recorrido: int.
(define (docs->idDoc doc)
  (list-ref doc 0)
  )
;Función selectora del autor del documento.
;Dominio:lista doc.
;Recorrido: str.
(define (docs->selectAutor doc)
   (list-ref doc 1)
  )
;Función selectora de la fecha del documento.
;Dominio:lista doc.
;Recorrido: fecha.
(define (docs->selectDate doc)
   (list-ref doc 2)
  )
;Función selectora del content del documento.
;Dominio:lista doc.
;Recorrido: str.
(define (docs->selectContent doc)
   (list-ref doc 3)
  )
;Función selectora del nombre del documento.
;Dominio:lista doc.
;Recorrido: str.
(define (docs->selectNombreDoc doc)
   (list-ref doc 4)
  )
;Selector especial
;Sirve para seleccionar un documento de lista de documentos.
;Dominio:lista doc.
;Recorrido: lista.
(define (docs->selectDoc id documentos)
   (list-ref documentos (- id 1))
  )

;Modificador
;la siguiente función actualiza el doc a partir de los cambios ejercidos.
;Dom: intXstrXlist
;Rec: list
(define (actualizaDoc id contenido documentos)
     (reemplaza documentos id (list id
          (docs->selectAutor (docs->selectDoc id documentos))
          (docs->selectDate (docs->selectDoc id documentos))
          (~a contenido (docs->selectContent (docs->selectDoc id documentos)) )
          (docs->selectNombreDoc (docs->selectDoc id documentos))
          )
       )
  )


;Funciones adicionales
;Funcion que crea un nuevo id a partir de los dados.
;Dom: list
;Rec: int
(define (newId documentos)
  (if(null? documentos)
     1
     (+ (length documentos)  1)
     )
  )

;Funcion que reemplaza un doc dentro de la lista de docs.
;Dom: listXintXlist
;Rec: list
(define reemplaza (lambda (docs id docAct)
                    (if (null? docs)
                        '()
                        (if(=  (car (car docs)) id)
                           (cons docAct (reemplaza (cdr docs) id docAct))
                           (cons (car docs) (reemplaza (cdr docs) id docAct))
                        )
                    )
                    )
 )

;string->registerUser nos permite obtener un str comprensible de los users registrados.
;Dom: list
;Rec: str

(define (string->registerUser usersList )
     (if  (null? usersList)
          "-----------------------------------------------\n"
          (string-append (~a (~a "usuario registrado: "(car (user->username usersList))) "\n") (string->registerUser (cdr usersList)))
          )
  )

;string->docs nos permite obtener un str comprensible de los docs.
;Dom: listxFunction
;Rec: str
(define (string->docs docsList decryptFn)
  (if (null? docsList)
      "------------------------------------------------\n"
    (string-append (~a (~a (~a(~a(~a (~a "id Doc: "(docs->idDoc (car docsList))) "\n")
     (~a (~a "-> " (decryptFn (docs->selectContent (car docsList) ) )) "\n"))
       (~a (~a (~a "Propietario: " (docs->selectAutor (car docsList)) )"\n")))
        (~a (~a (~a (~a "Fecha creacion: " (getDia (docs->selectDate (car docsList))) ) (~a " de " (getMonthName (getMes (docs->selectDate (car docsList)))))) (~a " del " (getAgno (docs->selectDate (car docsList)))) ) "\n"))
                  "------------------------------------------------\n" )
        (string->docs (cdr docsList) decryptFn))
    )
  )


;allSameId es una funcion auxiliar de idVerString, permite obtener una lista con los id iguales.
;Dom: intXlist
;Rec: list

(define (allSameId idDoc historyList)
          (if (null? historyList)
              null
              (if (eq? idDoc (historialActive->idDoc historyList))
                  (append (list (car historyList)) (allSameId idDoc (cdr historyList)) )
                  (allSameId idDoc (cdr historyList)))
              )
  )
;idVerString permite obtener un str comprensible de los id versions.
;Dom: listXFunction
;Rec: str
(define (idVerString allSameId decryptFn)
  (if (null? allSameId)
      "------------------------------------------------\n"
      (~a
      (~a (~a (~a "id Version: " (historialActive->idVer allSameId)) "\n")
          (~a (~a "-> " (decryptFn (docs->selectContent (car (historialActive->docSelect allSameId))))) "\n"))
          (idVerString (cdr allSameId) decryptFn)
       )

      )
  )



;string->activeDocs nos permite obtener un str comprensible de los docs con un user activo.
;Dom: listXFunctionXstrXlistXlist
;Rec: str

(define (string->activeDocs docsList decryptFn activeUser histList accessList)
  (if (null? docsList)
      "------------------------------------------------\n"
    
   (if (eq? (docs->selectAutor (car docsList)) activeUser)
      (string-append (~a (~a (~a (~a(~a(~a (~a "id Doc: "(docs->idDoc (car docsList))) "\n")
     (~a (~a "-> " (decryptFn (docs->selectContent (car docsList) ) )) "\n"))
       (~a (~a (~a "Propietario: " (docs->selectAutor (car docsList)) )"\n")))
        (~a (~a (~a (~a "Fecha creacion: " (getDia (docs->selectDate (car docsList))) ) (~a " de " (getMonthName (getMes (docs->selectDate (car docsList))))))
                (~a " del " (getAgno (docs->selectDate (car docsList)))) ) "\n"))
                 (idVerString (allSameId (docs->idDoc (car docsList)) histList) decryptFn))
                 (if (null? accessList)
                     "Documento no compartido.\n"
                     (string->activeAccess accessList (docs->idDoc (car docsList)))
                     )
                 )
        (string->activeDocs (cdr docsList) decryptFn activeUser histList accessList))
      (string->activeDocs (cdr docsList) decryptFn activeUser histList accessList))
    )
  )
 
;Selector adicionales

;(Se recrean los selectores de historial, ya que caería en un loop en caso de que se requiera el TDA historial)

;Función selectora del id doc del historial.
;Dominio:lista.
;Recorrido: str.
(define (historialActive->idDoc historial)
  (car(car historial))
  )
;Función selectora del id ver del historial.
;Dominio:lista.
;Recorrido: str.
(define (historialActive->idVer historial)
  (car(cdr(car historial)))
  )
;Función selectora del doc del historial.
;Dominio:lista.
;Recorrido: str.
(define (historialActive->docSelect historial)
  (cddr(car historial))
  )



;Funcion que nos permite saber el numero de documento y el modo en el que se encuentra
;Dom: listXlistxFunction
;Rec: Str
(define (entregaDocs listaIDs docs decryptFn)
  (if (null? listaIDs)
      "\n"
      (~a (~a (~a (~a (~a "doc No.: " (car listaIDs)) (~a " en modo: " (cadr listaIDs)))" \n") (~a (decryptFn (recorreDocs (car listaIDs) docs))"\n") ) (entregaDocs (cddr listaIDs) docs decryptFn))
  )
 )

;Recorre los docs con igual id
;Dom: intXlist
;rec: str
(define (recorreDocs id docs)
  (if (null? docs)
      null
      (if (eq? id (docs->idDoc (car docs)) )
          (docs->selectContent (car docs))
          (recorreDocs id (cdr docs))
          )
      )
  )



;Test
;(define test1 (nuevoDoc "heyhey" "pepe" (fecha 10 12 2020) '() "docTest"))
;(define test2 (nuevoDoc "heyhey" "pepe" (fecha 10 12 2020) test1 "docTest"))
;(define listaTest '((1 "driques" (30 10 2020) "primer documento" "doc1") (2 "pepe" (30 10 2020) "segundooo documento" "doc2")))
;(define pDocs (paradigmaDocs "pDocs" '(20 11 2020) encryptFn encryptFn))

;(display (paradigmaDocs->string pDocs testAutoF) )