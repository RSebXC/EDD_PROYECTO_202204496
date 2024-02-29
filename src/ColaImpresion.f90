MODULE ColaImpresionModule
    IMPLICIT NONE

    TYPE, PUBLIC :: ImagenType
        INTEGER :: idImagen
        CHARACTER(10) :: tipoImagen  ! "Pequeña" o "Grande"
        TYPE(ImagenType), POINTER :: siguiente => NULL()
    END TYPE ImagenType

    TYPE, PUBLIC :: ColaImpresionType
        TYPE(ImagenType), POINTER :: frente => NULL()
        TYPE(ImagenType), POINTER :: final => NULL()
    END TYPE ColaImpresionType

    CONTAINS

    SUBROUTINE Enqueue(cola, idImagen, tipoImagen)
        TYPE(ColaImpresionType), INTENT(INOUT) :: cola
        INTEGER, INTENT(IN) :: idImagen
        CHARACTER(10), INTENT(IN) :: tipoImagen
        TYPE(ImagenType), POINTER :: nuevaImagen

        ALLOCATE(nuevaImagen)
        nuevaImagen%idImagen = idImagen
        nuevaImagen%tipoImagen = tipoImagen
        nuevaImagen%siguiente => NULL()

        IF (ASSOCIATED(cola%frente)) THEN
            cola%final%siguiente => nuevaImagen
            cola%final => nuevaImagen
        ELSE
            cola%frente => nuevaImagen
            cola%final => nuevaImagen
        END IF
    END SUBROUTINE Enqueue

    FUNCTION Dequeue(cola) RESULT(imagen)
        TYPE(ColaImpresionType), INTENT(INOUT) :: cola
        TYPE(ImagenType), POINTER :: imagen

        IF (ASSOCIATED(cola%frente)) THEN
            imagen => cola%frente
            cola%frente => cola%frente%siguiente
            IF (.NOT. ASSOCIATED(cola%frente)) THEN
                cola%final => NULL()
            END IF
        ELSE
            imagen => NULL()
        END IF
    END FUNCTION Dequeue

    SUBROUTINE MostrarCola(cola)
        TYPE(ColaImpresionType), INTENT(IN) :: cola
        TYPE(ImagenType), POINTER :: imagenActual

        IF (ASSOCIATED(cola%frente)) THEN
            WRITE(*, '(A)') 'Cola de Impresión:'
            imagenActual => cola%frente
            DO WHILE (ASSOCIATED(imagenActual))
                WRITE(*, '(A, I2, A)') 'Imagen: ', imagenActual%idImagen, ', Tipo: ', TRIM(imagenActual%tipoImagen)
                imagenActual => imagenActual%siguiente
            END DO
        ELSE
            WRITE(*, '(A)') 'La cola de impresión está vacía.'
        END IF
    END SUBROUTINE MostrarCola

END MODULE ColaImpresionModule
