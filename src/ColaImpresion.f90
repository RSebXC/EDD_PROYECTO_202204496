MODULE ColaImpresion
    IMPLICIT NONE

    TYPE, PUBLIC :: NodoImpresion
        INTEGER :: imgPequena
        INTEGER :: imgGrande
        TYPE(NodoImpresion), POINTER :: siguiente => NULL()
    END TYPE NodoImpresion

    TYPE, PUBLIC :: ColaImpresionType
        TYPE(NodoImpresion), POINTER :: frente => NULL()
        TYPE(NodoImpresion), POINTER :: final => NULL()
    END TYPE ColaImpresionType

    CONTAINS

    SUBROUTINE AgregarColaImpresion(thisColaImpresion, imgPequena, imgGrande)
        TYPE(ColaImpresionType), INTENT(INOUT) :: thisColaImpresion
        INTEGER, INTENT(IN) :: imgPequena, imgGrande
        TYPE(NodoImpresion), POINTER :: nuevoNodo

        ALLOCATE(nuevoNodo)
        nuevoNodo%imgPequena = imgPequena
        nuevoNodo%imgGrande = imgGrande
        nuevoNodo%siguiente => NULL()

        IF (.NOT. ASSOCIATED(thisColaImpresion%frente)) THEN
            thisColaImpresion%frente => nuevoNodo
            thisColaImpresion%final => nuevoNodo
        ELSE
            thisColaImpresion%final%siguiente => nuevoNodo
            thisColaImpresion%final => nuevoNodo
        END IF
    END SUBROUTINE AgregarColaImpresion

    SUBROUTINE VaciarColaImpresion(thisColaImpresion)
        TYPE(ColaImpresionType), INTENT(INOUT) :: thisColaImpresion
        TYPE(NodoImpresion), POINTER :: temp

        DO WHILE (ASSOCIATED(thisColaImpresion%frente))
            temp => thisColaImpresion%frente
            thisColaImpresion%frente => thisColaImpresion%frente%siguiente
            DEALLOCATE(temp)
        END DO

        thisColaImpresion%final => NULL()
    END SUBROUTINE VaciarColaImpresion

END MODULE ColaImpresion
