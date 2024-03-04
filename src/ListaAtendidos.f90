MODULE ListaClientesAtendidos
    IMPLICIT NONE

    TYPE :: ClienteAtendido
        INTEGER :: id
        CHARACTER(50) :: nombre
        INTEGER :: img_pe
        INTEGER :: img_gr
        TYPE(ClienteAtendido), POINTER :: siguiente => NULL()
    END TYPE ClienteAtendido

    TYPE :: ListaClientes
        TYPE(ClienteAtendido), POINTER :: inicio => NULL()
    END TYPE ListaClientes

CONTAINS

    SUBROUTINE AgregarClienteAtendido(thisLista, id, nombre, img_peq, img_gra)
        TYPE(ListaClientes), INTENT(INOUT) :: thisLista
        INTEGER, INTENT(IN) :: id, img_peq, img_gra
        CHARACTER(50), INTENT(IN) :: nombre
        TYPE(ClienteAtendido), POINTER :: nuevoCliente

        ALLOCATE(nuevoCliente)
        nuevoCliente%id = id
        nuevoCliente%nombre = nombre
        nuevoCliente%img_pe = img_peq
        nuevoCliente%img_gr = img_gra
        nuevoCliente%siguiente => thisLista%inicio
        thisLista%inicio => nuevoCliente
        write (*,*) "El cliente: ", TRIM(nombre), " ha sido atendido"
    END SUBROUTINE AgregarClienteAtendido


    SUBROUTINE OrdenarClientesPorImgGrande(thisLista)
        TYPE(ListaClientes), INTENT(INOUT) :: thisLista
        TYPE(ClienteAtendido), POINTER :: current, next
        LOGICAL :: swapped
        INTEGER :: temp_id
        CHARACTER(50) :: temp_nombre
        INTEGER :: temp_img_pe, temp_img_gr

        IF (.NOT. ASSOCIATED(thisLista%inicio)) THEN
            WRITE (*,*) "La lista de clientes está vacía."
            RETURN
        END IF

        swapped = .TRUE.
        current => thisLista%inicio

        DO WHILE (swapped)
            swapped = .FALSE.
            current => thisLista%inicio

            DO WHILE (ASSOCIATED(current%siguiente))
                next => current%siguiente

                IF (current%img_gr < next%img_gr) THEN
                    ! Swap data
                    temp_id = current%id
                    temp_nombre = current%nombre
                    temp_img_pe = current%img_pe
                    temp_img_gr = current%img_gr

                    current%id = next%id
                    current%nombre = next%nombre
                    current%img_pe = next%img_pe
                    current%img_gr = next%img_gr

                    next%id = temp_id
                    next%nombre = temp_nombre
                    next%img_pe = temp_img_pe
                    next%img_gr = temp_img_gr

                    swapped = .TRUE.
                END IF

                current => current%siguiente
            END DO
        END DO
    END SUBROUTINE OrdenarClientesPorImgGrande

    

END MODULE ListaClientesAtendidos
