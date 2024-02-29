MODULE ListaEsperaModule
    IMPLICIT NONE

    TYPE, PUBLIC :: ImagenType
        INTEGER :: idImagen
        CHARACTER(10) :: tipoImagen  ! "Pequeña" o "Grande"
        TYPE(ImagenType), POINTER :: siguiente => NULL()
    END TYPE ImagenType

    TYPE, PUBLIC :: ListaImagenesType
        TYPE(ImagenType), POINTER :: primera => NULL()
        TYPE(ImagenType), POINTER :: ultima => NULL()
    END TYPE ListaImagenesType

    TYPE, PUBLIC :: ClienteType
        CHARACTER(50) :: nombre
        TYPE(ClienteType), POINTER :: siguiente => NULL()
        TYPE(ClienteType), POINTER :: anterior => NULL()
        TYPE(ListaImagenesType), POINTER :: listaImagenes => NULL()
    END TYPE ClienteType

    TYPE, PUBLIC :: ListaEsperaType
        TYPE(ClienteType), POINTER :: primero => NULL()
        TYPE(ClienteType), POINTER :: ultimo => NULL()
    END TYPE ListaEsperaType

    CONTAINS

    SUBROUTINE InicializarListaImagenes(listaImagenes)
        TYPE(ListaImagenesType), INTENT(OUT) :: listaImagenes
        listaImagenes%primera => NULL()
        listaImagenes%ultima => NULL()
    END SUBROUTINE InicializarListaImagenes

    SUBROUTINE AgregarImagen(listaImagenes, idImagen, tipoImagen)
        TYPE(ListaImagenesType), INTENT(INOUT) :: listaImagenes
        INTEGER, INTENT(IN) :: idImagen
        CHARACTER(10), INTENT(IN) :: tipoImagen
        TYPE(ImagenType), POINTER :: nuevaImagen

        ALLOCATE(nuevaImagen)
        nuevaImagen%idImagen = idImagen
        nuevaImagen%tipoImagen = tipoImagen
        nuevaImagen%siguiente => NULL()

        IF (ASSOCIATED(listaImagenes%primera)) THEN
            listaImagenes%ultima%siguiente => nuevaImagen
            nuevaImagen%siguiente => listaImagenes%primera
            listaImagenes%ultima => nuevaImagen
        ELSE
            listaImagenes%primera => nuevaImagen
            listaImagenes%ultima => nuevaImagen
            nuevaImagen%siguiente => nuevaImagen
        END IF
    END SUBROUTINE AgregarImagen

    SUBROUTINE EliminarListaImagenes(listaImagenes)
        TYPE(ListaImagenesType), INTENT(INOUT) :: listaImagenes
        TYPE(ImagenType), POINTER :: imagenActual, imagenSiguiente

        IF (ASSOCIATED(listaImagenes%primera)) THEN
            imagenActual => listaImagenes%primera
            DO WHILE (ASSOCIATED(imagenActual))
                imagenSiguiente => imagenActual%siguiente
                DEALLOCATE(imagenActual)
                imagenActual => imagenSiguiente
            END DO
            listaImagenes%primera => NULL()
            listaImagenes%ultima => NULL()
        END IF
    END SUBROUTINE EliminarListaImagenes

    SUBROUTINE InicializarListaEspera(listaEspera)
        TYPE(ListaEsperaType), INTENT(OUT) :: listaEspera
        listaEspera%primero => NULL()
        listaEspera%ultimo => NULL()
    END SUBROUTINE InicializarListaEspera

    SUBROUTINE AgregarCliente(listaEspera, nombre)
        TYPE(ListaEsperaType), INTENT(INOUT) :: listaEspera
        CHARACTER(50), INTENT(IN) :: nombre
        TYPE(ClienteType), POINTER :: nuevoCliente

        ALLOCATE(nuevoCliente)
        nuevoCliente%nombre = nombre
        nuevoCliente%siguiente => NULL()
        nuevoCliente%anterior => NULL()

        CALL InicializarListaImagenes(nuevoCliente%listaImagenes)

        IF (ASSOCIATED(listaEspera%primero)) THEN
            listaEspera%ultimo%siguiente => nuevoCliente
            nuevoCliente%siguiente => listaEspera%primero
            nuevoCliente%anterior => listaEspera%ultimo
            listaEspera%ultimo => nuevoCliente
            listaEspera%primero%anterior => listaEspera%ultimo
        ELSE
            listaEspera%primero => nuevoCliente
            listaEspera%ultimo => nuevoCliente
            nuevoCliente%siguiente => nuevoCliente
            nuevoCliente%anterior => nuevoCliente
        END IF
    END SUBROUTINE AgregarCliente

    SUBROUTINE EliminarCliente(listaEspera, cliente)
        TYPE(ListaEsperaType), INTENT(INOUT) :: listaEspera
        TYPE(ClienteType), POINTER :: cliente

        IF (ASSOCIATED(listaEspera%primero)) THEN
            IF (.NOT.ASSOCIATED(cliente%siguiente, cliente)) THEN
                cliente%anterior%siguiente => cliente%siguiente
                cliente%siguiente%anterior => cliente%anterior
                IF (ASSOCIATED(cliente, listaEspera%primero)) THEN
                    listaEspera%primero => cliente%siguiente
                ELSE IF (ASSOCIATED(cliente, listaEspera%ultimo)) THEN
                    listaEspera%ultimo => cliente%anterior
                END IF
            ELSE
                listaEspera%primero => NULL()
                listaEspera%ultimo => NULL()
            END IF

            CALL EliminarListaImagenes(cliente%listaImagenes)

            DEALLOCATE(cliente)
        END IF
    END SUBROUTINE EliminarCliente

    SUBROUTINE MostrarListaEspera(listaEspera)
        TYPE(ListaEsperaType), INTENT(IN) :: listaEspera
        TYPE(ClienteType), POINTER :: clienteActual
        TYPE(ImagenType), POINTER :: imagenActual

        IF (ASSOCIATED(listaEspera%primero)) THEN
            WRITE(*, '(A)') 'Lista de Clientes en Espera:'
            clienteActual => listaEspera%primero
            DO WHILE (ASSOCIATED(clienteActual))
                WRITE(*, '(A)') 'Cliente: ', TRIM(clienteActual%nombre)
                IF (ASSOCIATED(clienteActual%listaImagenes%primera)) THEN
                    imagenActual => clienteActual%listaImagenes%primera
                    DO WHILE (ASSOCIATED(imagenActual))
                        WRITE(*, '(A, I2, A)') '  Imagen: ', imagenActual%idImagen, ', Tipo: ', TRIM(imagenActual%tipoImagen)
                        imagenActual => imagenActual%siguiente
                    END DO
                ELSE
                    WRITE(*, '(A)') '  La lista de imágenes del cliente está vacía.'
                END IF
                clienteActual => clienteActual%siguiente
            END DO
        ELSE
            WRITE(*, '(A)') 'La lista de clientes en espera está vacía.'
        END IF
    END SUBROUTINE MostrarListaEspera

END MODULE ListaEsperaModule
