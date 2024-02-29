MODULE ClienteModule
    IMPLICIT NONE

    TYPE, PUBLIC :: ClienteType
        CHARACTER(50) :: nombre
        INTEGER :: ventanillaAtendida
        INTEGER :: numImagenesImpresas
        INTEGER :: pasosEnSistema
        TYPE(ClienteType), POINTER :: siguiente => NULL()
    END TYPE ClienteType

    TYPE, PUBLIC :: ListaClientesType
        TYPE(ClienteType), POINTER :: primero => NULL()
    END TYPE ListaClientesType

    CONTAINS

    SUBROUTINE AgregarCliente(listaClientes, nombre, ventanilla, numImagenes, pasosEnSistema)
        TYPE(ListaClientesType), POINTER :: listaClientes
        CHARACTER(50), INTENT(IN) :: nombre
        INTEGER, INTENT(IN) :: ventanilla, numImagenes, pasosEnSistema
        TYPE(ClienteType), POINTER :: nuevoCliente

        ALLOCATE(nuevoCliente)
        nuevoCliente%nombre = nombre
        nuevoCliente%ventanillaAtendida = ventanilla
        nuevoCliente%numImagenesImpresas = numImagenes
        nuevoCliente%pasosEnSistema = pasosEnSistema

        nuevoCliente%siguiente => listaClientes%primero
        listaClientes%primero => nuevoCliente
    END SUBROUTINE AgregarCliente

    SUBROUTINE MostrarListaClientes(listaClientes)
        TYPE(ListaClientesType), POINTER :: listaClientes
        TYPE(ClienteType), POINTER :: clienteActual

        IF (ASSOCIATED(listaClientes%primero)) THEN
            WRITE(*, '(A)') 'Lista de Clientes Atendidos:'
            clienteActual => listaClientes%primero
            DO WHILE (ASSOCIATED(clienteActual))
                WRITE(*, '(A, I2, A, I2, A, I2)') 'Cliente: ', TRIM(clienteActual%nombre), ', Ventanilla: ', &
                                                clienteActual%ventanillaAtendida, ', Imágenes: ', &
                                                clienteActual%numImagenesImpresas, ', Pasos en Sistema: ', &
                                                clienteActual%pasosEnSistema
                clienteActual => clienteActual%siguiente
            END DO
        ELSE
            WRITE(*, '(A)') 'La lista de clientes está vacía.'
        END IF
    END SUBROUTINE MostrarListaClientes

END MODULE ClienteModule
