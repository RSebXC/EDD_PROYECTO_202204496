program MainProgram
    use ColaCliente
    use VentanillaModule
    use PilaImagenes
    use ColaImpresion

    implicit none
    type(ColaClientes) :: recep
    type(listaVentanillas) :: listaVentanilla
    
    ! Menú principal
    call MenuPrincipal()
    
contains

    

    subroutine MenuPrincipal()
        integer :: opcion

        do
            print *, "---Menu de la aplicacion---"
            print *, "1. Parametros iniciales"
            print *, "2. Ejecutar paso"
            print *, "3. Estado en memoria de las estructuras"
            print *, "4. Reportes"
            print *, "5. Acerca de (datos del estudiante)"
            print *, "6. Salir"
            print *, "---Seleccione una opcion: ---"
            read *, opcion

            select case (opcion)
                case (1)
                    call ParametrosIniciales()
                case (2)
                    call EjecutarPaso()
                case (3)
                    call EstadoMemoria()
                case (4)
                    call GenerarReportes()
                case (5)
                    call AcercaDe()
                case (6)
                    exit
                case default
                    print *, "Opcion no válida. Intente nuevamente."
            end select
        end do
    end subroutine MenuPrincipal

    
    subroutine ParametrosIniciales()
        integer :: subopcion
        character(len = 80) :: narchi
        integer :: nVen

        do
            print *, "---Parametros Iniciales---"
            print *, "1. Carga masiva de clientes"
            print *, "2. Cantidad de ventanillas"
            print *, "3. Volver al menu principal"
            print *, "---Seleccione una subopcion: ---"
            read *, subopcion

            select case (subopcion)
                case (1)
                    print *, "Escribe el nombre del archivo: "
                    read *, narchi
                    call CargarClientes(narchi,recep)  ! Utiliza la subrutina del módulo
                    print *, "Carga masiva de clientes ejecutada correctamente."
                case (2)
                    print *, "Escribe el numero de ventanillas: "
                    read *, nVen
                    call CrearVentanillas(listaVentanilla,nVen)
                case (3)
                    exit
                case default
                    print *, "Subopcion no valida. Intente nuevamente."
            end select
        end do
    end subroutine ParametrosIniciales
    

    

    subroutine ConfigurarVentanillas()
        ! Lógica para configurar la cantidad de ventanillas
    end subroutine ConfigurarVentanillas

    ! Implementa las otras subrutinas según tus necesidades

    subroutine EjecutarPaso()
        TYPE(VentanillaType), POINTER :: ventanilla
        TYPE(Cliente) :: clienteEliminado
        TYPE(ColaImpresionType) :: Colaimpre
        INTEGER :: idImagen,i

        ! Obtener el cliente de la cola
        clienteEliminado = PopCliente(recep)

        IF (.not. clienteEliminado%id == 0) THEN
            
            ! Obtener la ventanilla disponible
            CALL AgregarIMG(listaVentanilla)
            CALL BuscarVentanillaDisponible(listaVentanilla, clienteEliminado)
            CALL GenerarClientesAleatorios(recep)
            CALL VaciarColaImpresion(Colaimpre)
        END IF
    end subroutine EjecutarPaso

    subroutine EstadoMemoria()
        ! Muestra el estado actual de las estructuras
    end subroutine EstadoMemoria

    subroutine GenerarReportes()
    
    end subroutine GenerarReportes

    subroutine AcercaDe()
        write (*,*) "Rodrigo Sebastian Castro Aguilar"
        write (*,*) "202204496"
        write (*,*) "Laboratorio Estructura de Datos"
        write (*,*) "Quinto Semestre"


    end subroutine AcercaDe

end program MainProgram
