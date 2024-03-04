module ColaCliente
    use json_module
    implicit none

    

    type, public :: Cliente
        integer :: id
        character(50) :: nombre
        integer :: img_g
        integer :: img_p
    end type Cliente

    type, public :: NodoCliente
        type(Cliente) :: cliente
        type(NodoCliente), pointer :: siguiente => null()
    end type NodoCliente

    
    type, public :: ColaClientes
        type(NodoCliente), pointer :: inicio => null()
        type(NodoCliente), pointer :: final => null()
    end type ColaClientes
    ! Lee el archivo JSON y carga los clientes

contains

    subroutine CargarClientes(Narch, thisCola)
        type(json_file) :: json   ! Se declara una variable del tipo json_file
    type(json_value), pointer :: listPointer, personPointer, attributePointer  ! Se declaran punteros a variables del tipo json_value
    type(json_core) :: jsonc  ! Se declara una variable del tipo json_core para acceder a las funciones básicas de JSON
    character(:), allocatable :: nombre  ! Se declara una cadena de caracteres que se asignará dinámicamente

    integer :: i,id, size, imgP, imgG       ! Se declaran variables enteras
    logical :: found
        type(ColaClientes), intent(INOUT) :: thisCola 
        character(len=*), intent(in) :: Narch
                call json%initialize()    ! Se inicializa el módulo JSON
            call json%load(filename= Narch )  ! Se carga el archivo JSON llamado 'data.json'
            !call json%print()         ! Se imprime el contenido del archivo JSON (opcional
            
            call json%info('',n_children=size)

            call json%get_core(jsonc)               ! Se obtiene el núcleo JSON para acceder a sus funciones básicas
            call json%get('', listPointer, found)

            do i = 1, size                          ! Se inicia un bucle sobre el número de elementos en el JSON
                call jsonc%get_child(listPointer, i, personPointer, found = found)  ! Se obtiene el i-ésimo hijo de listPointer
                
                call jsonc%get_child(personPointer, 'id', attributePointer, found = found)
                if (found)then
                    call jsonc%get(attributePointer,id)
                end if

                call jsonc%get_child(personPointer, 'nombre', attributePointer, found = found)  ! Se obtiene el valor asociado con la clave 'nombre' del hijo actual
                if (found) then                      ! Si se encuentra el valor asociado con la clave 'nombre'
                    call jsonc%get(attributePointer, nombre)  ! Se obtiene el valor y se asigna a la variable 'nombre'
                    print *, trim(nombre)           ! Se imprime el nombre sin espacios en blanco adicionales
                end if

                call jsonc%get_child(personPointer, 'img_p', attributePointer, found = found)  ! Se obtiene el valor asociado con la clave 'nombre' del hijo actual
                
                
                if (found) then                      ! Si se encuentra el valor asociado con la clave 'nombre'
                    call jsonc%get(attributePointer, imgP)  ! Se obtiene el valor y se asigna a la variable 'nombre'
                    print *, imgP           ! Se imprime el nombre sin espacios en blanco adicionales
                end if

                call jsonc%get_child(personPointer, 'img_g', attributePointer, found = found)  ! Se obtiene el valor asociado con la clave 'nombre' del hijo actual

                if (found) then                      ! Si se encuentra el valor asociado con la clave 'nombre'
                    call jsonc%get(attributePointer, imgG)  ! Se obtiene el valor y se asigna a la variable 'nombre'
                    print *, imgG           ! Se imprime el nombre sin espacios en blanco adicionales
                end if

                call PushCliente(thisCola,id,nombre,imgP,imgG)

            end do

            call json%destroy()                   
    end subroutine CargarClientes

    subroutine PushCliente(cola_recepcion,id, nombre, img_p, img_g)
        integer, intent(in) :: id
        character(len=*), intent(in) :: nombre
        integer, intent(in) :: img_p, img_g
        TYPE(ColaClientes), INTENT(INOUT) :: cola_recepcion
        type(NodoCliente), pointer :: nuevoNodo
        allocate(nuevoNodo)

        nuevoNodo%cliente%id = id
        nuevoNodo%cliente%nombre = nombre
        nuevoNodo%cliente%img_p = img_p
        nuevoNodo%cliente%img_g = img_g
        nuevoNodo%siguiente => null()
        if (.not. associated(cola_recepcion%inicio)) then
            cola_recepcion%inicio => nuevoNodo
            cola_recepcion%final => nuevoNodo
        else
            cola_recepcion%final%siguiente => nuevoNodo
            cola_recepcion%final => nuevoNodo
        end if
    end subroutine PushCliente

    function PopCliente(ColaRecepcion) result(ClienteEliminado)
        type(ColaClientes), intent(INOUT) :: ColaRecepcion
        type(Cliente) :: ClienteEliminado
        type(NodoCliente), pointer :: temp

        if (associated(ColaRecepcion%inicio)) then
            ClienteEliminado = ColaRecepcion%inicio%cliente
            
            ! Mover el puntero head al siguiente nodo
            temp => ColaRecepcion%inicio
            ColaRecepcion%inicio => ColaRecepcion%inicio%siguiente
            
            ! Verificar si la cola está ahora vacía
            if (.not. associated(ColaRecepcion%inicio)) then
                ColaRecepcion%final => null()
            end if
            
            deallocate(temp)
        else
            return 
        end if

    end function PopCliente

    
    subroutine GenerarClientesAleatorios(thisCola)
        type(ColaClientes), intent(INOUT) :: thisCola
        integer :: i, id, img_p, img_g
        character(50) :: nombre_completo

        do i = 1, 3
            ! Generar un ID único que no exista en la cola
            id = GenerarIDUnico(thisCola)

            ! Generar nombre completo aleatorio
            call GenerarNombreCompleto(nombre_completo)

            ! Generar número aleatorio de imágenes pequeñas y grandes
            img_p = floor(rand()*10)  ! Número aleatorio entre 0 y 9
            img_g = floor(rand()*10)

            ! Agregar el cliente a la cola
            call PushCliente(thisCola, id, trim(nombre_completo), img_p, img_g)
            write (*,*) "El cliente ", trim(nombre_completo), " se anadio a la cola"
        end do
    end subroutine GenerarClientesAleatorios

    function GenerarIDUnico(thisCola) result(id_generado)
        type(ColaClientes), intent(IN) :: thisCola
        integer :: id_generado
        logical :: id_repetido

        do
            id_generado = floor(rand()*1000) + 1  ! Número aleatorio entre 1 y 1000
            id_repetido = BuscarIDRepetido(thisCola, id_generado)

            if (.not. id_repetido) exit  ! Salir del bucle si el ID no está repetido
        end do
    end function GenerarIDUnico

    subroutine GenerarNombreCompleto(nombre_completo)
        character(50), intent(out) :: nombre_completo
        integer :: indice_nombre, indice_apellido

        ! Conjunto de nombres y apellidos disponibles
        character(5), dimension(9) :: nombres = ["Juann", "Maria", "Carlo", "Laura", "Pedro","Jenny","Sebas","Edwar","Natyy"]
        character(8), dimension(8) :: apellidos = ["Gomez", "Lopez", "Perez", "Rodri", "Ferna","Castr","Gutie","laine"]

        ! Seleccionar aleatoriamente un nombre y un apellido
        indice_nombre = floor(rand()*size(nombres)) + 1
        indice_apellido = floor(rand()*size(apellidos)) + 1

        ! Concatenar el nombre y el apellido
        nombre_completo = trim(nombres(indice_nombre)) // " " // trim(apellidos(indice_apellido))
    end subroutine GenerarNombreCompleto

    logical function BuscarIDRepetido(thisCola, id)
        type(ColaClientes), intent(IN) :: thisCola
        integer, intent(in) :: id
        type(NodoCliente), pointer :: temp

        temp => thisCola%inicio
        do while (associated(temp))
            if (temp%cliente%id == id) then
                BuscarIDRepetido = .true.
                return
            end if
            temp => temp%siguiente
        end do

        BuscarIDRepetido = .false.
    end function BuscarIDRepetido

    
end module ColaCliente