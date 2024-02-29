MODULE PilaImg
  use ColaCliente

  use VentanillaModule
  IMPLICIT NONE

  INTEGER, PARAMETER :: LIMITE_IMGP = 3   ! Establece el límite para IMGP
  INTEGER, PARAMETER :: LIMITE_IMGG = 5   ! Establece el límite para IMGG

  CONTAINS

  SUBROUTINE LlamarSubrutina(tipoImagen, cliente)
    CHARACTER(*), INTENT(IN) :: tipoImagen
    TYPE(Cliente), INTENT(INOUT) :: client

    SELECT CASE (tipoImagen)
      CASE ('IMGP')
        CALL CrearNodoVentanilla(client, LIMITE_IMGP, tipoImagen)
      CASE ('IMGG')
        CALL CrearNodoVentanilla(client, LIMITE_IMGG, tipoImagen)
      CASE DEFAULT
    END SELECT
  END SUBROUTINE LlamarSubrutina

  SUBROUTINE CrearNodoVentanilla(cliente, limite, tipoImagen)
    TYPE(Cliente), INTENT(INOUT) :: cliente
    INTEGER, INTENT(IN) :: limite
    CHARACTER(*), INTENT(IN) :: tipoImagen

    IF (cliente%ContadorImg(tipoImagen) < limite) THEN
      ! Se crea el nodo solo si no se ha alcanzado el límite
      CALL Apilar(cliente%Ventanilla%pila, tipoImagen)
      cliente%ContadorImg(tipoImagen) = cliente%ContadorImg(tipoImagen) + 1
    ELSE
      WRITE(*, '(A, A, A)') 'Cliente ', TRIM(cliente%nombre), ' alcanzó el límite para ', tipoImagen
    END IF
  END SUBROUTINE CrearNodoVentanilla

END MODULE PilaImg


MODULE VentanillaModule
    use ColaCliente
    IMPLICIT NONE
  
    TYPE, public :: NodoPilaType
      INTEGER :: idImagen
      TYPE(NodoPilaType), POINTER :: siguiente => null()
    END TYPE NodoPilaType
  
    TYPE, public :: PilaType
      TYPE(NodoPilaType), POINTER :: tope => NULL()
    END TYPE PilaType
  
    TYPE, public :: VentanillaType
      INTEGER :: id
      TYPE (Cliente) :: Clientes
      LOGICAL :: disponible = .true.
      TYPE(PilaType):: pila
      TYPE(VentanillaType), POINTER :: siguiente => null()
    END TYPE VentanillaType
  
    TYPE, public :: listaVentanillas
      TYPE(VentanillaType), POINTER :: tope => NULL()
    END TYPE listaVentanillas


  
    CONTAINS
  
    SUBROUTINE InicializarPila(pila)
      TYPE(PilaType), INTENT(OUT) :: pila
      ALLOCATE(pila%tope)
      pila%tope => NULL()
    END SUBROUTINE InicializarPila
  
    SUBROUTINE Apilar(pila, idImagen)
      TYPE(PilaType), INTENT(INOUT) :: pila
      INTEGER, INTENT(IN) :: idImagen
      TYPE(NodoPilaType), POINTER :: nuevoNodo
      ALLOCATE(nuevoNodo)
      nuevoNodo%idImagen = idImagen
      nuevoNodo%siguiente => pila%tope
      pila%tope => nuevoNodo
    END SUBROUTINE Apilar
  
    SUBROUTINE Desapilar(pila)
      TYPE(PilaType), INTENT(INOUT) :: pila
      TYPE(NodoPilaType), POINTER :: nodoDesapilado
  
      IF (ASSOCIATED(pila%tope)) THEN
        nodoDesapilado => pila%tope
        pila%tope => pila%tope%siguiente
        DEALLOCATE(nodoDesapilado)
      END IF
    END SUBROUTINE Desapilar
  
    SUBROUTINE CrearVentanillas(lista, numVentanillas)
      TYPE(listaVentanillas), INTENT(INOUT) :: lista
      TYPE(VentanillaType), POINTER :: nuevaVentanilla, actual
      INTEGER, INTENT(IN) :: numVentanillas
      INTEGER :: i
  
      DO i = 1, numVentanillas
        
        ALLOCATE(nuevaVentanilla)
        nuevaVentanilla%id = i
        nuevaVentanilla%siguiente => null()
        
        if (ASSOCIATED(lista%tope)) then 
          actual => lista%tope
          do while(associated(actual%siguiente))
              actual => actual%siguiente
          end do
          actual%siguiente => nuevaVentanilla               
      else
          lista%tope => nuevaVentanilla
      end if
      write(*,'(A, I0)') 'Se creo la ventanilla ', nuevaVentanilla%id
      END DO
    END SUBROUTINE CrearVentanillas


    SUBROUTINE BuscarVentanillaDisponible(lista, clientess)
      TYPE(listaVentanillas), INTENT(IN) :: lista
      TYPE(VentanillaType), POINTER :: ventanilla
      TYPE(Cliente), INTENT(INOUT) :: clientess
      ventanilla => lista%tope
    
      ! Buscar la primera ventanilla que no esté ocupada
      DO WHILE (ASSOCIATED(ventanilla) )
        if (ventanilla%disponible)then
          ventanilla%Clientes = clientess
          ventanilla%disponible = .false.
          write (*,'(A,A,A)') "Cliente ", trim(clientess%nombre), "Se agrego con exito a la ventanilla"
          exit
        END IF
        ventanilla => ventanilla%siguiente
      END DO
    END SUBROUTINE BuscarVentanillaDisponible
    
    FUNCTION VentanillaOcupada(ventanilla) RESULT(ocupada)
      TYPE(VentanillaType), INTENT(IN) :: ventanilla
      LOGICAL :: ocupada
    
      ! Verificar si la ventanilla está ocupada (pila no vacía)
      ocupada = ASSOCIATED(ventanilla%pila%tope)
    END FUNCTION VentanillaOcupada
    
    
  
  END MODULE VentanillaModule
  
