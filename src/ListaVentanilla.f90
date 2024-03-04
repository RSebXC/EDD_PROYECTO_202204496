MODULE VentanillaModule
  
    use ColaCliente
    use PilaImagenes
    use ColaImpresion
    use ListaClientesAtendidos
    IMPLICIT NONE
  
    TYPE, public :: VentanillaType
      INTEGER :: id
      TYPE(ListaClientes) :: Atendido
      TYPE(ColaImpresionType) :: ColaImp
      TYPE (Cliente) :: Client
      LOGICAL :: disponible = .true.
      LOGICAL :: colaImpresionEjecutada = .false.
      TYPE(PIMG):: pilita
      TYPE(VentanillaType), POINTER :: siguiente => null()
      integer :: contg,contp = 0
    END TYPE VentanillaType
  
    TYPE, public :: listaVentanillas
      TYPE(VentanillaType), POINTER :: tope => NULL()
    END TYPE listaVentanillas


  
    CONTAINS

  
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
          ventanilla%Client = clientess
          ventanilla%disponible = .false.
          write (*,'(A,A,A)') "Cliente ", trim(clientess%nombre), " se agrego con exito a la ventanilla"
          exit
        else
          ventanilla => ventanilla%siguiente
        END IF
        
      END DO
    END SUBROUTINE BuscarVentanillaDisponible
  
    
  subroutine AgregarIMG (listaVen)
    TYPE(listaVentanillas), INTENT(IN) :: listaVen
      TYPE(VentanillaType), POINTER :: ventanilla
      TYPE(Cliente), POINTER :: clientito
      ventanilla => listaVen%tope
    
      ! Buscar la primera ventanilla que no esté ocupada
      DO WHILE (ASSOCIATED(ventanilla) )
        if (.Not. ventanilla%disponible)then
        
            IF (ventanilla%contg <= ventanilla%Client%img_g) THEN
              CALL ventanilla%pilita%pushPila(1)
              ventanilla%contg = ventanilla%contg + 1
              WRITE(*, '(A, A, A)') 'Cliente ', TRIM(ventanilla%Client%nombre), ' agrego una imagen grande '

            ELSE IF (ventanilla%contp <= ventanilla%Client%img_p) THEN
                CALL ventanilla%pilita%pushPila(1)
                ventanilla%contp = ventanilla%contp + 1
                WRITE(*, '(A, A, A)') 'Cliente ', TRIM(ventanilla%Client%nombre), ' agrego una imagen pequena '

               else
              WRITE(*, '(A, A, A)') 'Cliente ', TRIM(ventanilla%Client%nombre), ' alcanzo el limite'
              ventanilla%disponible = .true.
              CALL ventanilla%pilita%popPila()
              CALL AgregarColaImpresion(ventanilla%ColaImp, ventanilla%Client%img_p, ventanilla%Client%img_g)
                WRITE(*,*) 'Las imagenes del Cliente: ', TRIM(ventanilla%Client%nombre), ' se agregaron a la cola de impresion'
              IF (ventanilla%colaImpresionEjecutada) THEN
                CALL AgregarClienteAtendido(ventanilla%Atendido, ventanilla%Client%id, &
                                            ventanilla%Client%nombre, ventanilla%Client%img_p, ventanilla%Client%img_g)

               ELSE
                ventanilla%colaImpresionEjecutada = .true.

              END IF
            END IF
        END IF
        ventanilla => ventanilla%siguiente
      END DO
  end  subroutine AgregarIMG
  
  END MODULE VentanillaModule
  
