MODULE PilaImagenes
    use ColaCliente
    IMPLICIT NONE
  
    TYPE, public :: NodoPila
        INTEGER :: idImagen
        TYPE(NodoPila), POINTER :: siguiente => null()
    END TYPE NodoPila
    
    TYPE, public :: PIMG

        TYPE(NodoPila), POINTER :: tope => NULL()
        contains
        procedure :: pushPila
        procedure :: popPila
    END TYPE PIMG
    CONTAINS
  
  
    subroutine pushPila(thispila, image)
      class(PIMG), intent(inout) :: thispila
      integer, intent(in) :: image
      type(NodoPila), pointer :: newNodo
  
      allocate(newNodo)
      newNodo%idImagen = image
      newNodo%siguiente => thispila%tope
      thispila%tope => newNodo
    end subroutine pushPila

    subroutine popPila(thispila)
      class(PIMG), intent(inout) :: thispila
      type(NodoPila), pointer :: temp

      do while (associated(thispila%tope))
          temp => thispila%tope
          thispila%tope => temp%siguiente
          deallocate(temp)
      end do
  end subroutine popPila
  
  END MODULE PilaImagenes