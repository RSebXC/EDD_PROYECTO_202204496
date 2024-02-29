module fortranRC
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Hello, fortranRC!"
  end subroutine say_hello
end module fortranRC
