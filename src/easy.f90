module easy
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Hello, easy!"
  end subroutine say_hello
end module easy
