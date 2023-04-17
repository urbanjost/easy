module easy__sample
implicit none
private
public :: mysqrt
public :: mysin
contains

elemental pure function mysqrt(r) result(answer)
!> MYSQRT(3f) is a trivial function for demonstration purposes.
!  It calls the intrinsic SQRT
real,intent(in) :: r
real            :: answer
   answer=sqrt(r)
end function mysqrt

elemental pure function mysin(r) result(answer)
!> MYSIN(3f) is a trivial function for demonstration purposes.
!  It calls the intrinsic SIN
real,intent(in) :: r
real            :: answer
   answer=sin(r)
end function mysin

end module easy__sample
