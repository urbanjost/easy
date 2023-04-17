program check
use easy__sample, only : mysqrt, mysin
implicit none
real,parameter :: PI=atan(1.0)*4.0

print *, "Put some tests in here!"
print *, epsilon(0.0)
print *, mysqrt(100.0)
print *, PI
! comparing reals
print *, mysin(PI).eq.0.0,mysin(PI),0.0
print *, mysin(PI/2.0).eq.1.0,mysin(PI/2.0),1.0
print *, mysin(PI/4.0)**2.eq.0.5, mysin(PI/4.0)**2,0.5
print *, mysin(0.0).eq.0, mysin(0.0),0
end program check
