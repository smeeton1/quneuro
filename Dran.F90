!! check jump operators for measuremnet
!! for learning teach bell coherant and noon statescheck time against classical
!! also see about impliementation against qnot gates
!! check starting with thermal distrubution for qbits.


module LineTest
  implicit none

contains

! swap operator for a line quantum walker
subroutine LWswap(phi)
 complex,dimension(:,:),intent(inout)::phi
 integer:: i,n
 complex:: hold
 n=size(phi,1)
 do i=2,n-1
   hold = phi(i,2)
   phi(i,2)=phi(i+1,1)
   phi(i+1,1)=hold
   hold=phi(i,1)
   phi(i,1)=phi(i-1,2)
   phi(i-1,2)=hold
 enddo

end subroutine LWswap


! coin operator for line quantum walker
subroutine LWmix(phi)
 complex,dimension(:,:),intent(inout)::phi
 complex::hold
 integer:: i,n
 n=size(phi,1)
 do i=2,n-1
   hold=phi(i,1)
   phi(i,1)=1/sqrt(2.0)*hold+1/sqrt(2.0)*phi(i,2)
   phi(i,2)=1/sqrt(2.0)*hold-1/sqrt(2.0)*phi(i,2)
 enddo
 

end subroutine LWmix

subroutine qinter(phi,qphi)
complex,dimension(:,:),intent(inout)::phi
complex,dimension(:,:),intent(inout)::qphi

end subroutine qinter

subroutine qsole(qphi)
complex,dimension(:,:),intent(inout)::qphi

end subroutine qsole

subroutine qjump(qphi)
complex,dimension(:,:),intent(inout)::qphi

end subroutine qjump

end module

program dran
use LineTest
implicit none

complex, dimension(:,:), allocatable:: phi,qphi
integer::n

n=10
allocate(phi(n,2))
allocate(qphi(n,2))

phi(:,:)=cmplx(0.0,0.0)
qphi(:,:)=cmplx(0.0,0.0)
phi(n/2,1)=0.5/sqrt(2.0);phi(n/2,2)=0.5/sqrt(2.0)

call LWmix(phi)
call LWswap(phi)


end program

