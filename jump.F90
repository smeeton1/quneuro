module jump
  implicit none
  
contains 

subroutine jmes(Q,Res,dt)
  complex*16,dimension(:),intent(inout) :: Q
  logical,intent(out)                   :: Res
  real,intent(in)                       :: dt
  complex*16                            :: hold,hold2
  real*8                                :: tsum,ran,ran1
  integer                               :: t(12),n,i
  
  t(1)=TIME()
  ran=-1.0
  do while(ran.lt.0.000001)
   tsum=real(CONJG(Q(1))*Q(1)+CONJG(Q(2))*Q(2))*dt
   call random_seed(PUT=t)
   call random_number(ran)! = RAND(t)
  end do

  
  if(ran.lt.tsum)then
    call random_number(ran1)! = RAND(t(1))
    do while((ran.gt.real(CONJG(Q(1))*Q(1)).and.ran1.gt.real(CONJG(Q(2))*Q(2))))
      do while((ran.lt.real(CONJG(Q(1))*Q(1)).and.ran1.lt.real(CONJG(Q(2))*Q(2))))
       do while(ran.lt.0.000001.and.ran1.lt.0.000001)
        call random_number(ran)!  = RAND(t(1))!
        call random_number(ran1)! = RAND(t(1))
       end do
      end do
    enddo
    
    if(ran.lt.real(CONJG(Q(1))*Q(1)))then
      Res=.true.
    else
      Res=.false.
    endif
    
    do i=1,10
     hold=Q(1)
     hold2=Q(2)
     Q(1)=hold+(-0.5*hold+cmplx(0.0,1.0)*hold2)*dt/10
     Q(2)=hold2+(-0.5*hold2+cmplx(0.0,1.0)*hold)*dt/10
     hold=Q(1)
     hold2=Q(2)
     Q(1)=hold/sqrt(CONJG(hold)*hold+CONJG(hold2)*hold2)
     Q(2)=hold2/sqrt(CONJG(hold)*hold+CONJG(hold2)*hold2)
    enddo
  endif
  

end subroutine

subroutine qbit_ev(qbit,phi,s,dt)
  complex*16,dimension(:),intent(inout) :: qbit,phi
  real,intent(in)                       :: dt,s
  complex*8                             :: g,hold,hold2
  integer                               :: l,i


l=size(phi)
g=cmplx(0.0,0.0)
do i=1,l
  g=g+phi(i)
enddo

if(abs(real(g*conjg(g))).gt.0.00001)then
  hold=qbit(1)
  hold2=qbit(2)

  qbit(1)=cos(s*g*dt)!*hold
  qbit(2)=(hold2/abs(hold2*hold2))*sqrt(((cos(s*g*dt))**2-1)*abs(hold*hold))
else
  hold=qbit(1)
  hold2=qbit(2)

  qbit(1)=exp(-dt)*hold
  qbit(2)=(hold2/abs(hold2*hold2))*sqrt(1-(exp(-2.0*dt))*abs(hold*hold))
endif


end subroutine

end module


program j_test
use jump
!use IFCORE
implicit none
complex*16,dimension(:),allocatable :: Q,P
integer                          :: R
Logical(4)                       :: S

allocate(Q(2),P(2))
Q(1)=cmplx(1/sqrt(2.0),0.0)
Q(2)=cmplx(1/sqrt(2.0),0.0)
Q(1)=Q(1)/(CONJG(Q(1))*Q(1)+CONJG(Q(2))*Q(2))
Q(2)=Q(2)/(CONJG(Q(1))*Q(1)+CONJG(Q(2))*Q(2))
S=.True.
P(1)=cmplx(0.0,0.0)
P(2)=cmplx(0.0,0.0)
Write(*,*)Q(1),' ',Q(2)
Write(*,*)(CONJG(Q(1))*Q(1)+CONJG(Q(2))*Q(2))
call qbit_ev(Q,P,10.0,5.0) !jmes(Q,S)
Write(*,*)Q(1),' ',Q(2)
Write(*,*)(CONJG(Q(1))*Q(1)+CONJG(Q(2))*Q(2))

end program