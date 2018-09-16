module jump
  implicit none
  
contains 

subroutine jmes(Q,Res)
  complex,dimension(:),intent(inout) :: Q
  logical,intent(out)                :: Res
  real*8                             :: tsum,ran,ran1
  
  tsum=real(CONJG(Q(1))*Q(1)+CONJG(Q(2))*Q(2))*0.1
  ran = RAND(TIME())
  
  if(ran.lt.tsum)then
    ran1 = RAND(TIME())
    do while((ran.gt.real(CONJG(Q(1))*Q(1)).and.ran1.gt.real(CONJG(Q(2))*Q(2))))
      do while((ran.lt.real(CONJG(Q(1))*Q(1)).and.ran1.lt.real(CONJG(Q(2))*Q(2))))
        ran  = RAND(TIME())!
        ran1 = RAND(TIME())
      end do
    enddo
    
    if(ran.lt.real(CONJG(Q(1))*Q(1)))then
      Res=.true.
    else
      Res=.false.
    endif
    
    Q(1)=Q(1)+(-0.5*Q(1)+cmplx(0.0,1.0)*Q(2))*0.1
    Q(2)=Q(2)+(-0.5*Q(2)+cmplx(0.0,1.0)*Q(1))*0.1
    Q(1)=Q(1)/(CONJG(Q(1))*Q(1)+CONJG(Q(2))*Q(2))
    Q(2)=Q(2)/(CONJG(Q(1))*Q(1)+CONJG(Q(2))*Q(2))
  
  endif
  

end subroutine



end module


program j_test
use jump
!use IFCORE
implicit none
complex,dimension(:),allocatable :: Q
integer                          :: R
Logical(4)                       :: S

allocate(Q(2))
Q(1)=cmplx(3.0,0.0)
Q(2)=cmplx(1.0,0.0)
Q(1)=Q(1)/(CONJG(Q(1))*Q(1)+CONJG(Q(2))*Q(2))
Q(2)=Q(2)/(CONJG(Q(1))*Q(1)+CONJG(Q(2))*Q(2))

call jmes(Q,S)

end program