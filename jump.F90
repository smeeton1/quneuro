module jump
  implicit none
  
contains 

subroutine jmes(Q,Res,dt)
  complex*16,dimension(:),intent(inout) :: Q
  logical,intent(out)                   :: Res
  real,intent(in)                       :: dt
  real*8                                :: tsum,ran,ran1
  integer                               :: t(12),n,i
  
  t(1)=TIME()
  
  tsum=real(CONJG(Q(1))*Q(1)+CONJG(Q(2))*Q(2))*dt
  call random_seed(PUT=t)
  call random_number(ran)! = RAND(t)

  
  if(ran.lt.tsum)then
    call random_number(ran1)! = RAND(t(1))
    do while((ran.gt.real(CONJG(Q(1))*Q(1)).and.ran1.gt.real(CONJG(Q(2))*Q(2))))
      do while((ran.lt.real(CONJG(Q(1))*Q(1)).and.ran1.lt.real(CONJG(Q(2))*Q(2))))
        call random_number(ran)!  = RAND(t(1))!
        call random_number(ran1)! = RAND(t(1))
      end do
    enddo
    
    if(ran.lt.real(CONJG(Q(1))*Q(1)))then
      Res=.true.
    else
      Res=.false.
    endif
    
    do i=1,10
     Q(1)=Q(1)+(-0.5*Q(1)+cmplx(0.0,1.0)*Q(2))*dt/10
     Q(2)=Q(2)+(-0.5*Q(2)+cmplx(0.0,1.0)*Q(1))*dt/10
     Q(1)=Q(1)/sqrt(CONJG(Q(1))*Q(1)+CONJG(Q(2))*Q(2))
     Q(2)=Q(2)/sqrt(CONJG(Q(1))*Q(1)+CONJG(Q(2))*Q(2))
    enddo
  endif
  

end subroutine



end module


! program j_test
! use jump
! !use IFCORE
! implicit none
! complex,dimension(:),allocatable :: Q
! integer                          :: R
! Logical(4)                       :: S
! 
! allocate(Q(2))
! Q(1)=cmplx(1/sqrt(2.0),0.0)
! Q(2)=cmplx(1/sqrt(2.0),0.0)
! Q(1)=Q(1)/(CONJG(Q(1))*Q(1)+CONJG(Q(2))*Q(2))
! Q(2)=Q(2)/(CONJG(Q(1))*Q(1)+CONJG(Q(2))*Q(2))
! S=.True.
! 
! Write(*,*)Q(1)
! Write(*,*)(CONJG(Q(1))*Q(1)+CONJG(Q(2))*Q(2))
! call jmes(Q,S)
! Write(*,*)S
! Write(*,*)(CONJG(Q(1))*Q(1)+CONJG(Q(2))*Q(2))
! 
! end program