!! check jump operators for measuremnet
!! for learning teach bell coherant and noon statescheck time against classical
!! also see about impliementation against qnot gates
!! check starting with thermal distrubution for qbits.


module LineTest
  use density
  implicit none

contains

! swap operator for a line quantum walker
subroutine LWswap(phi,open_node)
 complex*16,dimension(:,:),intent(inout)::phi
 logical,dimension(:), intent(in)       :: open_node
 integer                                :: i,n
 complex*16                             :: hold
 n=size(phi,1)
 do i=1,n-1
   if(open_node(i))then
     hold = phi(i,2)
     phi(i,2)=phi(i+1,1)
     phi(i+1,1)=hold
!    hold=phi(i,1)
!    phi(i,1)=phi(i-1,2)
!    phi(i-1,2)=hold
   endif
 enddo
!  hold=phi(n,2)
!  phi(n,2)=phi(1,1)
!  phi(1,1)=hold
end subroutine LWswap


! coin operator for line quantum walker
subroutine LWmix(phi,open_node)
 complex*16,dimension(:,:),intent(inout):: phi
 logical,dimension(:), intent(in)       :: open_node
 complex*16                             :: hold,hold1
 integer                                :: i,n
 n=size(phi,1)
 do i=2,n-1
  !if(open_node(i))then
    hold=phi(i,1)
    hold1=phi(i,2)
    phi(i,1)=1/sqrt(2.0)*hold+1/sqrt(2.0)*hold1
    phi(i,2)=1/sqrt(2.0)*hold-1/sqrt(2.0)*hold1
  !endif
 enddo
 !phi(n,:)=1/sqrt(2.0)*phi(n,:)
 

end subroutine LWmix

subroutine LWmix2(phi,open_node)
 complex*16,dimension(:,:),intent(inout):: phi
 logical,dimension(:), intent(in)       :: open_node
 complex*16                             :: hold,hold1
 integer                                :: i,n
 n=size(phi,1)
 do i=2,n-1
  !if(open_node(i))then
    hold=phi(i,1)
    hold1=phi(i,2)
    phi(i,1)=1/sqrt(2.0)*hold-cmplx(0,1/sqrt(2.0))*hold1
    phi(i,2)=-cmplx(0,1/sqrt(2.0))*hold+1/sqrt(2.0)*hold1
  !endif
 enddo
 !phi(n,:)=1/sqrt(2.0)*phi(n,:)
 

end subroutine LWmix2

subroutine qinter(phi,qphi)
complex*16,dimension(:,:),intent(inout)::phi
complex*16,dimension(:,:),intent(inout)::qphi
complex*16,dimension(:),allocatable    ::D
complex                                ::h1,h2,h3,h4
integer                                ::n,m,i
real                                   ::gamma
!interaction done qubit by qubit, q_n x phi
!then apply interaction to the node being looked at
n=size(phi,1)
m=size(phi,2)
gamma=0.1
!allocate(D(n*m*2))
!OPEN(11, file='Iout.dat', ACCESS='APPEND')
do i=1,n
  write(11,*)phi(i,:)
!   write(11,*)' '
!   write(11,*)qphi(i,:)
  if((real(phi(i,1)).ne.0).or.(real(phi(i,2)).ne.0).or.(aimag(phi(i,1)).ne.0).or.(aimag(phi(i,2)).ne.0))then
    allocate(D(n*m*2))
    call QWFphi_build(phi,qphi(i,:),D)
    !write(11,*)' '
    !write(11,*)i,(i-1)*4
    !write(11,*)D
    !write(11,*)' '
    h1=D(1+(i-1)*4);h2=D(2+(i-1)*4);h3=D(3+(i-1)*4);h4=D(4+(i-1)*4);
    D(1+(i-1)*4)=((1/sqrt(2.0))*h2+(1/sqrt(2.0))*h4)
    D(2+(i-1)*4)=((1/sqrt(2.0))*h1+(1/sqrt(2.0))*h3)
    D(3+(i-1)*4)=((1/sqrt(2.0))*h2-(1/sqrt(2.0))*h4)
    D(4+(i-1)*4)=((1/sqrt(2.0))*h1-(1/sqrt(2.0))*h3)
    !write(11,*)D(1+(i-1)*4),D(2+(i-1)*4),D(3+(i-1)*4),D(4+(i-1)*4)
  
    call QWFpar_traceA(phi,D)
    call par_traceB(qphi(i,:),D)
    !write(11,*)phi(i,:)
    !write(11,*)' '
    !write(11,*)qphi(i,:)
    !write(11,*)' '
    deallocate(D)
  endif

enddo
!write(11,*)'Done'
!write(11,*)' '
!close(11)
end subroutine qinter

subroutine qinter2(phi,qphi)
complex*16,dimension(:,:),intent(inout)::phi
complex*16,dimension(:,:),intent(inout)::qphi
complex*16,dimension(:),allocatable    ::D
complex                                ::h1,h2,h3,h4,norm
integer                                ::n,m,i,j,k
real                                   ::gamma
!interaction done qubit by qubit, q_n x phi
!then apply interaction to the node being looked at
n=size(phi,1)
m=size(phi,2)
gamma=0.1
!allocate(D(n*m*2))
!OPEN(11, file='Iout.dat', ACCESS='APPEND')
do i=2,2
!   write(11,*)phi(i,:)
!   write(11,*)' '
!   write(11,*)qphi(i,:)
  if((real(phi(i,1)).ne.0).or.(real(phi(i,2)).ne.0).or.(aimag(phi(i,1)).ne.0).or.(aimag(phi(i,2)).ne.0))then
    allocate(D(n*m*2))
    call QWFphi_build(phi,qphi(i,:),D)
!    write(11,*)' '
!    write(11,*)i,(i-1)*4
!    write(11,*)D
!    write(11,*)' '
    h1=D(1+(i-1)*4);h2=D(2+(i-1)*4);h3=D(3+(i-1)*4);h4=D(4+(i-1)*4);
    D(1+(i-1)*4)=((1/sqrt(2.0))*h2-cmplx(0,(1/sqrt(2.0)))*h4)
    D(2+(i-1)*4)=((1/sqrt(2.0))*h1-cmplx(0,(1/sqrt(2.0)))*h3)
    D(3+(i-1)*4)=(-cmplx(0,(1/sqrt(2.0)))*h2+(1/sqrt(2.0))*h4)
    D(4+(i-1)*4)=(-cmplx(0,(1/sqrt(2.0)))*h1+(1/sqrt(2.0))*h3)
!     write(11,*)D(1+(i-1)*4),D(2+(i-1)*4),D(3+(i-1)*4),D(4+(i-1)*4)
  
    call QWFpar_traceA(phi,D)
    call QWFpar_traceB(qphi(i,:),D,size(phi,2))
    norm=cmplx(0.0,0.0)
    do j=1,2
     norm=norm+qphi(i,j)*conjg(qphi(i,j))
    enddo
    qphi(i,:)=qphi(i,:)/sqrt(norm)
    norm=cmplx(0.0,0.0)
    do j=1,n
      do k=1,m
        norm=norm+phi(j,k)*conjg(phi(j,k))
      enddo
    enddo
    phi(:,:)=phi(:,:)/sqrt(norm)
!     write(11,*)'norm=',norm
!     write(11,*)phi(i,:)
!     write(11,*)' '
!     write(11,*)qphi(i,:)
!     write(11,*)' '
    deallocate(D)
  endif

enddo

! write(11,*)'Done'
! write(11,*)' '
! close(11)
end subroutine qinter2



end module




! program dran
! use LineTest
! use jump
! implicit none
! 
! complex, dimension(:,:), allocatable:: phi,qphi
! logical,dimension(:), allocatable   :: open_node
! integer::n,i,j
! 
! n=10
! allocate(phi(n,2))
! allocate(qphi(n,2))
! allocate(open_node(n))
! 
! open_node(:)=.false.
! phi(:,:)=cmplx(0.0,0.0)
! qphi(:,:)=cmplx(0.0,0.0)
! phi(n/2,1)=1/sqrt(2.0);phi(n/2,2)=cmplx(0.0,1.0)/sqrt(2.0)
! open_node(n/2)=.true.
! open_node(n/2-1)=.true.
! 
! OPEN(10, file='out.dat', status='REPLACE')
! do j=1,n
!     write(10,*)phi(j,1),phi(j,2),(phi(j,1)+phi(j,2))*conjg((phi(j,1)+phi(j,2)))
! enddo
! write(10,*)' '
! do i=1,3
!   call LWmix(phi)
!   call LWswap(phi,open_node)
!   call qinter(phi,qphi)
!   do j=1,n
!     call jmes(qphi(i,:),open_node(j))
!   enddo
! 
! 
!   do j=1,n
!     write(10,*)phi(j,1),phi(j,2),(phi(j,1)+phi(j,2))*conjg((phi(j,1)+phi(j,2)))
!   enddo
!   write(10,*)' '
! enddo
! close(10)
! 
! end program

