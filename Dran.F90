!testcode
program Dran
use LineTest
use jump
implicit none

complex*16, dimension(:,:), allocatable:: phi,qphi
logical,dimension(:), allocatable   :: open_node
integer::n,i,j
real::norm

n=10
allocate(phi(n,2))
allocate(qphi(n,2))
allocate(open_node(n))

open_node(:)=.false.
phi(:,:)=cmplx(0.0,0.0)
qphi(:,:)=cmplx(0.0,0.0)
phi(n/2,1)=1/sqrt(2.0);phi(n/2,2)=cmplx(0.0,1.0)/sqrt(2.0)
open_node(n/2)=.true.
open_node(n/2-1)=.true.
do j=1,n
 qphi(j,2)=1.0
enddo
qphi(n/2,1)=1.0
qphi(n/2-1,1)=1.0
qphi(n/2,2)=0.0
qphi(n/2-1,2)=0.0

OPEN(10, file='out.dat', status='REPLACE')
norm=0
do j=1,n
    write(10,'(A,2F8.4,A,2F8.4,A,2F8.4,A)')'(',phi(j,1),') (',phi(j,2),') (',(phi(j,1)+phi(j,2))*conjg((phi(j,1)+phi(j,2))),')'
    norm=norm+(phi(j,1)+phi(j,2))*conjg((phi(j,1)+phi(j,2)))
enddo
write(10,*)norm
write(10,*)' '
write(10,*)open_node
write(10,*)' '
do i=1,6
  call qinter(phi,qphi)
!  call LWmix(phi,open_node)
  call LWswap(phi,open_node)
  write(10,*)i
  !write(10,*)'pre interaction'
  norm=0
  do j=1,n
    write(10,'(A,2F8.4,A,2F8.4,A,2F8.4,A)')'(',phi(j,1),') (',phi(j,2),') (',(phi(j,1)+phi(j,2))*conjg((phi(j,1)+phi(j,2))),')'
    norm=norm+(phi(j,1)+phi(j,2))*conjg((phi(j,1)+phi(j,2)))
  enddo
  write(10,*)norm

!  call qinter(phi,qphi)
!   write(10,*)'pre measure'
!   do j=1,n
!     write(10,*)phi(j,1),phi(j,2),(phi(j,1)+phi(j,2))*conjg((phi(j,1)+phi(j,2)))
!   enddo
!   write(10,*)' '
  do j=1,n
    call jmes(qphi(i,:),open_node(j))
  enddo
  write(10,*)' '
  write(10,*)open_node
  write(10,*)' '
!   write(10,*)'pos interaction'
!   do j=1,n
!     write(10,*)phi(j,1),phi(j,2),(phi(j,1)+phi(j,2))*conjg((phi(j,1)+phi(j,2)))
!   enddo
!   write(10,*)' '
 enddo
close(10)

end program

