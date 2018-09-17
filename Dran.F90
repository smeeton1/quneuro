!testcode
program Dran
use LineTest
use jump
implicit none

complex, dimension(:,:), allocatable:: phi,qphi
logical,dimension(:), allocatable   :: open_node
integer::n,i,j

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

OPEN(10, file='out.dat', status='REPLACE')
do j=1,n
    write(10,*)phi(j,1),phi(j,2),(phi(j,1)+phi(j,2))*conjg((phi(j,1)+phi(j,2)))
enddo
write(10,*)' '
do i=1,3
  call LWmix(phi)
  call LWswap(phi,open_node)
  call qinter(phi,qphi)
  do j=1,n
    call jmes(qphi(i,:),open_node(j))
  enddo


  do j=1,n
    write(10,*)phi(j,1),phi(j,2),(phi(j,1)+phi(j,2))*conjg((phi(j,1)+phi(j,2)))
  enddo
  write(10,*)' '
enddo
close(10)

end program

