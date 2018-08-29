module density
  implicit none
  
contains 
 
subroutine dens_build(a,b,D)
  complex,dimension(:),intent(inout)    :: D
  complex,dimension(:),intent(in)       :: a,b
  integer                               :: i,j,k,n,m
  
  n=size(a)
  m=size(b)
  
  k=1
  do i=1,n
    do j=1,m
      d(k)=a(i)*b(j)
      k=k+1
    enddo
  enddo
  
  
end subroutine
  
  
 subroutine k_product(A,B,C)
  complex,dimension(:,:),intent(inout)  :: C
  complex,dimension(:,:),intent(in)     :: A,B
  integer                               :: i,j,k,l,n,m
  
  n=size(A,1)
  m=size(C,1)
  
  do i=1,n
   do j=1,n
    do k=1,n
     do l=1,n
      C(n*(i-1)+k,n*(j-1)+l)=A(i,j)*B(k,l)
     enddo
    enddo
   enddo
  enddo
  
 
 end subroutine
  
  
 subroutine par_traceA(A,D)!set to work for the case A and B are the same size
  complex,dimension(:),intent(inout)  :: A
  complex,dimension(:),intent(in)     :: D
  integer                             :: i,j,k,l,n,m
  
  n=size(D)
  m=size(A)
  l=n/m
  A(:)=cmplx(0,0)
  
  do i=1,m
    do j=1,l
      A(i)=A(i)+D(j+(i-1)*m)
     enddo
  enddo

 end subroutine
  
  
 subroutine par_traceB(B,D)!set to work for the case A and B are the same size
  complex,dimension(:),intent(inout)  :: B
  complex,dimension(:),intent(in)     :: D
  integer                             :: i,j,k,l,n,m
  
  n=size(B)
  m=size(D)
  l=m/n
  B(:)=cmplx(0,0)
  
  do i=1,n
    do j=1,l
      B(i)=B(i)+D((j-1)*n+i)
     enddo
  enddo
  
 end subroutine
  
end module

program K_test
use density
implicit none

complex, dimension(:,:), allocatable:: phi,qphi,C
complex, dimension(:), allocatable:: D,a
integer::n,i,j

n=2
allocate(phi(n,2))
allocate(qphi(n,2))
phi(:,:)=cmplx(0.0,0.0)
qphi(:,:)=cmplx(0.0,0.0)
do i=1,n
  qphi(i,1)=cmplx(1.0,0.0)
  do j=1,2
    phi(i,j)=j/real(10)*sqrt(2.0)
  enddo
enddo
allocate(D(4))
OPEN(10, file='out.dat', status='REPLACE')
allocate(C(n*n,n*n))
call k_product(phi,qphi,C)

write(10,*)phi(1,1),phi(1,2)
write(10,*)phi(2,1),phi(2,2)
write(10,*)' '
write(10,*)qphi(1,1),qphi(1,2)
write(10,*)qphi(2,1),qphi(2,2)
write(10,*)' '
write(10,*)C(1,1),C(1,2),C(1,3),C(1,4)
write(10,*)C(2,1),C(2,2),C(2,3),C(2,4)
write(10,*)C(3,1),C(3,2),C(3,3),C(3,4)
write(10,*)C(4,1),C(4,2),C(4,3),C(4,4)
write(10,*)' '


write(10,*)phi(1,:),qphi(1,:)
call dens_build(phi(1,:),qphi(1,:),D)
call par_traceA(phi(1,:),D)
call par_traceB(qphi(1,:),D)
write(10,*)phi(1,:),qphi(1,:)
write(10,*)D
write(10,*)' '


close(10)

end program
