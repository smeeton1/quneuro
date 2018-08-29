module density
  implicit none
  
  
 subroutine dens_build(a,b,D)
  complex(kdp),dimension(:),intent(inout)    :: D
  complex(kdp),dimension(:),intent(in)       :: a,b
  integer                                    :: i,j,k
  
  n=size(a)
  m=size(b)
  allocate(D(n*m))
  k=1
  do i=1,n
    do j=1,m
      d(k)=a(i)*b(j)
      k=k+1
    enddo
  enddo
  
  
 end subroutine
  
  
 subroutine k_product(A,B,C)
  complex(kdp),dimension(:,:),intent(inout)  :: C
  complex(kdp),dimension(:,:),intent(in)     :: A,B
  integer                                    :: i,j,k,l,n,m
  
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
  complex(kdp),dimension(:),intent(inout)  :: A
  complex(kdp),dimension(:),intent(in)     :: D
  integer                                  :: i,j,k,l,n,m
  
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
  complex(kdp),dimension(:),intent(inout)  :: B
  complex(kdp),dimension(:),intent(in)     :: D
  integer                                  :: i,j,k,l,n,m
  
  n=size(B)
  m=size(D)
  l=m/n
  B(:)=cmplx(0,0)
  
  do i=1,l
    do j=1,n
      B(j)=B(j)+D(i+(j-1)*n)
     enddo
  enddo
  
 end subroutine
  
end module

program K_test
use density
implicit none

complex, dimension(:,:), allocatable:: phi,qphi
complex, dimension(:), allocatable:: D,a
integer::n,i,j

n=10
allocate(phi(n,2))
allocate(qphi(n,2))
phi(:,:)=cmplx(0.0,0.0)
qphi(:,:)=cmplx(0.0,0.0)
do i=1,n
  qphi(i,1)=cmplx(1.0,0.0)
  do j=1,2
    phi(i,j)=1/sqrt(2.0)
  enddo
enddo

call dens_build(phi(1,:),qphi(1,:),D)

call


end program
