module density
  implicit none
  
  
  subroutine dens_build(a,b,D)
  complex(kdp),dimension(:,:),intent(inout)  :: D
  complex(kdp),dimension(:),intent(in)       :: a,a
  integer                                    :: i,j
  
  n=size(a)
  m=size(b)
  allocate(D(n,m))
  
  do i=1,n
    do j=1,m
      d(i,j)=a(i)*b(j)
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
  
  
 subroutine par_traceA(A,B)
  complex(kdp),dimension(:),intent(inout)  :: B
  complex(kdp),dimension(:,:),intent(in)     :: A
  integer                                    :: i,j,k,l,n,m
  
  n=size(B,1)
  m=size(A,1)
  B(:,:)=cmplx(0,0)
  
  do i=0,n-1
   do j=0,n-1
    do k=1,n
      B(i+1,j+1)=B(i+1,j+1)+A(i*n+k,j*n+k)
    enddo
   enddo
  enddo
  
 
 end subroutine
  
  
 subroutine par_traceB(A,B)
  complex(kdp),dimension(:,:),intent(inout)  :: B
  complex(kdp),dimension(:,:),intent(in)     :: A
  integer                                    :: i,j,k,l,n,m
  
  n=size(B,1)
  m=size(A,1)
  B(:,:)=cmplx(0,0)
  
  do i=1,n
   do j=1,n
    do k=0,n-1
      if(i.eq.j)then
       B(i,j)=B(i,j)+ A(i+k*n,j+(k*n))
      else if(i.gt.j)then
       B(i,j)=B(i,j)+ A((i-1)*n+k*n,(j-1)*n+(k*n))
      else
       B(i,j)=B(i,j)+ A((i-1)*n+k*n,j*n+(k*n))
      endif
    enddo
   enddo
  enddo
 
 end subroutine
  
end module

program K_test
use density
implicit none

complex, dimension(:,:), allocatable:: phi,qphi,D,a
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
