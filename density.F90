module density
  implicit none
  
contains 
 
subroutine dens_build(a,b,D)
  complex*16,dimension(:),intent(inout)    :: D
  complex*16,dimension(:),intent(in)       :: a,b
  integer                                  :: i,j,k,n,m
  
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
  complex*16,dimension(:,:),intent(inout)  :: C
  complex*16,dimension(:,:),intent(in)     :: A,B
  integer                                  :: i,j,k,l,n,m
  
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
  complex*16,dimension(:),intent(inout)  :: A
  complex*16,dimension(:),intent(in)     :: D
  integer                                :: i,j,k,l,n,m
  
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
  complex*16,dimension(:),intent(inout)  :: B
  complex*16,dimension(:),intent(in)     :: D
  integer                                :: i,j,k,l,n,m
  
  n=size(B)
  m=size(D)
  l=m/n
  B(:)=cmplx(0,0)
  
  do i=1,n
    do j=1,l
      B(i)=B(i)+D((j-1)*l+i)
     enddo
  enddo
  
 end subroutine
 
 !for the qw 2d phi
 
subroutine QWFphi_build(a,b,D)
  complex*16,dimension(:),intent(inout)    :: D
  complex*16,dimension(:,:),intent(in)     :: a
  complex*16,dimension(:),intent(in)       :: b
  integer                                  :: i,j,k,n,m,l,s
  
  n=size(a,1)
  s=size(a,2)
  m=size(b)
  k=1
  do i=1,n
    do l=1,s
      do j=1,m
	d(k)=a(i,l)*b(j)
	k=k+1
      enddo
    enddo
  enddo
end subroutine
  

 
subroutine QWFpar_traceA(A,D)!set to work for the case A and B are the same size
  complex*16,dimension(:,:),intent(inout)  :: A
  complex*16,dimension(:),intent(in)       :: D
  integer                                  :: i,j,k,l,n,m,s
  
  n=size(D)
  m=size(A,1)
  s=size(A,2)
  l=n/(m*s)
  A(:,:)=cmplx(0,0)
  !write(*,*)n,m,s,l,m*s
  do i=1,m
    do j=1,s
      do k=1,l
        A(i,j)=A(i,j)+D(k+(i-1)*s*l+(j-1)*s)
      enddo
     enddo
  enddo

 end subroutine
  
  
subroutine QWFpar_traceB(B,D,e)!set to work for the case A and B are the same size
  complex*16,dimension(:),intent(inout)  :: B
  complex*16,dimension(:),intent(in)     :: D
  integer                                :: e
  integer                                :: i,j,k,l,n,m
  
  n=size(B)
  m=size(D)
  l=m/n
  B(:)=cmplx(0,0)
  
  do i=1,n
    do j=1,l
      B(i)=B(i)+D((j-1)*e+i)
     enddo
  enddo
  
 end subroutine
 
 
end module

! program K_test
! use density
! implicit none
! 
! complex, dimension(:,:), allocatable:: phi,qphi,C
! complex, dimension(:), allocatable:: D,a
! integer::n,i,j
! 
! n=7
! allocate(phi(n,2))
! allocate(qphi(n,2))
! phi(:,:)=cmplx(0.0,0.0)
! qphi(:,:)=cmplx(0.0,0.0)
! do i=1,n
!   qphi(i,1)=cmplx(0.5,0.0)
!   qphi(i,2)=cmplx(0.5,0.0)
!   
! 
! enddo
! phi(int(n/2),1)=cmplx(1/sqrt(2.0),-1/sqrt(2.0))
! 
! allocate(D(4*n))
! OPEN(10, file='out.dat', status='REPLACE')
! allocate(C(n*n,n*n))
! call k_product(phi,qphi,C)
! 
! ! write(10,*)phi(1,1),phi(1,2)
! ! write(10,*)phi(2,1),phi(2,2)
! ! write(10,*)' '
! ! write(10,*)qphi(1,1),qphi(1,2)
! ! write(10,*)qphi(2,1),qphi(2,2)
! ! write(10,*)' '
! ! write(10,*)C(1,1),C(1,2),C(1,3),C(1,4)
! ! write(10,*)C(2,1),C(2,2),C(2,3),C(2,4)
! ! write(10,*)C(3,1),C(3,2),C(3,3),C(3,4)
! ! write(10,*)C(4,1),C(4,2),C(4,3),C(4,4)
! ! write(10,*)' '
! 
! write(*,*)'k product done'
! do i=1,n
! write(10,*)phi(i,1),phi(i,2)
! enddo
! write(10,*)' '
! write(10,*)qphi(1,:)
! write(10,*)' '
! 
! call QWFphi_build(phi,qphi(1,:),D)
! write(*,*)'D build done'
! call QWFpar_traceA(phi,D)
! write(*,*)'A trace Done'
! call par_traceB(qphi(1,:),D)
! write(*,*)'B trace done'
! do i=1,n
! write(10,*)phi(i,1),phi(i,2)
! enddo
! write(10,*)' '
! write(10,*)qphi(1,:)
! write(10,*)' '
! write(10,*)D
! write(10,*)' '
! 
! 
! close(10)
! 
! end program
