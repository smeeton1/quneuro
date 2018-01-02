!! check jump operators for measuremnet
!! for learning teach bell coherant and noon statescheck time against classical
!! also see about impliementation against qnot gates
!! check starting with thermal distrubution for qbits.


! swap operator for a line quantum walker
subroutine swap(phi)
 complex,dimention(:,:),intent(inout)::phi
 int:: i,n
 complex:: swap
 n=size(phi,1)
 do i=2,n-1
   swap = phi(i,2)
   phi(i,2)=phi(i+1,1)
   phi(i+1,1)=swap
   swap=phi(i,1)
   phi(i,1)=phi(i-1,2)
   phi(i-1,2)=swap
 enddo

end subroutine


! coin operator for line quantum walker
subroutine mix(phi)
 complex,dimention(:,:),intent(inout)::phi
 complex::hold
 int:: i,n
 n=size(phi,1)
 do i=2,n-1
   hold=phi(i,1)
   phi(i,1)=1/sqrt(2)*hold+1/sqrt(2)*phi(i,2)
   phi(i,2)=1/sqrt(2)*hold-1/sqrt(2)*phi(i,2)
 enddo
 

end subroutine

subroutine qinter(phi,qphi)

end subroutine

subroutine qsole(qphi)

end subroutine

subroutine qjump(qphi)

end subroutine

program dran
complex, dimension(:,:), allocatable:: phi,qphi
integer::n

n=10
allocate phi(n,2)
allocate qphi(n,2)

phi(:,:)=cmplx(0.0,0.0)
qphi(:,:)=cmplx(0.0,0.0)
phi(n/2,1)=0.5/sqrt(2);phi(n/2,2)=0.5/sqrt(2)

call mix(phi)
call swap(phi)


end program

