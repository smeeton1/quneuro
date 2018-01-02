!! check jump operators for measuremnet
!! for learning teach bell coherant and noon statescheck time against classical
!! also see about impliementation against qnot gates
!! check starting with thermal distrubution for qbits.

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

subroutine mix(phi)
 complex,dimention(:,:),intent(inout)::phi
 complex,dimention(2,2)::Coin
 complex::hold
 int:: i,n
 n=size(phi,1)
 Coin(1,1)=1/sqrt(2);Coin(1,2)=1/sqrt(2);Coin(2,1)=1/sqrt(2);Coin(2,2)=-1/sqrt(2)
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


allocate phi(n,2)
allocate qphi(n,2)



end program

