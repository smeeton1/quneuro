module jump
  implicit none
  
contains 

subroutine jmes(Q,Res)
  complex,dimension(:),intent(in) :: Q
  integer,intent(out)             :: Res
 
  

end subroutine



end module


program j_test
use jump
!use IFCORE
implicit none
complex,dimension(:),allocatable :: Q
integer                          :: R
Logical(4)                       :: S

S=PEEKCHARQQ()



end program