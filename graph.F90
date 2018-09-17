module graph
  implicit none
  
contains 

!open_node conatains a 1 for a open line and 0 for closed line make boulian array.
!set text colour red closed qubit and green open qubit.
!draw line by node 

subroutine clear()
integer   :: i

do i=0,50
  write(*,*)' '
enddo

end subroutine

subroutine draw_qubit(open_node,n)
logical,dimension(:),intent(in)    :: open_node
integer,intent(in)                 :: n
integer                            :: i

write(*,'(A)', ADVANCE = "NO")'      '

do i =1,n
  if(open_node(i))then
    write(*,'(A)', ADVANCE = "NO")'=   '
  else
    write(*,'(A)', ADVANCE = "NO")'=   '
  endif
enddo

end subroutine

subroutine Drawline(open_node,n)
logical,dimension(:),intent(in)    :: open_node
integer,intent(in)                 :: n
integer                            :: i

write(*,'(A)', ADVANCE = "NO")'      '

do i =1,n
  if(open_node(i))then
    write(*,'(A)', ADVANCE = "NO")'O---'
  else
    write(*,'(A)', ADVANCE = "NO")'O   '
  endif
enddo
    
end subroutine


subroutine draw_lineg(phi,open_node,n)
complex,dimension(:,:),intent(in)  :: phi
logical,dimension(:),intent(in)    :: open_node
integer,intent(in)                 :: n
integer                            :: i

call clear()

call draw_qubit(open_node,n)

write(*,*)' ' 

call Drawline(open_node,n)

do i=0,6
  write(*,*)' '
enddo

end subroutine

subroutine draw_graph(phi,A,open_node,n)
complex,dimension(:,:),intent(in)  :: phi
integer,dimension(:,:),intent(in)  :: A
logical,dimension(:),intent(in)    :: open_node
integer,intent(in)                 :: n
integer                            :: i


call clear()

call draw_qubit(open_node,n)

write(*,*)' ' 

call Drawline(open_node,n)

do i=0,6
  write(*,*)' '
enddo


end subroutine

end module




! program g_test
! use graph
! implicit none
! logical,dimension(:),allocatable  :: open_node
! integer                           :: n
! 
! n=4
! 
! allocate(open_node(n))
! 
! open_node(:)=.TRUE.
! open_node(3)=.false.
! 
! call clear()
! 
! call draw_qubit(open_node,n)
! 
! write(*,*)' ' 
! 
! call Drawline(open_node,n)
! 
! write(*,*)' '
! 
! 
! 
! end program