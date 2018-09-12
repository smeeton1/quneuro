module graph
  implicit none
  
contains 

!open_node conatains a 1 for a open line and 0 for closed line make boulian array.
!set text colour red closed qubit and green open qubit.
!draw line by node 

subroutine clear()

end subroutine


subroutine draw_line(phi,open_node,n)

end subroutine

subroutine draw_graph(phi,A,open_node,n)

end subroutine

subroutine draw_qubit(open_node,n)
do i =1,n
  if(open_node(i))then
    write(*,'(A)', ADVANCE = "NO")'[32m =   '
  else
    write(*,'(A)', ADVANCE = "NO")'[31m =   '
  endif
enddo

end subroutine

subroutine DrawOpenNode()
write(*,'(A)', ADVANCE = "NO")'[97m O---'

end subroutine

subroutine DrawClosedNode()
write(*,'(A)', ADVANCE = "NO")'[97m O   '

end subroutine

end module




program g_test
use graph
implicit none



end program