     program demo_unirnk
     ! rank an array, with removal of duplicate entries.
     use M_unirnk, only : unirnk
     implicit none
     character(len=*),parameter :: g='(*(g0,1x))'
     integer,allocatable :: xvalt(:)
     !
     xvalt=[10,5,7,1,4,5,6,8,9,10,1]
     call printme()
     xvalt=[-1,0,-2,0,-3,0,-4]
     call printme()
     contains
     subroutine printme()
     integer,allocatable :: irngt(:)
     integer :: nuni
        if(allocated(irngt))deallocate(irngt)
        allocate(irngt(size(xvalt)))
        write(*,g)'ORIGINAL:',xvalt
        call unirnk(xvalt,irngt,nuni)
        write(*,g)'NUMBER OF UNIQUE INDICES:',nuni
        write(*,g)'RETURNED INDICES:',irngt(:nuni)
        write(*,g)'SORTED DATA:',xvalt(irngt(:nuni))
     end subroutine
     end program demo_unirnk
