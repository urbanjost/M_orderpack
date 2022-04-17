     program demo_unipar
     use M_unipar, only : unipar
     implicit none
     character(len=*),parameter :: g='(*(g0,1x))'
     integer,allocatable :: xdont(:)
     integer,allocatable :: irngt(:)
     integer :: nord

     write(*,g)'If enough values are unique, will return NORD indices'
     if(allocated(irngt))deallocate(irngt)
     xdont=[10,5,7,1,4,5,6,8,9,10,1]
     nord=5
     allocate(irngt(nord))
     call printme()

     !BUG!write(*,g)'If not enough values are unique, will change NORD'
     !BUG!xdont=[-1,0,-1,0,-1,0,-1]
     !BUG!nord=5
     !BUG!if(allocated(irngt))deallocate(irngt)
     !BUG!allocate(irngt(nord))
     !BUG!call printme()
     contains
     subroutine printme()
        write(*,g)'ORIGINAL:',xdont
        write(*,g)'NUMBER OF INDICES TO SORT:',nord
        call unipar(xdont,irngt,nord)
        write(*,g)'NUMBER OF INDICES RETURNED:',nord
        write(*,g)'RETURNED INDICES:',irngt(:nord)
        write(*,g)nord,'SMALLEST UNIQUE VALUES:',xdont(irngt(:nord))
     end subroutine
     end program demo_unipar
