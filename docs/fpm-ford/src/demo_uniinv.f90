     program demo_uniinv
     use M_uniinv, only : uniinv
     implicit none
     character(len=*),parameter :: g='(*(g0,1x))'
     real,allocatable :: xdont(:)
        !
        xdont=[10.0, 5.0, 7.0, 1.0, 4.0, 5.0, 6.0, 8.0, 9.0, 10.0, 1.0]
        call printme()
        !
        xdont=[-1.0, 0.0, -1.0, 0.0, -1.0, 0.0, -1.0]
        call printme()
     contains
     subroutine printme()
     integer,allocatable :: igoest(:)
        write(*,g)'ORIGINAL:',xdont
        write(*,g)'NUMBER OF INDICES TO SORT:',size(xdont)
        if(allocated(igoest))deallocate(igoest)
        allocate(igoest(size(xdont)))
        call uniinv(xdont,igoest)
        write(*,g)'NUMBER OF INDICES RETURNED: ????'
        write(*,g)'RETURNED INDICES:',igoest(:)
        write(*,g)'SORTED VALUES:?????',xdont(igoest)
     end subroutine
     end program demo_uniinv
