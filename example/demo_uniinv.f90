     program demo_uniinv
     use M_uniinv, only : uniinv
     implicit none
     character(len=*),parameter :: g='(*(g0,1x))'
     integer,allocatable :: xdont(:)
        ! all values unique
        xdont=[0,11,22,33,44,55,66,77,88,99]
        xdont=xdont(size(xdont):1:-1) ! reverse it
        call printme()
        ! duplicate values
        xdont=[-1.0, 0.0, -1.0, 0.0, -1.0, 0.0, -1.0]
        call printme()
        xdont=[10.0, 5.0, 7.0, 1.0, 4.0, 5.0, 6.0, 8.0, 9.0, 10.0, 1.0]
        call printme()
        xdont=[10.0,20.0,30.0,10.0,20.0,30.0,10.0,20.0,30.0]
        call printme()
     contains
     subroutine printme()
     integer,allocatable :: igoest(:)
     integer,allocatable :: out(:)
     integer :: imx
     integer :: i
        write(*,g)'Original:                 ',xdont
        write(*,g)'Number of indices to sort:',size(xdont)
        if(allocated(igoest))deallocate(igoest)
        allocate(igoest(size(xdont)))
        call uniinv(xdont,igoest)
        imx=maxval(igoest)
        write(*,g)'Returned Indices:         ',igoest(:)
        write(*,g)'Number of unique indices :',imx
        if(allocated(out))deallocate(out)
        allocate(out(imx))
        do i=1,size(xdont)
           out(igoest(i))=xdont(i)
        enddo
        write(*,g)'Sorted unique values:     ',out
        write(*,g)
     end subroutine
     end program demo_uniinv
