     program demo_indmed
     use M_indmed, only : indmed
     implicit none
     real,allocatable :: xdont(:)
     integer :: ii
        xdont=[80.0,70.0,20.0,10.0,1000.0]
        call indmed(xdont,ii)
        write(*,*) ii,xdont(ii)
        !
        xdont=[11, 22, 33, 44, 55, 66, 77, 88]
        call indmed(xdont,ii)
        write(*,*) ii,xdont(ii)
        !
        xdont=[11.0d0,77.0d0,22.0d0,66.0d0,33.0d0,88.0d0]
        call indmed(xdont,ii)
        write(*,*) ii,xdont(ii)
        !
     end program demo_indmed
