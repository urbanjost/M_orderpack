     program demo_unista
     use M_unista, only : unista
     implicit none
     character(len=*),parameter :: list= '(*(g0:,","))'
     real,allocatable :: xdont(:)
     integer :: nuni
        xdont=[4.4,3.3,3.3,3.3,2.2,1.1,3.3,4.4,5.5,3.3]
        print list,'ORIGINAL:',xdont
        print *
        call unista(xdont,nuni)
        xdont=xdont(:nuni)
        print list,'UNIQUE:',xdont
     end program demo_unista
