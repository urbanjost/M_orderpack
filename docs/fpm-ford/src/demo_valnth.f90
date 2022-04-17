     program demo_valnth
     use M_valnth, only : valnth
     implicit none
     character(len=*),parameter :: list= '(*(g0:,", "))'
     real,parameter ::  xdont(*)=[1.1,20.20,3.3,10.10,5.5,4.4,2.2]
     integer :: i
        write(*,list) 'ORIGINAL:',xdont
        do i=1,size(xdont)
           write(*,list)i,valnth(xdont,i)
        enddo
     end program demo_valnth
